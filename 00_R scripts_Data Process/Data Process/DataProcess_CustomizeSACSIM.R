# This file is part of ITHIM Sacramento.

# File: DataProcess_CustomizeSACSIM.R
# Purpose: process the customized SACSIM output into the shape of ITHIM input

# set working dictionary
# some files are larger than 100M that exceed the limit of GitHub
# Those data sets have been uploaded into the dropbox (/data/Large Files)
setwd("/Users/Yizheng/Documents/02_Work/13_ITHIM/03_Data")

##### library definition
library(StatMatch)
library(foreign)

# Part 1 HOT-DECK IMPUTATION -------------------------------------------------------------

# function definition
# in the output of SACSIM,the unit of location is TAZ
# here we extract the TAZ list for each county in order to obtain the county-level data
getTAZ <- function(parc.input){
  ELD.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="ELD")])
  PLA.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="PLA")])
  SAC.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="SAC")])
  SUT.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="SUT")])
  YOL.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="YOL")])
  YUB.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="YUB")])
  
  return(list(
    ELD.TAZ = ELD.TAZ,
    PLA.TAZ = PLA.TAZ,
    SAC.TAZ = SAC.TAZ,
    SUT.TAZ = SUT.TAZ,
    YOL.TAZ = YOL.TAZ,
    YUB.TAZ = YUB.TAZ
  ))
}

# function of recode countyID and household income ID
recode <- function(pop,taz){
  #recode countyID (only 5 countyID since Sutter and Yuba are counted as one based on Puma No. in PUMS data)
  pop$countyID <- ifelse(pop$HHTAZ%in%taz$ELD.TAZ,1,
                         ifelse(pop$HHTAZ%in%taz$PLA.TAZ,2,
                                ifelse(pop$HHTAZ%in%taz$SAC.TAZ,3,
                                       ifelse(pop$HHTAZ%in%c(taz$SUT.TAZ,taz$YUB.TAZ),4,
                                              ifelse(pop$HHTAZ%in%taz$YOL.TAZ,5,99)))))
  # recode household income ID based on quartiles
  # 1: household income < 25%
  # 2: household income 25-50%
  # 3: household income 50-75%
  # 4: household income 75-100%
  pop$hincID<-
    ifelse(pop$HINC<=quantile(pop$HINC)[2],1,
           ifelse(pop$HINC<=quantile(pop$HINC)[3],2,
                  ifelse(pop$HINC<=quantile(pop$HINC)[4],3,4)))
  
  # rename the variable "age" in order to match it with PUMA 
  names(pop)[16]<-"AGEP"
  
  return(pop)
}

# function to apply hot deck imputation 
hotdeck.imputation <- function(pop){
  # matching variable - age
  X.mtc <- c("AGEP")
  
  # store the result
  pop.cmplt<-NULL
  
  # "for" loop to do the Hot-Deck imputation
  for (i in 1:5){ #countyID
    #print(i)
    for (j in 1:2){ #gender
      #print(j)
      for (k in 1:4){ #income
        #print(k)
        
        rnd <- RANDwNND.hotdeck(data.rec = pop[which(pop$countyID==i&pop$SEX==j&pop$hincID==k),],data.don = pums.h.p[which(pums.h.p$countyID==i&pums.h.p$SEX==j&pums.h.p$hincID==k),],weight.don = "PWGTP",match.vars = X.mtc)
        output.temp <- create.fused(data.rec = pop[which(pop$countyID==i&pop$SEX==j&pop$hincID==k),],data.don = pums.h.p[which(pums.h.p$countyID==i&pums.h.p$SEX==j&pums.h.p$hincID==k),],mtc.ids = rnd$mtc.ids,z.vars = "raceID")
        
        pop.cmplt <- rbind(pop.cmplt,output.temp)
      }
      
    }
  }
  return(pop.cmplt)
}

# update the countyID
update.countyID <- function(pop.cmplt,taz){
  #recode countyID (from 5 "counties" to 6 counties)
  pop.cmplt$countyID <- ifelse(pop.cmplt$HHTAZ%in%taz$ELD.TAZ,1,
                               ifelse(pop.cmplt$HHTAZ%in%taz$PLA.TAZ,2,
                                      ifelse(pop.cmplt$HHTAZ%in%taz$SAC.TAZ,3,
                                             ifelse(pop.cmplt$HHTAZ%in%taz$SUT.TAZ,4,
                                                    ifelse(pop.cmplt$HHTAZ%in%taz$YOL.TAZ,5,6)))))
  return(pop.cmplt)
}


# data process for PUMS data
# input the data from PUMS
pums.h <- read.csv("07_SacSim/PUMS/csv_hca_12/ss12hca.csv") #household data sets
pums.p <- read.csv("07_SacSim/PUMS/csv_pca_12/ss12pca.csv") # populatin data sets

# keep key variables in each record
keep.pums.p <- c("SERIALNO","PUMA","AGEP","PWGTP","SEX","SCHL","RAC1P","HISP")
pums.p.c <- pums.p[,names(pums.p)%in%keep.pums.p]

keep.pums.h <- c("SERIALNO","HINCP")
pums.h.c <- pums.h[,names(pums.h)%in%keep.pums.h]

# merge two data sets by "SERIALNO"
pums.h.p <- merge(pums.p.c,pums.h.c,by.x = "SERIALNO",by.y = "SERIALNO")
#head(pums.h.p)

# define puma no. for six counties
# source: https://www.census.gov/geo/maps-data/maps/2010puma/st06_ca.html
pumano.eld <- 1700         #countyID=1
pumano.pla <- c(6101:6103) #countyID=2
pumano.sac <- c(6701:6712) #countyID=3
pumano.sut.yub <- 10100    #countyID=4 include two counties (Sutter and Yuba)
pumano.yol <- 11300        #countyID=5

# recode county ID
pums.h.p$countyID <- ifelse(pums.h.p$PUMA==pumano.eld,1,
                            ifelse(pums.h.p$PUMA%in%pumano.pla,2,
                                   ifelse(pums.h.p$PUMA%in%pumano.sac,3,
                                          ifelse(pums.h.p$PUMA==pumano.sut.yub,4,
                                                 ifelse(pums.h.p$PUMA==pumano.yol,5,99)))))

# recode race ID
# 1: non-Hispanic White
# 2: non-Hispanic Black
# 3: non-Hispanic Other Races 
# 4: Hispanic Races
# 99: unknown
pums.h.p$raceID <- ifelse(pums.h.p$HISP==1&pums.h.p$RAC1P==1,1,
                          ifelse(pums.h.p$HISP==1&pums.h.p$RAC1P==2,2,
                                 ifelse(pums.h.p$HISP==1,3,
                                        ifelse(pums.h.p$HISP!=1,4,99))))

#recode age and sex ID (age.sex.ID)
pums.h.p$age.sex.ID <- ifelse(pums.h.p$AGEP<=4,1,
                              ifelse(pums.h.p$AGEP<=14,2,
                                     ifelse(pums.h.p$AGEP<=29,3,
                                            ifelse(pums.h.p$AGEP<=44,4,
                                                   ifelse(pums.h.p$AGEP<=59,5,
                                                          ifelse(pums.h.p$AGEP<=69,6,
                                                                 ifelse(pums.h.p$AGEP<=79,7,8
                                                                 )))))))

#recode educational level ID (eduID)
# 1: 8th grade or less
# 2: 9th through 12th grade
# 3: high school graduate / ged completed
# 4: some college credit but no degree
# 5: associate degree
# 6: bachelor's
# 7: master's
# 8: doctoral degree / professional degree
# 9: unknown
pums.h.p$eduID <- ifelse(pums.h.p$SCHL%in%c(1:11),1,
                         ifelse(pums.h.p$SCHL%in%c(12:15),2,
                                ifelse(pums.h.p$SCHL%in%c(16:17),3,
                                       ifelse(pums.h.p$SCHL%in%c(18,19),4,
                                              ifelse(pums.h.p$SCHL==20,5,
                                                     ifelse(pums.h.p$SCHL==21,6,
                                                            ifelse(pums.h.p$SCHL==22,7,
                                                                   ifelse(pums.h.p$SCHL%in%c(23,24),8,99))))))))
# add a column of "1"
pums.h.p$ones <- 1

# recode hincID
# get the quantiles for the household income in all six counties
svy.pums <- svydesign(ids = ~1,weights = ~PWGTP,data = pums.h.p)
inc.quan <- svyquantile(~HINCP,subset(svy.pums,countyID%in%c(1:5)&is.na(HINCP)==FALSE),c(0.25,0.5,0.75))

# define the income group
# 1: household income < 25%
# 2: household income 25-50%
# 3: household income 50-75%
# 4: household income 75-100%
pums.h.p$hincID <- ifelse(pums.h.p$HINCP<=inc.quan[1],1,
                          ifelse(pums.h.p$HINCP<=inc.quan[2],2,
                                 ifelse(pums.h.p$HINCP<=inc.quan[3],3,4)))

# Part 2 Process the ABM output -------------------------------------------------------------

# calculate the relative values (relative to the value of "female 15-29" according to ITHIM)
CalRelativeMatrix <- function(x){
  for (i in 1:4){
    # if the value in the reference cell equals to 0, we use a small number(0.01) to avoid NA.
    if (x[3,2*i]==0){
      ref = 0.01
      x[,c((2*i-1):(2*i))] <- x[,c((2*i-1):(2*i))]/ref
      x[3,2*i] <- 1
    }else{
      ref <- x[3,2*i]
      x[,c((2*i-1):(2*i))] <- x[,c((2*i-1):(2*i))]/ref
    }
  }
  return(x)
}

# recode the population data (adding the group ID of age, race, income)
recode.pop <- function(pop){
  # recode age category
  pop$ageID <- 
    ifelse(pop$AGE<=4,1,
           ifelse(pop$AGE<=14,2,
                  ifelse(pop$AGE<=29,3,
                         ifelse(pop$AGE<=44,4,
                                ifelse(pop$AGE<=59,5,
                                       ifelse(pop$AGE<=69,6,
                                              ifelse(pop$AGE<=79,7,8)))))))
  
  # recode ID for the combination of gender and income
  # 1. male + income <= 25%
  # 2. female + income <= 25%
  # 3. male + income <= 50%
  # 4. female + income <= 50%
  # 5. male + income <= 75%
  # 6. female + income <= 75%
  # 7. male + income > 75%
  # 8. female + income >75%
  pop$gender.inc<-
    ifelse(pop$SEX==1&pop$HINC<quantile(pop$HINC)[2],1,
           ifelse(pop$SEX==2&pop$HINC<quantile(pop$HINC)[2],2,
                  ifelse(pop$SEX==1&pop$HINC<quantile(pop$HINC)[3],3,
                         ifelse(pop$SEX==2&pop$HINC<quantile(pop$HINC)[3],4,
                                ifelse(pop$SEX==1&pop$HINC<quantile(pop$HINC)[4],5,
                                       ifelse(pop$SEX==2&pop$HINC<quantile(pop$HINC)[4],6,
                                              ifelse(pop$SEX==1,7,8)))))))
  
  # recode ID for the combination of gender and income
  # 1. male + NHW
  # 2. female + NHW
  # 3. male + NHB
  # 4. female + NHB
  # 5. male + NHO
  # 6. female + NHO
  # 7. male + HO
  # 8. female + HO
  pop$gender.race<-
    ifelse(pop$SEX==1&pop$raceID==1,1,
           ifelse(pop$SEX==2&pop$raceID==1,2,
                  ifelse(pop$SEX==1&pop$raceID==2,3,
                         ifelse(pop$SEX==2&pop$raceID==2,4,
                                ifelse(pop$SEX==1&pop$raceID==3,5,
                                       ifelse(pop$SEX==2&pop$raceID==3,6,
                                              ifelse(pop$SEX==1&pop$raceID==4,7,8)))))))
  
  # add a column "ID"  for combining household id and person id
  pop$ID <- paste0("h",pop$SERIALNO,"p",pop$PNUM)
  return(pop)
}

# merge the datasets of population and trip
prepTripPop <- function(pop,triptable){
  
  # compute the mean travel time and travel distance per capita by mode
  aggr.by.mode <- aggregate(triptable[,c("TRAVTIME","TRAVDIST")],list(triptable$SAMPN,triptable$PERSN,triptable$MODE),sum)
  # rename the variables
  names(aggr.by.mode) <- c("SAMPN","PERSN","MODE","TIME","DISTANCE")
  # sort the data by SAMPN, PERSN, and MODE
  aggr.by.mode <- aggr.by.mode[order(aggr.by.mode$SAMPN,aggr.by.mode$PERSN,aggr.by.mode$MODE),]
  
  # add a column "ID" for combining household id and person id
  aggr.by.mode$ID <- paste0("h",aggr.by.mode$SAMPN,"p",aggr.by.mode$PERSN)
  
  # merge two data sets by ID
  trip.pop <- merge(aggr.by.mode,pop,by.x = "ID",by.y = "ID")
  
  return(trip.pop)
}

#compute the population mean walking and cycing time (min/week)
pop.mean.at.time <- function(trip.pop,pop){
  
  #numeric matrix for active travel data
  Pop.AT.para.byRace <- Pop.AT.para.byIncome <- matrix(NA,nrow = 6,ncol = 8,
                                                       dimnames = list(paste0("county",c(1:6)),c("demogr1_walk","demogr1_cycle","demogr2_walk","demogr2_cycle","demogr3_walk","demogr3_cycle","demogr4_walk","demogr4_cycle"))) 
  
  for(i in 1:6){#county ID
    
    for (j in 1:4){#demographic group
      #population mean walking time (MODE=9) for each race group (unit: min/week)
      Pop.AT.para.byRace[i,j*2-1]=sum(trip.pop$TIME[which(trip.pop$MODE==9&trip.pop$countyID==i&trip.pop$raceID==j)])/length(which(pop$countyID==i&pop$raceID==j))*7
      #population mean cycling time (MODE=8) for each race group (unit: min/week)
      Pop.AT.para.byRace[i,2*j]=sum(trip.pop$TIME[which(trip.pop$MODE==8&trip.pop$countyID==i&trip.pop$raceID==j)])/length(which(pop$countyID==i&pop$raceID==j))*7
      
      #population mean walking time (MODE=9) for each income group (unit: min/week)
      Pop.AT.para.byIncome[i,j*2-1]=sum(trip.pop$TIME[which(trip.pop$MODE==9&trip.pop$countyID==i&trip.pop$hincID==j)])/length(which(pop$countyID==i&pop$hincID==j))*7
      #population mean cycling time (MODE=8) for each income group (unit: min/week)
      Pop.AT.para.byIncome[i,2*j]=sum(trip.pop$TIME[which(trip.pop$MODE==8&trip.pop$countyID==i&trip.pop$hincID==j)])/length(which(pop$countyID==i&pop$hincID==j))*7
      
    }
    
  }
  
  return(list(
    Pop.AT.para.byRace=Pop.AT.para.byRace,
    Pop.AT.para.byIncome=Pop.AT.para.byIncome
  ))
}

# process the data and compute the active travel information
ActiveTravelDataOutput <- function(pop,trip.pop,countyID,Demo){
  
  # numeric matrix for travel time
  travel.times.by.demo <-travel.distance.by.demo <- matrix(nrow = 72, ncol = 8)
  
  # numeric matrix for population distribution
  pop.age.gender.demo <- matrix(nrow=8,ncol = 8)
  
  # set the demographic ID
  demo.ID<- ifelse(Demo=="Race",demo.ID<- 35,
                   ifelse(Demo=="Income", demo.ID <- 34,
                          message("Please type 'Race' or 'Income'")))
  
  
  for (i in 1:8){ #gender.demo
    print(i)
    for (j in 1:8){ #age
      print(j)
      
      # extract the population distribution
      pop.temp <- nrow(pop[which(pop[demo.ID-6]==i&pop$ageID==j&pop$countyID%in%countyID),])
      pop.age.gender.demo[j,i]<-pop.temp
      
      # compute the travel time and travel distance
      for (k in 1:9){ #mode
        time.temp <- sum(trip.pop$TIME[which(trip.pop$ageID==j&trip.pop[demo.ID]==i&trip.pop$MODE==k&trip.pop$countyID%in%countyID)])
        travel.times.by.demo[j + 8 * (k - 1), i] <- time.temp
        
        distance.temp <- sum(trip.pop$DISTANCE[which(trip.pop$ageID==j&trip.pop[demo.ID]==i&trip.pop$MODE==k&trip.pop$countyID%in%countyID)])
        travel.distance.by.demo[j + 8 * (k - 1), i] <- distance.temp
      }
    }
  }
  
  # compute the travel time and distance per capita (if pop=0,then the output=0)
  walk.time.byDemo <- ifelse(pop.age.gender.demo==0,0,travel.times.by.demo[65:72,]/pop.age.gender.demo)
  walk.distance.byDemo <- ifelse(pop.age.gender.demo==0,0,travel.distance.by.demo[65:72,]/pop.age.gender.demo)
  cycle.time.byDemo <- ifelse(pop.age.gender.demo==0,0,travel.times.by.demo[57:64,]/pop.age.gender.demo)
  cycle.distance.byDemo <- ifelse(pop.age.gender.demo==0,0,travel.distance.by.demo[57:64,]/pop.age.gender.demo)
  
  # compute the relative values
  re.walk.time.byDemo <- CalRelativeMatrix(walk.time.byDemo)
  re.walk.distance.byDemo <- CalRelativeMatrix(walk.distance.byDemo)
  re.cycle.time.byDemo <- CalRelativeMatrix(cycle.time.byDemo)
  re.cycle.distance.byDemo <- CalRelativeMatrix(cycle.distance.byDemo)
  
  # compute the speed (mph) 
  walk.speed.byDemo <- walk.distance.byDemo/(walk.time.byDemo/60)
  walk.speed.byDemo[is.na(walk.speed.byDemo)] <- 0
  cycle.speed.byDemo <- cycle.distance.byDemo/(cycle.time.byDemo/60)
  cycle.speed.byDemo[is.na(cycle.speed.byDemo)] <- 0
  
  # compute the relative value for speed
  re.walk.speed.byDemo <- CalRelativeMatrix(walk.speed.byDemo)
  re.cycle.speed.byDemo <- CalRelativeMatrix(cycle.speed.byDemo)
  
  # add dimnames
  temp.gender <- rep(c("male","female"),4)
  dimname <- list(c(paste0("ageCat",1:8)),paste0(temp.gender,".Demogr.",rep(1:4,each=2)))
  dimnames(re.walk.time.byDemo)<-dimnames(re.cycle.time.byDemo)<-
    dimnames(re.walk.speed.byDemo)<-dimnames(re.cycle.speed.byDemo)<-dimnames(pop.age.gender.demo)<-dimname
  
  return(list(
    travel.distance.by.demo = travel.distance.by.demo,
    travel.times.by.demo = travel.times.by.demo,
    pop.age.gender.demo = pop.age.gender.demo,
    re.walk.time.byDemo = re.walk.time.byDemo,
    re.cycle.time.byDemo = re.cycle.time.byDemo,
    re.walk.speed.byDemo = re.walk.speed.byDemo,
    re.cycle.speed.byDemo = re.cycle.speed.byDemo
  ))
  
}

# function of shaping the outout file
format.output <- function(pop,trip.pop,demogr){
  format.at <- NULL
  format.pop <- NULL
  
  for (i in 1:6){ # countyID
    # compute the active travel data
    Travel.Output.byDemo <- ActiveTravelDataOutput(pop,trip.pop,countyID = i,demogr)
    
    format.at <- rbind(format.at,
                       cbind(Travel.Output.byDemo$re.walk.time.byDemo,c("relative.walk.time",rep("",7))),cuttingline, #relative walking time
                       cbind(Travel.Output.byDemo$re.cycle.time.byDemo,c("relative.cycle.time",rep("",7))),cuttingline, # relative cycling time
                       cbind(Travel.Output.byDemo$re.walk.speed.byDemo,c("relative.walk.speed",rep("",7))),cuttingline, # relative walking speed
                       cbind(Travel.Output.byDemo$re.cycle.speed.byDemo,c("relative.cycle.speed",rep("",7))),cuttingline # relative cycling speed
    )
    
    # obtain the population 
    format.pop <- rbind(format.pop,Travel.Output.byDemo$pop.age.gender.demo[,1:8],cuttingline[1:8])
  }
  # add a column for county names
  col.countyname.at<-matrix(c("ELD",rep("",35),"PLA",rep("",35),"SAC",rep("",35),
                              "SUT",rep("",35),"YOL",rep("",35),"YUB",rep("",35)),216,1)
  col.countyname.pop<-matrix(c("ELD",rep("",8),"PLA",rep("",8),"SAC",rep("",8),
                               "SUT",rep("",8),"YOL",rep("",8),"YUB",rep("",8)),54,1)
  final.format.at <- cbind(format.at,col.countyname.at)
  final.format.pop <- cbind(format.pop,col.countyname.pop)
  
  return(list(final.format.at = final.format.at,
              final.format.pop = final.format.pop))
}

#compute VMT by traffic mode for injury module (need further discussion)
computeVMTbymode <- function(countyID,trip.pop,pop){
  ModeNames <- c("bike","walk","motorcycle","car","truck","bus")
  mode.vmt.byrace2 <- matrix(NA,nrow = 6,ncol = 2,dimnames = list(ModeNames,c("NHW","Other")))
  
  for (i in 1:2){# 1=NHW; 2=other three
    if (i==1){
      pop.race <- length(which(pop$countyID==countyID&pop$raceID==i))
      
      #bike
      mode.vmt.byrace2[1,i] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==8&trip.pop$countyID==countyID&trip.pop$raceID==i)])/pop.race
      #walk
      mode.vmt.byrace2[2,i] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==9&trip.pop$countyID==countyID&trip.pop$raceID==i)])/pop.race
      
      vmt <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==5&trip.pop$countyID==countyID&trip.pop$raceID==i)])/(3.5*pop.race)+sum(trip.pop$DISTANCE[which(trip.pop$MODE==6&trip.pop$countyID==countyID&trip.pop$raceID==i)])/(2*pop.race)+
        sum(trip.pop$DISTANCE[which(trip.pop$MODE==7&trip.pop$countyID==countyID&trip.pop$raceID==i)])/(pop.race)
    }else{
      pop.race <- length(which(pop$countyID==countyID&pop$raceID%in%c(2:4)))
      
      #bike
      mode.vmt.byrace2[1,i] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==8&trip.pop$countyID==countyID&trip.pop$raceID%in%c(2:4))])/pop.race
      #walk
      mode.vmt.byrace2[2,i] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==9&trip.pop$countyID==countyID&trip.pop$raceID%in%c(2:4))])/pop.race
      
      vmt <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==5&trip.pop$countyID==countyID&trip.pop$raceID%in%c(2:4))])/(3.5*pop.race)+sum(trip.pop$DISTANCE[which(trip.pop$MODE==6&trip.pop$countyID==countyID&trip.pop$raceID%in%c(2:4))])/(2*pop.race)+
        sum(trip.pop$DISTANCE[which(trip.pop$MODE==7&trip.pop$countyID==countyID&trip.pop$raceID%in%c(2:4))])/(pop.race)
    }
    
    #motorcycle
    mode.vmt.byrace2[3,i]<-vmt*0.02
    
    #car
    mode.vmt.byrace2[4,i]<-vmt*0.9
    
    #truck
    mode.vmt.byrace2[5,i]<-vmt*0.06
    
    #bus
    mode.vmt.byrace2[6,i]<-vmt*0.02
  }
  
  return(mode.vmt.byrace2)
}

# Part 3 Calculation Example -------------------------------------------------------------

# read the data (here, 2012 output is selected as an example)
pop.customized <- read.dbf("07_SacSim/2012_pop_parc_AM1/2012_pop.dbf") # pop - population
parc.customized <- read.dbf("07_SacSim/2012_pop_parc_AM1/2012_parc.dbf") # parc - parcel information 

# use read.dbf() in package "foreign" to input the .dbf data
triptable.customized <- read.dbf("07_SacSim/trip_2012.dbf")

# extrac the taz list for each county
taz.customized <- getTAZ(parc.customized)

# recode countyID (1:5) and household income ID
pop.customized <- recode(pop.customized,taz.customized)

# apply the function of hot deck imputation 
pop.customized.cmplt <- hotdeck.imputation(pop.customized)

# update the countyID from 5 counties to 6 counties
pop.customized.cmplt <- update.countyID(pop.customized.cmplt,taz.customized)

#recode the variables in population matrix
pop.customized <- recode.pop(pop.customized.cmplt)

#merge pop and trip data sets
trip.pop.customized <- prepTripPop(pop.customized,triptable.customized)

#output the mean pop mean walk time and cycle time
Pop.AT.para.customized <- pop.mean.at.time(trip.pop.customized,pop.customized)

#cuttingline <- matrix("",1,9)

# save the output (population mean active travel time by race) into .csv files
write.csv(Pop.AT.para.customized$Pop.AT.para.byRace,file = "00_output/PopulationMeanATTimebyRace.csv")

# save the output (population mean active travel time by income) into .csv files
write.csv(Pop.AT.para.customized$Pop.AT.para.byIncome,file = "00_output/PopulationMeanATTimebyIncome.csv")

# output the detail active transport information for each county and save them as .csv files
# by race
Travel.Output.byRace.customized <- format.output(pop.customized,trip.pop.customized,"Race")
write.csv(Travel.Output.byRace.customized$final.format.at,file = "00_output/ActiveTransport_byRace.customized.csv")
write.csv(Travel.Output.byRace.customized$final.format.pop,file = "00_output/Population_byRace.customized.csv")

#by income
Travel.Output.byIncome.customized <- format.output(pop.customized,trip.pop.customized,"Income")
write.csv(Travel.Output.byIncome.customized$final.format.at,file = "00_output/ActiveTransport_byIncome.customized.csv")
write.csv(Travel.Output.byIncome.customized$final.format.pop,file = "00_output/Population_byIncome.customized.csv")

# compute VMT by traffic mode for injury module (two races version)
vmt.customized <- lapply(c(1:6),function(x) computeVMTbymode(x,trip.pop.customized,pop.customized))
names(vmt.customized)<-c("ELD","PLA","SAC","SUT","YOL","YUB")

write.csv(vmt.customized,file = "00_output/VMT_customized.csv")

