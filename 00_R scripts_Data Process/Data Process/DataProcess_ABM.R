# This file is part of ITHIM Sacramento.

# File: DataProcess_ABM.R
# Purpose: Process data from SACSIM - an activity based model develped by SACOG.
#          Obtain the active travel data for base year and future years

# Library definitions
library(foreign)

# Prevent scientific notation
options(scipen = 100)

# Set working drectory
setwd("/Users/Yizheng/Documents/02_Work/13_ITHIM/03_Data/07_SacSim")

# Part 1: Function Definition----------------------------------------------------------------------------

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
  pop$ID <- paste0('h',pop$SERIALNO,'p',pop$PNUM)
  return(pop)
}

# merge the datasets of population and trip
prepTripPop <- function(pop,triptable){
  
  # compute the mean travel time and travel distance per capita by mode
  aggr.by.mode <- aggregate(triptable[,c("TRAVTIME","TRAVDIST")],list(triptable$SAMPN,triptable$PERSN,triptable$MODE),sum)
  # rename the variables
  names(aggr.by.mode) <- c('SAMPN',"PERSN","MODE","TIME","DISTANCE")
  # sort the data by SAMPN, PERSN, and MODE
  aggr.by.mode <- aggr.by.mode[order(aggr.by.mode$SAMPN,aggr.by.mode$PERSN,aggr.by.mode$MODE),]
  
  # add a column "ID" for combining household id and person id
  aggr.by.mode$ID <- paste0('h',aggr.by.mode$SAMPN,'p',aggr.by.mode$PERSN)
  
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
  col.countyname.at<-matrix(c('ELD',rep("",35),'PLA',rep("",35),'SAC',rep("",35),'SUT',rep("",35),'YOL',rep("",35),'YUB',rep("",35)),216,1)
  col.countyname.pop<-matrix(c('ELD',rep("",8),'PLA',rep("",8),'SAC',rep("",8),'SUT',rep("",8),'YOL',rep("",8),'YUB',rep("",8)),54,1)
  final.format.at <- cbind(format.at,col.countyname.at)
  final.format.pop <- cbind(format.pop,col.countyname.pop)
  
  return(list(final.format.at = final.format.at,
              final.format.pop = final.format.pop))
}

#compute VMT by traffic mode for injury module
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


# Part 2: Data Processing---------------------------------------

# use read.dbf() in package 'foreign' to input the .dbf data
triptable.2012 <- read.dbf('trip_2012.dbf')
triptable.2020 <- read.dbf('trip_2020_am1.dbf')
triptable.2036 <- read.dbf('trip_2036.dbf')

# load the processed syn pop (after hot-deck imputation)
load('pop.2012.cmplt')
load('pop.2020.cmplt')
load('pop.2036.cmplt')

#recode the variables in population matrix
pop.2012 <- recode.pop(pop.2012.cmplt)
pop.2020 <- recode.pop(pop.2020.cmplt[,-c(24:27)]) #delete three redundant columns
pop.2036 <- recode.pop(pop.2036.cmplt)

#merge pop and trip data sets
trip.pop.2012 <- prepTripPop(pop.2012,triptable.2012)
trip.pop.2020 <- prepTripPop(pop.2020,triptable.2020)
trip.pop.2036 <- prepTripPop(pop.2036,triptable.2036)

#output the mean pop mean walk time and cycle time
# walk: mode = 9
# cycle: mode = 8
Pop.AT.para.2012 <- pop.mean.at.time(trip.pop.2012,pop.2012)
Pop.AT.para.2020 <- pop.mean.at.time(trip.pop.2020,pop.2020)
Pop.AT.para.2036 <- pop.mean.at.time(trip.pop.2036,pop.2036)

cuttingline <- matrix("",1,9)

# save the output (population mean active travel time by race) into .csv files
write.csv(rbind(
  cbind(Pop.AT.para.2012$Pop.AT.para.byRace,c("2012",rep("",5))),cuttingline,
  cbind(Pop.AT.para.2020$Pop.AT.para.byRace,c("2020",rep("",5))),cuttingline, 
  cbind(Pop.AT.para.2036$Pop.AT.para.byRace,c("2036",rep("",5)))
),file = "00_output/PopulationMeanATTimebyRace.csv")

# save the output (population mean active travel time by income) into .csv files
write.csv(rbind(
  cbind(Pop.AT.para.2012$Pop.AT.para.byIncome,c("2012",rep("",5))),cuttingline,
  cbind(Pop.AT.para.2020$Pop.AT.para.byIncome,c("2020",rep("",5))),cuttingline, 
  cbind(Pop.AT.para.2036$Pop.AT.para.byIncome,c("2036",rep("",5)))
),file = "00_output/PopulationMeanATTimebyIncome.csv")

# output the detail active transport information for each county and save them as .csv files
Travel.Output.byRace.2012 <- format.output(pop.2012,trip.pop.2012,'Race')
write.csv(Travel.Output.byRace.2012$final.format.at,file = '00_output/ActiveTransport_byRace.2012.csv')
write.csv(Travel.Output.byRace.2012$final.format.pop,file = '00_output/Population_byRace.2012.csv')

Travel.Output.byRace.2020 <- format.output(pop.2020,trip.pop.2020,'Race')
write.csv(Travel.Output.byRace.2020$final.format.at,file = '00_output/ActiveTransport_byRace.2020.csv')
write.csv(Travel.Output.byRace.2020$final.format.pop,file = '00_output/Population_byRace.2020.csv')

Travel.Output.byRace.2036 <- format.output(pop.2036,trip.pop.2036,'Race')
write.csv(Travel.Output.byRace.2036$final.format.at,file = '00_output/ActiveTransport_byRace.2036.csv')
write.csv(Travel.Output.byRace.2036$final.format.pop,file = '00_output/Population_byRace.2036.csv')

Travel.Output.byIncome.2012 <- format.output(pop.2012,trip.pop.2012,'Income')
write.csv(Travel.Output.byIncome.2012$final.format.at,file = '00_output/ActiveTransport_byIncome.2012.csv')
write.csv(Travel.Output.byIncome.2012$final.format.pop,file = '00_output/Population_byIncome.2012.csv')

Travel.Output.byIncome.2020 <- format.output(pop.2020,trip.pop.2020,'Income')
write.csv(Travel.Output.byIncome.2020$final.format.at,file = '00_output/ActiveTransport_byIncome.2020.csv')
write.csv(Travel.Output.byIncome.2020$final.format.pop,file = '00_output/Population_byIncome.2020.csv')

Travel.Output.byIncome.2036 <- format.output(pop.2036,trip.pop.2036,'Income')
write.csv(Travel.Output.byIncome.2036$final.format.at,file = '00_output/ActiveTransport_byIncome.2036.csv')
write.csv(Travel.Output.byIncome.2036$final.format.pop,file = '00_output/Population_byIncome.2036.csv')

#compute VMT by traffic mode for injury module (two races version)
vmt.baseline <- lapply(c(1:6),function(x) computeVMTbymode(x,trip.pop.2012,pop.2012))
vmt.2020 <- lapply(c(1:6),function(x) computeVMTbymode(x,trip.pop.2020,pop.2020))
vmt.2036 <- lapply(c(1:6),function(x) computeVMTbymode(x,trip.pop.2036,pop.2036))
names(vmt.baseline)<-names(vmt.2020)<-names(vmt.2036)<-c('ELD','PLA','SAC','SUT','YOL','YUB')

write.csv(vmt.baseline,file = '00_output/VMT_baseline.csv')
write.csv(vmt.2020,file = '00_output/VMT_2020.csv')
write.csv(vmt.2036,file = '00_output/VMT_2036.csv')



