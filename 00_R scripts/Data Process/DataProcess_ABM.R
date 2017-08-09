# Author: Yizheng Wu
# File: DataProcess_ABM.R
# Purpose: Process data from SACSIM - an activity based model develped by SACOG.

# Library definitions
library(foreign)

# Prevent scientific notation
options(scipen = 100)

# Set working drectory
setwd("/Users/Yizheng/Documents/02_Work/13_ITHIM/03_Data/07_SacSim")

# -----------------------------------
# Function Definition
# -----------------------------------
# calculate the relative values (relative to the value of "female 15-29")
CalRelativeMatrix <- function(x){
  for (i in 1:4){
    # if the value in that cell equals to 0, we use a small number to avoid NA.
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
  
  Pop.AT.para.byRace <- Pop.AT.para.byIncome <- matrix(NA,nrow = 6,ncol = 8,
                        dimnames = list(paste0("county",c(1:6)),c("demogr1_walk","demogr1_cycle","demogr2_walk","demogr2_cycle","demogr3_walk","demogr3_cycle","demogr4_walk","demogr4_cycle"))) 
  
  for(i in 1:6){#county
    
    for (j in 1:4){#demographic group
      #population mean walking time (min/day)
      Pop.AT.para.byRace[i,j*2-1]=sum(trip.pop$TIME[which(trip.pop$MODE==9&trip.pop$countyID==i&trip.pop$raceID==j)])/length(which(pop$countyID==i&pop$raceID==j))*7
      Pop.AT.para.byRace[i,2*j]=sum(trip.pop$TIME[which(trip.pop$MODE==8&trip.pop$countyID==i&trip.pop$raceID==j)])/length(which(pop$countyID==i&pop$raceID==j))*7
      
      Pop.AT.para.byIncome[i,j*2-1]=sum(trip.pop$TIME[which(trip.pop$MODE==9&trip.pop$countyID==i&trip.pop$hincID==j)])/length(which(pop$countyID==i&pop$hincID==j))*7
      Pop.AT.para.byIncome[i,2*j]=sum(trip.pop$TIME[which(trip.pop$MODE==8&trip.pop$countyID==i&trip.pop$hincID==j)])/length(which(pop$countyID==i&pop$hincID==j))*7
      
      
    }
    
 }
  
  return(list(
    Pop.AT.para.byRace=Pop.AT.para.byRace,
    Pop.AT.para.byIncome=Pop.AT.para.byIncome
  ))
}

#compute VMT by traffic mode
computeVMTbymode <- function(countyID,trip.pop,pop){
  ModeNames <- c("bike","walk","motorcycle","car","bus","truck")
  mode.vmt.byrace2 <- matrix(NA,nrow = 6,ncol = 2,dimnames = list(ModeNames,c("NHW","Other")))
  
  for (i in 1:2){
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

    #bus
    mode.vmt.byrace2[5,i]<-vmt*0.02

    #truck
    mode.vmt.byrace2[6,i]<-vmt*0.06
  }
  
  # mode.vmt.byrace <- matrix(NA,nrow = 6,ncol = 4,dimnames = list(ModeNames,c("NHW","NHB","NHO","HO")))
  # for (i in 1:4){
  #   pop.race <- length(which(pop$countyID==countyID&pop$raceID==i))
  #   
  #   #bike
  #   mode.vmt.byrace[1,i] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==8&trip.pop$countyID==countyID&trip.pop$raceID==i)])/pop.race
  #   #walk
  #   mode.vmt.byrace[2,i] <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==9&trip.pop$countyID==countyID&trip.pop$raceID==i)])/pop.race
  #   
  #   vmt <- sum(trip.pop$DISTANCE[which(trip.pop$MODE==5&trip.pop$countyID==countyID&trip.pop$raceID==i)])/(3.5*pop.race)+sum(trip.pop$DISTANCE[which(trip.pop$MODE==6&trip.pop$countyID==countyID&trip.pop$raceID==i)])/(2*pop.race)+
  #     sum(trip.pop$DISTANCE[which(trip.pop$MODE==7&trip.pop$countyID==countyID&trip.pop$raceID==i)])/(pop.race)
  #   
  #   #motorcycle
  #   mode.vmt.byrace[3,i]<-vmt*0.02
  #   
  #   #car
  #   mode.vmt.byrace[4,i]<-vmt*0.9
  #   
  #   #bus
  #   mode.vmt.byrace[5,i]<-vmt*0.02
  #   
  #   #truck
  #   mode.vmt.byrace[6,i]<-vmt*0.06
  #   
  # }
  
  return(mode.vmt.byrace2)
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

# -----------------------------------
# Data Processing
# -----------------------------------

# use read.dbf() in package 'foreign' to input the .dbf data
triptable.2012 <- read.dbf('trip_2012.dbf')
triptable.2020 <- read.dbf('trip_2020_am1.dbf')
triptable.2036 <- read.dbf('trip_2036.dbf')

# load the processed syn pop
load('pop.2012.cmplt')
load('pop.2020.cmplt')
load('pop.2036.cmplt')

#recode the variables in population matrix
pop.2012 <- recode.pop(pop.2012.cmplt)
pop.2020 <- recode.pop(pop.2020.cmplt[,-c(24:27)])
pop.2036 <- recode.pop(pop.2036.cmplt)

#merge pop and trip data sets
trip.pop.2012 <- prepTripPop(pop.2012,triptable.2012)
trip.pop.2020 <- prepTripPop(pop.2020,triptable.2020)
trip.pop.2036 <- prepTripPop(pop.2036,triptable.2036)

#compute overall active travel time
#population <- 3082285
#walkingtime <- sum(trip.pop.2036$TIME[which(trip.pop.2036$MODE==9)])/population*7
#cyclingtime <- sum(trip.pop.2036$TIME[which(trip.pop.2036$MODE==8)])/population*7

#compute VMT by traffic mode
vmt.baseline <- lapply(c(1:6),function(x) computeVMTbymode(x,trip.pop.2012,pop.2012))
vmt.2020 <- lapply(c(1:6),function(x) computeVMTbymode(x,trip.pop.2020,pop.2020))
vmt.2036 <- lapply(c(1:6),function(x) computeVMTbymode(x,trip.pop.2036,pop.2036))

#output the mean pop mean walk time and cycle time
# walk: mode = 9
# cycle: mode = 8

Pop.AT.para.2012 <- pop.mean.at.time(trip.pop.2012,pop.2012)
Pop.AT.para.2020 <- pop.mean.at.time(trip.pop.2020,pop.2020)
Pop.AT.para.2036 <- pop.mean.at.time(trip.pop.2036,pop.2036)

cuttingline <- matrix("",1,9)

write.csv(rbind(
  cbind(Pop.AT.para.2012$Pop.AT.para.byRace,c("2012",rep("",5))),cuttingline,
  cbind(Pop.AT.para.2020$Pop.AT.para.byRace,c("2020",rep("",5))),cuttingline, 
  cbind(Pop.AT.para.2036$Pop.AT.para.byRace,c("2036",rep("",5)))
),file = "00_output/PopulationMeanATTimebyRace.csv")

write.csv(rbind(
  cbind(Pop.AT.para.2012$Pop.AT.para.byIncome,c("2012",rep("",5))),cuttingline,
  cbind(Pop.AT.para.2020$Pop.AT.para.byIncome,c("2020",rep("",5))),cuttingline, 
  cbind(Pop.AT.para.2036$Pop.AT.para.byIncome,c("2036",rep("",5)))
),file = "00_output/PopulationMeanATTimebyIncome.csv")

# output the active transport information
#countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
Travel.Output.byIncome.2012 <- ActiveTravelDataOutput(pop.2012,trip.pop.2012,1,"Income")
Travel.Output.byRace.2012 <- ActiveTravelDataOutput(pop.2012,trip.pop.2012,1,"Race")

Travel.Output.byIncome.2020 <- ActiveTravelDataOutput(pop.2020,trip.pop.2020,2,"Income")
Travel.Output.byRace.2020 <- ActiveTravelDataOutput(pop.2020,trip.pop.2020,2,"Race")

Travel.Output.byIncome.2036 <- ActiveTravelDataOutput(pop.2036,trip.pop.2036,6,"Income")
Travel.Output.byRace.2036 <- ActiveTravelDataOutput(pop.2036,trip.pop.2036,6,"Race")

# Output the .csv files for active transport data and population
cuttingline <- matrix(" ",1,9)

#active transport data by race
write.csv(rbind(
  cbind(Travel.Output.byRace.2020$re.walk.time.byDemo,c("relative.walk.time",rep("",7))),cuttingline, #relative walking time
  cbind(Travel.Output.byRace.2020$re.cycle.time.byDemo,c("relative.cycle.time",rep("",7))),cuttingline, # relative cycling time
  cbind(Travel.Output.byRace.2020$re.walk.speed.byDemo,c("relative.walk.speed",rep("",7))),cuttingline, # relative walking speed
  cbind(Travel.Output.byRace.2020$re.cycle.speed.byDemo,c("relative.cycle.speed",rep("",7))) # relative cycling speed
),file = "00_output/PLA_ABM_by_race.2020.csv")

# population data by race
write.csv(Travel.Output.byRace.2020$pop.age.gender.demo[,1:8],
          file = "00_output/PLA_pop_race.2020.csv")


#active transport data by income
write.csv(rbind(
  cbind(Travel.Output.byIncome.2020$re.walk.time.byDemo,c("relative.walk.time",rep("",7))),cuttingline, #relative walking time
  cbind(Travel.Output.byIncome.2020$re.cycle.time.byDemo,c("relative.cycle.time",rep("",7))),cuttingline, # relative cycling time
  cbind(Travel.Output.byIncome.2020$re.walk.speed.byDemo,c("relative.walk.speed",rep("",7))),cuttingline, # relative walking speed
  cbind(Travel.Output.byIncome.2020$re.cycle.speed.byDemo,c("relative.cycle.speed",rep("",7))) # relative cycling speed
),file = "00_output/PLA_by_income.2020.csv")

# population data by income
write.csv(Travel.Output.byIncome.2020$pop.age.gender.demo[,1:8],
          file = "00_output/PLA_pop_income.2020.csv")

#for black group in all counties

NHB.walk.time <- NHB.cycle.time <- matrix(NA,8,2)
NHB.walk.speed <- NHB.cycle.speed <- matrix(NA,8,2)
for(i in (1:8)){#age
  for (j in (3:4)){#gender.demo
    
    pop.temp <- nrow(pop.2036[which(pop.2036$ageID==i&pop.2036$gender.race==j),])
    
    temp.time.walk <- sum(trip.pop.2036$TIME[which(trip.pop.2036$ageID==i&trip.pop.2036$gender.race==j&trip.pop.2036$MODE==9)])/pop.temp
    NHB.walk.time[i,(j-2)]<-temp.time.walk
    
    temp.time.cycle <- sum(trip.pop.2036$TIME[which(trip.pop.2036$ageID==i&trip.pop.2036$gender.race==j&trip.pop.2036$MODE==8)])/pop.temp
    NHB.cycle.time[i,(j-2)]<-temp.time.cycle
    
    temp.dis.walk <- sum(trip.pop.2036$DISTANCE[which(trip.pop.2036$ageID==i&trip.pop.2036$gender.race==j&trip.pop.2036$MODE==9)])/pop.temp
    NHB.walk.speed[i,(j-2)]<-temp.dis.walk/(temp.time.walk/60)
    
    temp.dis.cycle <- sum(trip.pop.2036$DISTANCE[which(trip.pop.2036$ageID==i&trip.pop.2036$gender.race==j&trip.pop.2036$MODE==8)])/pop.temp
    NHB.cycle.speed[i,(j-2)]<-temp.dis.cycle/(temp.time.cycle/60)
    
  }
} 
  
pop.mean.attime.NHB <- sum(trip.pop.2036$TIME[which(trip.pop.2036$MODE==9&trip.pop.2036$raceID==2)])/length(which(pop.2036$raceID==2))*7

#sum(trip.pop.2012$TIME[which(trip.pop.2012$MODE==9&trip.pop.2012$raceID==2)])

#sum(trip.pop$TIME[which(trip.pop$MODE==9&trip.pop$countyID==i&trip.pop$raceID==j)])/length(which(pop$countyID==i&pop$raceID==j))*7
#sum(trip.pop$TIME[which(trip.pop$ageID==j&trip.pop[demo.ID]==i&trip.pop$MODE==k&trip.pop$countyID%in%countyID)])





 # test.population <- matrix(nrow = 8,ncol = 2)
 # for (i in 1:8){ #age
 #   for (k in 1:2){ #gender
 #     pop.temp <- nrow(pop.2012[which(pop.2012$SEX==k&pop.2012$ageID==i),])
 #     test.population[i,k]<-pop.temp
 #   }
 # }


# trip.pop.2012$AC.MODE <- ifelse(trip.pop.2012$MODE%in%c(8,9),1,0)
# aggr.by.mode <- aggregate(trip.pop.2012[,"TIME"],list(trip.pop.2012$ID,trip.pop.2012$AC.MODE),sum)
# 
# 
# sum(trip.pop.2012$TIME[which(trip.pop.2012$MODE==9)])/nrow(pop.2012)
# sum(trip.pop.2012$TIME[which(trip.pop.2012$MODE==8)])/nrow(pop.2012)
# sum(trip.pop.2012.cv$TIME[which(trip.pop.2012.cv$MODE==99)])/nrow(pop.2012)
# 
# sd <- sd(c(trip.pop.2012.cv$TIME[which(trip.pop.2012.cv$MODE==99)],rep(0,nrow(pop.2012)-length(trip.pop.2012.cv$TIME[which(trip.pop.2012.cv$MODE==99)]))))
# sd <- sd(c(trip.pop.2012$TIME[which(trip.pop.2012$MODE%in%c(8,9))],rep(0,(2*nrow(pop.2012)-length(trip.pop.2012$TIME[which(trip.pop.2012$MODE%in%c(8,9))])))))
# sd/4.680896
# 
# sd(trip.pop.2012$TIME[which(trip.pop.2012$MODE==9)])
#Pop.AC.para[i,3]=-0.0108*(Pop.AC.para[i,1]+Pop.AC.para[i,2])+1.2682+0.7
#sd <- sd(c(trip.pop.2012.cv$TIME[which(trip.pop.2012.cv$MODE==99&trip.pop.2012.cv$countyID==i)],rep(0,(length(which(pop.2012$countyID==i))-length(trip.pop.2012.cv$TIME[which(trip.pop.2012.cv$MODE==99&trip.pop.2012$countyID==i)])))))
#mean <- sum(trip.pop.2012.cv$TIME[which(trip.pop.2012.cv$MODE==99&trip.pop.2012.cv$countyID==i)])/length(which(pop.2012$countyID==i))
#Pop.AC.para[i,3]=sd/mean

