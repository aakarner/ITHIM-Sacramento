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
CalRelativeMatrix <- function(x){
  for (i in 1:4){
    ref <- x[3,2*i] #female in the category of 15~29 years old
    x[,c((2*i-1):(2*i))] <- x[,c((2*i-1):(2*i))]/ref
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
    ifelse(pop$SEX==1&pop$HINC<quantile(pop.2012$HINC)[2],1,
           ifelse(pop$SEX==2&pop$HINC<quantile(pop.2012$HINC)[2],2,
                  ifelse(pop$SEX==1&pop$HINC<quantile(pop.2012$HINC)[3],3,
                         ifelse(pop$SEX==2&pop$HINC<quantile(pop.2012$HINC)[3],4,
                                ifelse(pop$SEX==1&pop$HINC<quantile(pop.2012$HINC)[4],5,
                                       ifelse(pop$SEX==2&pop$HINC<quantile(pop.2012$HINC)[4],6,
                                              ifelse(pop$SEX==1,7,8)))))))
  
  # add a column for combining household id and person id
  pop$ID <- paste0('h',pop$SERIALNO,'p',pop$PNUM)
  return(pop)

  
}

getTAZ <- function(parc.input){
  ELD.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="ELD")])
  PLA.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="PLA")])
  SAC.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="SAC")])
  SUT.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="SUT")])
  YOL.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="YOL")])
  YUB.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="YUB")])
  
  return(list(
    ELD.TAZ = ELD.TAZ,
    PLA.TAZ = PLA.TAZ,
    SAC.TAZ = SAC.TAZ,
    SUT.TAZ = SUT.TAZ,
    YOL.TAZ = YOL.TAZ,
    YUB.TAZ = YUB.TAZ
  ))
}


prepTripPop <- function(pop,triptable){
  # compute the mean travel time and travel distance per capita by mode
  aggr.by.mode <- aggregate(triptable[,c("TRAVTIME","TRAVDIST")],list(triptable$SAMPN,triptable$PERSN,triptable$MODE),sum)
  names(aggr.by.mode) <- c('SAMPN',"PERSN","MODE","TIME","DISTANCE")
  aggr.by.mode <- aggr.by.mode[order(aggr.by.mode$SAMPN,aggr.by.mode$PERSN,aggr.by.mode$MODE),]
  
  # add a column for combining household id and person id
  aggr.by.mode$ID <- paste0('h',aggr.by.mode$SAMPN,'p',aggr.by.mode$PERSN)
  
  # merge two data sets by ID
  trip.pop <- merge(aggr.by.mode,pop,by.x = "ID",by.y = "ID")
  
  return(trip.pop)
}

ActiveTravelDataOutput <- function(pop,trip.pop,TAZ,countyID){
  travel.times.by.demo <-travel.distance.by.demo <- matrix(nrow = 72, ncol = 8)
  pop.age.gender.demo <- matrix(nrow=8,ncol = 8)
  
  taz.selected<-NULL
  for(i in 1:length(countyID)){
    taz.selected <- c(taz.selected,TAZ[[i]])
  }
  
  for (i in 1:8){ #gender.income
    for (j in 1:8){ #age
      pop.temp <- nrow(pop[which(pop$gender.inc==i&pop$ageID==j&pop$HHTAZ%in%taz.selected),])
      pop.age.gender.demo[j,i]<-pop.temp
      
      for (k in 1:9){ #mode
        time.temp <- sum(trip.pop$TIME[which(trip.pop$ageID==j&trip.pop$gender.inc==i&trip.pop$MODE==k&trip.pop$HHTAZ%in%taz.selected)])
        travel.times.by.demo[j + 8 * (k - 1), i] <- time.temp
        
        distance.temp <- sum(trip.pop$DISTANCE[which(trip.pop$ageID==j&trip.pop$gender.inc==i&trip.pop$MODE==k&trip.pop$HHTAZ%in%taz.selected)])
        travel.distance.by.demo[j + 8 * (k - 1), i] <- distance.temp
      }
    }
  }
  
  # compute the travel time and distance per capita
  walk.time.byDemo <- travel.times.by.demo[65:72,]/pop.age.gender.demo
  walk.distance.byDemo <- travel.distance.by.demo[65:72,]/pop.age.gender.demo
  cycle.time.byDemo <- travel.times.by.demo[57:64,]/pop.age.gender.demo
  cycle.distance.byDemo <- travel.distance.by.demo[57:64,]/pop.age.gender.demo
  
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
  dimname <- list(c(paste0("ageCat",1:8)),paste0(temp.gender,".Demo.",rep(1:4,each=2)))
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
pop.2012 <- read.dbf('2012_pop_parc_AM1/2012_pop.dbf')

parc.2012 <- read.dbf('2012_pop_parc_AM1/2012_parc.dbf')
#head(triptable.2012)
head(pop.2012)
#head(parc.2012)

#recode the variables in population matrix
pop.2012 <- recode.pop(pop.2012)

#obtain the taz number and county information
taz <- getTAZ(parc.2012)

#merge pop and trip data sets
trip.pop.2012 <- prepTripPop(pop.2012,triptable.2012)

# output the active transport information
#countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
Travel.Output.byIncome <- ActiveTravelDataOutput(pop.2012,trip.pop.2012,taz,countyID = c(1,2,3,4,5,6))

# Output the .csv files for active transport data and population
cuttingline <- matrix(" ",1,9)

# write.csv(rbind(
#   Travel.Output.byIncome$re.walk.time.byDemo,cuttingline, #relative walking time
#   Travel.Output.byIncome$re.cycle.time.byDemo,cuttingline, # relative cycling time
#   Travel.Output.byIncome$re.walk.speed.byDemo,cuttingline, # relative walking speed
#   Travel.Output.byIncome$re.cycle.speed.byDemo # relative cycling speed
# ),file = "test_ABM_by_income.csv")


write.csv(rbind(
  cbind(Travel.Output.byIncome$re.walk.time.byDemo,c("relative.walk.time",rep("",7))),cuttingline, #relative walking time
  cbind(Travel.Output.byIncome$re.cycle.time.byDemo,c("relative.cycle.time",rep("",7))),cuttingline, # relative cycling time
  cbind(Travel.Output.byIncome$re.walk.speed.byDemo,c("relative.walk.speed",rep("",7))),cuttingline, # relative walking speed
  cbind(Travel.Output.byIncome$re.cycle.speed.byDemo,c("relative.cycle.speed",rep("",7))) # relative cycling speed
),file = "test_ABM_by_income.csv")


write.csv(Travel.Output.byIncome$pop.age.gender.demo[,1:8],
          file = "test_pop_income.csv")

 # test.population <- matrix(nrow = 8,ncol = 2)
 # for (i in 1:8){ #age
 #   for (k in 1:2){ #gender
 #     pop.temp <- nrow(pop.2012[which(pop.2012$SEX==k&pop.2012$ageID==i),])
 #     test.population[i,k]<-pop.temp
 #   }
 # }





