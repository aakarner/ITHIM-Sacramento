# Author: Yizheng Wu
# File: DataProcess_SWITRS.R
# Purpose: Obtain the collision and party data 
# from SWITRS(https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system) 
# and TIMS (https://tims.berkeley.edu)

# set your working directory
setwd("/Users/Yizheng/Documents/02_Work/14_GitHub/00_ITHIM/01_Data")

########## Data Preparation ##########

# input the collision and party data  
filenames <- readLines("04_SWITRS/filenames.txt")
colnames.collision <- colnames(read.csv(filenames[1]))
colnames.parties <- colnames(read.csv(filenames[15]))

collision.temp <- read.csv(filenames[1],header = FALSE,skip = 1)
parties.temp <- read.csv(filenames[15],header = FALSE,skip = 1)

for (i in 2:14){
  collision.temp <- rbind(collision.temp,read.csv(filenames[i],header=FALSE,skip = 1))
  parties.temp <- rbind(parties.temp,read.csv(filenames[i+14],header=FALSE,skip = 1))
}

colnames(collision.temp)<-colnames.collision
colnames(parties.temp)<-colnames.parties

# keep the required variables in collision
keep.collision <- c('CASEID','COUNTY','PARTIES','KILLED','SEVINJ','CHPTYPE')
collision.t <- collision.temp[,names(collision.temp)%in%keep.collision]
collision.party.1 <- collision.t[which(collision.t$PARTIES==1),] #one party collisions
collision.party.2 <- collision.t[which(collision.t$PARTIES==2),] #two parties collisions

# keep the required variables in party
keep.party <- c('CASEID','PARNUM','ATFAULT',"VEHTYPE",'PRACE')
party.c <- parties.temp[,names(parties.temp)%in%keep.party]

# merge party and collision data by using case ID
injury.party.1 <- merge(collision.party.1,party.c,by.x = 'CASEID',by.y = 'CASEID')
injury.party.2 <- merge(collision.party.2,party.c,by.x = 'CASEID',by.y = 'CASEID')
#head(injury)

########## Function Definition ##########

# function for recoding the traffic mode ID, road type ID, and race ID
# traffic mode ID
# 1: bicycle
# 2: pedestrian
# 3: motorcycle
# 4: passenger car
# 5: bus
# 6: truck
# 
# road type ID
# 1: local
# 2: arterial
# 3: highway
#
# race ID
# 1: non-hispanic white
# 2: non-hispanic black
# 3: non-hispanic other races
# 4: hispanic

# county ID
# 1: EL DORADO
# 2: PLACER
# 3: SACRAMENTO
# 4: SUTTER
# 5: YOLO
# 6: YUBA

recode <- function(injury){
  injury$modeID <- ifelse(injury$VEHTYPE=='L',1,
                          ifelse(injury$VEHTYPE=='N',2,
                                 ifelse(injury$VEHTYPE=='C',3,
                                        ifelse(injury$VEHTYPE%in%c('A','B'),4,
                                               ifelse(injury$VEHTYPE%in%c('D','E','F','G'),5,
                                                      ifelse(injury$VEHTYPE%in%c('H','I'),6,99))))))
  
  injury$roadtype <- ifelse(injury$CHPTYPE %in%c(4,5),1,
                            ifelse(injury$CHPTYPE==3,2,
                                   ifelse(injury$CHPTYPE%in%c(1,2),3,99)))
  
  injury$raceID <- ifelse(injury$PRACE=='W',1,
                          ifelse(injury$PRACE=='B',2,
                                 ifelse(injury$PRACE%in%c('O','A'),3,
                                        ifelse(injury$PRACE=='H',4,99))))
  
  injury$countyID <- ifelse(injury$COUNTY=='EL DORADO',1,
                            ifelse(injury$COUNTY=='PLACER',2,
                                   ifelse(injury$COUNTY=='SACRAMENTO',3,
                                          ifelse(injury$COUNTY=='SUTTER',4,
                                                 ifelse(injury$COUNTY=='YOLO',5,
                                                        ifelse(injury$COUNTY=='YUBA',6,99))))))
  
  return(injury)
}

# function for searching the case ID of the combination of striking modes and victim modes on specific road type
get.case.ID <- function(SV.ID,VV.ID,roadtype,raceID,countyID){
  #"AT FAULT" = Y we define it as the striking mode
  #"AT FAULT" = N we define it as the victim mode
  CASE.SV <- injury.party.2$CASEID[which(injury.party.2$ATFAULT=='Y'&injury.party.2$modeID==SV.ID&injury.party.2$roadtype==roadtype&injury.party.2$countyID%in%countyID)]
  CASE.VV <- injury.party.2$CASEID[which(injury.party.2$ATFAULT=='N'&injury.party.2$modeID==VV.ID&injury.party.2$roadtype==roadtype&injury.party.2$raceID==raceID&injury.party.2$countyID%in%countyID)]
  return(intersect(CASE.SV,CASE.VV))
}

# function of get injury matrix by race
get.race.injury <- function(raceID,countyID){
  
  injury.list <-  vector('list',6)
  names(injury.list) <- c('local+fatal','arterial+fatal','highway+fatal','local+serious','arterial+serious','highway+serious')
  
  fatal.matrix.temp <- serious.matrix.temp <- matrix(NA,nrow = 6,ncol = 7)
  
  #raceID<-1
  for(k in 1:3){ # road type
    for (i in 1:6){ #Victim mode
      fatal.matrix.temp[i,7]<-sum(injury.party.1$KILLED[injury.party.1$modeID==i&injury.party.1$roadtype==k&injury.party.1$raceID==raceID&injury.party.1$countyID%in%countyID])/5 # annual average number
      serious.matrix.temp[i,7]<-sum(injury.party.1$SEVINJ[injury.party.1$modeID==i&injury.party.1$roadtype==k&injury.party.1$raceID==raceID&injury.party.1$countyID%in%countyID])/5 # annual average number
      
      for (j in 1:6){ #Striking mode
        fatal.matrix.temp[i,j]<-sum(collision.party.2[collision.party.2$CASEID%in%get.case.ID(j,i,k,raceID,countyID),3])/5 # annual average number
        serious.matrix.temp[i,j]<-sum(collision.party.2[collision.party.2$CASEID%in%get.case.ID(j,i,k,raceID,countyID),5])/5 # annual average number
      }
    }
    injury.list[[k]]<-fatal.matrix.temp
    injury.list[[k+3]]<-serious.matrix.temp
  }
  
  # combine the list in order to output as a .csv file
  result <- rbind(injury.list[[1]],injury.list[[2]],injury.list[[3]],injury.list[[4]],injury.list[[5]],injury.list[[6]])
  colnames(result)<- c('bike','walk','motorcycle','car','bus','truck','one-party')
  rownames(result)<- rep(c('bike','walk','motorcycle','car','bus','truck'),6)
  
  # add the road type and injury type in the .csv file
  result.t <- cbind(result,1)
  result.t[1:6,8] <- "local fatal"
  result.t[7:12,8] <- "arterial fatal"
  result.t[13:18,8] <- "highway fatal"
  result.t[19:24,8] <- "local serious"
  result.t[25:30,8] <- "arterial serious"
  result.t[31:36,8] <- "highway serious"
  
  return(result.t)
}

########## Data Processing ##########
injury.party.1 <- recode(injury.party.1)
injury.party.2 <- recode(injury.party.2)

# output the injury matrix as the input of injury module
output.NHW <- get.race.injury(raceID=1,countyID=1) #NHW
write.csv(output.NHW,file = "06_Equity Analysis/05_baseline injury/YUB_01_injury baseline_NHW.csv")

output.NHB <- get.race.injury(2,6)
write.csv(output.NHB,file = "06_Equity Analysis/05_baseline injury/YUB_02_injury baseline_NHB.csv")

output.NHO <- get.race.injury(3,6)
write.csv(output.NHO,file = "06_Equity Analysis/05_baseline injury/YUB_03_injury baseline_NHO.csv")

output.HO <- get.race.injury(4,6)
write.csv(output.HO,file = "06_Equity Analysis/05_baseline injury/YUB_04_injury baseline_HO.csv")
