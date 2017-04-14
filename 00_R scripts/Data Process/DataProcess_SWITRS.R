# Author: Yizheng Wu
# File: DataProcess_SWITRS.R
# Purpose: Obtain the collision and party data 
# from SWITRS(https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system) 
# and TIMS (https://tims.berkeley.edu)

# set your working directory
setwd("/Users/Yizheng/Documents/02_Work/14_GitHub/00_ITHIM/01_Data")

# input the collision and party data  
filenames <- readLines("05_SWITRS/filenames.txt")
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
collision.c <- collision.t[which(collision.t$PARTIES==2),]

# keep the required variables in party
keep.party <- c('CASEID','PARNUM','ATFAULT',"VEHTYPE",'PRACE')
party.c <- parties.temp[,names(parties.temp)%in%keep.party]
#head(party.c)

# merge party and collision data by using case ID
injury <- merge(collision.c,party.c,by.x = 'CASEID',by.y = 'CASEID')
#head(injury)

# recode the traffic mode ID
# 1: bicycle
# 2: pedestrian
# 3: motorcycle
# 4: passenger car
# 5: bus
# 6: truck
injury$modeID <- ifelse(injury$VEHTYPE=='L',1,
                        ifelse(injury$VEHTYPE=='N',2,
                               ifelse(injury$VEHTYPE=='C',3,
                                      ifelse(injury$VEHTYPE%in%c('A','B'),4,
                                             ifelse(injury$VEHTYPE%in%c('D','E','F','G'),5,
                                                    ifelse(injury$VEHTYPE%in%c('H','I'),6,99))))))
# recode the road type ID
# 1: local
# 2: arterial
# 3: highway
injury$roadtype <- ifelse(injury$CHPTYPE %in%c(4,5),1,
                          ifelse(injury$CHPTYPE==3,2,
                                 ifelse(injury$CHPTYPE%in%c(1,2),3,99)))

# recode the race ID
# 1: non-hispanic white
# 2: non-hispanic black
# 3: non-hispanic other races
# 4: hispanic
injury$raceID <- ifelse(injury$PRACE=='W',1,
                        ifelse(injury$PRACE=='B',2,
                               ifelse(injury$PRACE%in%c('O','A'),3,
                                      ifelse(injury$PRACE=='H',4,99))))

# function for searching the case ID of the combination of striking modes and victim modes on specific road type
get.case.ID <- function(SV.ID,VV.ID,roadtype,raceID){
  CASE.SV <- injury$CASEID[which(injury$ATFAULT=='Y'&injury$modeID==SV.ID&injury$roadtype==roadtype)]
  CASE.VV <- injury$CASEID[which(injury$ATFAULT=='N'&injury$modeID==VV.ID&injury$roadtype==roadtype&injury$raceID==raceID)]
  return(intersect(CASE.SV,CASE.VV))
}

# function of get injury matrix by race
get.race.injury <- function(raceID){
  
  injury.list <-  vector('list',6)
  names(injury.list) <- c('local+fatal','arterial+fatal','highway+fatal','local+serious','arterial+serious','highway+serious')
  
  fatal.matrix.temp <- serious.matrix.temp <- matrix(NA,nrow = 6,ncol = 6)
  
  for(k in 1:3){ # road type
    for (i in 1:6){ #Victim mode
      for (j in 1:6){ #striking mode
        fatal.matrix.temp[i,j]<-sum(collision.c[collision.c$CASEID%in%get.case.ID(j,i,k,raceID),3])/5 # annual average number
        serious.matrix.temp[i,j]<-sum(collision.c[collision.c$CASEID%in%get.case.ID(j,i,k,raceID),5])/5 # annual average number
      }
    }
    injury.list[[k]]<-fatal.matrix.temp
    injury.list[[k+3]]<-serious.matrix.temp
  }
  
  # combine the list in order to output as a .csv file
  result <- rbind(injury.list[[1]],injury.list[[2]],injury.list[[3]],injury.list[[4]],injury.list[[5]],injury.list[[6]])
  colnames(result)<- c('bike','walk','motorcycle','car','bus','truck')
  rownames(result)<- rep(c('bike','walk','motorcycle','car','bus','truck'),6)
  
  # add the road type and injury type in the .csv file
  result.t <- cbind(result,1)
  result.t[1:6,7] <- "local fatal"
  result.t[7:12,7] <- "arterial fatal"
  result.t[13:18,7] <- "highway fatal"
  result.t[19:24,7] <- "local serious"
  result.t[25:30,7] <- "arterial serious"
  result.t[31:36,7] <- "highway serious"
  
  return(result.t)
}

# output the injury matrix as the input of injury module
output.NHW <- get.race.injury(1)
write.csv(output.NHW,file = "04_Equity Analysis/11_baseline injury/injury baseline_NHW.csv")

 output.NHB <- get.race.injury(2)
write.csv(output.NHB,file = "04_Equity Analysis/11_baseline injury/injury baseline_NHB.csv")

output.NHO <- get.race.injury(3)
write.csv(output.NHO,file = "04_Equity Analysis/11_baseline injury/injury baseline_NHO.csv")

output.HO <- get.race.injury(4)
write.csv(output.HO,file = "04_Equity Analysis/11_baseline injury/injury baseline_HO.csv")



