# This file is part of ITHIM Sacramento.

# File: DataProcess_SWITRS.R
# Purpose: Obtain the collision, party and victim data 
# from SWITRS(https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system) 
# and TIMS (https://tims.berkeley.edu)

# set your working directory
setwd("/Users/Yizheng/Documents/02_Work/14_GitHub/00_ITHIM/01_Data")

# Part 1  Data Preparation -------------------------------------------------------------

# input the collision and party data  
file.names <- list.files(path = "04_SWITRS/00_TIMS_06-16/")

colnames.collision <- colnames(read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[1])))
colnames.parties <- colnames(read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[10])))
colnames.victims <- colnames(read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[19])))

collision.temp <- read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[1]),header = FALSE,skip = 1)
parties.temp <- read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[10]),header = FALSE,skip = 1)
victims.temp <- read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[19]),header = FALSE,skip = 1)

for (i in 2:9){
  collision.temp <- rbind(collision.temp,read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[i]),header=FALSE,skip = 1))
  parties.temp <- rbind(parties.temp,read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[i+9]),header=FALSE,skip = 1))
  victims.temp <- rbind(victims.temp,read.csv(paste0("04_SWITRS/00_TIMS_06-16/",file.names[i+18]),header=FALSE,skip = 1))
  
}

colnames(collision.temp)<-colnames.collision
colnames(parties.temp)<-colnames.parties
colnames(victims.temp)<-colnames.victims

# keep the required variables in collision
keep.collision <- c('CASEID','COUNTY','PARTIES','KILLED','SEVINJ','CHPTYPE')
collision.t <- collision.temp[,names(collision.temp)%in%keep.collision]
collision.party.1 <- collision.t[which(collision.t$PARTIES==1),] #one party collisions
collision.party.2 <- collision.t[which(collision.t$PARTIES==2),] #two parties collisions
collision.party.3 <- collision.t[which(collision.t$PARTIES==3),] #three parties collisions

# keep the required variables in party
keep.party <- c('CASEID','PARNUM','ATFAULT',"VEHTYPE",'PRACE')
party.c <- parties.temp[,names(parties.temp)%in%keep.party]

# keep the required variables in victims
keep.victims <- c('CASEID','PARNUM','VSEX','VAGE','VINJURY','county')
victim.c <- victims.temp[,names(victims.temp)%in%keep.victims]
victim.c.killed <- victim.c[which(victim.c$VINJURY==1),]

# merge party and collision data by using case ID
injury.party.1 <- merge(collision.party.1,party.c,by.x = 'CASEID',by.y = 'CASEID')
injury.party.2 <- merge(collision.party.2,party.c,by.x = 'CASEID',by.y = 'CASEID')
injury.party.3 <- merge(collision.party.3,party.c,by.x = 'CASEID',by.y = 'CASEID')

# merge party and victims data by using case ID and PARNUM
victim.party.killed <- merge(victim.c.killed,party.c,by=c('CASEID','PARNUM'))

# Part 1  Function Definition -------------------------------------------------------------

# function for recoding the traffic mode ID, road type ID, and race ID
# traffic mode ID
# 1: bicycle
# 2: pedestrian
# 3: motorcycle
# 4: passenger car
# 5: truck
# 6: bus
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
get.case.ID <- function(injury,SV.ID,VV.ID,roadtype,raceID,countyID){
  #"AT FAULT" = Y we define it as the striking mode
  #"AT FAULT" = N we define it as the victim mode
  CASE.SV <- injury$CASEID[which(injury$ATFAULT=='Y'&injury$modeID==SV.ID&injury$roadtype==roadtype&injury$countyID%in%countyID)]
  CASE.VV <- injury$CASEID[which(injury$ATFAULT=='N'&injury$modeID==VV.ID&injury$roadtype==roadtype&injury$raceID==raceID&injury$countyID%in%countyID)]
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
      fatal.matrix.temp[i,7]<-sum(injury.party.1$KILLED[injury.party.1$modeID==i&injury.party.1$roadtype==k&injury.party.1$raceID==raceID&injury.party.1$countyID%in%countyID])/11 # annual average number
      serious.matrix.temp[i,7]<-sum(injury.party.1$SEVINJ[injury.party.1$modeID==i&injury.party.1$roadtype==k&injury.party.1$raceID==raceID&injury.party.1$countyID%in%countyID])/11 # annual average number
      
      for (j in 1:6){ #Striking mode
        fatal.matrix.temp[i,j]<-sum(collision.party.2[collision.party.2$CASEID%in%get.case.ID(injury.party.2,j,i,k,raceID,countyID),3])/11 +
          sum(collision.party.3[collision.party.3$CASEID%in%get.case.ID(injury.party.3,j,i,k,raceID,countyID),3])/2/11 # annual average number (11 years)
        serious.matrix.temp[i,j]<-sum(collision.party.2[collision.party.2$CASEID%in%get.case.ID(injury.party.2,j,i,k,raceID,countyID),5])/11 +
          sum(collision.party.3[collision.party.3$CASEID%in%get.case.ID(injury.party.3,j,i,k,raceID,countyID),5])/2/11# annual average number (11 years)
      }
    }
    injury.list[[k]]<-fatal.matrix.temp
    injury.list[[k+3]]<-serious.matrix.temp
  }
  
  # combine the list in order to output as a .csv file
  result <- rbind(injury.list[[1]],injury.list[[2]],injury.list[[3]],injury.list[[4]],injury.list[[5]],injury.list[[6]])
  colnames(result)<- c('bike','walk','motorcycle','car','truck','bus','one-party')
  rownames(result)<- rep(c('bike','walk','motorcycle','car','truck','bus'),6)
  
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

# shape the output file
shape.format.output <- function(countyID){
  output.byrace <- NULL
  cutting.line <- matrix("",1,8)
  
  for(i in 1:4){
    temp <- get.race.injury(raceID=i,countyID=countyID)
    output.byrace <- rbind(output.byrace,temp,cutting.line)
  }
  
  race.name.col <- matrix(c("NHW",rep("",36),"NHB",rep("",36),"NHO",rep("",36),"HO",rep("",36)),148,1)
  final.output.byrace <- cbind(output.byrace,race.name.col)
  
  return(final.output.byrace)
}

# recode victim.killed
# race ID
victim.party.killed$raceID <- ifelse(victim.party.killed$PRACE=='W',1,2)

# county ID
victim.party.killed$countyID <- ifelse(victim.party.killed$county=='ELD',1,
                                       ifelse(victim.party.killed$county=='PLA',2,
                                              ifelse(victim.party.killed$county=='SAC',3,
                                                     ifelse(victim.party.killed$county=='SUT',4,
                                                            ifelse(victim.party.killed$county=='YOL',5,
                                                                   ifelse(victim.party.killed$county=='YUB',6,99))))))
#age group
victim.party.killed$age.sex.ID <- ifelse(victim.party.killed$VAGE<=4&victim.party.killed$VSEX=='M',1,
                                    ifelse(victim.party.killed$VAGE<=14&victim.party.killed$VSEX=='M',2,
                                           ifelse(victim.party.killed$VAGE<=29&victim.party.killed$VSEX=='M',3,
                                                  ifelse(victim.party.killed$VAGE<=44&victim.party.killed$VSEX=='M',4,
                                                         ifelse(victim.party.killed$VAGE<=59&victim.party.killed$VSEX=='M',5,
                                                                ifelse(victim.party.killed$VAGE<=69&victim.party.killed$VSEX=='M',6,
                                                                       ifelse(victim.party.killed$VAGE<=79&victim.party.killed$VSEX=='M',7,
                                                                              ifelse(victim.party.killed$VSEX=='M',8,
                                                                                     ifelse(victim.party.killed$VAGE<=4&victim.party.killed$VSEX=='F',9,
                                                                                            ifelse(victim.party.killed$VAGE<=14&victim.party.killed$VSEX=='F',10,
                                                                                                   ifelse(victim.party.killed$VAGE<=29&victim.party.killed$VSEX=='F',11,
                                                                                                          ifelse(victim.party.killed$VAGE<=44&victim.party.killed$VSEX=='F',12,
                                                                                                                 ifelse(victim.party.killed$VAGE<=59&victim.party.killed$VSEX=='F',13,
                                                                                                                        ifelse(victim.party.killed$VAGE<=69&victim.party.killed$VSEX=='F',14,
                                                                                                                               ifelse(victim.party.killed$VAGE<=79&victim.party.killed$VSEX=='F',15,16)))))))))))))))
# count the killed victim number
victim.killed.matrix <- NULL
victim.killed.matrix.temp <- matrix(data = NA,nrow = 16,ncol = 2)

for (k in 1:6){
  for (i in 1:16){
    for (j in 1:2){
      victim.killed.matrix.temp[i,j]=nrow(victim.party.killed[which(victim.party.killed$age.sex.ID==i&
                                                                      victim.party.killed$raceID==j&
                                                                      victim.party.killed$countyID==k),])/11 #11
    }
  }
  victim.killed.matrix <- cbind(victim.killed.matrix,victim.killed.matrix.temp)
}

dimnames(victim.killed.matrix) = list(c(paste0("maleAgeClass ",1:8),
                                  paste0("femaleAgeClass ",1:8)),rep((c('Race1','Race2')),6) )




# Part 3  Output -------------------------------------------------------------
injury.party.1 <- recode(injury.party.1)
injury.party.2 <- recode(injury.party.2)
injury.party.3 <- recode(injury.party.3)

# county ID
# 1: ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB
output.ELD <- shape.format.output(countyID = 1)
write.csv(output.ELD,file = "06_Equity Analysis/05_baseline injury/test.eld.INJURY.csv")

output.PLA <- shape.format.output(countyID = 2)
write.csv(output.PLA,file = "06_Equity Analysis/05_baseline injury/test.pla.INJURY.csv")

output.SAC <- shape.format.output(countyID = 3)
write.csv(output.SAC,file = "06_Equity Analysis/05_baseline injury/test.sac.INJURY.csv")

output.SUT <- shape.format.output(countyID = 4)
write.csv(output.SUT,file = "06_Equity Analysis/05_baseline injury/test.sut.INJURY.csv")

output.YOL <- shape.format.output(countyID = 5)
write.csv(output.YOL,file = "06_Equity Analysis/05_baseline injury/test.yol.INJURY.csv")

output.YUB <- shape.format.output(countyID = 6)
write.csv(output.YUB,file = "06_Equity Analysis/05_baseline injury/test.yub.INJURY.csv")

write.csv(victim.killed.matrix,file = "06_Equity Analysis/05_baseline injury/mortality_traffic injury.csv")


