############ ITHIM application for Sacramento County ##########

##### Injury Module #####

#set your working directory
setwd("~/Documents/02_Work/13_ITHIM/03_Data/02_Injury")

# Prevent scientific notation
options(scipen = 100)

##### Baseline Injuries #####

#read the csv of injury data
temp <- read.table("Injury.csv",header=T,sep=',')
injury <- temp[,2:10]

# number of traffic modes, injuried types (fatal & serious), and road types (local, arterial, and highway)
nTrafficModeV <- 8 #victim
nTrafficModeS <- 9 #striking (include NOV)
nInjuriedType <- 2
nRoadType <- 3
ModeNames <- colnames(injury)
RoadTypes <- c("local","arterial","highway")

# build the list of baseline injury data
injury.baseline <- list()

for (n in 1:(nInjuriedType*nRoadType)){
  injury.baseline[[n]] <-injury[((n-1)*nTrafficModeV+1):(nTrafficModeV*n),1:(nTrafficModeS)]
}
injury.baseline <- lapply(injury.baseline,function(x) {
  row.names(x) <- ModeNames[1:8]
  return (x)
})
names(injury.baseline) <- c("local.fatal","arterial.fatal","highway.fatal","local.serious","arterial.serious","highway.serious")

##### Distribution of Person & Vehicle Distance by Road Types  #####

# person distance (person) for all road types (miles per day)
t.person.distance.baseline <- c(0.2314295,0.16619841,0.43905542,27.0677899,0.08906508,1.70441975,NA,0.12788347)
t.person.distance.scenario <- c(0.449455623452742,0.0864333319829324,0.282430577165011,18.5556107757615,NA,0.0000007664437775167,NA,0.12788347)

# matrix of distance (person) by road types (miles per day)
distance.baseline.person <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(colnames(injury)[1:8],c("local","arterial","highway")))
distance.baseline.person[,1] <- t.person.distance.baseline * c(0.75,0.53,0.364,0.17756911,NA,0.17756911,NA,0.17756911)
distance.baseline.person[,2] <- t.person.distance.baseline * c(0.25,0.47,0.626,0.25345185,NA,0.25345185,NA,0.25345185)
distance.baseline.person[,3] <- t.person.distance.baseline * c(0.0000000667,0.0000000667,0.01,0.56897904,NA,0.56897904,NA,0.56897904)

# for the scenario 3
distance.scenario.person <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(colnames(injury)[1:8],c("local","arterial","highway")))
distance.scenario.person[,1] <- t.person.distance.scenario * c(0.75,0.53,0.075048750103747,0.0810998439977028,NA,0.075048750103747,NA,0.0810998439977028)
distance.scenario.person[,2] <- t.person.distance.scenario * c(0.25,0.47,0.252611107543058,0.288307729519899,NA,0.252611107543058,NA,0.288307729519899)
distance.scenario.person[,3] <- t.person.distance.scenario * c(0.0000000667,0.0000000667,0.672340142353195,0.630592426482398,NA,0.672340142353195,NA,0.630592426482398)

# matrix of occupancy
occ.baseline <- c(1,1,10,1.74763709,NA,1,NA,1.00069988)
occ.scenario <- c(1,1,10,1.33434155662758,NA,1,NA,1.00069988)

# matrix of distance (vehicle) by road types (miles per day)
distance.baseline.vehicle <- distance.baseline.person/occ.baseline
distance.scenario.vehicle <- distance.scenario.person/occ.scenario

##### Scenario Injury Multiplier #####

# safety factors
str.veh.safety.RR <- 0.5
speed.safety <- 1.0
other <- 1.0
victim.safety <- 0.5

# compute the list of scenario injury multiplier (i:row;j:col;k:element)
scenario.multiplier <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames[1:8],ModeNames)))), nRoadType)
names(scenario.multiplier) <- RoadTypes

for (k in 1:3) {
  # for first four traffic modes 
  for (j in 1:4) {
    for (i in 1:4) {
      scenario.multiplier[[k]][i,j] <- (distance.scenario.person[i,k]/distance.baseline.person[i,k])^victim.safety*(distance.scenario.vehicle[j,k]/distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
    }
    for (i in 5:7) {
      scenario.multiplier[[k]][i,j] <- (distance.scenario.person[i+1,k]/distance.baseline.person[i+1,k])^victim.safety*(distance.scenario.vehicle[j,k]/distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
    }
  }
  # for five to seven traffic modes
  for (j in 5:7) {
    for (i in 1:4) {
      scenario.multiplier[[k]][i,j] <- (distance.scenario.person[i,k]/distance.baseline.person[i,k])^victim.safety*(distance.scenario.vehicle[j+1,k]/distance.baseline.vehicle[j+1,k])^str.veh.safety.RR*speed.safety*other
    }
    for (i in 5:7) {
      scenario.multiplier[[k]][i,j] <- (distance.scenario.person[i+1,k]/distance.baseline.person[i+1,k])^victim.safety*(distance.scenario.vehicle[j+1,k]/distance.baseline.vehicle[j+1,k])^str.veh.safety.RR*speed.safety*other
    }
  }
  # for the last traffic mode (NOV)
  for (i in 1:4) {
    scenario.multiplier[[k]][i,9] <- (distance.scenario.person[i,k]/distance.baseline.person[i,k])^victim.safety
  }
  for (i in 5:7) {
    scenario.multiplier[[k]][i,9] <- (distance.scenario.person[i+1,k]/distance.baseline.person[i+1,k])^victim.safety
  }
}

##### Scenario Injuries #####
#compute the scenario injuries
injury.scenario <- mapply(function(x,y) x*y,injury.baseline,c(scenario.multiplier,scenario.multiplier),SIMPLIFY=FALSE)

##### Injuries results #####
injury.result <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames[1:8],ModeNames)))), 4)
names(injury.result) <- c("baseline fatalities","baseline injuries","scenario fatalities","scenario injuries")

injury.result[[1]] <- injury.baseline[[1]]+injury.baseline[[2]]+injury.baseline[[3]]
injury.result[[2]] <- injury.baseline[[4]]+injury.baseline[[5]]+injury.baseline[[6]]
injury.result[[3]] <- injury.scenario[[1]]+injury.scenario[[2]]+injury.scenario[[3]]
injury.result[[4]] <- injury.scenario[[4]]+injury.scenario[[5]]+injury.scenario[[6]]

injury.result <- lapply(injury.result,function(x) {
  x[is.na(x)]=0 
  return(x)})

injury.RR <- rep(list((matrix(NA,nrow=nTrafficModeV+1,ncol=3,dimnames=list(c(ModeNames[1:8],"total"),c("baseline","scenario","RR"))))), 2)
names(injury.RR) <- c("fatalities","serious injuries")

for (i in 1:2) {
  injury.RR[[i]][1:8,1] <- rowSums(injury.result[[i]])
  injury.RR[[i]][1:8,2] <- rowSums(injury.result[[i+2]])
  injury.RR[[i]][9,1:2] <- colSums(injury.RR[[i]][1:8,1:2])
  injury.RR[[i]][,3] <- injury.RR[[i]][,2]/injury.RR[[i]][,1]
}




