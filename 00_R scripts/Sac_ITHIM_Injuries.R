############ ITHIM application for Sacramento County ##########

##### Injury Module #####

#set your working directory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/03_Sacramento Case")

# Prevent scientific notation
options(scipen = 100)

# read the csv of injury data
# data source: SWITRS 2006-2010 (Statewide Integrated Traffic Reporting System: 5-year annual average number of road traffic injuries)
temp <- read.table("05_Injury.csv",header=T,sep=',')
injury <- temp[,2:10]
  
# number of traffic modes, injuried types, and road types
nTrafficModeV <- 8L #victim mode
nTrafficModeS <- 9L #striking mode (include NOV)
nInjuriedType <- 2L #fatal & serious
ModeNames <- colnames(injury)
RoadTypes <- c("local","arterial","highway")
nRoadType <- length(RoadTypes)

##### Baseline Injuries #####
createBaselineInjury <- function(){
  # build the list of baseline injury data (the sheet "Baseline injuries" in ITHIM)
  injury.baseline <- list()
  for (n in 1:(nInjuriedType*nRoadType)){
    injury.baseline[[n]] <-injury[((n-1)*nTrafficModeV+1):(nTrafficModeV*n),1:(nTrafficModeS)]
  }
  injury.baseline <- lapply(injury.baseline,function(x) {
    row.names(x) <- ModeNames[1:8]
    return (x)
  })
  names(injury.baseline) <- c("local.fatal","arterial.fatal","highway.fatal","local.serious","arterial.serious","highway.serious")
  
  return(injury.baseline)
}

##### Distribution of Person & Vehicle Distance by Road Types  #####

#create the sheet "Dist by road type" in ITHIM spreadsheet 
createDistribution <- function(){
  # person distance for all modes (miles per day)
  # source: CHTS2012 & NHTS2009
  person.vehicle.distance_input <- read.csv("06_PersonVehicleDistance.csv")

  # for baseline
  t.person.distance.baseline <- person.vehicle.distance_input[1:8,3]
  
  # matrix of distance (person) distribution by road types and modes for baseline (%)
  # data source: for walk and bike, hard coded estimate; for other modes, CHTS2012
  precent.person.baseline <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(ModeNames[1:8],c("local","arterial","highway")))
  precent.person.baseline[,1]<-person.vehicle.distance_input[10:17,3]
  precent.person.baseline[,2]<-person.vehicle.distance_input[19:26,3]
  precent.person.baseline[,3]<-person.vehicle.distance_input[28:35,3]
  
  # matrix of distance (person) by road types and modes for baseline (miles per day)
  distance.baseline.person <- t.person.distance.baseline * precent.person.baseline
   
  # matrix of occupancy
  # source: hard coded estimate & CHTS2012
  occ.baseline <- person.vehicle.distance_input[38:45,3]
  
  # for scenario
  
  #person distance (person) for all road types and all modes (miles per day)
  # data source: MTC Travel Model One
  t.person.distance.scenario <- person.vehicle.distance_input[1:8,5]
  
  # matrix of distance (person) distribution by road types and modes for scenario (%)
  # data source: for walk and bike, hard coded estimate; for other modes, MTC Travel Model One
  precent.person.scenario <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(ModeNames[1:8],c("local","arterial","highway")))
  precent.person.scenario[,1] <- person.vehicle.distance_input[10:17,5]
  precent.person.scenario[,2] <- person.vehicle.distance_input[19:26,5]
  precent.person.scenario[,3] <- person.vehicle.distance_input[28:35,5]
  
  # matrix of distance (person) by road types for scenario (miles per day)
  distance.scenario.person <- t.person.distance.scenario * precent.person.scenario
  
  # matrix of occupancy
  # source: hard coded estimate & MTC Travel Model One
  occ.scenario <- person.vehicle.distance_input[38:45,5]
  
  # # for the scenario case
  # if (scenario.no ==1 | scenario.no==2 | scenario.no==3){
  #   # person distance (person) for all road types and all modes (miles per day)
  #   # data source: MTC Travel Model One
  #   t.person.distance.scenario <- c(0.449455623452742,0.0864333319829324,0.282430577165011,18.5556107757615,NA,0.0000007664437775167,NA,0.12788347)
  #   
  #   # matrix of distance (person) distribution by road types and modes for scenario (%)
  #   # data source: for walk and bike, hard coded estimate; for other modes, MTC Travel Model One
  #   precent.person.scenario <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(ModeNames[1:8],c("local","arterial","highway")))
  #   precent.person.scenario[,1] <- c(0.75,0.53,0.075048750103747,0.0810998439977028,NA,0.075048750103747,NA,0.0810998439977028)
  #   precent.person.scenario[,2] <- c(0.25,0.47,0.252611107543058,0.288307729519899,NA,0.252611107543058,NA,0.288307729519899)
  #   precent.person.scenario[,3] <- c(0.0000000667,0.0000000667,0.672340142353195,0.630592426482398,NA,0.672340142353195,NA,0.630592426482398)
  #   
  #   # matrix of distance (person) by road types for scenario (miles per day)
  #   distance.scenario.person <- t.person.distance.scenario * precent.person.scenario
  # 
  #   # matrix of occupancy
  #   # source: hard coded estimate & MTC Travel Model One
  #   occ.scenario <- c(1,1,10,1.33434155662758,NA,1,NA,1.00069988)
  #       
  # }else if (scenario.no ==4){
  #   # person distance (person) for all road types and all modes (miles per day)
  #   # data source: MTC Travel Model One
  #   t.person.distance.scenario <- c(0.623498784292235,2.49399513716894,2.875830553354,21.4168339676784,0.583379853097641,1.70441975367477,NA,0.127883472083225)
  #   
  #   # matrix of distance (person) distribution by road types and modes for scenario (%)
  #   # data source: baseline distribution
  #   precent.person.scenario <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(ModeNames[1:8],c("local","arterial","highway")))
  #   precent.person.scenario[,1] <- c(0.75,0.53,0.364,0.17756911,NA,0.17756911,NA,0.17756911)
  #   precent.person.scenario[,2] <- c(0.25,0.47,0.626,0.25345185,NA,0.25345185,NA,0.25345185)
  #   precent.person.scenario[,3] <- c(0.0000000667,0.0000000667,0.01,0.56897904,NA,0.56897904,NA,0.56897904)
  #   
  #   # matrix of distance (person) by road types for scenario (miles per day)
  #   distance.scenario.person <- t.person.distance.scenario * precent.person.scenario
  #   
  #   # adjust the value for mode "car"
  #   for (i in 1:3) {
  #     testvalue<-distance.baseline.person[4,i]-sum(distance.scenario.person[1:3,i])+sum(distance.baseline.person[1:3,i])-t.person.distance.scenario[5]*precent.person.scenario[3,i]+t.person.distance.baseline[5]*precent.person.scenario[3,i]
  #     ifelse (testvalue<0,distance.scenario.person[4,i]<-0.01,distance.scenario.person[4,i]<-testvalue) 
  #   }
  #    
  #   # source: hard coded estimate & MTC Travel Model One
  #   occ.scenario <- c(1,1,10,1.74763708716959,NA,1,NA,1.00069988)
  #   
  # }else {  
  #   return("Wrong scenario number. Please input a number among 1,2,3,and 4.")
  # }
     
  # matrix of distance (vehicle) by road types (miles per day)
  distance.baseline.vehicle <- distance.baseline.person/occ.baseline
  distance.scenario.vehicle <- distance.scenario.person/occ.scenario
  
  return(list(
    distance.baseline.person = distance.baseline.person,
    distance.scenario.person = distance.scenario.person,
    distance.baseline.vehicle = distance.baseline.vehicle,
    distance.scenario.vehicle = distance.scenario.vehicle
    ))
}


##### Scenario Injury Multiplier #####

createScenarioInjuryMultiplier <- function(dist){
  # safety factors (source: hard coded estimate)
  str.veh.safety.RR <- 0.5
  speed.safety <- 1.0
  other <- 1.0
  victim.safety <- 0.5
  
  # compute the list of scenario injury multiplier (i:row;j:col;k:element)
  scenario.multiplier <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames[1:8],ModeNames)))), nRoadType)
  names(scenario.multiplier) <- RoadTypes
  
  # using different calculation method for diffrent combinations of striking and victim modes
  for (k in 1:3) {
    # for first four traffic modes 
    for (j in 1:4) {
      for (i in 1:4) {
        scenario.multiplier[[k]][i,j] <- (dist$distance.scenario.person[i,k]/dist$distance.baseline.person[i,k])^victim.safety*(dist$distance.scenario.vehicle[j,k]/dist$distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
      }
      for (i in 5:7) {
        scenario.multiplier[[k]][i,j] <- (dist$distance.scenario.person[i+1,k]/dist$distance.baseline.person[i+1,k])^victim.safety*(dist$distance.scenario.vehicle[j,k]/dist$distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
      }
    }
    # for fifth to seventh traffic modes
    for (j in 5:7) {
      for (i in 1:4) {
        scenario.multiplier[[k]][i,j] <- (dist$distance.scenario.person[i,k]/dist$distance.baseline.person[i,k])^victim.safety*(dist$distance.scenario.vehicle[j+1,k]/dist$distance.baseline.vehicle[j+1,k])^str.veh.safety.RR*speed.safety*other
      }
      for (i in 5:7) {
        scenario.multiplier[[k]][i,j] <- (dist$distance.scenario.person[i+1,k]/dist$distance.baseline.person[i+1,k])^victim.safety*(dist$distance.scenario.vehicle[j+1,k]/dist$distance.baseline.vehicle[j+1,k])^str.veh.safety.RR*speed.safety*other
      }
    }
    # for the last traffic mode (NOV)
    for (i in 1:4) {
      scenario.multiplier[[k]][i,9] <- (dist$distance.scenario.person[i,k]/dist$distance.baseline.person[i,k])^victim.safety
    }
    for (i in 5:7) {
      scenario.multiplier[[k]][i,9] <- (dist$distance.scenario.person[i+1,k]/dist$distance.baseline.person[i+1,k])^victim.safety
    }
  }
  
  return(scenario.multiplier)
}


##### Scenario Injuries #####
#compute the scenario injuries
computeScenarioInjury <- function(injury.baseline,scenario.multiplier){
  injury.scenario <- mapply(function(x,y) x*y,injury.baseline,c(scenario.multiplier,scenario.multiplier),SIMPLIFY=FALSE)
  
  return(injury.scenario)
}

##### Injuries results #####

createInjuryResults <- function(injury.baseline,injury.scenario){
  injury.number <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames[1:8],ModeNames)))), 4)
  names(injury.number) <- c("baseline fatalities","baseline injuries","scenario fatalities","scenario injuries")
  
  #computing the total injury number
  injury.number[[1]] <- injury.baseline[[1]]+injury.baseline[[2]]+injury.baseline[[3]]#baseline fatalities
  injury.number[[2]] <- injury.baseline[[4]]+injury.baseline[[5]]+injury.baseline[[6]]#baseline injuries
  injury.number[[3]] <- injury.scenario[[1]]+injury.scenario[[2]]+injury.scenario[[3]]#scenario fatalities
  injury.number[[4]] <- injury.scenario[[4]]+injury.scenario[[5]]+injury.scenario[[6]]#scenario injuries
  
  # replace the NA with 0
  injury.number <- lapply(injury.number,function(x) {
    x[is.na(x)]=0 
    return(x)})
  
  # compute the RR
  injury.RR <- rep(list((matrix(NA,nrow=nTrafficModeV+1,ncol=3,dimnames=list(c(ModeNames[1:8],"total"),c("baseline","scenario","RR"))))), 2)
  names(injury.RR) <- c("fatalities","serious injuries") 
  for (i in 1:2) {
    injury.RR[[i]][1:8,1] <- rowSums(injury.number[[i]])
    injury.RR[[i]][1:8,2] <- rowSums(injury.number[[i+2]])
    injury.RR[[i]][9,1:2] <- colSums(injury.RR[[i]][1:8,1:2])
    injury.RR[[i]][,3] <- injury.RR[[i]][,2]/injury.RR[[i]][,1]
  }
  
  return(list(
    injury.number=injury.number,
    injury.RR=injury.RR
    ))
}

############################calculation example#################################

#obtain the ITHIM sheet "Baseline injuries"
injury.baseline <- createBaselineInjury()

#obtain the ITHIM sheet "Dist by rode type"
dist <-  createDistribution() 

#compute the "Scenario Injury Multiplier"
scenario.multiplier <- createScenarioInjuryMultiplier(dist)

#compute the scenario injuries
injury.scenario <- computeScenarioInjury(injury.baseline,scenario.multiplier)

#Summarize the injury results
injury.result <- createInjuryResults(injury.baseline,injury.scenario)

