###################### ITHIM application for Equity Analysis - Injury Module ######################

#set your working directory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/04_Equity Analysis")

# Prevent scientific notation
options(scipen = 100)

# read the csv of injury data
# data source: SWITRS 2012-2016 (Statewide Integrated Traffic Reporting System)
# data source: Transportation Injury Mapping System (TIMS by US Berkeley)
# 5-year annual average number of road traffic injuries

# build a list to store the baseline injury data sets for different races
injury.list <- rep(list(matrix(NA,36,6)),4)
temp.NHW <- read.csv("11_baseline injury/injury baseline_NHW.csv")
injury.list[[1]] <- temp.NHW[,2:7]
temp.NHB <- read.csv("11_baseline injury/injury baseline_NHB.csv")
injury.list[[2]] <- temp.NHB[,2:7]
temp.NHO <- read.csv("11_baseline injury/injury baseline_NHO.csv")
injury.list[[3]] <- temp.NHO[,2:7]
temp.HO <- read.csv("11_baseline injury/injury baseline_HO.csv")
injury.list[[4]] <- temp.HO[,2:7]

names(injury.list)<- c('NHW','NHB','NHO','HO')

# input the person & vehicle distance matrix
# include the distance distribution for three road types and occupancy for each traffic mode
person.vehicle.distance_input <- read.csv("12_PersonVehicleDistance.csv")
person.vehicle.distance_input.list <- rep(list(matrix(NA,34,1)),4)
for (i in 1:4){
  person.vehicle.distance_input.list[[i]]<-person.vehicle.distance_input[1:34,(4*i-1):(4*i+1)]
}
names(person.vehicle.distance_input.list)<- c('NHW','NHB','NHO','HO')

# number of traffic modes, injuried types, and road types
nTrafficModeV <- 6L #victim mode
nTrafficModeS <- 6L #striking mode (include NOV)
nInjuriedType <- 2L #fatal & serious
ModeNames <- colnames(injury.list[[1]])
RoadTypes <- c("local","arterial","highway")
nRoadType <- length(RoadTypes)

##### Function of creating the matrix of baseline injuries #####
createBaselineInjury <- function(injury){
  # build the list of baseline injury data (the sheet "Baseline injuries" in ITHIM spreadsheet)
  injury.baseline <- list()
  for (n in 1:(nInjuriedType*nRoadType)){
    injury.baseline[[n]] <-injury[((n-1)*nTrafficModeV+1):(nTrafficModeV*n),1:(nTrafficModeS)]
  }
  injury.baseline <- lapply(injury.baseline,function(x) {
    row.names(x) <- ModeNames[1:6]
    return (x)
  })
  names(injury.baseline) <- c("local.fatal","arterial.fatal","highway.fatal","local.serious","arterial.serious","highway.serious")
  
  return(injury.baseline)
}

##### Function of create the Distribution of Person & Vehicle Distance by Road Types  #####
#create the sheet "Dist by road type" in ITHIM spreadsheet 

createScenarioInjuryMultiplier <- function(person.vehicle.distance_input){
  # person distance for all modes (miles per day)
  # source: CHTS2012 & NHTS2009

  ### for baseline
  t.person.distance.baseline <- person.vehicle.distance_input[1:6,1]
  
  # matrix of distance (person) distribution by road types and modes for baseline (%)
  # data source: for walk and bike, hard coded estimate; for other modes, CHTS2012
  percent.person.baseline <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(ModeNames[1:6],c("local","arterial","highway")))
  percent.person.baseline[,1]<-person.vehicle.distance_input[8:13,1]
  percent.person.baseline[,2]<-person.vehicle.distance_input[15:20,1]
  percent.person.baseline[,3]<-person.vehicle.distance_input[22:27,1]
  
  # matrix of distance (person) by road types and modes for baseline (miles per day)
  distance.baseline.person <- t.person.distance.baseline * percent.person.baseline
   
  # matrix of occupancy
  # source: hard coded estimate & CHTS2012
  occ.baseline <- person.vehicle.distance_input[29:34,1]
  
  ### for scenario
  # person distance (person) for all road types and all modes (miles per day)
  # data source: MTC Travel Model One
  t.person.distance.scenario <- person.vehicle.distance_input[1:6,3]
  
  # matrix of distance (person) distribution by road types and modes for scenario (%)
  # data source: for walk and bike, hard coded estimate; for other modes, MTC Travel Model One
  percent.person.scenario <- matrix(NA,nrow=nTrafficModeV,ncol=3,dimnames=list(ModeNames[1:6],c("local","arterial","highway")))
  percent.person.scenario[,1] <- person.vehicle.distance_input[8:13,3]
  percent.person.scenario[,2] <- person.vehicle.distance_input[15:20,3]
  percent.person.scenario[,3] <- person.vehicle.distance_input[22:27,3]
  
  # matrix of distance (person) by road types for scenario (miles per day)
  distance.scenario.person <- t.person.distance.scenario * percent.person.scenario
  
  # matrix of occupancy
  # source: hard coded estimate & MTC Travel Model One
  occ.scenario <- person.vehicle.distance_input[29:34,3]
     
  # matrix of distance (vehicle) by road types (miles per day)
  distance.baseline.vehicle <- distance.baseline.person/occ.baseline
  distance.scenario.vehicle <- distance.scenario.person/occ.scenario
  
  # combine those matrix into a list
  dist<-list(
    distance.baseline.person = distance.baseline.person,
    distance.scenario.person = distance.scenario.person,
    distance.baseline.vehicle = distance.baseline.vehicle,
    distance.scenario.vehicle = distance.scenario.vehicle
  )
  
  ### create the multiplier
  # safety factors (source: hard coded estimate)
  str.veh.safety.RR <- 0.5
  speed.safety <- 1.0
  other <- 1.0
  victim.safety <- 0.5
  
  # compute the list of scenario injury multiplier (i:row;j:col;k:road type)
  scenario.multiplier <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames[1:6],ModeNames)))), nRoadType)
  names(scenario.multiplier) <- RoadTypes
  
  # using different calculation method for diffrent combinations of striking and victim modes
  for (k in 1:3){ #road type
    for (j in 1:6){ #striking vehicle mode
      for (i in 1:6){ #victim vehicle mode
        scenario.multiplier[[k]][i,j] <- 
          (dist$distance.scenario.person[i,k]/dist$distance.baseline.person[i,k])^victim.safety*(dist$distance.scenario.vehicle[j,k]/dist$distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
      }
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
  injury.number <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames[1:6],ModeNames)))), 4)
  names(injury.number) <- c("baseline fatalities","baseline injuries","scenario fatalities","scenario injuries")
  
  #computing the total injury number
  injury.number[[1]] <- injury.baseline[[1]]+injury.baseline[[2]]+injury.baseline[[3]]#baseline fatalities
  injury.number[[2]] <- injury.baseline[[4]]+injury.baseline[[5]]+injury.baseline[[6]]#baseline serious injuries
  injury.number[[3]] <- injury.scenario[[1]]+injury.scenario[[2]]+injury.scenario[[3]]#scenario fatalities
  injury.number[[4]] <- injury.scenario[[4]]+injury.scenario[[5]]+injury.scenario[[6]]#scenario serious injuries
  
  # replace the NA with 0
  injury.number <- lapply(injury.number,function(x) {
    x[is.na(x)]=0 
    return(x)})
  
  # compute the RR
  injury.RR <- rep(list((matrix(NA,nrow=nTrafficModeV+1,ncol=3,dimnames=list(c(ModeNames[1:6],"total"),c("baseline","scenario","RR"))))), 2)
  names(injury.RR) <- c("fatalities","serious injuries") 
  for (i in 1:2) {
    injury.RR[[i]][1:6,1] <- rowSums(injury.number[[i]])
    injury.RR[[i]][1:6,2] <- rowSums(injury.number[[i+2]])
    injury.RR[[i]][7,1:2] <- colSums(injury.RR[[i]][1:6,1:2])
    injury.RR[[i]][,3] <- injury.RR[[i]][,2]/injury.RR[[i]][,1]
  }
  
  return(list(
    injury.number=injury.number,
    injury.RR=injury.RR
    ))
}


############################calculation example#################################

#obtain the ITHIM sheet "Baseline injuries"
injury.baseline.byRace <-  lapply(injury.list,function(x) createBaselineInjury(x))

#compute the "Scenario Injury Multiplier"
scenario.multiplier.byRace <- lapply(person.vehicle.distance_input.list,function(x) createScenarioInjuryMultiplier(x))

#compute the scenario injuries
injury.scenario.byRace <- mapply(function(x,y) computeScenarioInjury(x,y),injury.baseline.byRace,scenario.multiplier.byRace,SIMPLIFY = FALSE)

#Summarize the injury results
injury.result.byRace <- mapply(function(x,y) createInjuryResults(x,y),injury.baseline.byRace,injury.scenario.byRace,SIMPLIFY = FALSE)

