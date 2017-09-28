###################### ITHIM application for Equity Analysis - Injury Module ######################

#set your working directory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/06_Equity Analysis")

# Prevent scientific notation
options(scipen = 100)

# number of traffic modes, injuried types, and road types
nTrafficModeV <- 6L #victim mode
nTrafficModeS <- 7L #striking mode (include one-party)
nInjuriedType <- 2L #fatal & serious
ModeNames <- c("bike","walk","motorcycle","car","bus","truck")
RoadTypes <- c("local","arterial","highway")
nRoadType <- length(RoadTypes)

#### function for reading csv files of injury data and VMT
# data source: SWITRS 2006-2016 (Statewide Integrated Traffic Reporting System)
# data source: Transportation Injury Mapping System (TIMS by US Berkeley)
# 11-year annual average number of road traffic injuries
input.csv <- function(countyID){
  #test
  #countyID = 1
  
  filenames.injury <- readLines("05_baseline injury/00_filenames.txt")
  
  # build a list to store the baseline injury data sets for all races
  injury.list <- rep(list(matrix(NA,36,7)),4)
  temp.NHW <- read.csv(filenames.injury[4*countyID-3]) # non hispanic white
  injury.list[[1]] <- temp.NHW[,2:8]
  temp.NHB <- read.csv(filenames.injury[4*countyID-2]) # non hispanic black
  injury.list[[2]] <- temp.NHB[,2:8]
  temp.NHO <- read.csv(filenames.injury[4*countyID-1]) # non hispanic other races
  injury.list[[3]] <- temp.NHO[,2:8]
  temp.HO <- read.csv(filenames.injury[4*countyID])   # hispanic 
  injury.list[[4]] <- temp.HO[,2:8]
  names(injury.list)<- c('NHW','NHB','NHO','HO')
  
  # input the person & vehicle distance matrix
  # include the distance distribution for three road types and occupancy for each traffic mode
  filenames.vmt <- readLines("06_PersonVehicleDistance/00_filenames.txt")
  
  person.vehicle.distance_input <- read.csv(filenames.vmt[countyID])
  person.vehicle.distance_input.list <- rep(list(matrix(NA,34,1)),4)
  for (i in 1:4){
    person.vehicle.distance_input.list[[i]]<-person.vehicle.distance_input[1:34,(6*i-3):(6*i+1)]
  }
  names(person.vehicle.distance_input.list)<- c('NHW','NHB','NHO','HO')
  
  return(list(
    injury.list=injury.list,
    person.vehicle.distance_input.list=person.vehicle.distance_input.list
  ))
}


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
  
  #test
  #person.vehicle.distance_input = person.vehicle.distance_input.list$NHW
  
  ### for baseline ###
  # person distance for all modes (miles per day)
  # source: CHTS2012 & NHTS2009
  t.person.distance.baseline <- person.vehicle.distance_input[1:6,1]
  
  # matrix of distance (person) distribution by road types and modes for baseline (%)
  # data source: for walk and bike, hard coded estimate; for other modes, CHTS2012
  percent.person.baseline <- matrix(NA,nrow=6,ncol=3,dimnames=list(ModeNames,c("local","arterial","highway")))
  percent.person.baseline[,1]<-person.vehicle.distance_input[8:13,1]
  percent.person.baseline[,2]<-person.vehicle.distance_input[15:20,1]
  percent.person.baseline[,3]<-person.vehicle.distance_input[22:27,1]
  
  # matrix of distance (person) by road types and modes for baseline (miles per day)
  distance.baseline.person <- t.person.distance.baseline * percent.person.baseline
   
  # matrix of occupancy
  # source: hard coded estimate & CHTS2012
  occ.baseline <- person.vehicle.distance_input[29:34,1]
  
  ### for scenario ###
  # person distance (person) for all road types and all modes (miles per day)
  # data source: MTC Travel Model One
  t.person.distance.scenario.2020 <- person.vehicle.distance_input[1:6,3]
  t.person.distance.scenario.2036 <- person.vehicle.distance_input[1:6,5]
  
  # matrix of distance (person) distribution by road types and modes for scenario (%)
  # data source: for walk and bike, hard coded estimate; for other modes, MTC Travel Model One
  percent.person.scenario.2020 <- matrix(NA,nrow=6,ncol=3,dimnames=list(ModeNames,c("local","arterial","highway")))
  percent.person.scenario.2020[,1] <- person.vehicle.distance_input[8:13,3]
  percent.person.scenario.2020[,2] <- person.vehicle.distance_input[15:20,3]
  percent.person.scenario.2020[,3] <- person.vehicle.distance_input[22:27,3]
  
  percent.person.scenario.2036 <- matrix(NA,nrow=6,ncol=3,dimnames=list(ModeNames,c("local","arterial","highway")))
  percent.person.scenario.2036[,1] <- person.vehicle.distance_input[8:13,5]
  percent.person.scenario.2036[,2] <- person.vehicle.distance_input[15:20,5]
  percent.person.scenario.2036[,3] <- person.vehicle.distance_input[22:27,5]
  
  # matrix of distance (person) by road types for scenario (miles per day)
  distance.scenario.person.2020 <- t.person.distance.scenario.2020 * percent.person.scenario.2020
  distance.scenario.person.2036 <- t.person.distance.scenario.2036 * percent.person.scenario.2036
  
  # matrix of occupancy
  # source: hard coded estimate & MTC Travel Model One
  occ.scenario.2020 <- person.vehicle.distance_input[29:34,3]
  occ.scenario.2036 <- person.vehicle.distance_input[29:34,5]
     
  # matrix of distance (vehicle) by road types (miles per day)
  distance.baseline.vehicle <- distance.baseline.person/occ.baseline
  distance.scenario.vehicle.2020 <- distance.scenario.person.2020/occ.scenario.2020
  distance.scenario.vehicle.2036 <- distance.scenario.person.2036/occ.scenario.2036
  
  # combine those matrix into a list
  dist<-list(
    distance.baseline.person = distance.baseline.person,
    distance.scenario.person.2020 = distance.scenario.person.2020,
    distance.scenario.person.2036 = distance.scenario.person.2036,
    distance.baseline.vehicle = distance.baseline.vehicle,
    distance.scenario.vehicle.2020 = distance.scenario.vehicle.2020,
    distance.scenario.vehicle.2036 = distance.scenario.vehicle.2036
  )
  
  ### create the multiplier
  # safety factors (source: hard coded estimate)
  str.veh.safety.RR <- 0.5
  speed.safety <- 1.0
  other <- 1.0
  victim.safety <- 0.5
  
  # compute the list of scenario injury multiplier (i:row;j:col;k:road type)
  scenario.multiplier.2020 <- scenario.multiplier.2036 <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames,c(ModeNames,'one.party'))))), nRoadType)
  names(scenario.multiplier.2020) <- names(scenario.multiplier.2036) <- RoadTypes
  
  # using different calculation method for different party numbers (1vs2)
  for (k in 1:3){ #road type
    for (i in 1:6){ #victim vehicle mode
      # for one party collisions
      scenario.multiplier.2020[[k]][i,7]<- 
        (dist$distance.scenario.person.2020[i,k]/dist$distance.baseline.person[i,k])^victim.safety*speed.safety*other
      scenario.multiplier.2036[[k]][i,7]<- 
        (dist$distance.scenario.person.2036[i,k]/dist$distance.baseline.person[i,k])^victim.safety*speed.safety*other
      
      for (j in 1:6){ #striking vehicle mode
        scenario.multiplier.2020[[k]][i,j] <- 
          (dist$distance.scenario.person.2020[i,k]/dist$distance.baseline.person[i,k])^victim.safety*(dist$distance.scenario.vehicle.2020[j,k]/dist$distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
        scenario.multiplier.2036[[k]][i,j] <- 
          (dist$distance.scenario.person.2036[i,k]/dist$distance.baseline.person[i,k])^victim.safety*(dist$distance.scenario.vehicle.2036[j,k]/dist$distance.baseline.vehicle[j,k])^str.veh.safety.RR*speed.safety*other
        
        }
    }
  }
  return(list(
    scenario.multiplier.2020=scenario.multiplier.2020,
    scenario.multiplier.2036=scenario.multiplier.2036
  )
    )
  
}

##### Scenario Injuries #####
#compute the scenario injuries (the product of baseline injury and scenario multiplier)
computeScenarioInjury <- function(injury.baseline,scenario.multiplier){
  
  injury.scenario.2020 <- mapply(function(x,y) x*y,injury.baseline,c(scenario.multiplier$scenario.multiplier.2020,scenario.multiplier$scenario.multiplier.2020),SIMPLIFY=FALSE)
  injury.scenario.2036 <- mapply(function(x,y) x*y,injury.baseline,c(scenario.multiplier$scenario.multiplier.2036,scenario.multiplier$scenario.multiplier.2036),SIMPLIFY=FALSE)
  
  return(list(
    injury.scenario.2020 = injury.scenario.2020,
    injury.scenario.2036 = injury.scenario.2036
    ))
}

##### Injuries results #####

createInjuryResults <- function(injury.baseline,injury.scenario){
  injury.number.2020 <- injury.number.2036 <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames,c(ModeNames,'one.party'))))), 4)
  names(injury.number.2020) <- names(injury.number.2036) <- c("baseline fatalities","baseline injuries","scenario fatalities","scenario injuries")
  
  #computing the total injury number
  injury.number.2020[[1]] <- injury.baseline[[1]]+injury.baseline[[2]]+injury.baseline[[3]]#baseline fatalities
  injury.number.2020[[2]] <- injury.baseline[[4]]+injury.baseline[[5]]+injury.baseline[[6]]#baseline serious injuries
  injury.number.2020[[3]] <- injury.scenario$injury.scenario.2020[[1]]+injury.scenario$injury.scenario.2020[[2]]+injury.scenario$injury.scenario.2020[[3]]#scenario fatalities
  injury.number.2020[[4]] <- injury.scenario$injury.scenario.2020[[4]]+injury.scenario$injury.scenario.2020[[5]]+injury.scenario$injury.scenario.2020[[6]]#scenario serious injuries
  
  injury.number.2036[[1]] <- injury.baseline[[1]]+injury.baseline[[2]]+injury.baseline[[3]]#baseline fatalities
  injury.number.2036[[2]] <- injury.baseline[[4]]+injury.baseline[[5]]+injury.baseline[[6]]#baseline serious injuries
  injury.number.2036[[3]] <- injury.scenario$injury.scenario.2036[[1]]+injury.scenario$injury.scenario.2036[[2]]+injury.scenario$injury.scenario.2036[[3]]#scenario fatalities
  injury.number.2036[[4]] <- injury.scenario$injury.scenario.2036[[4]]+injury.scenario$injury.scenario.2036[[5]]+injury.scenario$injury.scenario.2036[[6]]#scenario serious injuries
  
  
  # replace the NA with 0
  injury.number.2020 <- lapply(injury.number.2020,function(x) {
    x[is.na(x)]=0 
    return(x)})
  injury.number.2036 <- lapply(injury.number.2036,function(x) {
    x[is.na(x)]=0 
    return(x)})
  
  # compute the RR
  injury.RR.2020 <- injury.RR.2036 <- rep(list((matrix(NA,nrow=nTrafficModeV+1,ncol=3,dimnames=list(c(ModeNames[1:6],"total"),c("baseline","scenario","RR"))))), 2)
  names(injury.RR.2020) <- names(injury.RR.2036) <- c("fatalities","serious injuries") 
  for (i in 1:2) {
    injury.RR.2020[[i]][1:6,1] <- rowSums(injury.number.2020[[i]])
    injury.RR.2020[[i]][1:6,2] <- rowSums(injury.number.2020[[i+2]])
    injury.RR.2020[[i]][7,1:2] <- colSums(injury.RR.2020[[i]][1:6,1:2])
    injury.RR.2020[[i]][,3] <- injury.RR.2020[[i]][,2]/injury.RR.2020[[i]][,1]
    #injury.RR.2020[[i]][,3] <- replace(injury.RR.2020[[i]][,3],is.na(injury.RR.2020[[i]][,3]),1)
    
    injury.RR.2036[[i]][1:6,1] <- rowSums(injury.number.2036[[i]])
    injury.RR.2036[[i]][1:6,2] <- rowSums(injury.number.2036[[i+2]])
    injury.RR.2036[[i]][7,1:2] <- colSums(injury.RR.2036[[i]][1:6,1:2])
    injury.RR.2036[[i]][,3] <- injury.RR.2036[[i]][,2]/injury.RR.2036[[i]][,1]
    #injury.RR.2036[[i]][,3] <- replace(injury.RR.2036[[i]][,3],is.na(injury.RR.2036[[i]][,3]),1)
    
    
  }
  
  return(list(
    injury.number.2020=injury.number.2020,
    injury.number.2036=injury.number.2036,
    injury.RR.2020=injury.RR.2020,
    injury.RR.2036=injury.RR.2036
    ))
}

output.result <- function(countyID){
  # countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
  injury.list <- input.csv(countyID = countyID)[[1]]
  person.vehicle.distance_input.list <- input.csv(countyID = countyID)[[2]]
  
  #obtain the ITHIM sheet "Baseline injuries"
  injury.baseline.byRace <-  lapply(injury.list,function(x) createBaselineInjury(x))
  
  #compute the "Scenario Injury Multiplier"
  scenario.multiplier.byRace <- lapply(person.vehicle.distance_input.list,function(x) createScenarioInjuryMultiplier(x))
  
  #compute the scenario injuries
  injury.scenario.byRace <- mapply(function(x,y) computeScenarioInjury(x,y),injury.baseline.byRace,scenario.multiplier.byRace,SIMPLIFY = FALSE)
  
  #Summarize the injury results
  injury.result.byRace <- mapply(function(x,y) createInjuryResults(x,y),injury.baseline.byRace,injury.scenario.byRace,SIMPLIFY = FALSE)
  
  #format the output file  
  cutting.line.col <- matrix(c('2020 fatal','2020 serious','2036 fatal','2036 serious',rep('',24)),7,4,byrow = TRUE)
  NHW <- cbind(injury.result.byRace[[1]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[1]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
               injury.result.byRace[[1]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[1]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('NHW',rep('',6)),7,1))
  NHB <- cbind(injury.result.byRace[[2]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[2]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
               injury.result.byRace[[2]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[2]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('NHB',rep('',6)),7,1))
  NHO <- cbind(injury.result.byRace[[3]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[3]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
               injury.result.byRace[[3]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[3]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('NHO',rep('',6)),7,1))
  HO <-  cbind(injury.result.byRace[[4]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[4]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
               injury.result.byRace[[4]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[4]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('HO',rep('',6)),7,1))
  
  cutting.line.row <- matrix('',1,17)
  output <- rbind(NHW,cutting.line.row,NHB,cutting.line.row,NHO,cutting.line.row,HO)
  
  return(output)
}

############################calculation example#################################
write.csv(output.result(countyID=1),file = '00_HealthOutcome/00_Injury/ELD.injuryresult.csv')
write.csv(output.result(countyID=2),file = '00_HealthOutcome/00_Injury/PLA.injuryresult.csv')
write.csv(output.result(countyID=3),file = '00_HealthOutcome/00_Injury/SAC.injuryresult.csv')
write.csv(output.result(countyID=4),file = '00_HealthOutcome/00_Injury/SUT.injuryresult.csv')
write.csv(output.result(countyID=5),file = '00_HealthOutcome/00_Injury/YOL.injuryresult.csv')
write.csv(output.result(countyID=6),file = '00_HealthOutcome/00_Injury/YUB.injuryresult.csv')

