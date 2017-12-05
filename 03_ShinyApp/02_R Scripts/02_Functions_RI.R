# This file is part of ITHIM Sacramento.

# File: 02_Functions_RI.R
# Purpose: Functions for calculating the estimated change of health burden related to traffic injury

# Structure:
# Part 1: Read External Data Sources and Define Parameter
# Part 2: Function Definition - Data Inputs
# Part 3: Function Definition - Data Process (Baseline injury + Scenario Injury Multiplier)
# Part 4: Function Definition - Compute Scenario Injury
# Part 5: Function Definition - Output (ggplot)

# Part 1 Read External Data Sources and Define Parameter --------------------------------------------------

#test
#setwd('/Users/Yizheng/Documents/02_Work/15_GitHub-Central/ITHIM-Sacramento/03_ShinyApp')

# number of traffic modes, injuried types, and road types
nTrafficModeV <- 6L #victim mode
nTrafficModeS <- 7L #striking mode (include one-party)
nInjuriedType <- 2L #fatal & serious
ModeNames <- c("bike", "walk", "motorcycle", "car", "truck", "bus") #traffic mode
RoadTypes <- c("local", "arterial", "highway") # road type
nRoadType <- length(RoadTypes)

# disease burden
dbNames <- c("Deaths","DALYs")

# county names
countyNames <- c("El Dorado", "Placer", "Sacramento", "Sutter", "Yolo", "Yuba")

# input the population data for each race group
Pop.file.race <- read.csv("01_Data/01_Population/02_Population_byRace_2012.csv")
Pop.file.twoRaces <- cbind(Pop.file.race[,c(2,3)],
                           rowSums(Pop.file.race[,c(4,6,8)]),
                           rowSums(Pop.file.race[,c(5,7,9)]))
colnames(Pop.file.twoRaces) <- c("male.white","female.white","male.other","female.other")

# regionwide population
Pop.file.region <- NULL
temp.Pop.file.region <- cbind(rowSums(Pop.file.twoRaces[,c(1,3)]),rowSums(Pop.file.twoRaces[,c(2,4)]))

for (j in 1:2){
  for(i in 1:8){
    Pop.file.region[8*j-8+i] <- temp.Pop.file.region[i,j]+
      temp.Pop.file.region[i+9,j]+temp.Pop.file.region[i+18,j]+
      temp.Pop.file.region[i+27,j]+temp.Pop.file.region[i+36,j]+temp.Pop.file.region[i+45,j]
  }
}

# input the US population
US.pop <- read.csv("01_Data/01_Population/01_Population_US_EA.csv")
US.pop <- matrix(cbind(US.pop[,2],US.pop[,3]),16,1)

# input the GBD data
GBD.injury <- read.csv("01_Data/04_GBD/14_GBD_US_TrafficInjury.csv")
local.mort <- read.csv("01_Data/04_GBD/15_GBD_SACOG_mortality_TrafficInjury.csv")

# Part 2 Function Definition - Data Inputs ------------------------------------------------------

# function for reading csv files of injury data and VMT
# data source: SWITRS 2006-2016 (Statewide Integrated Traffic Reporting System)
# data source: Transportation Injury Mapping System (TIMS by US Berkeley)
# 11-year annual average number of road traffic injuries
input.csv <- function(countyID,scenarioID){
  #scenarioID: 0-2012,1-2020,2-2036,3-2027,4-S1,5-S2,6-S3,7-C1,8-C2,9-C3
  #countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
  
  filenames.injury <- list.files(path = "01_Data/05_baseline injury")[1:6]
  
  # build a list to store the baseline injury data sets for all races
  injury.list <- rep(list(matrix(NA,36,7)),2)
  temp <- read.csv(paste0("01_Data/05_baseline injury/",filenames.injury[countyID])) 
  injury.list[[1]] <- temp[1:36,2:8] # non hispanic white
  injury.list[[2]] <- temp[38:73,2:8] # other three races
  
  names(injury.list)<- c("NHW","Others")
  
  # input the person & vehicle distance matrix
  # include the distance distribution for three road types and occupancy for each traffic mode
  filenames.dist <- list.files(path="01_Data/06_PersonVehicleDistance")
  person.vehicle.distance_input.matrix <- matrix(NA,34,2)
  person.vehicle.distance_input <- read.csv(paste0("01_Data/06_PersonVehicleDistance/",
                                                   filenames.dist[scenarioID+1]))
  colnames(person.vehicle.distance_input.matrix)<- c("NHW","Others")
  
  #NHW
  person.vehicle.distance_input.matrix[,1]<-person.vehicle.distance_input[1:34,(4*countyID-1)]
  #Other three races
  person.vehicle.distance_input.matrix[,2]<-person.vehicle.distance_input[1:34,(4*countyID+1)]
  
  #GBD
  local.death.white <- local.mort[,2*countyID]
  local.death.other <- local.mort[,(2*countyID+1)]
  
  GBD.local.white <- GBD.injury[,2:5] * 
    matrix(cbind(Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),1],
                 Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),2]),16,1)/ US.pop
  
  GBD.local.Other <- GBD.injury[,2:5] * 
    matrix(cbind(Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),3],
                 Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),4]),16,1)/ US.pop
  
  GBD.local.white$deaths <- local.death.white
  GBD.local.Other$deaths <- local.death.other
  
  for (i in 1:16){
    if (GBD.local.white$deaths[i]>0){
      GBD.local.white$daly[i] <- GBD.local.white$deaths[i]/GBD.injury$deaths[i]*GBD.injury$daly[i]
    }
    
    if (GBD.local.Other$deaths[i]>0){
      GBD.local.Other$daly[i] <- GBD.local.Other$deaths[i]/GBD.injury$deaths[i]*GBD.injury$daly[i]
    }
    
  }
  
  return(list(
    injury.list = injury.list,
    person.vehicle.distance_input.matrix = person.vehicle.distance_input.matrix,
    GBD.local.white = GBD.local.white,
    GBD.local.Other = GBD.local.Other
  ))
}

# Part 3 Function Definition - Data Process --------------------------------------------------

# Function of creating the matrix of baseline injuries
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
  names(injury.baseline) <- c("local.fatal","arterial.fatal","highway.fatal",
                              "local.serious","arterial.serious","highway.serious")
  
  return(injury.baseline)
}

# Function of create the Distribution of Person & Vehicle Distance by Road Types

# create the sheet "Dist by road type" in ITHIM spreadsheet 
createScenarioInjuryMultiplier <- function(countyID,scenarioID){

  ### for baseline ###
  # person distance for all modes (miles per day)
  # source: SACSIM15
  t.person.distance.baseline <- input.csv(countyID,scenarioID = 0)
  t.person.distance.baseline.NHW <- t.person.distance.baseline$person.vehicle.distance_input.matrix[1:6,1]
  t.person.distance.baseline.Other <- t.person.distance.baseline$person.vehicle.distance_input.matrix[1:6,2]
  
  # matrix of distance (person) distribution by road types and modes for baseline (%)
  percent.person.baseline.NHW <- 
    percent.person.baseline.Other <- matrix(NA,nrow = 6,ncol = 3,
                                            dimnames=list(ModeNames,c("local","arterial","highway")))
  percent.person.baseline.NHW[,1] <- t.person.distance.baseline$person.vehicle.distance_input.matrix[8:13,1]
  percent.person.baseline.NHW[,2] <- t.person.distance.baseline$person.vehicle.distance_input.matrix[15:20,1]
  percent.person.baseline.NHW[,3] <- t.person.distance.baseline$person.vehicle.distance_input.matrix[22:27,1]
  
  percent.person.baseline.Other[,1] <- t.person.distance.baseline$person.vehicle.distance_input.matrix[8:13,2]
  percent.person.baseline.Other[,2] <- t.person.distance.baseline$person.vehicle.distance_input.matrix[15:20,2]
  percent.person.baseline.Other[,3] <- t.person.distance.baseline$person.vehicle.distance_input.matrix[22:27,2]
  
  # matrix of distance (person) by road types and modes for baseline (miles per day)
  distance.baseline.person.NHW <- t.person.distance.baseline.NHW * percent.person.baseline.NHW
  distance.baseline.person.Other <- t.person.distance.baseline.Other * percent.person.baseline.Other
  
  # matrix of occupancy
  # source: hard coded estimate
  occ.baseline.NHW <- t.person.distance.baseline$person.vehicle.distance_input.matrix[29:34,1]
  occ.baseline.Other <- t.person.distance.baseline$person.vehicle.distance_input.matrix[29:34,2]
  
  ### for scenario ###
  # person distance (person) for all road types and all modes (miles per day)
  # data source: SACSIM15
  t.person.distance.scenario <-input.csv(countyID,scenarioID = scenarioID)
  
  t.person.distance.scenario.NHW <- t.person.distance.scenario$person.vehicle.distance_input.matrix[1:6,1]
  t.person.distance.scenario.Other <- t.person.distance.scenario$person.vehicle.distance_input.matrix[1:6,2]
  
  # matrix of distance (person) distribution by road types and modes for scenario (%)
  # data source: for walk and bike, hard coded estimate; for other modes, MTC Travel Model One
  percent.person.scenario.NHW <- 
    percent.person.scenario.Other <- 
    matrix(NA,nrow = 6,ncol = 3,dimnames = list(ModeNames,c("local","arterial","highway")))
  percent.person.scenario.NHW[,1] <- t.person.distance.scenario$person.vehicle.distance_input.matrix[8:13,1]
  percent.person.scenario.NHW[,2] <- t.person.distance.scenario$person.vehicle.distance_input.matrix[15:20,1]
  percent.person.scenario.NHW[,3] <- t.person.distance.scenario$person.vehicle.distance_input.matrix[22:27,1]
  
  percent.person.scenario.Other[,1] <- t.person.distance.scenario$person.vehicle.distance_input.matrix[8:13,2]
  percent.person.scenario.Other[,2] <- t.person.distance.scenario$person.vehicle.distance_input.matrix[15:20,2]
  percent.person.scenario.Other[,3] <- t.person.distance.scenario$person.vehicle.distance_input.matrix[22:27,2]
  
  # matrix of distance (person) by road types for scenario (miles per day)
  distance.scenario.person.NHW <- t.person.distance.scenario.NHW * percent.person.scenario.NHW
  distance.scenario.person.Other <- t.person.distance.scenario.Other * percent.person.scenario.Other
  
  # matrix of occupancy
  occ.scenario.NHW <- t.person.distance.scenario$person.vehicle.distance_input.matrix[29:34,1]
  occ.scenario.Other <- t.person.distance.scenario$person.vehicle.distance_input.matrix[29:34,2]
  
  # matrix of distance (vehicle) by road types (miles per day)
  distance.baseline.vehicle.NHW <- distance.baseline.person.NHW/occ.baseline.NHW
  distance.baseline.vehicle.Other <- distance.baseline.person.Other/occ.baseline.Other
  
  distance.scenario.vehicle.NHW <- distance.scenario.person.NHW/occ.scenario.NHW
  distance.scenario.vehicle.Other <- distance.scenario.person.Other/occ.scenario.Other
  
  # combine those matrix into a list
  dist<-list(
    distance.baseline.person.NHW = distance.baseline.person.NHW,
    distance.baseline.person.Other = distance.baseline.person.Other,
    
    distance.scenario.person.NHW = distance.scenario.person.NHW,
    distance.scenario.person.Other = distance.scenario.person.Other,
    
    distance.baseline.vehicle.NHW = distance.baseline.vehicle.NHW,
    distance.baseline.vehicle.Other = distance.baseline.vehicle.Other,
    
    distance.scenario.vehicle.NHW = distance.scenario.vehicle.NHW,
    distance.scenario.vehicle.Other = distance.scenario.vehicle.Other
  )
  
  ### create the multiplier
  # safety factors (source: hard coded estimate)
  str.veh.safety.RR <- 0.5
  speed.safety <- 1.0
  other <- 1.0
  victim.safety <- 0.5
  
  # compute the list of scenario injury multiplier (i:row;j:col;k:road type)
  scenario.multiplier.NHW <- scenario.multiplier.Other <- 
    rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,
                     dimnames=list(ModeNames,c(ModeNames,"one.party"))))), nRoadType)
  names(scenario.multiplier.NHW) <- names(scenario.multiplier.Other) <- RoadTypes
  
  # using different calculation method for different party numbers (1vs2)
  for (k in 1:3){ #road type
    for (i in 1:6){ #victim vehicle mode
      # for one party collisions
      scenario.multiplier.NHW[[k]][i,7]<- 
        (dist$distance.scenario.person.NHW[i,k]/dist$distance.baseline.person.NHW[i,k])^victim.safety*speed.safety*other
      scenario.multiplier.Other[[k]][i,7]<- 
        (dist$distance.scenario.person.Other[i,k]/dist$distance.baseline.person.Other[i,k])^victim.safety*speed.safety*other
      
      for (j in 1:6){ #striking vehicle mode
        scenario.multiplier.NHW[[k]][i,j] <- 
          (dist$distance.scenario.person.NHW[i,k]/dist$distance.baseline.person.NHW[i,k])^victim.safety*(dist$distance.scenario.vehicle.NHW[j,k]/dist$distance.baseline.vehicle.NHW[j,k])^str.veh.safety.RR*speed.safety*other
        scenario.multiplier.Other[[k]][i,j] <- 
          (dist$distance.scenario.person.Other[i,k]/dist$distance.baseline.person.Other[i,k])^victim.safety*(dist$distance.scenario.vehicle.Other[j,k]/dist$distance.baseline.vehicle.Other[j,k])^str.veh.safety.RR*speed.safety*other
        
      }
    }
  }
  return(list(
    scenario.multiplier.NHW = scenario.multiplier.NHW,
    scenario.multiplier.Other = scenario.multiplier.Other
  )
  )
  
}

# Part 4 Function Definition - Compute Scenario Injury --------------------------------------------------

##### Scenario Injuries
#compute the scenario injuries (the product of baseline injury and scenario multiplier)
computeScenarioInjury <- function(injury.baseline,scenario.multiplier){
  
  injury.scenario.NHW <- mapply(function(x,y) x*y,
                                injury.baseline$NHW,
                                c(scenario.multiplier$scenario.multiplier.NHW,
                                  scenario.multiplier$scenario.multiplier.NHW),SIMPLIFY=FALSE)
  injury.scenario.Other <- mapply(function(x,y) x*y,
                                  injury.baseline$Other,
                                  c(scenario.multiplier$scenario.multiplier.Other,
                                    scenario.multiplier$scenario.multiplier.Other),SIMPLIFY=FALSE)
  
  return(list(
    injury.scenario.NHW = injury.scenario.NHW,
    injury.scenario.Other = injury.scenario.Other
  ))
}

##### Injuries results
createInjuryResults <- function(countyID,scenarioID){

  injury.list <- input.csv(countyID = countyID,scenarioID = 0)[[1]]
  
  # baseline injury
  injury.baseline.byRace <-  lapply(injury.list,function(x) createBaselineInjury(x))
  
  # scenario multiplier
  scenario.multiplier.byRace <- createScenarioInjuryMultiplier(countyID = countyID,scenarioID = scenarioID)
  
  # scenario injury
  injury.scenario.byRace <- computeScenarioInjury(injury.baseline.byRace,scenario.multiplier.byRace)
  
  # compute the injury by race group
  total.injury.baseline.NHW <- lapply(injury.baseline.byRace$NHW,sum) 
  total.injury.baseline.Other <- lapply(injury.baseline.byRace$Others,sum) 
  
  total.injury.scenario.NHW <- lapply(injury.scenario.byRace$injury.scenario.NHW,sum) 
  total.injury.scenario.Other <- lapply(injury.scenario.byRace$injury.scenario.Other,sum) 
  
  # compute the relative risk of fatality for each race group
  RR.fatality.NHW <- (total.injury.scenario.NHW[[1]]+
                        total.injury.scenario.NHW[[2]]+total.injury.scenario.NHW[[3]])/
    (total.injury.baseline.NHW[[1]]+total.injury.baseline.NHW[[2]]+total.injury.baseline.NHW[[3]])
  RR.fatality.Other <-(total.injury.scenario.Other[[1]]+
                         total.injury.scenario.Other[[2]]+total.injury.scenario.Other[[3]])/
    (total.injury.baseline.Other[[1]]+total.injury.baseline.Other[[2]]+total.injury.baseline.Other[[3]])
  
  # compute the relative risk of serious injuries for each race group
  RR.serious.NHW <-(total.injury.scenario.NHW[[4]]+total.injury.scenario.NHW[[5]]+total.injury.scenario.NHW[[6]])/(total.injury.baseline.NHW[[4]]+total.injury.baseline.NHW[[5]]+total.injury.baseline.NHW[[6]])
  RR.serious.Other <- (total.injury.scenario.Other[[4]]+total.injury.scenario.Other[[5]]+total.injury.scenario.Other[[6]])/(total.injury.baseline.Other[[4]]+total.injury.baseline.Other[[5]]+total.injury.baseline.Other[[6]])
  
  # GBD by race groups
  GBD.white.temp <- input.csv(countyID = countyID,scenarioID = 0)[[3]]
  GBD.other.temp <- input.csv(countyID = countyID,scenarioID = 0)[[4]]
  
  # compute the reduction of deaths and DALYs by using the RR computed above
  Reduction.Death.white.disaggr <- matrix(GBD.white.temp[,1]*(1-RR.fatality.NHW),16,1)
  Reduction.Death.other.disaggr <- matrix(GBD.other.temp[,1]*(1-RR.fatality.Other),16,1)
  
  Reduction.yll.white.disaggr <- matrix(GBD.white.temp[,2]*(1-RR.fatality.NHW),16,1)
  Reduction.yll.other.disaggr <- matrix(GBD.other.temp[,2]*(1-RR.fatality.Other),16,1)
  
  Reduction.yld.white.disaggr <- matrix(GBD.white.temp[,3]*(1-RR.serious.NHW),16,1)
  Reduction.yld.other.disaggr <- matrix(GBD.other.temp[,3]*(1-RR.serious.Other),16,1)
  
  Reduction.DALYs.white.disaggr <- Reduction.yll.white.disaggr+Reduction.yld.white.disaggr
  Reduction.DALYs.other.disaggr <- Reduction.yll.other.disaggr+Reduction.yld.other.disaggr
  
  return(list(

    Reduction.Death.white=sum(Reduction.Death.white.disaggr),
    Reduction.Death.other=sum(Reduction.Death.other.disaggr),
    Reduction.DALYs.white=sum(Reduction.DALYs.white.disaggr),
    Reduction.DALYs.other=sum(Reduction.DALYs.other.disaggr),
    
    Reduction.Death.white.disaggr=Reduction.Death.white.disaggr,
    Reduction.Death.other.disaggr=Reduction.Death.other.disaggr,
    Reduction.DALYs.white.disaggr=Reduction.DALYs.white.disaggr,
    Reduction.DALYs.other.disaggr=Reduction.DALYs.other.disaggr
  ))
}

# function for computing the age.std results
computeAgeStdOutput.injury <- function(scenario,countyID){
   
  fatality.NHW.age.gender <- scenario$Reduction.Death.white.disaggr/matrix(
    cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1],
          Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),2]),16,1)*100000
  fatality.Other.age.gender <- scenario$Reduction.Death.other.disaggr/matrix(
    cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),3],
          Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),4]),16,1)*100000
  
  DALYs.NHW.age.gender <- scenario$Reduction.DALYs.white.disaggr/matrix(
    cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1],
          Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),2]),16,1)*100000
  DALYs.Other.age.gender <- scenario$Reduction.DALYs.other.disaggr/matrix(
    cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1],
          Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),2]),16,1)*100000
  
  age.std.fatality.NHW <- sum(fatality.NHW.age.gender * US.pop)/sum(US.pop)
  age.std.fatality.other <- sum(fatality.Other.age.gender * US.pop)/sum(US.pop)
  
  age.std.DALYs.NHW <- sum(DALYs.NHW.age.gender * US.pop)/sum(US.pop)
  age.std.DALYs.other <- sum(DALYs.Other.age.gender * US.pop)/sum(US.pop)
  
  return(list(
    age.std.fatality.NHW = age.std.fatality.NHW,
    age.std.fatality.other = age.std.fatality.other,
    age.std.DALYs.NHW = age.std.DALYs.NHW,
    age.std.DALYs.other = age.std.DALYs.other
  ))
  
}

# Part 5 Function Definition - Output (ggplot) --------------------------------------------------

# barID: 1-future years,2-Scenarios,3-customized
# typeID: 1-raw,2-age.std
DFforFigure.injury <- function(barID,countyID,typeID){
  
  reduction.fatality.value <- reduction.DALYs.value <-matrix(NA,6,1)
  
  if (barID == 1){#future years
    scenario.1 <- createInjuryResults(countyID = countyID,scenarioID = 1)#2020
    scenario.2 <- createInjuryResults(countyID = countyID,scenarioID = 3)#2027
    scenario.3 <- createInjuryResults(countyID = countyID,scenarioID = 2)#2036
    
    scenario.name  <- rep(c("2020","2027","2036"),each= 2)
    
  }else if (barID==2){#scenarios
    scenario.1 <- createInjuryResults(countyID = countyID,scenarioID = 4)
    scenario.2 <- createInjuryResults(countyID = countyID,scenarioID = 5)
    scenario.3 <- createInjuryResults(countyID = countyID,scenarioID = 6)
    
    scenario.name  <- rep(c("S1","S2","S3"),each= 2)
    
  }else if(barID==3){#customized
    scenario.1 <- createInjuryResults(countyID = countyID,scenarioID = 7)
    scenario.2 <- createInjuryResults(countyID = countyID,scenarioID = 8)
    scenario.3 <- createInjuryResults(countyID = countyID,scenarioID = 9)
    
    scenario.name  <- rep(c("C1","C2","C3"),each= 2)
  }
  
  if (typeID == 2){
    scenario.1 <- computeAgeStdOutput.injury(scenario.1,countyID)
    scenario.2 <- computeAgeStdOutput.injury(scenario.2,countyID)
    scenario.3 <- computeAgeStdOutput.injury(scenario.3,countyID)
  }
  
  # matrix of death reduction
  reduction.fatality.value[1,1] <-scenario.1[[1]]
  reduction.fatality.value[2,1] <-scenario.1[[2]]
  reduction.fatality.value[3,1] <-scenario.2[[1]]
  reduction.fatality.value[4,1] <-scenario.2[[2]]
  reduction.fatality.value[5,1] <-scenario.3[[1]]
  reduction.fatality.value[6,1] <-scenario.3[[2]]
  
  # matrix of DALYs reduction
  reduction.DALYs.value[1,1] <-scenario.1[[3]]
  reduction.DALYs.value[2,1] <-scenario.1[[4]]
  reduction.DALYs.value[3,1] <-scenario.2[[3]]
  reduction.DALYs.value[4,1] <-scenario.2[[4]]
  reduction.DALYs.value[5,1] <-scenario.3[[3]]
  reduction.DALYs.value[6,1] <-scenario.3[[4]]
 
  raceGroup <- rep(c("1.White","2.People of color"),3)
  
  # build the data frame
  df.fatality <- data.frame(Scenario=scenario.name,DemogrGroup=raceGroup,V1 =(reduction.fatality.value))
  df.DALYs <- data.frame(Scenario=scenario.name,DemogrGroup=raceGroup,V1 =(reduction.DALYs.value))
  
  return(list(
    df.fatality=df.fatality,
    df.DALYs=df.DALYs
  ))
}

# barID: 1-future years,2-Scenarios,3-customized
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
plot.shiny.app.injury <- function(countyID, barID, yaxisID){
  
  if (countyID %in% c(1:6)) {
    
    if (yaxisID == 1){
      
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
      df.plot <- df.result.injury$df.fatality
      
      plot.title <- paste0(countyNames[countyID],": Reduction in Total Deaths from\n Traffic Injury Module")
      ylabel <- "Reduction in deaths (total)"

    }else if (yaxisID == 2){
      
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
      df.plot <- df.result.injury$df.fatality
      
      plot.title <- paste0(countyNames[countyID],": Total Deaths from\n Traffic Injury Module",
                           "Standardized by Age and Population")
      ylabel <- "Reduction in deaths\n(per 100,000 population)"
      
    }else if (yaxisID == 3){
      
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
      df.plot <- df.result.injury$df.DALYs
      
      plot.title <- paste0(countyNames[countyID],": Reduction in Total DALYs from Traffic Injury Module")
      ylabel <- "Reduction in DALYs (total)"
      
    }else if (yaxisID == 4){
      
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
      df.plot <- df.result.injury$df.DALYs
      
      plot.title <- paste0(countyNames[countyID],": Total DALYs from Traffic Injury Module\n",
                           "Standardized by Age and Population")
      ylabel <- "Reduction in DALYs\n(per 100,000 population)"

    }
    
    ggplot(data = df.plot, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
      geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.5)) + 
      scale_fill_brewer(palette = "Set1") + 
      xlab(NULL) + 
      ylab(ylabel) +
      geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
                position = position_dodge(width = 0.5)) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom",
            plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
      labs(caption = paste("Planning scenarios and future years are shown relative to ",
                                     "the baseline year 2012.")) +
      ggtitle(plot.title)
    
  }else if (countyID == 7 ){
    
    df.region <- NULL
    
    if (yaxisID == 1){
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
        df.region <- rbind(df.region,df.temp$df.fatality)
      }
      
      plot.title <- paste0("Region Wide: Reduction in Total Deaths from\n Traffic Injury Module")
      ylabel <- "Reduction in deaths (total)"
      
    }else if (yaxisID == 2){
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
        df.region <- rbind(df.region,df.temp$df.fatality)
      }
     
      plot.title <- paste0("Region Wide: Deaths from Traffic Injury Module\n",
                           "Standardized by Age and Population")
      ylabel <- "Reduction in deaths\n(per 100,000 population)"
      
    }else if (yaxisID == 3){
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
        
        df.region <- rbind(df.region,df.temp$df.DALYs)
        
      }
      
      plot.title <- paste0("Region Wide: Reduction in Total DALYs from Traffic Injury Module")
      ylabel <- "Reduction in DALYs (total)"
      
    }else if (yaxisID == 4){
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
        
        df.region <- rbind(df.region,df.temp$df.DALYs)
        
      }
      
      plot.title <- paste0("Region Wide: Age-Standardized Reduction in Total DALYs from Traffic Injury Module")
      ylabel <- "Reduction in DALYs\n(per 100,000 population)"
      
    }
    
    df.region$county <- rep(countyNames,each = 6)
    
    ggplot(data = df.region, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
      geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.5)) + 
      scale_fill_brewer(palette = "Set1") + 
      xlab(NULL) + 
      ylab(ylabel) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom",
            plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
      labs(caption = paste("Planning scenarios and future years are shown relative to ", 
                           "the baseline year 2012.")) +
      ggtitle(plot.title) +
      facet_wrap(~county)
    
  }
  
  
}


