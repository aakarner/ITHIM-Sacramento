# This file is part of ITHIM Sacramento.

# File: 01_Functions_PA.R
# Purpose: Functions for calculating the estimated change of health burden related to physical activity

# Structure:
# Part 1: Function Definition - Data Inputs
# Part 2: Function Definition - Data Process (GBD + Total Exposure + Relative Risk)
# Part 3: Function Definition - Computing Health Burden
# Part 4: Function Definition - Output (ggplot)
# Part 5: Read external data sources and define parameter
# Part 6: Computation for ggplot

# Code
# Part 1 Function Definition - Data Inputs -------------------------------------------------------------

# function for reading .csv files by countyID
read.csv.files <- function(countyID){
  
  # input the population file (source: 2012 ACS PUMS one-year estimates)
  Pop_Input_byRace <- Pop.file.race[(countyID*9-8):(countyID*9-1),1:9]
  Pop_Input_byIncome <- Pop.file.income[(countyID*9-8):(countyID*9-1),1:9]
 
  # input the parameters of active transport of baseline,2020, 2027,and 2036
  # and for scenarios (S1,S2,S3) and customized data sets (C1,C2,C3)
  # (include relative walking/cycling time and speed)
  AT_Input_byRace.baseline <- AT.file.baseline.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.baseline <- AT.file.baseline.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.2020 <- AT.file.2020.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.2020 <- AT.file.2020.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.2036 <- AT.file.2036.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.2036 <- AT.file.2036.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.2027 <- AT.file.2027.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.2027 <- AT.file.2027.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.S1 <- AT.file.S1.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.S1 <- AT.file.S1.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.S2 <- AT.file.S2.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.S2 <- AT.file.S2.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.S3 <- AT.file.S3.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.S3 <- AT.file.S3.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.C1 <- AT.file.C1.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.C1 <- AT.file.C1.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.C2 <- AT.file.C2.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.C2 <- AT.file.C2.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  AT_Input_byRace.C3 <- AT.file.C3.byRace[(countyID*36-35):(countyID*36-1),1:10]
  AT_Input_byIncome.C3 <- AT.file.C3.byIncome[(countyID*36-35):(countyID*36-1),1:10]
  
  #input the GBD data
  gbd.file.names <- list.files(path = "01_Data/04_GBD")
  gbd_Input_byRace <- read.csv(paste0("01_Data/04_GBD/", gbd.file.names[countyID*2]))
  gbd_Input_byIncome <- read.csv(paste0("01_Data/04_GBD/", gbd.file.names[countyID*2+1]))
  
  # combine all inputs of baseline into a list by demographic variable
  InputPara_byRace <- InputPara(Pop_Input_byRace,nonTravelMET_Input_byRace,gbd_Input_byRace)
  InputPara_byIncome <- InputPara(Pop_Input_byIncome,nonTravelMET_Input_byIncome,gbd_Input_byIncome)
  
  # return required parameters
  return(list(
    InputPara_byRace = InputPara_byRace,
    InputPara_byIncome = InputPara_byIncome,
    
    AT_Input_byRace.baseline = AT_Input_byRace.baseline,
    AT_Input_byIncome.baseline = AT_Input_byIncome.baseline,
    AT_Input_byRace.2020 = AT_Input_byRace.2020,
    AT_Input_byIncome.2020 = AT_Input_byIncome.2020,
    AT_Input_byRace.2036 = AT_Input_byRace.2036,
    AT_Input_byIncome.2036 = AT_Input_byIncome.2036,
    AT_Input_byRace.2027 = AT_Input_byRace.2027,
    AT_Input_byIncome.2027 = AT_Input_byIncome.2027,
    AT_Input_byRace.S1 = AT_Input_byRace.S1,
    AT_Input_byIncome.S1 = AT_Input_byIncome.S1,
    AT_Input_byRace.S2 = AT_Input_byRace.S2,
    AT_Input_byIncome.S2 = AT_Input_byIncome.S2,
    AT_Input_byRace.S3 = AT_Input_byRace.S3,
    AT_Input_byIncome.S3 = AT_Input_byIncome.S3,
    AT_Input_byRace.C1 = AT_Input_byRace.C1,
    AT_Input_byIncome.C1 = AT_Input_byIncome.C1,
    AT_Input_byRace.C2 = AT_Input_byRace.C2,
    AT_Input_byIncome.C2 = AT_Input_byIncome.C2,
    AT_Input_byRace.C3 = AT_Input_byRace.C3,
    AT_Input_byIncome.C3 = AT_Input_byIncome.C3,
    
    #population mean active travel time by race
    AT_Pop_MeanWalkTimebyRace.baseline =as.numeric(AT_Pop_MeanTimebyRace.baseline[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.baseline = as.numeric(AT_Pop_MeanTimebyRace.baseline[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.2020 = as.numeric(AT_Pop_MeanTimebyRace.2020[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.2020 = as.numeric(AT_Pop_MeanTimebyRace.2020[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.2036 = as.numeric(AT_Pop_MeanTimebyRace.2036[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.2036 = as.numeric(AT_Pop_MeanTimebyRace.2036[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.2027 = as.numeric(AT_Pop_MeanTimebyRace.2027[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.2027 = as.numeric(AT_Pop_MeanTimebyRace.2027[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.S1 = as.numeric(AT_Pop_MeanTimebyRace.S1[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.S1 = as.numeric(AT_Pop_MeanTimebyRace.S1[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.S2 = as.numeric(AT_Pop_MeanTimebyRace.S2[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.S2 = as.numeric(AT_Pop_MeanTimebyRace.S2[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.S3 = as.numeric(AT_Pop_MeanTimebyRace.S3[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.S3 = as.numeric(AT_Pop_MeanTimebyRace.S3[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.C1 = as.numeric(AT_Pop_MeanTimebyRace.C1[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.C1 = as.numeric(AT_Pop_MeanTimebyRace.C1[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.C2 = as.numeric(AT_Pop_MeanTimebyRace.C2[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.C2 = as.numeric(AT_Pop_MeanTimebyRace.C2[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyRace.C3 = as.numeric(AT_Pop_MeanTimebyRace.C3[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyRace.C3 = as.numeric(AT_Pop_MeanTimebyRace.C3[countyID,c(3,5,7,9)]),
    
    # population mean active travel time by income
    AT_Pop_MeanWalkTimebyIncome.baseline = as.numeric(AT_Pop_MeanTimebyIncome.baseline[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.baseline = as.numeric(AT_Pop_MeanTimebyIncome.baseline[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.2020 = as.numeric(AT_Pop_MeanTimebyIncome.2020[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.2020 = as.numeric(AT_Pop_MeanTimebyIncome.2020[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.2036 = as.numeric(AT_Pop_MeanTimebyIncome.2036[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.2036 = as.numeric(AT_Pop_MeanTimebyIncome.2036[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.2027 = as.numeric(AT_Pop_MeanTimebyIncome.2027[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.2027 = as.numeric(AT_Pop_MeanTimebyIncome.2027[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.S1 = as.numeric(AT_Pop_MeanTimebyIncome.S1[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.S1 = as.numeric(AT_Pop_MeanTimebyIncome.S1[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.S2 = as.numeric(AT_Pop_MeanTimebyIncome.S2[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.S2 = as.numeric(AT_Pop_MeanTimebyIncome.S2[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.S3 = as.numeric(AT_Pop_MeanTimebyIncome.S3[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.S3 = as.numeric(AT_Pop_MeanTimebyIncome.S3[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.C1 = as.numeric(AT_Pop_MeanTimebyIncome.C1[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.C1 = as.numeric(AT_Pop_MeanTimebyIncome.C1[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.C2 = as.numeric(AT_Pop_MeanTimebyIncome.C2[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.C2 = as.numeric(AT_Pop_MeanTimebyIncome.C2[countyID,c(3,5,7,9)]),
    AT_Pop_MeanWalkTimebyIncome.C3 = as.numeric(AT_Pop_MeanTimebyIncome.C3[countyID,c(2,4,6,8)]),
    AT_Pop_MeanCycleTimebyIncome.C3 = as.numeric(AT_Pop_MeanTimebyIncome.C3[countyID,c(3,5,7,9)])
    
  ))
  
}

# function for shaping the input data sets (.csv)
InputPara <- function (Pop_Input,nonTravelMET_Input,gbd_Input){
  # input the population and calculate the proportion of population into each demo categories
  Pop_List_byDemo <- rep(list((matrix(NA,nrow = nAgeClass,ncol = 2))), nDemoClass)
  for(i in 1:nDemoClass){
    Pop_List_byDemo[[i]] <- as.matrix(Pop_Input[1:nAgeClass,(2*i):(2*i+1)])
  }
  PopProp_List_byDemo <- mapply(function(x) x/sum(x), Pop_List_byDemo, SIMPLIFY = FALSE)
  
  # input the whole US population in 2010. source: US Census 2010
  allPop <- matrix (c(Pop_Input_US[1:8,2],Pop_Input_US[1:8,3]), 
                    byrow = TRUE, ncol = 1, nrow = nAgeClass*2, 
                    dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                       paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  
  # input the non travel METs into each demo group
  nonTravelMET_List_byDemo <- rep(list((matrix(NA,nrow = 2*nAgeClass,ncol = 5))), nDemoClass)
  for(i in 1:nDemoClass){
    nonTravelMET_List_byDemo[[i]] <- as.matrix(nonTravelMET_Input[(17*i-16):(17*i-1),2:6])
    dimnames(nonTravelMET_List_byDemo[[i]]) = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                                      paste0("femaleAgeClass ",1:nAgeClass))),
                                                   seq(0.1,0.9,by=0.2))
  }
  
  #return required parameters
  return(list(
    Pop_List_byDemo = Pop_List_byDemo,
    PopProp_List_byDemo = PopProp_List_byDemo,
    allPop = allPop,
    nonTravelMET_List_byDemo = nonTravelMET_List_byDemo,
    gbd_Input = gbd_Input
  )
  )
}

# Part 2 Function Definition - Data Process -------------------------------------------------------------

# function for creating total exposure matrix 
TotalExposure <- function(PopMeanWalkTime, PopMeanCycleTime, AT_Input, PopProp, nonTravelMET){
  
  # The population mean walking/cycling speed (mph)
  #"It is common practice in MPOs to assume average walk speed of 3 mph and bicycle speed of 12 mph" 
  # from ITHIM user's manual
  PopMeanWalkSpeed <- 3.0
  PopMeanCycleSpeed <- 12.0
  cv <- -0.0108 * (PopMeanWalkTime + PopMeanCycleTime) / 7 + 1.2682 + 0.7
  
  # Numerical matrices for the relative walking time (relative to the value of "female 15-29") 
  # and the mean walking time
  # Source: SACSIM15
  Rwt <- as.matrix(AT_Input[1:8,1:2])
  dimnames(Rwt) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanWalkTime <- Rwt / PopProp / sum(PopProp*Rwt) * PopMeanWalkTime * PopProp
  meanWalkTime <- replace(meanWalkTime, is.na(meanWalkTime), 0)
  
  # Numerical matrices for the relative cycling time (relative to the value of "female 15-29") 
  # and the mean cycling time 
  # Source: SACSIM15
  Rct <- as.matrix(AT_Input[10:17,1:2])
  dimnames(Rct) = list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanCycleTime <- Rct / PopProp / sum(PopProp * Rct) * PopMeanCycleTime * PopProp
  meanCycleTime <- replace(meanCycleTime,is.na(meanCycleTime),0)
  
  # Numerical matrices for the proportion of mean cycling time to total active transport time
  PropMeanCycleTime <- meanCycleTime / (meanWalkTime+meanCycleTime)
  PropMeanCycleTime <- replace(PropMeanCycleTime,is.na(PropMeanCycleTime),0)
  
  # Numerical matrices for the relative walking speed (relative to the value of "female 15-29") 
  # and the mean walking speed
  Rws <- as.matrix(AT_Input[19:26,1:2])
  dimnames(Rws) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanWalkSpeed <- Rws / PopProp / sum(PopProp * Rws) * PopMeanWalkSpeed * PopProp
  meanWalkSpeed <- replace(meanWalkSpeed,is.na(meanWalkSpeed),0)
  
  # Numerical matrices for the relative cycling speed (relative to the value of "female 15-29") 
  # and the mean cycling speed
  Rcs <- as.matrix(AT_Input[28:35,1:2])
  dimnames(Rcs) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanCycleSpeed <- Rcs / PopProp / sum(PopProp * Rcs) * PopMeanCycleSpeed * PopProp
  meanCycleSpeed <- replace(meanCycleSpeed,is.na(meanCycleSpeed),0)
  
  # Numerical matrices for the mean walking/cycling MET values
  meanWalkMET <- ifelse(1.2216 * meanWalkSpeed + 0.0838 < 2.5, 2.5, 1.2216 * meanWalkSpeed + 0.0838)
  meanCycleMET <- matrix(6, byrow = TRUE, ncol = 2, nrow = nAgeClass, 
                         dimnames = list(paste0("ageClass ", 1:nAgeClass), c("M","F")))
  
  # Compute Quintiles of Active Transport Time
  
  # Total Active Transport Time
  totalATTime <- meanWalkTime + meanCycleTime
  
  # Numerical matrices for the log-normal distribution (mean, sd, log mean, log sd)
  meanATtime <- c(totalATTime[,1],totalATTime[,2])
  sd <- meanATtime * cv
  logMean <- log( meanATtime / sqrt ( 1 + ( meanATtime * cv / meanATtime )^2) )
  logMean <- replace(logMean,is.na(logMean),0)
  logSD <- sqrt( log( 1 + ( meanATtime * cv / meanATtime )^2))
  logSD <- replace(logSD,is.na(logSD),0)
  
  lognorm <- matrix(c(meanATtime,sd,logMean,logSD), byrow=FALSE, ncol = 4, nrow = nAgeClass*2,
                    dimnames = list(c(paste0("maleAgeClass ",1:nAgeClass),
                                      paste0("femaleAgeClass ",1:nAgeClass)),
                                    c("Mean","SD","log Mean","log sd")))
  
  # Compute quintiles of total AT time
  quintiles <- seq(0.1, 0.9, by=0.2)
  quintVec <- c()
  for (quant in quintiles) {
    quintVec <- c(quintVec, mapply(qlnorm,lognorm[,3],lognorm[,4],p=quant))
  }
  
  quintTotalATTime <- matrix (quintVec, byrow=FALSE, ncol = 5, nrow = nAgeClass*2,
                              dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                                 paste0("femaleAgeClass ",1:nAgeClass))),
                                              seq(0.1,0.9,by=0.2)))
  quintTotalATTime[which(logMean==0),] <- 0
  
  # Compute the quintiles of total walking/cycling time
  PropMeanCycleTimeCol <- c(PropMeanCycleTime[,1],PropMeanCycleTime[,2])
  
  quintWalkTime <- quintTotalATTime * ( 1 - PropMeanCycleTimeCol)
  quintCycleTime <- quintTotalATTime - quintWalkTime
  
  # Compute the walking/cycling MET-hours and total AC MET-hours
  meanWalkMETCol <- c(meanWalkMET[,1], meanWalkMET[,2])
  quintWalkMET <- quintWalkTime * meanWalkMETCol / 60
  
  meanCycleMETCol <- c(meanCycleMET[,1],meanCycleMET[,2])
  quintCycleMET <- quintCycleTime * meanCycleMETCol / 60
  
  quintTotalTravelMET <- quintWalkMET + quintCycleMET
  
  # adding up total AT METs and non travel METs
  totalExposure <- ifelse( quintTotalTravelMET + nonTravelMET > 2.5, quintTotalTravelMET + nonTravelMET, 0.1)
  
  # if the population in that category equals 0, then total exposure =0
  pop.prop.reformat <- matrix( PopProp, 16, 1)
  totalExposure[which(pop.prop.reformat==0),] <- 0
  
  #return the matrix of total exposure
  return(
    totalExposure <- totalExposure
  )
}

# function for creating total exposure matrix by demographic groups
List_TotalExposure <- function(df_PopMeanWalkTime, df_PopMeanCycleTime,InputPara,AT_Input){
  
  # process the AT_input data into each demo catrgories
  AT_List_byDemo <- rep(list((matrix(NA,nrow = nrow(AT_Input), ncol = 2))), nDemoClass)
  for(i in 1:nDemoClass){
    AT_List_byDemo[[i]] <- as.matrix(AT_Input[1:nrow(AT_Input),(2*i):(2*i+1)])
  }
  
  # Calculate the total exposure matrix for demographic categories
  TotalExposure_List_byDemo <- rep(list((matrix(NA,nrow = 2 * nAgeClass,ncol = 5))), nDemoClass)
  
  # apply the function of TotalExposure() for each demographic categories
  for(i in 1:nDemoClass){
    TotalExposure_List_byDemo[[i]] <- TotalExposure(
      df_PopMeanWalkTime[i], df_PopMeanCycleTime[i],
      AT_List_byDemo[[i]],
      InputPara$PopProp_List_byDemo[[i]],
      InputPara$nonTravelMET_List_byDemo[[i]])
  }
  
  return(
    TotalExposure_List_byDemo = TotalExposure_List_byDemo
  )
}

# function for computing relative risks of physical activity  
create.PA.RR <- function(){
  
  RR.lit <- exposure <- matrix(NA,nrow = nAgeClass, ncol = 2,
                               dimnames=list(paste0("agClass",1:nAgeClass),c("F","M")))
  
  # all cause mortality (source: Woodcock)
  exposure[1:nAgeClass,1:2] <- 11
  RR.lit[1:nAgeClass,1:2] <- 0.81
  
  #compute RR matrix
  RR <- RR.lit^( 1 / exposure )^k
  
  #reshape RR matrix
  reshapeRR <- function(RR, nQuantiles = 5){
    matrix(c(RR[,"M"],RR[,"F"]),nrow = nAgeClass * 2,ncol = nQuantiles,
           dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                              paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2)))
    
  }
  
  RR <- reshapeRR(RR,nQuantiles = 5)
  
  return(RR)
  
}

#function for computing local disease burden (scaling process)
computeLocalGBD <- function (death_target,TargetPop,InputPara){
  
  #obtain the gbd and the death data of U.S.
  gbd_US <- gbd_Input_US[,2:5]
  death_US <- as.numeric(gbd_Input_US[,2])
  
  # calculate the RR (the ratio of death numbers for target area with those for whole U.S.)
  RR_gbd <- matrix(NA,ncol = 1,nrow = 2*nAgeClass,dimnames = list((c(paste0("maleAgeclass",1:nAgeClass),paste0("femaleAgeclass",1:nAgeClass))),"RR"))
  RR_gbd <- (death_target/TargetPop)/(death_US/InputPara$allPop)
  RR_gbd <- replace(RR_gbd,is.na(RR_gbd)|is.infinite(RR_gbd),1.0)
  
  # obtain the local gbd data
  gbd.local<-RR_gbd*gbd_US/InputPara$allPop*TargetPop
  
  return(gbd.local)
}

#function for computing local disease burden by demographic groups
List_LocalGBD <- function(InputPara){
  
  LocalGBD_List_byDemo <- rep(list(matrix(NA,nrow = 2 * nAgeClass, ncol = 4)), nDemoClass)
  
  for (i in 1:nDemoClass){
    death_target <- InputPara$gbd_Input[,i+1]
    TargetPop <- matrix (InputPara$PopProp_List_byDemo[[i]] * sum(InputPara$Pop_List_byDemo[[i]]), 
                         byrow = TRUE, ncol = 1, nrow = nAgeClass * 2, 
                         dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                            paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
    death_target[which(TargetPop==0)] <- 0
    gbd.local <- computeLocalGBD(death_target,TargetPop,InputPara)
    LocalGBD_List_byDemo[[i]] <- gbd.local
  }
  
  return(
    LocalGBD_List_byDemo = LocalGBD_List_byDemo)
  
}

# Part 3 Function Definition - Computing Health Burden --------------------------------------------------------

#functions for computing health outcome
computeHealthOutcome <- function (RR.PA,BaselineTotalExpo,ScenarioTotalExpo,gbd.local){
  
  #Compute RR for an exposure of x MET
  RR.Baseline <- RR.PA^( BaselineTotalExpo^k )
  RR.Scenario <- RR.PA^( ScenarioTotalExpo^k )
  
  #Compute Ratio of DB relative to group 1
  RatioDB.Baseline <- RR.Baseline / RR.Baseline[,1]
  RatioDB.Scenario <- RR.Scenario / RR.Scenario[,1]
  sum.RatioDB.Baseline <- rowSums(RatioDB.Baseline)
  sum.RatioDB.Scenario <- rowSums(RatioDB.Scenario)
  
  #Compute New Burden and AF
  new.burden <- rowSums(RR.Scenario) / rowSums(RR.Baseline)
  AF <- 1 - new.burden
  
  #Compute the health outcomes
  #Define a function for outputing health outcomes
  fun.outcome <- function(x,y){
    x[,1] <- y
    x[,c(2:5)] <- x[,c(2:5)]*y
    return(x)}
  
  #Compute deaths per group
  dproj.scenario.firstCol <- new.burden * gbd.local$deaths / sum.RatioDB.Scenario
  dproj.scenario <- fun.outcome(RatioDB.Scenario,dproj.scenario.firstCol)
  
  dproj.baseline.firstCol <- gbd.local$deaths / sum.RatioDB.Baseline
  dproj.baseline <- fun.outcome(RatioDB.Baseline,dproj.baseline.firstCol)
  
  #Compute YLL per group
  yll.scenario.firstCol <- new.burden * gbd.local$yll / sum.RatioDB.Scenario
  yll.scenario <- fun.outcome(RatioDB.Scenario,yll.scenario.firstCol)
  
  yll.baseline.firstCol <- gbd.local$yll / sum.RatioDB.Baseline
  yll.baseline <- fun.outcome(RatioDB.Baseline,yll.baseline.firstCol)
  
  #Compute YLD per group
  yld.scenario.firstCol <- new.burden * gbd.local$yld / sum.RatioDB.Scenario
  yld.scenario <- fun.outcome(RatioDB.Scenario, yld.scenario.firstCol)
  
  yld.baseline.firstCol <- gbd.local$yld / sum.RatioDB.Baseline
  yld.baseline <- fun.outcome(RatioDB.Baseline,yld.baseline.firstCol)
  
  #Compute the delta Burden, total delta Burden, and the proportion
  delta.Burden <- (matrix(NA,nrow = nAgeClass * 2,ncol = 4,
                          dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),
                                             paste0("femaleAgeClass ",1:nAgeClass))),
                                          c("delta.Deaths","delta.YLL","delta.YLD","DALYS"))))
  
  delta.Burden[,1] <- rowSums(dproj.scenario) - rowSums(dproj.baseline) #deaths
  delta.Burden[,2] <- rowSums(yll.scenario) - rowSums(yll.baseline)     #yll
  delta.Burden[,3] <- rowSums(yld.scenario) - rowSums(yld.baseline)     #yld
  delta.Burden[,4] <- delta.Burden[,2] + delta.Burden[,3]               #dalys
  
  total.delta.Burden <- colSums(delta.Burden)
  total.gbd.local <- ifelse(colSums(gbd.local)!=0,colSums(gbd.local),0.0001)
  
  prop.delta.Burden <- total.delta.Burden / total.gbd.local
  
  return(list(
    AF = AF,
    new.burden = new.burden,
    delta.Burden = delta.Burden,
    prop.delta.Burden = prop.delta.Burden
  ))
}

#output the detail heath outcome for each county
output.HealthOutcome <- function(countyID){
  # reading the csv files after inputting countyID
  # countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
  All.InputPara <- read.csv.files(countyID = countyID)
  
  #Create the total exposure matrices by inputing parameters 
  #(mean walking time(min per week), mean cycling time(min per week))
  BaselineTotalExpo_byRace <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.baseline,
                                                 All.InputPara$AT_Pop_MeanCycleTimebyRace.baseline,
                                                 All.InputPara$InputPara_byRace,
                                                 All.InputPara$AT_Input_byRace.baseline)
  ScenarioTotalExpo_byRace.2020 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.2020,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyRace.2020,
                                                      All.InputPara$InputPara_byRace,
                                                      All.InputPara$AT_Input_byRace.2020)
  ScenarioTotalExpo_byRace.2036 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.2036,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyRace.2036,
                                                      All.InputPara$InputPara_byRace,
                                                      All.InputPara$AT_Input_byRace.2036)
  ScenarioTotalExpo_byRace.2027 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.2027,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyRace.2027,
                                                      All.InputPara$InputPara_byRace,
                                                      All.InputPara$AT_Input_byRace.2027)
  ScenarioTotalExpo_byRace.S1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.S1,
                                                    All.InputPara$AT_Pop_MeanCycleTimebyRace.S1,
                                                    All.InputPara$InputPara_byRace,
                                                    All.InputPara$AT_Input_byRace.S1)
  ScenarioTotalExpo_byRace.S2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.S2,
                                                    All.InputPara$AT_Pop_MeanCycleTimebyRace.S2,
                                                    All.InputPara$InputPara_byRace,
                                                    All.InputPara$AT_Input_byRace.S2)
  ScenarioTotalExpo_byRace.S3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.S3,
                                                    All.InputPara$AT_Pop_MeanCycleTimebyRace.S3,
                                                    All.InputPara$InputPara_byRace,
                                                    All.InputPara$AT_Input_byRace.S3)
  ScenarioTotalExpo_byRace.C1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.C1,
                                                    All.InputPara$AT_Pop_MeanCycleTimebyRace.C1,
                                                    All.InputPara$InputPara_byRace,
                                                    All.InputPara$AT_Input_byRace.C1)
  ScenarioTotalExpo_byRace.C2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.C2,
                                                    All.InputPara$AT_Pop_MeanCycleTimebyRace.C2,
                                                    All.InputPara$InputPara_byRace,
                                                    All.InputPara$AT_Input_byRace.C2)
  ScenarioTotalExpo_byRace.C3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.C3,
                                                    All.InputPara$AT_Pop_MeanCycleTimebyRace.C3,
                                                    All.InputPara$InputPara_byRace,
                                                    All.InputPara$AT_Input_byRace.C3)
  
  BaselineTotalExpo_byIncome <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.baseline,
                                                   All.InputPara$AT_Pop_MeanCycleTimebyIncome.baseline,
                                                   All.InputPara$InputPara_byIncome,
                                                   All.InputPara$AT_Input_byIncome.baseline)
  ScenarioTotalExpo_byIncome.2020 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.2020,
                                                        All.InputPara$AT_Pop_MeanCycleTimebyIncome.2020,
                                                        All.InputPara$InputPara_byIncome,
                                                        All.InputPara$AT_Input_byIncome.2020)
  ScenarioTotalExpo_byIncome.2036 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.2036,
                                                        All.InputPara$AT_Pop_MeanCycleTimebyIncome.2036,
                                                        All.InputPara$InputPara_byIncome,
                                                        All.InputPara$AT_Input_byIncome.2036)
  ScenarioTotalExpo_byIncome.2027 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.2027,
                                                        All.InputPara$AT_Pop_MeanCycleTimebyIncome.2027,
                                                        All.InputPara$InputPara_byIncome,
                                                        All.InputPara$AT_Input_byIncome.2027)
  ScenarioTotalExpo_byIncome.S1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.S1,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyIncome.S1,
                                                      All.InputPara$InputPara_byIncome,
                                                      All.InputPara$AT_Input_byIncome.S1)
  ScenarioTotalExpo_byIncome.S2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.S2,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyIncome.S2,
                                                      All.InputPara$InputPara_byIncome,
                                                      All.InputPara$AT_Input_byIncome.S2)
  ScenarioTotalExpo_byIncome.S3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.S3,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyIncome.S3,
                                                      All.InputPara$InputPara_byIncome,
                                                      All.InputPara$AT_Input_byIncome.S3)
  ScenarioTotalExpo_byIncome.C1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.C1,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyIncome.C1,
                                                      All.InputPara$InputPara_byIncome,
                                                      All.InputPara$AT_Input_byIncome.C1)
  ScenarioTotalExpo_byIncome.C2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.C2,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyIncome.C2,
                                                      All.InputPara$InputPara_byIncome,
                                                      All.InputPara$AT_Input_byIncome.C2)
  ScenarioTotalExpo_byIncome.C3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.C3,
                                                      All.InputPara$AT_Pop_MeanCycleTimebyIncome.C3,
                                                      All.InputPara$InputPara_byIncome,
                                                      All.InputPara$AT_Input_byIncome.C3)
  
  #compute the relative risks of Physical Activity (1MET)
  RR.PA <- create.PA.RR()
  
  #compute local disease burden, and create a list to store local gbd for all races
  LocalGBD_List_byRace <- List_LocalGBD(All.InputPara$InputPara_byRace)
  LocalGBD_List_byIncome <- List_LocalGBD(All.InputPara$InputPara_byIncome)
  
  #compute health outcomes
  HealthOutcome_byRace.2020 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.2020,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.2020) <- raceGroupNames
  
  HealthOutcome_byIncome.2020 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.2020,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.2020) <- incomeGroupNames
  
  HealthOutcome_byRace.2036 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.2036,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.2036) <- raceGroupNames
  
  HealthOutcome_byIncome.2036 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.2036,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.2036) <- incomeGroupNames
  
  HealthOutcome_byRace.2027 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.2027,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.2027) <- raceGroupNames
  
  HealthOutcome_byIncome.2027 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.2027,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.2027) <- incomeGroupNames
  
  HealthOutcome_byRace.S1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.S1,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.S1) <- raceGroupNames
  
  HealthOutcome_byIncome.S1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.S1,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.S1) <- incomeGroupNames
  
  HealthOutcome_byRace.S2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.S2,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.S2) <- raceGroupNames
  
  HealthOutcome_byIncome.S2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.S2,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.S2) <- incomeGroupNames
  
  HealthOutcome_byRace.S3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.S3,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.S3) <- raceGroupNames
  
  HealthOutcome_byIncome.S3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.S3,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.S3) <- incomeGroupNames
  
  HealthOutcome_byRace.C1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.C1,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.C1) <- raceGroupNames
  
  HealthOutcome_byIncome.C1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.C1,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.C1) <- incomeGroupNames
  
  HealthOutcome_byRace.C2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.C2,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.C2) <- raceGroupNames
  
  HealthOutcome_byIncome.C2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.C2,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.C2) <- incomeGroupNames
  
  HealthOutcome_byRace.C3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.C3,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.C3) <- raceGroupNames
  
  HealthOutcome_byIncome.C3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),
           BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.C3,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.C3) <- incomeGroupNames
  
  return(list(
    HealthOutcome_byRace.2020 = HealthOutcome_byRace.2020,
    HealthOutcome_byIncome.2020 = HealthOutcome_byIncome.2020,
    
    HealthOutcome_byRace.2036 = HealthOutcome_byRace.2036,
    HealthOutcome_byIncome.2036 = HealthOutcome_byIncome.2036,
    
    HealthOutcome_byRace.2036 = HealthOutcome_byRace.2036,
    HealthOutcome_byIncome.2036 = HealthOutcome_byIncome.2036,
    
    HealthOutcome_byRace.2027 = HealthOutcome_byRace.2027,
    HealthOutcome_byIncome.2027 = HealthOutcome_byIncome.2027,
    
    HealthOutcome_byRace.S1 = HealthOutcome_byRace.S1,
    HealthOutcome_byIncome.S1 = HealthOutcome_byIncome.S1,
    
    HealthOutcome_byRace.S2 = HealthOutcome_byRace.S2,
    HealthOutcome_byIncome.S2 = HealthOutcome_byIncome.S2,
    
    HealthOutcome_byRace.S3 = HealthOutcome_byRace.S3,
    HealthOutcome_byIncome.S3 = HealthOutcome_byIncome.S3,
    
    HealthOutcome_byRace.C1 = HealthOutcome_byRace.C1,
    HealthOutcome_byIncome.C1 = HealthOutcome_byIncome.C1,
    
    HealthOutcome_byRace.C2 = HealthOutcome_byRace.C2,
    HealthOutcome_byIncome.C2 = HealthOutcome_byIncome.C2,
    
    HealthOutcome_byRace.C3 = HealthOutcome_byRace.C3,
    HealthOutcome_byIncome.C3 = HealthOutcome_byIncome.C3
  ))
}

#function for computing health outcome (absolute total reduction)
Reduction.output <- function(countyID=c(1:6)){
  
  Reduction.Death.matrix.race.2020 <- Reduction.Death.matrix.race.2036 <-Reduction.Death.matrix.race.2027<-
    Reduction.Death.matrix.race.S1<-Reduction.Death.matrix.race.S2<-Reduction.Death.matrix.race.S3<-
    Reduction.Death.matrix.race.C1<-Reduction.Death.matrix.race.C2<-Reduction.Death.matrix.race.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],raceGroupNames))
  
  Reduction.DALYs.matrix.race.2020 <- Reduction.DALYs.matrix.race.2036 <- Reduction.DALYs.matrix.race.2027<-
    Reduction.DALYs.matrix.race.S1<-Reduction.DALYs.matrix.race.S2<-Reduction.DALYs.matrix.race.S3<-
    Reduction.DALYs.matrix.race.C1<-Reduction.DALYs.matrix.race.C2<-Reduction.DALYs.matrix.race.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],raceGroupNames))
  
  Reduction.Death.matrix.income.2020 <- Reduction.Death.matrix.income.2036 <-Reduction.Death.matrix.income.2027<-
    Reduction.Death.matrix.income.S1<-Reduction.Death.matrix.income.S2<-Reduction.Death.matrix.income.S3<-
    Reduction.Death.matrix.income.C1<-Reduction.Death.matrix.income.C2<-Reduction.Death.matrix.income.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],incomeGroupNames))
  
  Reduction.DALYs.matrix.income.2020 <- Reduction.DALYs.matrix.income.2036 <-Reduction.DALYs.matrix.income.2027<-
    Reduction.DALYs.matrix.income.S1<-Reduction.DALYs.matrix.income.S2<-Reduction.DALYs.matrix.income.S3<-
    Reduction.DALYs.matrix.income.C1<-Reduction.DALYs.matrix.income.C2<-Reduction.DALYs.matrix.income.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],incomeGroupNames))
  
  m=1
  for(j in countyID){
    temp <- output.HealthOutcome(j)
    
    for(i in 1:4){
      Reduction.Death.matrix.race.2020[m,i]<-colSums(temp$HealthOutcome_byRace.2020[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.2020[m,i]<-colSums(temp$HealthOutcome_byRace.2020[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.2036[m,i]<-colSums(temp$HealthOutcome_byRace.2036[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.2036[m,i]<-colSums(temp$HealthOutcome_byRace.2036[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.2027[m,i]<-colSums(temp$HealthOutcome_byRace.2027[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.2027[m,i]<-colSums(temp$HealthOutcome_byRace.2027[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.S1[m,i]<-colSums(temp$HealthOutcome_byRace.S1[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.S1[m,i]<-colSums(temp$HealthOutcome_byRace.S1[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.S2[m,i]<-colSums(temp$HealthOutcome_byRace.S2[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.S2[m,i]<-colSums(temp$HealthOutcome_byRace.S2[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.S3[m,i]<-colSums(temp$HealthOutcome_byRace.S3[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.S3[m,i]<-colSums(temp$HealthOutcome_byRace.S3[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.C1[m,i]<-colSums(temp$HealthOutcome_byRace.C1[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.C1[m,i]<-colSums(temp$HealthOutcome_byRace.C1[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.C2[m,i]<-colSums(temp$HealthOutcome_byRace.C2[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.C2[m,i]<-colSums(temp$HealthOutcome_byRace.C2[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.race.C3[m,i]<-colSums(temp$HealthOutcome_byRace.C3[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.race.C3[m,i]<-colSums(temp$HealthOutcome_byRace.C3[[i]]$delta.Burden)[4]
      
      Reduction.Death.matrix.income.2020[m,i]<-colSums(temp$HealthOutcome_byIncome.2020[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.2020[m,i]<-colSums(temp$HealthOutcome_byIncome.2020[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.2036[m,i]<-colSums(temp$HealthOutcome_byIncome.2036[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.2036[m,i]<-colSums(temp$HealthOutcome_byIncome.2036[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.2027[m,i]<-colSums(temp$HealthOutcome_byIncome.2027[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.2027[m,i]<-colSums(temp$HealthOutcome_byIncome.2027[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.S1[m,i]<-colSums(temp$HealthOutcome_byIncome.S1[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.S1[m,i]<-colSums(temp$HealthOutcome_byIncome.S1[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.S2[m,i]<-colSums(temp$HealthOutcome_byIncome.S2[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.S2[m,i]<-colSums(temp$HealthOutcome_byIncome.S2[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.S3[m,i]<-colSums(temp$HealthOutcome_byIncome.S3[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.S3[m,i]<-colSums(temp$HealthOutcome_byIncome.S3[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.C1[m,i]<-colSums(temp$HealthOutcome_byIncome.C1[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.C1[m,i]<-colSums(temp$HealthOutcome_byIncome.C1[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.C2[m,i]<-colSums(temp$HealthOutcome_byIncome.C2[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.C2[m,i]<-colSums(temp$HealthOutcome_byIncome.C2[[i]]$delta.Burden)[4]
      Reduction.Death.matrix.income.C3[m,i]<-colSums(temp$HealthOutcome_byIncome.C3[[i]]$delta.Burden)[1]
      Reduction.DALYs.matrix.income.C3[m,i]<-colSums(temp$HealthOutcome_byIncome.C3[[i]]$delta.Burden)[4]
      
      
    }
    m=m+1
  }
  
  return(list(
    Reduction.Death.matrix.race.2020=Reduction.Death.matrix.race.2020,
    Reduction.Death.matrix.race.2027=Reduction.Death.matrix.race.2027,
    Reduction.Death.matrix.race.2036=Reduction.Death.matrix.race.2036,
    Reduction.Death.matrix.race.S1=Reduction.Death.matrix.race.S1,
    Reduction.Death.matrix.race.S2=Reduction.Death.matrix.race.S2,
    Reduction.Death.matrix.race.S3=Reduction.Death.matrix.race.S3,
    Reduction.Death.matrix.race.C1=Reduction.Death.matrix.race.C1,
    Reduction.Death.matrix.race.C2=Reduction.Death.matrix.race.C2,
    Reduction.Death.matrix.race.C3=Reduction.Death.matrix.race.C3,
    
    Reduction.DALYs.matrix.race.2020=Reduction.DALYs.matrix.race.2020,
    Reduction.DALYs.matrix.race.2027=Reduction.DALYs.matrix.race.2027,
    Reduction.DALYs.matrix.race.2036=Reduction.DALYs.matrix.race.2036,
    Reduction.DALYs.matrix.race.S1=Reduction.DALYs.matrix.race.S1,
    Reduction.DALYs.matrix.race.S2=Reduction.DALYs.matrix.race.S2,
    Reduction.DALYs.matrix.race.S3=Reduction.DALYs.matrix.race.S3,
    Reduction.DALYs.matrix.race.C1=Reduction.DALYs.matrix.race.C1,
    Reduction.DALYs.matrix.race.C2=Reduction.DALYs.matrix.race.C2,
    Reduction.DALYs.matrix.race.C3=Reduction.DALYs.matrix.race.C3,
    
    Reduction.Death.matrix.income.2020=Reduction.Death.matrix.income.2020,
    Reduction.Death.matrix.income.2027=Reduction.Death.matrix.income.2027,
    Reduction.Death.matrix.income.2036=Reduction.Death.matrix.income.2036,
    Reduction.Death.matrix.income.S1=Reduction.Death.matrix.income.S1,
    Reduction.Death.matrix.income.S2=Reduction.Death.matrix.income.S2,
    Reduction.Death.matrix.income.S3=Reduction.Death.matrix.income.S3,
    Reduction.Death.matrix.income.C1=Reduction.Death.matrix.income.C1,
    Reduction.Death.matrix.income.C2=Reduction.Death.matrix.income.C2,
    Reduction.Death.matrix.income.C3=Reduction.Death.matrix.income.C3,
    
    Reduction.DALYs.matrix.income.2020=Reduction.DALYs.matrix.income.2020,
    Reduction.DALYs.matrix.income.2027=Reduction.DALYs.matrix.income.2027,
    Reduction.DALYs.matrix.income.2036=Reduction.DALYs.matrix.income.2036,
    Reduction.DALYs.matrix.income.S1=Reduction.DALYs.matrix.income.S1,
    Reduction.DALYs.matrix.income.S2=Reduction.DALYs.matrix.income.S2,
    Reduction.DALYs.matrix.income.S3=Reduction.DALYs.matrix.income.S3,
    Reduction.DALYs.matrix.income.C1=Reduction.DALYs.matrix.income.C1,
    Reduction.DALYs.matrix.income.C2=Reduction.DALYs.matrix.income.C2,
    Reduction.DALYs.matrix.income.C3=Reduction.DALYs.matrix.income.C3
    
  ))
}

#functions for computing age-normalized health outcome
computeAgeStdOutput <- function(All.InputPara_byDemo,HealthOutcome_byDemo){
  
  # input the US population as the reference
  US.pop <- All.InputPara_byDemo$allPop
  
  #shape the matrix 
  local.pop <- sapply(All.InputPara_byDemo$Pop_List_byDemo,function(i) matrix(i,nrow = 16,ncol = 1))
  
  delta.death <- cbind(HealthOutcome_byDemo[[1]]$delta.Burden[,1],HealthOutcome_byDemo[[2]]$delta.Burden[,1],HealthOutcome_byDemo[[3]]$delta.Burden[,1],HealthOutcome_byDemo[[4]]$delta.Burden[,1])
  delta.DALYs <- cbind(HealthOutcome_byDemo[[1]]$delta.Burden[,4],HealthOutcome_byDemo[[2]]$delta.Burden[,4],HealthOutcome_byDemo[[3]]$delta.Burden[,4],HealthOutcome_byDemo[[4]]$delta.Burden[,4])
  
  death.rate <- replace(delta.death/local.pop*100000,is.na(delta.death/local.pop),0) 
  DALYs.rate <- replace(delta.DALYs/local.pop*100000,is.na(delta.DALYs/local.pop),0) 
  
  age.std.death <- age.std.DALYs <- matrix(NA,1,4)
  
  #scale process
  for (i in 1:4){
    age.std.death[1,i]=sum(death.rate[,i]*US.pop)/sum(US.pop)
    age.std.DALYs[1,i]=sum(DALYs.rate[,i]*US.pop)/sum(US.pop)
  }
  
  return(list(
    age.std.death = age.std.death,
    age.std.DALYs = age.std.DALYs
  ))
}

# function for computing age-normalized health outcome 
# (two races in order to integrate with injury module)
computeAgeStdOutput.twoRaces <- function(All.InputPara_byDemo,HealthOutcome_byDemo){
  
  # input the US population as the reference
  US.pop <- All.InputPara_byDemo$allPop
  
  #shape the matrix 
  local.pop <- sapply(All.InputPara_byDemo$Pop_List_byDemo,function(i) matrix(i,nrow = 16,ncol = 1))
  local.pop.twoRaces<- cbind(local.pop[,1],rowSums(local.pop[,c(2,3,4)]))
  
  delta.death <- cbind(HealthOutcome_byDemo[[1]]$delta.Burden[,1],HealthOutcome_byDemo[[2]]$delta.Burden[,1],HealthOutcome_byDemo[[3]]$delta.Burden[,1],HealthOutcome_byDemo[[4]]$delta.Burden[,1])
  delta.death.twoRaces <- cbind(delta.death[,1],rowSums(delta.death[,c(2,3,4)]))
  
  delta.DALYs <- cbind(HealthOutcome_byDemo[[1]]$delta.Burden[,4],HealthOutcome_byDemo[[2]]$delta.Burden[,4],HealthOutcome_byDemo[[3]]$delta.Burden[,4],HealthOutcome_byDemo[[4]]$delta.Burden[,4])
  delta.DALYs.twoRaces <- cbind(delta.DALYs[,1],rowSums(delta.DALYs[,c(2,3,4)]))
  
  death.rate <- replace(delta.death.twoRaces/local.pop.twoRaces*100000,is.na(delta.death.twoRaces/local.pop.twoRaces),0) 
  DALYs.rate <- replace(delta.DALYs.twoRaces/local.pop.twoRaces*100000,is.na(delta.DALYs.twoRaces/local.pop.twoRaces),0) 
  
  age.std.death.twoRaces <- age.std.DALYs.twoRaces <- matrix(NA,1,2)
  
  #scale process
  for (i in 1:2){
    age.std.death.twoRaces[1,i]=sum(death.rate[,i]*US.pop)/sum(US.pop)
    age.std.DALYs.twoRaces[1,i]=sum(DALYs.rate[,i]*US.pop)/sum(US.pop)
  }
  
  return(list(
    age.std.death.twoRaces = age.std.death.twoRaces,
    age.std.DALYs.twoRaces = age.std.DALYs.twoRaces
  ))
}

#output the age.std health outcome
AgeStdHealthOutcome <- function(countyID) {
  
  AgeStdDeath.matrix.race.2020 <- AgeStdDeath.matrix.race.2036 <-AgeStdDeath.matrix.race.2027<-
    AgeStdDeath.matrix.race.S1<-AgeStdDeath.matrix.race.S2<-AgeStdDeath.matrix.race.S3<-
    AgeStdDeath.matrix.race.C1<-AgeStdDeath.matrix.race.C2<-AgeStdDeath.matrix.race.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],raceGroupNames))
  
  AgeStdDeath.matrix.income.2020 <-AgeStdDeath.matrix.income.2036 <-AgeStdDeath.matrix.income.2027<-
    AgeStdDeath.matrix.income.S1<-AgeStdDeath.matrix.income.S2<-AgeStdDeath.matrix.income.S3<-
    AgeStdDeath.matrix.income.C1<-AgeStdDeath.matrix.income.C2<-AgeStdDeath.matrix.income.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],incomeGroupNames))
  
  AgeStdDALYs.matrix.race.2020 <- AgeStdDALYs.matrix.race.2036 <-AgeStdDALYs.matrix.race.2027<-
    AgeStdDALYs.matrix.race.S1<-AgeStdDALYs.matrix.race.S2<-AgeStdDALYs.matrix.race.S3<-
    AgeStdDALYs.matrix.race.C1<-AgeStdDALYs.matrix.race.C2<-AgeStdDALYs.matrix.race.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],raceGroupNames))
  
  AgeStdDALYs.matrix.income.2020 <-AgeStdDALYs.matrix.income.2036 <-AgeStdDALYs.matrix.income.2027<-
    AgeStdDALYs.matrix.income.S1<-AgeStdDALYs.matrix.income.S2<-AgeStdDALYs.matrix.income.S3<-
    AgeStdDALYs.matrix.income.C1<-AgeStdDALYs.matrix.income.C2<-AgeStdDALYs.matrix.income.C3<-
    matrix(NA,nrow = length(countyID),ncol = nDemoClass,dimnames = list(countyNames[countyID],incomeGroupNames))
  
  j=1
  for (i in countyID){
    HealthOutcome <- output.HealthOutcome(i)
    All.InputPara <- read.csv.files(i)
    
    temp.race.2020 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                          HealthOutcome$HealthOutcome_byRace.2020)
    temp.income.2020 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                            HealthOutcome$HealthOutcome_byIncome.2020)
    temp.race.2036 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                          HealthOutcome$HealthOutcome_byRace.2036)
    temp.income.2036 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                            HealthOutcome$HealthOutcome_byIncome.2036)
    temp.race.2027 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                          HealthOutcome$HealthOutcome_byRace.2027)
    temp.income.2027 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                            HealthOutcome$HealthOutcome_byIncome.2027)
    temp.race.S1 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                        HealthOutcome$HealthOutcome_byRace.S1)
    temp.income.S1 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                          HealthOutcome$HealthOutcome_byIncome.S1)
    temp.race.S2 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                        HealthOutcome$HealthOutcome_byRace.S2)
    temp.income.S2 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                          HealthOutcome$HealthOutcome_byIncome.S2)
    temp.race.S3 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                        HealthOutcome$HealthOutcome_byRace.S3)
    temp.income.S3 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                          HealthOutcome$HealthOutcome_byIncome.S3)
    temp.race.C1 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                        HealthOutcome$HealthOutcome_byRace.C1)
    temp.income.C1 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                          HealthOutcome$HealthOutcome_byIncome.C1)
    temp.race.C2 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                        HealthOutcome$HealthOutcome_byRace.C2)
    temp.income.C2 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                          HealthOutcome$HealthOutcome_byIncome.C2)
    temp.race.C3 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,
                                        HealthOutcome$HealthOutcome_byRace.C3)
    temp.income.C3 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,
                                          HealthOutcome$HealthOutcome_byIncome.C3)
    
    AgeStdDeath.matrix.race.2020[j,] <- temp.race.2020$age.std.death
    AgeStdDeath.matrix.race.2036[j,] <- temp.race.2036$age.std.death
    AgeStdDeath.matrix.race.2027[j,] <- temp.race.2027$age.std.death
    AgeStdDeath.matrix.race.S1[j,] <- temp.race.S1$age.std.death
    AgeStdDeath.matrix.race.S2[j,] <- temp.race.S2$age.std.death
    AgeStdDeath.matrix.race.S3[j,] <- temp.race.S3$age.std.death
    AgeStdDeath.matrix.race.C1[j,] <- temp.race.C1$age.std.death
    AgeStdDeath.matrix.race.C2[j,] <- temp.race.C2$age.std.death
    AgeStdDeath.matrix.race.C3[j,] <- temp.race.C3$age.std.death
    
    AgeStdDeath.matrix.income.2020[j,] <- temp.income.2020$age.std.death
    AgeStdDeath.matrix.income.2036[j,] <- temp.income.2036$age.std.death
    AgeStdDeath.matrix.income.2027[j,] <- temp.income.2027$age.std.death
    AgeStdDeath.matrix.income.S1[j,] <- temp.income.S1$age.std.death
    AgeStdDeath.matrix.income.S2[j,] <- temp.income.S2$age.std.death
    AgeStdDeath.matrix.income.S3[j,] <- temp.income.S3$age.std.death
    AgeStdDeath.matrix.income.C1[j,] <- temp.income.C1$age.std.death
    AgeStdDeath.matrix.income.C2[j,] <- temp.income.C2$age.std.death
    AgeStdDeath.matrix.income.C3[j,] <- temp.income.C3$age.std.death
    
    AgeStdDALYs.matrix.race.2020[j,] <- temp.race.2020$age.std.DALYs
    AgeStdDALYs.matrix.race.2036[j,] <- temp.race.2036$age.std.DALYs
    AgeStdDALYs.matrix.race.2027[j,] <- temp.race.2027$age.std.DALYs
    AgeStdDALYs.matrix.race.S1[j,] <- temp.race.S1$age.std.DALYs
    AgeStdDALYs.matrix.race.S2[j,] <- temp.race.S2$age.std.DALYs
    AgeStdDALYs.matrix.race.S3[j,] <- temp.race.S3$age.std.DALYs
    AgeStdDALYs.matrix.race.C1[j,] <- temp.race.C1$age.std.DALYs
    AgeStdDALYs.matrix.race.C2[j,] <- temp.race.C2$age.std.DALYs
    AgeStdDALYs.matrix.race.C3[j,] <- temp.race.C3$age.std.DALYs
    
    AgeStdDALYs.matrix.income.2020[j,] <- temp.income.2020$age.std.DALYs
    AgeStdDALYs.matrix.income.2036[j,] <- temp.income.2036$age.std.DALYs
    AgeStdDALYs.matrix.income.2027[j,] <- temp.income.2027$age.std.DALYs
    AgeStdDALYs.matrix.income.S1[j,] <- temp.income.S1$age.std.DALYs
    AgeStdDALYs.matrix.income.S2[j,] <- temp.income.S2$age.std.DALYs
    AgeStdDALYs.matrix.income.S3[j,] <- temp.income.S3$age.std.DALYs
    AgeStdDALYs.matrix.income.C1[j,] <- temp.income.C1$age.std.DALYs
    AgeStdDALYs.matrix.income.C2[j,] <- temp.income.C2$age.std.DALYs
    AgeStdDALYs.matrix.income.C3[j,] <- temp.income.C3$age.std.DALYs
    
    j=j+1
  }
  
  return(list(
    AgeStdDeath.matrix.race.2020=AgeStdDeath.matrix.race.2020,
    AgeStdDeath.matrix.race.2027=AgeStdDeath.matrix.race.2027,
    AgeStdDeath.matrix.race.2036=AgeStdDeath.matrix.race.2036,
    AgeStdDeath.matrix.race.S1=AgeStdDeath.matrix.race.S1,
    AgeStdDeath.matrix.race.S2=AgeStdDeath.matrix.race.S2,
    AgeStdDeath.matrix.race.S3=AgeStdDeath.matrix.race.S3,
    AgeStdDeath.matrix.race.C1=AgeStdDeath.matrix.race.C1,
    AgeStdDeath.matrix.race.C2=AgeStdDeath.matrix.race.C2,
    AgeStdDeath.matrix.race.C3=AgeStdDeath.matrix.race.C3,
    
    AgeStdDALYs.matrix.race.2020=AgeStdDALYs.matrix.race.2020,
    AgeStdDALYs.matrix.race.2027=AgeStdDALYs.matrix.race.2027,
    AgeStdDALYs.matrix.race.2036=AgeStdDALYs.matrix.race.2036,
    AgeStdDALYs.matrix.race.S1=AgeStdDALYs.matrix.race.S1,
    AgeStdDALYs.matrix.race.S2=AgeStdDALYs.matrix.race.S2,
    AgeStdDALYs.matrix.race.S3=AgeStdDALYs.matrix.race.S3,
    AgeStdDALYs.matrix.race.C1=AgeStdDALYs.matrix.race.C1,
    AgeStdDALYs.matrix.race.C2=AgeStdDALYs.matrix.race.C2,
    AgeStdDALYs.matrix.race.C3=AgeStdDALYs.matrix.race.C3,
    
    AgeStdDeath.matrix.income.2020=AgeStdDeath.matrix.income.2020,
    AgeStdDeath.matrix.income.2027=AgeStdDeath.matrix.income.2027,
    AgeStdDeath.matrix.income.2036=AgeStdDeath.matrix.income.2036,
    AgeStdDeath.matrix.income.S1=AgeStdDeath.matrix.income.S1,
    AgeStdDeath.matrix.income.S2=AgeStdDeath.matrix.income.S2,
    AgeStdDeath.matrix.income.S3=AgeStdDeath.matrix.income.S3,
    AgeStdDeath.matrix.income.C1=AgeStdDeath.matrix.income.C1,
    AgeStdDeath.matrix.income.C2=AgeStdDeath.matrix.income.C2,
    AgeStdDeath.matrix.income.C3=AgeStdDeath.matrix.income.C3,
    
    AgeStdDALYs.matrix.income.2020=AgeStdDALYs.matrix.income.2020,
    AgeStdDALYs.matrix.income.2027=AgeStdDALYs.matrix.income.2027,
    AgeStdDALYs.matrix.income.2036=AgeStdDALYs.matrix.income.2036,
    AgeStdDALYs.matrix.income.S1=AgeStdDALYs.matrix.income.S1,
    AgeStdDALYs.matrix.income.S2=AgeStdDALYs.matrix.income.S2,
    AgeStdDALYs.matrix.income.S3=AgeStdDALYs.matrix.income.S3,
    AgeStdDALYs.matrix.income.C1=AgeStdDALYs.matrix.income.C1,
    AgeStdDALYs.matrix.income.C2=AgeStdDALYs.matrix.income.C2,
    AgeStdDALYs.matrix.income.C3=AgeStdDALYs.matrix.income.C3
  ))
  
}

# output the age.std health outcome 
# (two races in order to integrate with injury module)
AgeStdHealthOutcome.twoRaces <- function(countyID){

  AgeStdDeath.matrix.race.2020 <- AgeStdDeath.matrix.race.2036 <-AgeStdDeath.matrix.race.2027<-
    AgeStdDeath.matrix.race.S1<-AgeStdDeath.matrix.race.S2<-AgeStdDeath.matrix.race.S3<-
    AgeStdDeath.matrix.race.C1<-AgeStdDeath.matrix.race.C2<-AgeStdDeath.matrix.race.C3<-
    matrix(NA,nrow = length(countyID),ncol = 2,dimnames = list(countyNames[countyID],
                                                               c('1.NHW','2.People of color')))
  
  AgeStdDALYs.matrix.race.2020 <- AgeStdDALYs.matrix.race.2036 <-AgeStdDALYs.matrix.race.2027<-
    AgeStdDALYs.matrix.race.S1<-AgeStdDALYs.matrix.race.S2<-AgeStdDALYs.matrix.race.S3<-
    AgeStdDALYs.matrix.race.C1<-AgeStdDALYs.matrix.race.C2<-AgeStdDALYs.matrix.race.C3<-
    matrix(NA,nrow = length(countyID),ncol = 2,dimnames = list(countyNames[countyID],
                                                               c('1.NHW','2.People of color')))
  
  j=1
  for (i in countyID){
    
    HealthOutcome <- output.HealthOutcome(i)
    All.InputPara <- read.csv.files(i)
    
    temp.race.2020 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                   HealthOutcome$HealthOutcome_byRace.2020)
    temp.race.2036 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                   HealthOutcome$HealthOutcome_byRace.2036)
    temp.race.2027 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                   HealthOutcome$HealthOutcome_byRace.2027)
    temp.race.S1 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                 HealthOutcome$HealthOutcome_byRace.S1)
    temp.race.S2 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                 HealthOutcome$HealthOutcome_byRace.S2)
    temp.race.S3 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                 HealthOutcome$HealthOutcome_byRace.S3)
    temp.race.C1 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                 HealthOutcome$HealthOutcome_byRace.C1)
    temp.race.C2 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                 HealthOutcome$HealthOutcome_byRace.C2)
    temp.race.C3 <- computeAgeStdOutput.twoRaces(All.InputPara$InputPara_byRace,
                                                 HealthOutcome$HealthOutcome_byRace.C3)
    
    AgeStdDeath.matrix.race.2020[j,] <- temp.race.2020$age.std.death
    AgeStdDeath.matrix.race.2036[j,] <- temp.race.2036$age.std.death
    AgeStdDeath.matrix.race.2027[j,] <- temp.race.2027$age.std.death
    AgeStdDeath.matrix.race.S1[j,] <- temp.race.S1$age.std.death
    AgeStdDeath.matrix.race.S2[j,] <- temp.race.S2$age.std.death
    AgeStdDeath.matrix.race.S3[j,] <- temp.race.S3$age.std.death
    AgeStdDeath.matrix.race.C1[j,] <- temp.race.C1$age.std.death
    AgeStdDeath.matrix.race.C2[j,] <- temp.race.C2$age.std.death
    AgeStdDeath.matrix.race.C3[j,] <- temp.race.C3$age.std.death
    
    AgeStdDALYs.matrix.race.2020[j,] <- temp.race.2020$age.std.DALYs
    AgeStdDALYs.matrix.race.2036[j,] <- temp.race.2036$age.std.DALYs
    AgeStdDALYs.matrix.race.2027[j,] <- temp.race.2027$age.std.DALYs
    AgeStdDALYs.matrix.race.S1[j,] <- temp.race.S1$age.std.DALYs
    AgeStdDALYs.matrix.race.S2[j,] <- temp.race.S2$age.std.DALYs
    AgeStdDALYs.matrix.race.S3[j,] <- temp.race.S3$age.std.DALYs
    AgeStdDALYs.matrix.race.C1[j,] <- temp.race.C1$age.std.DALYs
    AgeStdDALYs.matrix.race.C2[j,] <- temp.race.C2$age.std.DALYs
    AgeStdDALYs.matrix.race.C3[j,] <- temp.race.C3$age.std.DALYs
    
    j=j+1
  }
  
  return(list(
    AgeStdDeath.matrix.race.2020.twoRaces = AgeStdDeath.matrix.race.2020,
    AgeStdDeath.matrix.race.2027.twoRaces = AgeStdDeath.matrix.race.2027,
    AgeStdDeath.matrix.race.2036.twoRaces = AgeStdDeath.matrix.race.2036,
    AgeStdDeath.matrix.race.S1.twoRaces = AgeStdDeath.matrix.race.S1,
    AgeStdDeath.matrix.race.S2.twoRaces = AgeStdDeath.matrix.race.S2,
    AgeStdDeath.matrix.race.S3.twoRaces = AgeStdDeath.matrix.race.S3,
    AgeStdDeath.matrix.race.C1.twoRaces = AgeStdDeath.matrix.race.C1,
    AgeStdDeath.matrix.race.C2.twoRaces = AgeStdDeath.matrix.race.C2,
    AgeStdDeath.matrix.race.C3.twoRaces = AgeStdDeath.matrix.race.C3,
    
    AgeStdDALYs.matrix.race.2020.twoRaces = AgeStdDALYs.matrix.race.2020,
    AgeStdDALYs.matrix.race.2027.twoRaces = AgeStdDALYs.matrix.race.2027,
    AgeStdDALYs.matrix.race.2036.twoRaces = AgeStdDALYs.matrix.race.2036,
    AgeStdDALYs.matrix.race.S1.twoRaces = AgeStdDALYs.matrix.race.S1,
    AgeStdDALYs.matrix.race.S2.twoRaces = AgeStdDALYs.matrix.race.S2,
    AgeStdDALYs.matrix.race.S3.twoRaces = AgeStdDALYs.matrix.race.S3,
    AgeStdDALYs.matrix.race.C1.twoRaces = AgeStdDALYs.matrix.race.C1,
    AgeStdDALYs.matrix.race.C2.twoRaces = AgeStdDALYs.matrix.race.C2,
    AgeStdDALYs.matrix.race.C3.twoRaces = AgeStdDALYs.matrix.race.C3
    
  ))
}

# Part 4 Function Definition - Output (ggplot) ------------------------------------------------------------

# shape the outcomes for ggplot
# race: demogrID = 1; income: demogrID=2
DFforFigure <- function(OutcomeMatrix.list,demogrID,countyID,barID){
  
  if(barID==1){
    OutcomeMatrix.Scenario.1 <- OutcomeMatrix.list[[1]]
    OutcomeMatrix.Scenario.2 <- OutcomeMatrix.list[[2]]
    OutcomeMatrix.Scenario.3 <- OutcomeMatrix.list[[3]]
    
    scenario.name <- rep(c('2020','2027','2036'),each=4)
    
  }else if(barID==2){
    OutcomeMatrix.Scenario.1 <- OutcomeMatrix.list[[4]]
    OutcomeMatrix.Scenario.2 <- OutcomeMatrix.list[[5]]
    OutcomeMatrix.Scenario.3 <- OutcomeMatrix.list[[6]]
    
    scenario.name <- rep(c('S1','S2','S3'),each=4)
  }else if(barID==3){
    OutcomeMatrix.Scenario.1 <- OutcomeMatrix.list[[7]]
    OutcomeMatrix.Scenario.2 <- OutcomeMatrix.list[[8]]
    OutcomeMatrix.Scenario.3 <- OutcomeMatrix.list[[9]]
    
    scenario.name <- rep(c('C1','C2','C3'),each=4)
  }
  
  OutcomeMatrix <- rbind(OutcomeMatrix.Scenario.1[countyID,],
                         OutcomeMatrix.Scenario.2[countyID,],
                         OutcomeMatrix.Scenario.3[countyID,])
  
  raceGroup <- rep(c("1.White",'2.Black','3.Hisp','4.Other'),3)
  #income group names
  incomeGroup <- rep(incomeGroupNames,3)
  
  if (demogrID==1) {
    demogrGroup = raceGroup
    #shape the outcome as data.frame
    outcome <- outcome.update <- as.data.frame(matrix(t(OutcomeMatrix),nDemoClass*nrow(OutcomeMatrix),1))
    for (i in 1:3){
      outcome.update[4*i-1,1] <- outcome[4*i,1]
      outcome.update[4*i,1] <- outcome[4*i-1,1]
    }
    
    df <- data.frame(Scenario = scenario.name,DemogrGroup = demogrGroup,v = (-outcome.update))
    
  }else{
    demogrGroup = incomeGroup
    #shape the outcome as data.frame
    outcome <- as.data.frame(matrix(t(OutcomeMatrix),nDemoClass*nrow(OutcomeMatrix),1))
    df <- data.frame(Scenario = scenario.name,DemogrGroup = demogrGroup,v = (-outcome))
    
  }
  
  return(df=df)
}

# shape the outcomes for ggplot (two races: white + people of color)
DFforFigure.PA.twoRaces<-function(OutcomeMatrix.list,countyID,barID){
  
  if(barID==1){
    OutcomeMatrix.Scenario.1 <- OutcomeMatrix.list[[1]]
    OutcomeMatrix.Scenario.2 <- OutcomeMatrix.list[[2]]
    OutcomeMatrix.Scenario.3 <- OutcomeMatrix.list[[3]]
    
    scenario.name <- rep(c('2020','2027','2036'),each=2)
    
  }else if(barID==2){
    OutcomeMatrix.Scenario.1 <- OutcomeMatrix.list[[4]]
    OutcomeMatrix.Scenario.2 <- OutcomeMatrix.list[[5]]
    OutcomeMatrix.Scenario.3 <- OutcomeMatrix.list[[6]]
    
    scenario.name <- rep(c('S1','S2','S3'),each=2)
    
  }else if(barID==3){
    OutcomeMatrix.Scenario.1 <- OutcomeMatrix.list[[7]]
    OutcomeMatrix.Scenario.2 <- OutcomeMatrix.list[[8]]
    OutcomeMatrix.Scenario.3 <- OutcomeMatrix.list[[9]]
    
    scenario.name <- rep(c('C1','C2','C3'),each=2)
  }
  
  OutcomeMatrix <- rbind(OutcomeMatrix.Scenario.1[countyID,],
                         OutcomeMatrix.Scenario.2[countyID,],
                         OutcomeMatrix.Scenario.3[countyID,])

  raceGroup <- rep(c("1.White",'2.People of color'),3)
  
  demogrGroup = raceGroup
  
  #shape the outcome as data.frame
  outcome <- as.data.frame(matrix(t(OutcomeMatrix),2 * nrow(OutcomeMatrix),1))
  
  df <- data.frame(Scenario=scenario.name,DemogrGroup=demogrGroup,v =(-outcome))
  
  return(df.twoRaces=df)
}

# data frame for region-wide results
DFforRegionWide <- function(ReductionOutcome,demogrID,dbID,barID){
  df.region <- NULL
  
  for (i in 1:6){# countyID
    df.temp <- DFforFigure(ReductionOutcome[c((demogrID*18+dbID*9-26):(demogrID*18+dbID*9-18))],demogrID,i,barID)
    df.region <- rbind(df.region,df.temp)
  }
  
  df.region$county <- rep(countyNames,each = 12)
  
  return(df.region)
}

# data frame for physical activity data (active travel time, unit:min/week)
DFforPhysicalActivity <- function(barID,countyID,demogrID){
  if (barID==1){
    OutcomeMatrix.walk.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.2020[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.2027[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.2036[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.2020[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.2027[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.2036[c(countyID),c(3,5,7,9)])
    
    OutcomeMatrix.walk.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.2020[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.2027[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.2036[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.2020[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.2027[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.2036[c(countyID),c(3,5,7,9)])
    
    scenario.name <- rep(c('2012','2020','2027','2036'),each=4)
    
  }else if (barID==2){
    
    OutcomeMatrix.walk.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.S1[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.S2[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.S3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.S1[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.S2[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.S3[c(countyID),c(3,5,7,9)])
    
    OutcomeMatrix.walk.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.S1[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.S2[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.S3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.S1[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.S2[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.S3[c(countyID),c(3,5,7,9)])
    
    scenario.name <- rep(c('2012','S1','S2','S3'),each=4)
    
  }else if (barID==3){
    
    OutcomeMatrix.walk.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.C1[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.C2[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.C3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.C1[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.C2[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.C3[c(countyID),c(3,5,7,9)])
    
    OutcomeMatrix.walk.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.C1[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.C2[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.C3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.C1[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.C2[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.C3[c(countyID),c(3,5,7,9)])
    
    scenario.name <- rep(c('2012','C1','C2','C3'),each=4)
  }
  
  #race group names
  raceGroup <- rep(c("1.White",'2.Black','3.Hisp','4.Other'),4)
  #income group names
  incomeGroup <- rep(incomeGroupNames,4)
  
  if (demogrID==1) {
    demogrGroup = raceGroup
    #shape the outcome as data.frame
    outcome.walk <- outcome.update.walk <- 
      as.data.frame(matrix(t(OutcomeMatrix.walk.byRace),nDemoClass*nrow(OutcomeMatrix.walk.byRace),1))
    outcome.cycle <- outcome.update.cycle <-
      as.data.frame(matrix(t(OutcomeMatrix.cycle.byRace),nDemoClass*nrow(OutcomeMatrix.cycle.byRace),1))
    
    for (i in 1:3){
      outcome.update.walk[4*i-1,1] <- outcome.walk[4*i,1]
      outcome.update.walk[4*i,1] <- outcome.walk[4*i-1,1]
      
      outcome.update.cycle[4*i-1,1] <- outcome.cycle[4*i,1]
      outcome.update.cycle[4*i,1] <- outcome.cycle[4*i-1,1]
    }
    
    df.walk <- data.frame(Scenario=scenario.name,
                          DemogrGroup = demogrGroup,Mode = 'walk',v1 =(outcome.update.walk))
    df.cycle <- data.frame(Scenario=scenario.name,
                           DemogrGroup = demogrGroup,Mode = 'cycle',v1 =(outcome.update.cycle))
    df.at <- rbind(df.walk,df.cycle)
    
  }else{
    demogrGroup = incomeGroup
    #shape the outcome as data.frame
    outcome.walk <- as.data.frame(matrix(t(OutcomeMatrix.walk.byIncome),
                                         nDemoClass*nrow(OutcomeMatrix.walk.byIncome),1))
    outcome.cycle <- as.data.frame(matrix(t(OutcomeMatrix.cycle.byIncome),
                                          nDemoClass*nrow(OutcomeMatrix.cycle.byIncome),1))
    
    df.walk <- data.frame(Scenario=scenario.name,
                          DemogrGroup=demogrGroup,Mode = 'walk',v1 =(outcome.walk))
    df.cycle <- data.frame(Scenario=scenario.name,
                           DemogrGroup=demogrGroup,Mode = 'cycle',v1 =(outcome.cycle))
    df.at <- rbind(df.walk,df.cycle)
    
  }
  return(df.at)
}

#countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
#dbID: 1-death,2-DALYs
#typeID: 1-raw,2-age.std
#demogrID: 1-race,2-income
#barID: 1-future years, 2-scenarios
plot.shiny.app.PA <- function(countyID,dbID,typeID,demogrID,barID){

  if(typeID == 1){
    
    if (countyID %in% c(1:6)){
      df.result <- DFforFigure(RawReductionOutcome[c((demogrID*18+dbID*9-26):(demogrID*18+dbID*9-18))],
                               demogrID, countyID, barID)
      
      plot.title <- paste0(countyNames[countyID],": Reduction in Total ",
                           dbNames[dbID],"\nfrom Physical Activity Module")
      
      ggplot(data = df.result, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
        geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") +
        xlab(NULL) + 
        ylab("Total Reduction in Health Burden")+
        geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
                  position = position_dodge(width = 0.5)) +
        theme_bw(base_size = 15) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
        labs(caption = plot.caption.text) +
        ggtitle(plot.title)
      
    }else if(countyID == 7){
      df.result <- DFforRegionWide(RawReductionOutcome,demogrID = demogrID,dbID = dbID,barID = barID)
      
      plot.title <- paste0("Region-Wide: Reduction in Total ",
                           dbNames[dbID],"\nfrom Physical Activity Module")
      
      ggplot(data = df.result, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
        geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") + 
        xlab(NULL) +
        ylab("Total Reduction in Health Burden") +
        # geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
        #           position = position_dodge(width = 0.5)) +
        theme_bw(base_size = 15) +
        ggtitle(plot.title) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
        labs(caption = plot.caption.text) +
        facet_wrap(~county)
    }
    
  }else if (typeID == 2) {
    
    if (countyID%in%c(1:6)){
      df.result <- DFforFigure(AgeStdReductionOutcome[c((demogrID*18+dbID*9-26):(demogrID*18+dbID*9-18))],
                               demogrID, countyID, barID)
      
      plot.title <- paste0(countyNames[countyID],': ', dbNames[dbID],  
                           " from Physical Activity Module\nStandardized by Age and Population")
      
      ggplot(data = df.result, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") +
        xlab(NULL) + 
        ylab("Reduction in Health Burden\n(per 100,000 population)")+
        geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
                  position = position_dodge(width = 0.5)) +
        theme_bw(base_size = 15) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
        labs(caption = plot.caption.text) +
        ggtitle(plot.title)
      
    }else if(countyID==7){
      df.result <- DFforRegionWide(AgeStdReductionOutcome,demogrID = demogrID,dbID = dbID,barID = barID)
      
      plot.title <- paste0("Region-Wide: Total ", dbNames[dbID],
                           " from Physical Activity Module\nStandardized by Age and Population")
      
      ggplot(data = df.result, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
        geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") +
        xlab(NULL) + 
        ylab("Reduction in Health Buden\n(per 100,000 population)")+
        # geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
        #           position = position_dodge(width = 0.5)) +
        theme_bw(base_size = 15) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
        labs(caption = plot.caption.text) +
        ggtitle(plot.title) + 
        facet_wrap(~county)
    }
    
  }else{
    # plot for physical activity data
    if (countyID %in% c(1:6)){
      df.at <- DFforPhysicalActivity(barID,countyID,demogrID)
      
      plot.title <- paste0(countyNames[countyID],": Active Travel Time")
      
      ggplot(data = df.at, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
        geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") +
        xlab(NULL) + 
        ylab("Active Travel Time\n(mins per week per capita)") +
        geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
                  position = position_dodge(width = 0.5)) +
        theme_bw(base_size = 15) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15)),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(caption = plot.caption.text) + 
        ggtitle(plot.title) +
        facet_grid(Mode ~ ., scales = "free")
      
    }else if (countyID == 7) {
      
      df.region <- NULL
      
      for (i in 1:6){
        df.temp <- DFforPhysicalActivity(barID,i,demogrID)
        df.region <- rbind(df.region,df.temp)
      }
      
      df.region$county <- rep(countyNames,each = 32)
      
      ggplot(data = df.region, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") +
        xlab(NULL) + 
        ylab("Active Travel Time\n(mins per week per capita)") +
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        theme_bw(base_size = 15) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15)),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(caption = plot.caption.text) + 
        ggtitle("Region-Wide: Active Travel Time") +
        facet_grid(Mode ~ county, scales = "free")
    }
  }
}

#write the outcome into .csv files
write.csv.func <- function(HealthOutcome){
  temp <- NULL
  cutting.line <- matrix(" ",1,4)
  
  for(i in (1:nRaceClass)){
    temp.prop <- matrix(HealthOutcome[[i]]$prop.delta.Burden,1,nRaceClass)
    temp.sum <- matrix(colSums(HealthOutcome[[i]]$delta.Burden),1,nRaceClass)
    rownames(temp.prop) <- "proportion"
    rownames(temp.sum) <- "total"
    temp<-rbind(temp,HealthOutcome[[i]]$delta.Burden,temp.sum,temp.prop,cutting.line)
  }
  
  return(temp)
}

# Part 5 Read external data sources and define parameter -------------------------------------------------

# Number of age & demographic categories
nAgeClass <- 8L
nRaceClass <- nIncomeClass <- nDemoClass <- 4L

# paramter of Physical Activity Risk Function (power)
k <- 0.5

# disease names
#diseaseNames <- c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes")

# group names for race/ethnicity and income 
raceGroupNames <- c("1.NHW","2.NHB","3.NHO","4.HO")
incomeGroupNames <- c("Quantile 1","Quantile 2","Quantile 3","Quantile 4")

# disease burden
dbNames <- c('Deaths', 'DALYs')

# county names
countyNames <- c("El Dorado", "Placer", "Sacramento", "Sutter", "Yolo", "Yuba")

# plot caption
plot.caption.text <- paste("Planning scenarios and future years are shown relative to the baseline year 2012.\n\n",
                           "Income quantiles are defined as follows: Quantile 1 is <$32,000/yr,\n",
                           "Quantile 2 is $32,000 - $62,090/yr, Quantile 3 is $62,090 - 105,000/yr,\n",
                           "and Quantile 4 is >$105,000/yr.")

# population input
Pop_Input_US <- read.csv("01_Data/01_Population/01_Population_US_EA.csv")
gbd_Input_US <- read.csv("01_Data/04_GBD/01_GBD_US_AllCause.csv")

Pop.file.race <- read.csv("01_Data/01_Population/02_Population_byRace_2012.csv")
Pop.file.income <- read.csv("01_Data/01_Population/03_Population_byIncome_2012.csv")

# input the .csv files of active travel data
AT.file.baseline.byRace <- read.csv("01_Data/02_ActiveTransport/01_ActiveTransport_Baseline_EA/01_ActiveTransport_byRace.2012.csv")
AT.file.baseline.byIncome <- read.csv("01_Data/02_ActiveTransport/01_ActiveTransport_Baseline_EA/02_ActiveTransport_byIncome.2012.csv")
AT_Pop_MeanTimebyRace.baseline <- read.csv("01_Data/02_ActiveTransport/01_ActiveTransport_Baseline_EA/03_PopulationMeanATTimebyRace.2012.csv")
AT_Pop_MeanTimebyIncome.baseline <- read.csv("01_Data/02_ActiveTransport/01_ActiveTransport_Baseline_EA/04_PopulationMeanATTimebyIncome.2012.csv")

AT.file.2020.byRace <- read.csv("01_Data/02_ActiveTransport/02_ActiveTransport_2020_EA/01_ActiveTransport_byRace.2020.csv")
AT.file.2020.byIncome <- read.csv("01_Data/02_ActiveTransport/02_ActiveTransport_2020_EA/02_ActiveTransport_byIncome.2020.csv")
AT_Pop_MeanTimebyRace.2020 <- read.csv("01_Data/02_ActiveTransport/02_ActiveTransport_2020_EA/03_PopulationMeanATTimebyRace.2020.csv")
AT_Pop_MeanTimebyIncome.2020 <- read.csv("01_Data/02_ActiveTransport/02_ActiveTransport_2020_EA/04_PopulationMeanATTimebyIncome.2020.csv")

AT.file.2036.byRace <- read.csv("01_Data/02_ActiveTransport/03_ActiveTransport_2036_EA/01_ActiveTransport_byRace.2036.csv")
AT.file.2036.byIncome <- read.csv("01_Data/02_ActiveTransport/03_ActiveTransport_2036_EA/02_ActiveTransport_byIncome.2036.csv")
AT_Pop_MeanTimebyRace.2036 <- read.csv("01_Data/02_ActiveTransport/03_ActiveTransport_2036_EA/03_PopulationMeanATTimebyRace.2036.csv")
AT_Pop_MeanTimebyIncome.2036 <- read.csv("01_Data/02_ActiveTransport/03_ActiveTransport_2036_EA/04_PopulationMeanATTimebyIncome.2036.csv")

AT.file.2027.byRace <- read.csv("01_Data/02_ActiveTransport/04_ActiveTransport_2027_EA/01_ActiveTransport_byRace.2027.csv")
AT.file.2027.byIncome <- read.csv("01_Data/02_ActiveTransport/04_ActiveTransport_2027_EA/02_ActiveTransport_byIncome.2027.csv")
AT_Pop_MeanTimebyRace.2027 <- read.csv("01_Data/02_ActiveTransport/04_ActiveTransport_2027_EA/03_PopulationMeanATTimebyRace.2027.csv")
AT_Pop_MeanTimebyIncome.2027 <- read.csv("01_Data/02_ActiveTransport/04_ActiveTransport_2027_EA/04_PopulationMeanATTimebyIncome.2027.csv")

AT.file.S1.byRace <- read.csv("01_Data/02_ActiveTransport/05_ActiveTransport_S1_EA/01_ActiveTransport_byRace.S1.csv")
AT.file.S1.byIncome <- read.csv("01_Data/02_ActiveTransport/05_ActiveTransport_S1_EA/02_ActiveTransport_byIncome.S1.csv")
AT_Pop_MeanTimebyRace.S1 <- read.csv("01_Data/02_ActiveTransport/05_ActiveTransport_S1_EA/03_PopulationMeanATTimebyRace.S1.csv")
AT_Pop_MeanTimebyIncome.S1 <- read.csv("01_Data/02_ActiveTransport/05_ActiveTransport_S1_EA/04_PopulationMeanATTimebyIncome.S1.csv")

AT.file.S2.byRace <- read.csv("01_Data/02_ActiveTransport/06_ActiveTransport_S2_EA/01_ActiveTransport_byRace.S2.csv")
AT.file.S2.byIncome <- read.csv("01_Data/02_ActiveTransport/06_ActiveTransport_S2_EA/02_ActiveTransport_byIncome.S2.csv")
AT_Pop_MeanTimebyRace.S2 <- read.csv("01_Data/02_ActiveTransport/06_ActiveTransport_S2_EA/03_PopulationMeanATTimebyRace.S2.csv")
AT_Pop_MeanTimebyIncome.S2 <- read.csv("01_Data/02_ActiveTransport/06_ActiveTransport_S2_EA/04_PopulationMeanATTimebyIncome.S2.csv")

AT.file.S3.byRace <- read.csv("01_Data/02_ActiveTransport/07_ActiveTransport_S3_EA/01_ActiveTransport_byRace.S3.csv")
AT.file.S3.byIncome <- read.csv("01_Data/02_ActiveTransport/07_ActiveTransport_S3_EA/02_ActiveTransport_byIncome.S3.csv")
AT_Pop_MeanTimebyRace.S3 <- read.csv("01_Data/02_ActiveTransport/07_ActiveTransport_S3_EA/03_PopulationMeanATTimebyRace.S3.csv")
AT_Pop_MeanTimebyIncome.S3 <- read.csv("01_Data/02_ActiveTransport/07_ActiveTransport_S3_EA/04_PopulationMeanATTimebyIncome.S3.csv")

AT.file.C1.byRace <- read.csv("01_Data/02_ActiveTransport/08_ActiveTransport_C1_EA/01_ActiveTransport_byRace.C1.csv")
AT.file.C1.byIncome <- read.csv("01_Data/02_ActiveTransport/08_ActiveTransport_C1_EA/02_ActiveTransport_byIncome.C1.csv")
AT_Pop_MeanTimebyRace.C1 <- read.csv("01_Data/02_ActiveTransport/08_ActiveTransport_C1_EA/03_PopulationMeanATTimebyRace.C1.csv")
AT_Pop_MeanTimebyIncome.C1 <- read.csv("01_Data/02_ActiveTransport/08_ActiveTransport_C1_EA/04_PopulationMeanATTimebyIncome.C1.csv")

AT.file.C2.byRace <- read.csv("01_Data/02_ActiveTransport/09_ActiveTransport_C2_EA/01_ActiveTransport_byRace.C2.csv")
AT.file.C2.byIncome <- read.csv("01_Data/02_ActiveTransport/09_ActiveTransport_C2_EA/02_ActiveTransport_byIncome.C2.csv")
AT_Pop_MeanTimebyRace.C2 <- read.csv("01_Data/02_ActiveTransport/09_ActiveTransport_C2_EA/03_PopulationMeanATTimebyRace.C2.csv")
AT_Pop_MeanTimebyIncome.C2 <- read.csv("01_Data/02_ActiveTransport/09_ActiveTransport_C2_EA/04_PopulationMeanATTimebyIncome.C2.csv")

AT.file.C3.byRace <- read.csv("01_Data/02_ActiveTransport/10_ActiveTransport_C3_EA/01_ActiveTransport_byRace.C3.csv")
AT.file.C3.byIncome <- read.csv("01_Data/02_ActiveTransport/10_ActiveTransport_C3_EA/02_ActiveTransport_byIncome.C3.csv")
AT_Pop_MeanTimebyRace.C3 <- read.csv("01_Data/02_ActiveTransport/10_ActiveTransport_C3_EA/03_PopulationMeanATTimebyRace.C3.csv")
AT_Pop_MeanTimebyIncome.C3 <- read.csv("01_Data/02_ActiveTransport/10_ActiveTransport_C3_EA/04_PopulationMeanATTimebyIncome.C3.csv")

# input the matrix of Non-travel METs 
# source: CHIS2005 (Per capita weekly non-travel related physical activity expressed as metabolic equivalent tasks (kcal/kg body weight/hr of activity))
nonTravelMET_Input_byRace <- read.csv("01_Data/03_nonTravelMET/01_nonTravelMET_byRace.csv")
nonTravelMET_Input_byIncome <- read.csv("01_Data/03_nonTravelMET/02_nonTravelMET_byIncome.csv")

# Part 6 Computation for ggplot -------------------------------------------------------------

# compute the raw total reduction of health burdens
RawReductionOutcome <- Reduction.output(c(1:6))
# compute the age.std reduction of health burdens
AgeStdReductionOutcome <- AgeStdHealthOutcome(c(1:6))
# compute the age.std reduction of health burdens for two races
AgeStdReductionOutcome.twoRaces <- AgeStdHealthOutcome.twoRaces(c(1:6))
