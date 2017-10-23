###################### ITHIM application for Equity Analysis - Web Interface - Shiny App ######################

# read R scripts for physical activity module and traffic injury module
#setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/00_R scripts")
#source('EquityAnalysis_ITHIM_PA.R')

#setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/00_R scripts")
#source('EquityAnalysis_ITHIM_Injuries_TwoRaces.R')

###################### ITHIM application for Equity Analysis - Physical Activity Module ######################
#library definition
library(ggplot2)

#set your working directory
#setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/06_Equity Analysis/")

# Prevent scientific notation
options(scipen = 100)

###################### Function Definition ##############################
# function for reading .csv files by countyID
read.csv.files <- function(countyID){
  
  # input the population (source: US Census)
  Pop_Input_byRace <- Pop.file.race[(countyID*9-8):(countyID*9-1),1:9]
  Pop_Input_byIncome <- Pop.file.income[(countyID*9-8):(countyID*9-1),1:9]
  #Pop_Input_byRace <- read.csv(paste0("01_Population/",pop.file.names[countyID*2]))
  #Pop_Input_byIncome <- read.csv(paste0("01_Population/",pop.file.names[countyID*2+1]))
  
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
  
  #AT.file.names.baseline <- list.files(path = "02_ActiveTransport/01_ActiveTransport_Baseline_EA")
  #AT_Input_byRace.baseline <- read.csv(paste0("02_ActiveTransport/01_ActiveTransport_Baseline_EA/",AT.file.names.baseline[countyID*2-1]))
  #AT_Input_byIncome.baseline <- read.csv(paste0("02_ActiveTransport/01_ActiveTransport_Baseline_EA/",AT.file.names.baseline[countyID*2]))
  
  # AT.file.names.2020 <- list.files(path = "02_ActiveTransport/02_ActiveTransport_2020_EA")
  # # input the parameters of active transport of 2020 (include relative walking/cycling time and speed)
  # AT_Input_byRace.2020 <- read.csv(paste0("02_ActiveTransport/02_ActiveTransport_2020_EA/",AT.file.names.2020[countyID*2-1]))
  # AT_Input_byIncome.2020 <- read.csv(paste0("02_ActiveTransport/02_ActiveTransport_2020_EA/",AT.file.names.2020[countyID*2]))
  # 
  # AT.file.names.2036 <- list.files(path = "02_ActiveTransport/03_ActiveTransport_2036_EA")
  # # input the parameters of active transport of 2036 (include relative walking/cycling time and speed)
  # AT_Input_byRace.2036 <- read.csv(paste0("02_ActiveTransport/03_ActiveTransport_2036_EA/",AT.file.names.2036[countyID*2-1]))
  # AT_Input_byIncome.2036 <- read.csv(paste0("02_ActiveTransport/03_ActiveTransport_2036_EA/",AT.file.names.2036[countyID*2]))
  # 
  
  #AT_Pop_MeanTime <- read.csv("02_ActiveTransport/PopulationMeanATTime.csv")
  #AT_Pop_List_MeanTime <- rep(list((matrix(NA,nrow=6,ncol=2))),3)
  #for(i in 1:3){
  #AT_Pop_List_MeanTime[[i]] <- as.matrix(AT_Pop_MeanTime[((7*i-6):(7*i-1)),2:3])
  #}
  #names(AT_Pop_List_MeanTime) <- c("baseline",'2020','2036')
  
  # input the population mean cycling and walking time by race (min/week)
  
  # AT_Pop_List_MeanTimebyRace <- rep(list((matrix(NA,nrow=6,ncol=8))),7)
  # for(i in 1:7){
  #   AT_Pop_List_MeanTimebyRace[[i]] <- as.matrix(AT_Pop_MeanTimebyRace[((7*i-6):(7*i-1)),2:9])
  # }
  # names(AT_Pop_List_MeanTimebyRace) <- c("baseline",'2020','2036','2027','S1','S2','S3')
  # 
  # # input the population mean cycling and walking time by income (min/week)
  # 
  # AT_Pop_List_MeanTimebyIncome <- rep(list((matrix(NA,nrow=6,ncol=8))),7)
  # for(i in 1:7){
  #   AT_Pop_List_MeanTimebyIncome[[i]] <- as.matrix(AT_Pop_MeanTimebyIncome[((7*i-6):(7*i-1)),2:9])
  # }
  # names(AT_Pop_List_MeanTimebyIncome) <- c("baseline",'2020','2036','2027','S1','S2','S3')
  # 
  
  #input the GBD data
  gbd.file.names <- list.files(path = "04_GBD")
  gbd_Input_byRace <- read.csv(paste0("04_GBD/",gbd.file.names[countyID*2]))
  gbd_Input_byIncome <- read.csv(paste0("04_GBD/",gbd.file.names[countyID*2+1]))
  
  # combine all inputs of baseline into a list
  InputPara_byRace <- InputPara(Pop_Input_byRace,nonTravelMET_Input_byRace,gbd_Input_byRace)
  InputPara_byIncome <- InputPara(Pop_Input_byIncome,nonTravelMET_Input_byIncome,gbd_Input_byIncome)
  
  # # input the population (source: US Census/Finance Department)
  # Pop_Input_US <- read.csv("01_Population/01_Population_US_EA.csv")
  # Pop_Input_byRace <- read.csv("01_Population/99_Test_Population_Local_byRace_EA.csv")
  # Pop_Input_byIncome <- read.csv("01_Population/99_Test_Population_Local_byIncome_EA.csv")
  
  # input the parameters of active transport (include relative walking/cycling time and speed)
  #AT_Input_byRace.baseline <- read.csv("02_ActiveTransport/01_ActiveTransport_Baseline_EA/99_Test_ActiveTransport_byRace_Baseline_EA.csv")
  #AT_Input_byIncome.baseline <- read.csv("02_ActiveTransport/01_ActiveTransport_Baseline_EA/99_Test_ActiveTransport_byIncome_Baseline_EA.csv")
  
  #AT_Input_byRace.2020 <- read.csv("02_ActiveTransport/02_ActiveTransport_2020_EA/99_Test_ActiveTransport_byRace_2020_EA.csv")
  #AT_Input_byIncome.2020 <- read.csv("02_ActiveTransport/02_ActiveTransport_2020_EA/99_Test_ActiveTransport_byIncome_2020_EA.csv")
  
  #AT_Input_byRace.2036 <- read.csv("02_ActiveTransport/03_ActiveTransport_2036_EA/99_Test_ActiveTransport_byRace_2036_EA.csv")
  #AT_Input_byIncome.2036 <- read.csv("02_ActiveTransport/03_ActiveTransport_2036_EA/99_Test_ActiveTransport_byIncome_2036_EA.csv")
  
  #input the gbd data
  #gbd_Input_US <- read.csv("04_GBD/99_Test_GBD_US_EA.csv")
  #gbd_Input_byRace <- read.csv("04_GBD/99_Test_GBD_Local_byRace_EA.csv")
  #gbd_Input_byIncome <- read.csv("04_GBD/99_Test_GBD_Local_byIncome_EA.csv")
  
  # combine all inputs of baseline into a list object
  #InputPara_byRace <- InputPara(Pop_Input_byRace,nonTravelMET_Input_byRace,gbd_Input_byRace)
  #InputPara_byIncome <- InputPara(Pop_Input_byIncome,nonTravelMET_Input_byIncome,gbd_Input_byIncome)
  
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
    
    # AT_Pop_MeanWalkTimebyRace.baseline = AT_Pop_List_MeanTimebyRace[[1]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyRace.baseline = AT_Pop_List_MeanTimebyRace[[1]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyRace.2020 = AT_Pop_List_MeanTimebyRace[[2]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyRace.2020 = AT_Pop_List_MeanTimebyRace[[2]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyRace.2036 = AT_Pop_List_MeanTimebyRace[[3]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyRace.2036 = AT_Pop_List_MeanTimebyRace[[3]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyRace.2027 = AT_Pop_List_MeanTimebyRace[[4]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyRace.2027 = AT_Pop_List_MeanTimebyRace[[4]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyRace.S1 = AT_Pop_List_MeanTimebyRace[[5]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyRace.S1 = AT_Pop_List_MeanTimebyRace[[5]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyRace.S2 = AT_Pop_List_MeanTimebyRace[[6]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyRace.S2 = AT_Pop_List_MeanTimebyRace[[6]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyRace.S3 = AT_Pop_List_MeanTimebyRace[[7]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyRace.S3 = AT_Pop_List_MeanTimebyRace[[7]][countyID,c(2,4,6,8)],
    # 
    # AT_Pop_MeanWalkTimebyIncome.baseline = AT_Pop_List_MeanTimebyIncome[[1]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyIncome.baseline = AT_Pop_List_MeanTimebyIncome[[1]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyIncome.2020 = AT_Pop_List_MeanTimebyIncome[[2]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyIncome.2020 = AT_Pop_List_MeanTimebyIncome[[2]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyIncome.2036 = AT_Pop_List_MeanTimebyIncome[[3]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyIncome.2036 = AT_Pop_List_MeanTimebyIncome[[3]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyIncome.2027 = AT_Pop_List_MeanTimebyIncome[[4]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyIncome.2027 = AT_Pop_List_MeanTimebyIncome[[4]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyIncome.S1 = AT_Pop_List_MeanTimebyIncome[[5]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyIncome.S1 = AT_Pop_List_MeanTimebyIncome[[5]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyIncome.S2 = AT_Pop_List_MeanTimebyIncome[[6]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyIncome.S2 = AT_Pop_List_MeanTimebyIncome[[6]][countyID,c(2,4,6,8)],
    # AT_Pop_MeanWalkTimebyIncome.S3 = AT_Pop_List_MeanTimebyIncome[[7]][countyID,c(1,3,5,7)],
    # AT_Pop_MeanCycleTimebyIncome.S3 = AT_Pop_List_MeanTimebyIncome[[7]][countyID,c(2,4,6,8)]
    
    
  ))
  
}

# function for shaping the input data sets (.csv)
InputPara <- function (Pop_Input,nonTravelMET_Input,gbd_Input){
  # input the population and calculate the proportion of population into each demo categories
  Pop_List_byDemo <- rep(list((matrix(NA,nrow=nAgeClass,ncol=2))), nDemoClass)
  for(i in 1:nDemoClass){
    Pop_List_byDemo[[i]] <- as.matrix(Pop_Input[1:nAgeClass,(2*i):(2*i+1)])
  }
  PopProp_List_byDemo <- mapply(function(x) x/sum(x),Pop_List_byDemo,SIMPLIFY = FALSE)
  
  # input the whole US population in 2010. source: US Census 2010
  allPop <- matrix (c(Pop_Input_US[1:8,2],Pop_Input_US[1:8,3]), 
                    byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  
  # # input the active transport data into each demo catrgories
  # AT_List_byDemo <- rep(list((matrix(NA,nrow=nrow(AT_Input),ncol=2))), nDemoClass)
  # for(i in 1:nDemoClass){
  #   AT_List_byDemo[[i]] <- as.matrix(AT_Input[1:nrow(AT_Input),(2*i):(2*i+1)])
  # }
  
  # input the non travel METs into each demo group
  nonTravelMET_List_byDemo <- rep(list((matrix(NA,nrow=2*nAgeClass,ncol=5))), nDemoClass)
  for(i in 1:nDemoClass){
    nonTravelMET_List_byDemo[[i]] <- as.matrix(nonTravelMET_Input[(17*i-16):(17*i-1),2:6])
    dimnames(nonTravelMET_List_byDemo[[i]]) = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2))
  }
  
  #return required parameters
  return(list(
    Pop_List_byDemo = Pop_List_byDemo,
    PopProp_List_byDemo=PopProp_List_byDemo,
    allPop=allPop,
    #AT_List_byDemo=AT_List_byDemo,
    nonTravelMET_List_byDemo=nonTravelMET_List_byDemo,
    gbd_Input = gbd_Input
  )
  )
}

# function for creating total exposure matrix 
TotalExposure <- function(PopMeanWalkTime, PopMeanCycleTime, AT_Input, PopProp, nonTravelMET){
  #######Test
  # PopMeanWalkTime = 36.46005
  # PopMeanCycleTime = 3.397624
  # AT_Input = AT_List_byDemo[[2]]
  # PopProp = All.InputPara$InputPara_byIncome$PopProp_List_byDemo[[2]]
  
  # The population mean walking/cycling speed (mph)
  #"It is common practice in MPOs to assume average walk speed of 3 mph and bicycle speed of 12 mph" from ITHIM user's manual
  PopMeanWalkSpeed <- 3.0
  PopMeanCycleSpeed <- 12.0
  cv <- -0.0108*(PopMeanWalkTime+PopMeanCycleTime)/7+1.2682+0.7
  
  # Numerical matrices for the relative walking time (relative to the value of "female 15-29") and the mean walking time
  # Source: CHTS2012 (Per capita mean daily travel time by mode)
  Rwt <- as.matrix(AT_Input[1:8,1:2])
  dimnames(Rwt) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanWalkTime <- Rwt/PopProp/sum(PopProp*Rwt)*PopMeanWalkTime*PopProp
  meanWalkTime <- replace(meanWalkTime,is.na(meanWalkTime),0)
  
  # Numerical matrices for the relative cycling time (relative to the value of "female 15-29") and the mean cycling time 
  # Source: CHTS2012 (Per capita mean daily travel time by mode)
  Rct <- as.matrix(AT_Input[10:17,1:2])
  dimnames(Rct) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanCycleTime <- Rct/PopProp/sum(PopProp*Rct)*PopMeanCycleTime*PopProp
  meanCycleTime <- replace(meanCycleTime,is.na(meanCycleTime),0)
  
  # Numerical matrices for the proportion of mean cycling time to total active transport time
  PropMeanCycleTime <- meanCycleTime/(meanWalkTime+meanCycleTime)
  PropMeanCycleTime <- replace(PropMeanCycleTime,is.na(PropMeanCycleTime),0)
  
  # Numerical matrices for the relative walking speed (relative to the value of "female 15-29") and the mean walking speed
  # Hard code in spreadsheet with a comment from James W "these will be fixed"
  Rws <- as.matrix(AT_Input[19:26,1:2])
  dimnames(Rws) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanWalkSpeed <- Rws/PopProp/sum(PopProp*Rws)*PopMeanWalkSpeed*PopProp
  meanWalkSpeed <- replace(meanWalkSpeed,is.na(meanWalkSpeed),0)
  
  # Numerical matrices for the relative cycling speed (relative to the value of "female 15-29") and the mean cycling speed
  # Hard code in spreadsheet with a comment from James W "these will be fixed"
  Rcs <- as.matrix(AT_Input[28:35,1:2])
  dimnames(Rcs) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanCycleSpeed <- Rcs/PopProp/sum(PopProp*Rcs)*PopMeanCycleSpeed*PopProp
  meanCycleSpeed <- replace(meanCycleSpeed,is.na(meanCycleSpeed),0)
  
  # Numerical matrices for the mean walking/cycling MET values
  meanWalkMET <- ifelse(1.2216*meanWalkSpeed + 0.0838 < 2.5, 2.5,  1.2216*meanWalkSpeed + 0.0838)
  meanCycleMET <- matrix(6, byrow=TRUE, ncol = 2, nrow = nAgeClass, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
  
  # Compute Quintiles of Active Transport Time
  
  # Total Active Transport Time
  totalATTime <- meanWalkTime + meanCycleTime
  
  # Numerical matrices for the log-normal distribution (mean, sd, log mean, log sd)
  meanATtime <- c(totalATTime[,1],totalATTime[,2])
  sd <- meanATtime*cv
  logMean <- log(meanATtime/sqrt(1+(meanATtime*cv/meanATtime)^2))
  logMean <- replace(logMean,is.na(logMean),0)
  logSD <- sqrt(log(1+(meanATtime*cv/meanATtime)^2))
  logSD <- replace(logSD,is.na(logSD),0)
  
  lognorm <- matrix(c(meanATtime,sd,logMean,logSD), byrow=FALSE, ncol = 4, nrow = nAgeClass*2,
                    dimnames = list(c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass)),c("Mean","SD","log Mean","log sd")))
  
  # Compute quintiles of total AT time
  quintiles <- seq(0.1,0.9,by=0.2)
  quintVec <- c()
  for (quant in quintiles) {
    quintVec <- c(quintVec,mapply(qlnorm,lognorm[,3],lognorm[,4],p=quant))
  }
  
  quintTotalATTime <- matrix (quintVec, byrow=FALSE, ncol = 5, nrow = nAgeClass*2,
                              dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2)))
  quintTotalATTime[which(logMean==0),] <- 0
  
  # Compute the quintiles of total walking/cycling time
  PropMeanCycleTimeCol <- c(PropMeanCycleTime[,1],PropMeanCycleTime[,2])
  
  quintWalkTime <- quintTotalATTime*(1-PropMeanCycleTimeCol)
  quintCycleTime <- quintTotalATTime-quintWalkTime
  
  # Compute the walking/cycling MET-hours and total AC MET-hours
  meanWalkMETCol <- c(meanWalkMET[,1],meanWalkMET[,2])
  quintWalkMET <- quintWalkTime*meanWalkMETCol/60
  
  meanCycleMETCol <- c(meanCycleMET[,1],meanCycleMET[,2])
  quintCycleMET <- quintCycleTime*meanCycleMETCol/60
  
  quintTotalTravelMET <- quintWalkMET+quintCycleMET
  
  # adding up total AC METs and non travel METs
  totalExposure <-ifelse(quintTotalTravelMET + nonTravelMET > 2.5, quintTotalTravelMET + nonTravelMET, 0.1)
  
  # if the population in that category equals 0, then total exposure =0
  pop.prop.reformat <- matrix(PopProp,16,1)
  totalExposure[which(pop.prop.reformat==0),] <- 0
  
  #return the matrix of total exposure
  return(
    totalExposure <- totalExposure
  )
}

# function for creating total exposure matrix by demographic groups
List_TotalExposure <- function(df_PopMeanWalkTime, df_PopMeanCycleTime,InputPara,AT_Input){
  # test###
  # PopMeanWalkTime = 36.46005
  # PopMeanCycleTime = 3.397624
  # AT_Input = All.InputPara$AT_Input_byIncome.baseline
  # InputPara = All.InputPara$InputPara_byIncome
  
  # process the AT_input data into each demo catrgories
  AT_List_byDemo <- rep(list((matrix(NA,nrow=nrow(AT_Input),ncol=2))), nDemoClass)
  for(i in 1:nDemoClass){
    AT_List_byDemo[[i]] <- as.matrix(AT_Input[1:nrow(AT_Input),(2*i):(2*i+1)])
  }
  
  # Calculate the total exposure matrix for demographic categories
  TotalExposure_List_byDemo <- rep(list((matrix(NA,nrow=2*nAgeClass,ncol=5))), nDemoClass)
  
  # apply the function of TotalExposure() for each demographic categories
  for(i in 1:nDemoClass){
    TotalExposure_List_byDemo[[i]] <- TotalExposure(
      df_PopMeanWalkTime[i], df_PopMeanCycleTime[i],
      AT_List_byDemo[[i]],
      InputPara$PopProp_List_byDemo[[i]],
      InputPara$nonTravelMET_List_byDemo[[i]])
  }
  
  return(
    TotalExposure_List_byDemo=TotalExposure_List_byDemo
  )
}

#function for computing relative risks of physical activity  
create.PA.RR <- function(){
  
  RR.lit <- exposure <- matrix(NA,nrow=nAgeClass,ncol=2,dimnames=list(paste0("agClass",1:nAgeClass),c("F","M")))
  
  # all cause mortality (source: Woodcock)
  exposure[1:nAgeClass,1:2] <- 11
  RR.lit[1:nAgeClass,1:2] <- 0.81
  
  #compute RR matrix
  RR <- RR.lit^(1/exposure)^k
  
  #reshape RR matrix
  reshapeRR <- function(RR, nQuantiles = 5){
    matrix(c(RR[,"M"],RR[,"F"]),nrow=nAgeClass*2,ncol=nQuantiles,dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2)))
    #list( M = matrix(RR[,"M"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))),F = matrix(RR[,"F"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))))
  }
  
  RR <- reshapeRR(RR,nQuantiles = 5)
  
  return(RR)
  
  # RR.lit <- exposure <- rep(list((matrix(NA,nrow=nAgeClass,ncol=2,dimnames=list(paste0("agClass",1:nAgeClass),c("F","M"))))), length(diseaseNames))
  # 
  # names(RR.lit) <- names(exposure) <- diseaseNames
  # 
  # #physical
  # exposure[["BreastCancer"]][1:nAgeClass,"F"] <- 4.5
  # RR.lit[["BreastCancer"]][1:nAgeClass,"F"] <- 0.944
  # 
  # exposure[["BreastCancer"]][1:nAgeClass,"M"] <- 1
  # RR.lit[["BreastCancer"]][1:nAgeClass,"M"] <- 1
  # 
  # exposure[["ColonCancer"]][1:nAgeClass,"M"] <- 30.9
  # RR.lit[["ColonCancer"]][1:nAgeClass,"M"] <- 0.8
  # 
  # exposure[["ColonCancer"]][1:nAgeClass,"F"] <- 30.1
  # RR.lit[["ColonCancer"]][1:nAgeClass,"F"] <- 0.86
  # 
  # exposure[["CVD"]][1:nAgeClass,1:2] <- 7.5
  # RR.lit[["CVD"]][1:nAgeClass,1:2] <- 0.84
  # 
  # exposure[["Dementia"]][1:nAgeClass,1:2] <- 31.5
  # RR.lit[["Dementia"]][1:nAgeClass,1:2] <- 0.72
  # 
  # exposure[["Diabetes"]][1:nAgeClass,1:2] <- 10
  # RR.lit[["Diabetes"]][1:nAgeClass,1:2] <- 0.83
  # 
  # exposure[["Depression"]][1:3,1:2] <- 11.25
  # RR.lit[["Depression"]][1:3,1:2] <- 0.927945490148335
  # 
  # exposure[["Depression"]][4:nAgeClass,1:2] <- 11.25
  # RR.lit[["Depression"]][4:nAgeClass,1:2] <- 0.859615572255727
  # 
  # # exposure[["Stroke"]] <- exposure[["CVD"]]
  # # RR.lit[["Stroke"]] <- RR.lit[["CVD"]]
  # #
  # # exposure[["HHD"]] <- exposure[["CVD"]]
  # # RR.lit[["HHD"]] <- RR.lit[["CVD"]]
  # 
  # #reshape RR matrix
  # reshapeRR <- function(RR, nQuantiles = 5){
  #   matrix(c(RR[,"M"],RR[,"F"]),nrow=nAgeClass*2,ncol=nQuantiles,dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2)))
  #   #list( M = matrix(RR[,"M"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))),F = matrix(RR[,"F"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))))
  # }
  # 
  # 
  # #compute RR matrix
  # RR <- mapply(function(x,y,z) x^(1/y)^z, RR.lit, exposure, k, SIMPLIFY=FALSE)
  # RR <- lapply(RR, reshapeRR, nQuantiles = 5)
  # return(RR)
}

#function for computing local disease burden (scaling process)
computeLocalGBD <- function (death_target,TargetPop,InputPara){
  #test
  #death_target = All.InputPara$InputPara_byIncome$gbd_Input[,2]
  #TargetPop <- matrix (All.InputPara$InputPara_byIncome$PopProp_List_byDemo[[1]]*sum(All.InputPara$InputPara_byIncome$Pop_List_byDemo[[1]]), byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  #InputPara = All.InputPara$InputPara_byIncome
  
  #obtain the gbd and the death data of U.S.
  gbd_US <- gbd_Input_US[,2:5]
  death_US <- as.numeric(gbd_Input_US[,2])
  
  # calculate the RR (the ratio of death numbers for target area with those for whole U.S.)
  RR_gbd <- matrix(NA,ncol = 1,nrow = 2*nAgeClass,dimnames = list((c(paste0("maleAgeclass",1:nAgeClass),paste0("femaleAgeclass",1:nAgeClass))),"RR"))
  RR_gbd <- (death_target/TargetPop)/(death_US/InputPara$allPop)
  RR_gbd <- replace(RR_gbd,is.na(RR_gbd)|is.infinite(RR_gbd),1.0)
  
  # obtain the local gbd data
  gbd.local<-RR_gbd*gbd_US/InputPara$allPop*TargetPop
  
  
  # #obtain the gbd and the death data of U.S.
  # gbd_List_US <- split(gbd_Input_US[,3:6],gbd_Input_US$Disease)
  # death_List_US <- split(gbd_Input_US[,3],gbd_Input_US$Disease)
  # 
  # # calculate the RR (the ratio of death numbers for target area with those for whole U.S.)
  # RR_gbd <- rep(list(matrix(NA,ncol = 1,nrow = 2*nAgeClass,dimnames = list((c(paste0("maleAgeclass",1:nAgeClass),paste0("femaleAgeclass",1:nAgeClass))),"RR"))),length(diseaseNames))
  # names(RR_gbd) <- diseaseNames
  # RR_gbd <- mapply(function(x,y) (x/TargetPop)/(y/InputPara$allPop),death_List_target,death_List_US,SIMPLIFY = FALSE)
  # RR_gbd <- lapply(RR_gbd,function(x) replace(x,is.na(x),1.0)) #replace NAs with 1.0
  # 
  # # obtain the local gbd data  
  # gbd.local <- mapply(function(x,y) x*y/InputPara$allPop*TargetPop,RR_gbd,gbd_List_US,SIMPLIFY = FALSE)
  # 
  # # update the colon cancer data with the parameter "colon % of colorectal cancer Male 79% Female 81% "
  # # The source of data is the CDPH Death Statistical Master file for the years 2009 to 2011.
  # gbd.local$ColonCancer[c(1:8),] <- gbd.local$ColonCancer[c(1:8),]*0.7878193
  # gbd.local$ColonCancer[c(9:16),] <- gbd.local$ColonCancer[c(9:16),]*0.814
  
  return(gbd.local)
}

#function for computing local disease burden by demographic groups
List_LocalGBD <- function(InputPara){
  
  LocalGBD_List_byDemo <- rep(list(matrix(NA,nrow=2*nAgeClass,ncol = 4)),nDemoClass)
  
  for (i in 1:nDemoClass){
    death_target <- InputPara$gbd_Input[,i+1]
    TargetPop <- matrix (InputPara$PopProp_List_byDemo[[i]]*sum(InputPara$Pop_List_byDemo[[i]]), byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
    death_target[which(TargetPop==0)] <- 0
    gbd.local <- computeLocalGBD(death_target,TargetPop,InputPara)
    LocalGBD_List_byDemo[[i]] <- gbd.local
  }
  
  return(
    LocalGBD_List_byDemo=LocalGBD_List_byDemo)
  
  #LocalGBD_List_byDemo <- rep(list(rep(list(matrix(NA,nrow=2*nAgeClass,ncol = 4)),length(diseaseNames))),nDemoClass)
  
  # apply the function "computeLocalGBD" for calculating the local gbd data for all demographic groups
  # for (i in 1:nDemoClass){
  #   death_List_target <-  split(InputPara$gbd_Input[,i+2],InputPara$gbd_Input$Disease)
  #   TargetPop <- matrix (InputPara$PopProp_List_byDemo[[i]], byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  #   gbd.local <- computeLocalGBD(death_List_target,TargetPop,InputPara)
  #   LocalGBD_List_byDemo[[i]] <- gbd.local
  # }
  
  
}

#functions for computing health outcome
computeHealthOutcome <- function (RR.PA,BaselineTotalExpo,ScenarioTotalExpo,gbd.local){
  #BaselineTotalExpo <- BaselineTotalExpo_byRace[[1]]
  #ScenarioTotalExpo <- ScenarioTotalExpo_byRace.2020[[1]]
  #gbd.local <- LocalGBD_List_byRace[[1]]
  
  #Compute RR for an exposure of x MET
  RR.Baseline <- RR.PA^(BaselineTotalExpo^k)
  RR.Scenario <- RR.PA^(ScenarioTotalExpo^k)
  
  #Compute Ratio of DB relative to group 1
  RatioDB.Baseline <- RR.Baseline/RR.Baseline[,1]
  RatioDB.Scenario <- RR.Scenario/RR.Scenario[,1]
  sum.RatioDB.Baseline <- rowSums(RatioDB.Baseline)
  sum.RatioDB.Scenario <- rowSums(RatioDB.Scenario)
  
  #Compute New Burden and AF
  new.burden <- rowSums(RR.Scenario)/rowSums(RR.Baseline)
  AF <- 1-new.burden
  
  #Compute the health outcomes
  #Define a function for outputing health outcomes
  fun.outcome <- function(x,y){
    x[,1] <- y
    x[,c(2:5)] <- x[,c(2:5)]*y
    return(x)}
  
  #Compute deaths per group
  dproj.scenario.firstCol <- new.burden*gbd.local$deaths/sum.RatioDB.Scenario
  dproj.scenario <- fun.outcome(RatioDB.Scenario,dproj.scenario.firstCol)
  
  dproj.baseline.firstCol <- gbd.local$deaths/sum.RatioDB.Baseline
  dproj.baseline <- fun.outcome(RatioDB.Baseline,dproj.baseline.firstCol)
  
  #Compute YLL per group
  yll.scenario.firstCol <- new.burden*gbd.local$yll/sum.RatioDB.Scenario
  yll.scenario <- fun.outcome(RatioDB.Scenario,yll.scenario.firstCol)
  
  yll.baseline.firstCol <- gbd.local$yll/sum.RatioDB.Baseline
  yll.baseline <- fun.outcome(RatioDB.Baseline,yll.baseline.firstCol)
  
  #Compute YLD per group
  yld.scenario.firstCol <- new.burden*gbd.local$yld/sum.RatioDB.Scenario
  yld.scenario <- fun.outcome(RatioDB.Scenario,yld.scenario.firstCol)
  
  yld.baseline.firstCol <- gbd.local$yld/sum.RatioDB.Baseline
  yld.baseline <- fun.outcome(RatioDB.Baseline,yld.baseline.firstCol)
  
  #Compute the âBurden, total âBurden, and the proportion
  delta.Burden <- (matrix(NA,nrow=nAgeClass*2,ncol=4,dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),c("delta.Deaths","delta.YLL","delta.YLD","DALYS"))))
  
  delta.Burden[,1] <- rowSums(dproj.scenario)-rowSums(dproj.baseline) #deaths
  delta.Burden[,2] <- rowSums(yll.scenario)-rowSums(yll.baseline)     #yll
  delta.Burden[,3] <- rowSums(yld.scenario)-rowSums(yld.baseline)     #yld
  delta.Burden[,4] <- delta.Burden[,2]+delta.Burden[,3]               #dalys
  
  total.delta.Burden <- colSums(delta.Burden)
  total.gbd.local <- ifelse(colSums(gbd.local)!=0,colSums(gbd.local),0.0001)
  
  prop.delta.Burden <- total.delta.Burden/total.gbd.local
  
  return(list(
    AF=AF,
    new.burden=new.burden,
    delta.Burden=delta.Burden,
    prop.delta.Burden= prop.delta.Burden
  ))
  
  # total.gbd.local <- lapply(gbd.local,function(x){
  #   ifelse(colSums(x)!=0,colSums(x),0.0001)
  # })
  # 
  # prop.delta.Burden <- mapply(function(x,y) x/y, total.delta.Burden,total.gbd.local,SIMPLIFY=FALSE)
  
  
  # #Compute RR for an exposure of x MET
  # RR.Baseline <- sapply(RR.PA, function(x) {x^(BaselineTotalExpo^k)}, simplify = FALSE)
  # RR.Scenario <- sapply(RR.PA, function(x) {x^(ScenarioTotalExpo^k)}, simplify = FALSE)  
  # 
  # #Compute Ratio of DB relative to group 1
  # RatioDB.Baseline <- lapply(RR.Baseline,function(x) x/x[,1])
  # RatioDB.Scenario <- lapply(RR.Scenario,function(x) x/x[,1])
  # sum.RatioDB.Baseline <-lapply(RatioDB.Baseline,rowSums)
  # sum.RatioDB.Scenario <-lapply(RatioDB.Scenario,rowSums)
  # 
  # #Compute New Burden and AF
  # sum.RR.Baseline<-lapply(RR.Baseline,rowSums)
  # sum.RR.Scenario<-lapply(RR.Scenario,rowSums)
  # new.burden <- mapply(function(x,y) x/y,sum.RR.Scenario,sum.RR.Baseline,SIMPLIFY=FALSE)
  # AF <- sapply(new.burden, function(x) 1-x, simplify=FALSE)
  # 
  # #Compute the health outcomes
  # #Define a function for outputing health outcomes
  # fun.outcome <- function(x,y){
  #   x[,1] <- y
  #   x[,c(2:5)] <- x[,c(2:5)]*y
  #   return(x)}
  # 
  # #Compute deaths per group
  # dproj.scenario.firstCol <- mapply(function(x,y,z) x*y$deaths/z, new.burden,gbd.local,sum.RatioDB.Scenario,SIMPLIFY=FALSE)
  # dproj.scenario <- mapply(fun.outcome,RatioDB.Scenario,dproj.scenario.firstCol,SIMPLIFY=FALSE)
  # 
  # dproj.baseline.firstCol <- mapply(function(x,y) x$deaths/y, gbd.local,sum.RatioDB.Baseline,SIMPLIFY=FALSE)
  # dproj.baseline <- mapply(fun.outcome,RatioDB.Baseline,dproj.baseline.firstCol,SIMPLIFY=FALSE)
  # 
  # #Compute YLL per group
  # yll.scenario.firstCol <- mapply(function(x,y,z) x*y$yll/z, new.burden,gbd.local,sum.RatioDB.Scenario,SIMPLIFY=FALSE)
  # yll.scenario <- mapply(fun.outcome,RatioDB.Scenario,yll.scenario.firstCol,SIMPLIFY=FALSE)
  # 
  # yll.baseline.firstCol <- mapply(function(x,y) x$yll/y, gbd.local,sum.RatioDB.Baseline,SIMPLIFY=FALSE)
  # yll.baseline <- mapply(fun.outcome,RatioDB.Baseline,yll.baseline.firstCol,SIMPLIFY=FALSE)
  # 
  # #Compute YLD per group
  # yld.scenario.firstCol <- mapply(function(x,y,z) x*y$yld/z, new.burden,gbd.local,sum.RatioDB.Scenario,SIMPLIFY=FALSE)
  # yld.scenario <- mapply(fun.outcome,RatioDB.Scenario,yld.scenario.firstCol,SIMPLIFY=FALSE)
  # 
  # yld.baseline.firstCol <- mapply(function(x,y) x$yld/y, gbd.local,sum.RatioDB.Baseline,SIMPLIFY=FALSE)
  # yld.baseline <- mapply(fun.outcome,RatioDB.Baseline,yld.baseline.firstCol,SIMPLIFY=FALSE)
  # 
  # #Compute the âBurden, total âBurden, and the proportion
  # delta.Burden <- rep(list((matrix(NA,nrow=nAgeClass*2,ncol=4,dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),c("âDeaths","âYLL","âYLD","DALYS"))))), length(diseaseNames))
  # names(delta.Burden) <- diseaseNames
  # 
  # delta.Burden <- mapply(function (x,a,b,c,d,e,f) {
  #   x[,1] <- rowSums(a)-rowSums(b) #deaths
  #   x[,2] <- rowSums(c)-rowSums(d) #yll
  #   x[,3] <- rowSums(e)-rowSums(f) #yld
  #   x[,4] <- x[,2] + x[,3]         #dalys
  #   return(x)
  # },delta.Burden, dproj.scenario,dproj.baseline,yll.scenario,yll.baseline,yld.scenario,yld.baseline, SIMPLIFY=FALSE)
  # 
  # total.delta.Burden <- lapply(delta.Burden, colSums)
  # total.gbd.local <- lapply(gbd.local,function(x){
  #   ifelse(colSums(x)!=0,colSums(x),0.0001)
  # })
  # 
  # prop.delta.Burden <- mapply(function(x,y) x/y, total.delta.Burden,total.gbd.local,SIMPLIFY=FALSE)
  
  
}

#output the detail heath outcome for each county
output.HealthOutcome <- function(countyID){
  # reading the csv files after inputting countyID
  # countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
  All.InputPara <- read.csv.files(countyID = countyID)
  
  #Create the total exposure matrices by inputing parameters 
  #(mean walking time(min per week), mean cycling time(min per week))
  BaselineTotalExpo_byRace <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.baseline,All.InputPara$AT_Pop_MeanCycleTimebyRace.baseline,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.baseline)
  ScenarioTotalExpo_byRace.2020 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.2020,All.InputPara$AT_Pop_MeanCycleTimebyRace.2020,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.2020)
  ScenarioTotalExpo_byRace.2036 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.2036,All.InputPara$AT_Pop_MeanCycleTimebyRace.2036,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.2036)
  ScenarioTotalExpo_byRace.2027 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.2027,All.InputPara$AT_Pop_MeanCycleTimebyRace.2027,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.2027)
  ScenarioTotalExpo_byRace.S1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.S1,All.InputPara$AT_Pop_MeanCycleTimebyRace.S1,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.S1)
  ScenarioTotalExpo_byRace.S2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.S2,All.InputPara$AT_Pop_MeanCycleTimebyRace.S2,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.S2)
  ScenarioTotalExpo_byRace.S3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.S3,All.InputPara$AT_Pop_MeanCycleTimebyRace.S3,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.S3)
  ScenarioTotalExpo_byRace.C1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.C1,All.InputPara$AT_Pop_MeanCycleTimebyRace.C1,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.C1)
  ScenarioTotalExpo_byRace.C2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.C2,All.InputPara$AT_Pop_MeanCycleTimebyRace.C2,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.C2)
  ScenarioTotalExpo_byRace.C3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyRace.C3,All.InputPara$AT_Pop_MeanCycleTimebyRace.C3,All.InputPara$InputPara_byRace,All.InputPara$AT_Input_byRace.C3)
  
  BaselineTotalExpo_byIncome <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.baseline,All.InputPara$AT_Pop_MeanCycleTimebyIncome.baseline,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.baseline)
  ScenarioTotalExpo_byIncome.2020 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.2020,All.InputPara$AT_Pop_MeanCycleTimebyIncome.2020,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.2020)
  ScenarioTotalExpo_byIncome.2036 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.2036,All.InputPara$AT_Pop_MeanCycleTimebyIncome.2036,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.2036)
  ScenarioTotalExpo_byIncome.2027 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.2027,All.InputPara$AT_Pop_MeanCycleTimebyIncome.2027,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.2027)
  ScenarioTotalExpo_byIncome.S1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.S1,All.InputPara$AT_Pop_MeanCycleTimebyIncome.S1,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.S1)
  ScenarioTotalExpo_byIncome.S2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.S2,All.InputPara$AT_Pop_MeanCycleTimebyIncome.S2,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.S2)
  ScenarioTotalExpo_byIncome.S3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.S3,All.InputPara$AT_Pop_MeanCycleTimebyIncome.S3,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.S3)
  ScenarioTotalExpo_byIncome.C1 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.C1,All.InputPara$AT_Pop_MeanCycleTimebyIncome.C1,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.C1)
  ScenarioTotalExpo_byIncome.C2 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.C2,All.InputPara$AT_Pop_MeanCycleTimebyIncome.C2,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.C2)
  ScenarioTotalExpo_byIncome.C3 <- List_TotalExposure(All.InputPara$AT_Pop_MeanWalkTimebyIncome.C3,All.InputPara$AT_Pop_MeanCycleTimebyIncome.C3,All.InputPara$InputPara_byIncome,All.InputPara$AT_Input_byIncome.C3)
  
  #compute the relative risks of Physical Activity (1MET)
  RR.PA <- create.PA.RR()
  
  #compute local disease burden, and create a list to store local gbd for all races
  LocalGBD_List_byRace <- List_LocalGBD(All.InputPara$InputPara_byRace)
  LocalGBD_List_byIncome <- List_LocalGBD(All.InputPara$InputPara_byIncome)
  
  #compute health outcomes
  HealthOutcome_byRace.2020 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.2020,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.2020) <- raceGroupNames
  
  HealthOutcome_byIncome.2020 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.2020,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.2020) <- incomeGroupNames
  
  HealthOutcome_byRace.2036 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.2036,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.2036) <- raceGroupNames
  
  HealthOutcome_byIncome.2036 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.2036,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.2036) <- incomeGroupNames
  
  HealthOutcome_byRace.2027 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.2027,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.2027) <- raceGroupNames
  
  HealthOutcome_byIncome.2027 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.2027,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.2027) <- incomeGroupNames
  
  HealthOutcome_byRace.S1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.S1,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.S1) <- raceGroupNames
  
  HealthOutcome_byIncome.S1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.S1,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.S1) <- incomeGroupNames
  
  HealthOutcome_byRace.S2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.S2,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.S2) <- raceGroupNames
  
  HealthOutcome_byIncome.S2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.S2,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.S2) <- incomeGroupNames
  
  HealthOutcome_byRace.S3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.S3,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.S3) <- raceGroupNames
  
  HealthOutcome_byIncome.S3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.S3,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.S3) <- incomeGroupNames
  
  HealthOutcome_byRace.C1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.C1,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.C1) <- raceGroupNames
  
  HealthOutcome_byIncome.C1 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.C1,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.C1) <- incomeGroupNames
  
  HealthOutcome_byRace.C2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.C2,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.C2) <- raceGroupNames
  
  HealthOutcome_byIncome.C2 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.C2,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.C2) <- incomeGroupNames
  
  HealthOutcome_byRace.C3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byRace,ScenarioTotalExpo_byRace.C3,LocalGBD_List_byRace,SIMPLIFY = FALSE)
  names(HealthOutcome_byRace.C3) <- raceGroupNames
  
  HealthOutcome_byIncome.C3 <- 
    mapply(function(x,y,z) computeHealthOutcome(RR.PA,x,y,z),BaselineTotalExpo_byIncome,ScenarioTotalExpo_byIncome.C3,LocalGBD_List_byIncome,SIMPLIFY = FALSE)
  names(HealthOutcome_byIncome.C3) <- incomeGroupNames
  
  return(list(
    HealthOutcome_byRace.2020=HealthOutcome_byRace.2020,
    HealthOutcome_byIncome.2020=HealthOutcome_byIncome.2020,
    
    HealthOutcome_byRace.2036=HealthOutcome_byRace.2036,
    HealthOutcome_byIncome.2036=HealthOutcome_byIncome.2036,
    
    HealthOutcome_byRace.2036=HealthOutcome_byRace.2036,
    HealthOutcome_byIncome.2036=HealthOutcome_byIncome.2036,
    
    HealthOutcome_byRace.2027=HealthOutcome_byRace.2027,
    HealthOutcome_byIncome.2027=HealthOutcome_byIncome.2027,
    
    HealthOutcome_byRace.S1=HealthOutcome_byRace.S1,
    HealthOutcome_byIncome.S1=HealthOutcome_byIncome.S1,
    
    HealthOutcome_byRace.S2=HealthOutcome_byRace.S2,
    HealthOutcome_byIncome.S2=HealthOutcome_byIncome.S2,
    
    HealthOutcome_byRace.S3=HealthOutcome_byRace.S3,
    HealthOutcome_byIncome.S3=HealthOutcome_byIncome.S3,
    
    HealthOutcome_byRace.C1=HealthOutcome_byRace.C1,
    HealthOutcome_byIncome.C1=HealthOutcome_byIncome.C1,
    
    HealthOutcome_byRace.C2=HealthOutcome_byRace.C2,
    HealthOutcome_byIncome.C2=HealthOutcome_byIncome.C2,
    
    HealthOutcome_byRace.C3=HealthOutcome_byRace.C3,
    HealthOutcome_byIncome.C3=HealthOutcome_byIncome.C3
    
    
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
    
    temp.race.2020 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.2020)
    temp.income.2020 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.2020)
    temp.race.2036 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.2036)
    temp.income.2036 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.2036)
    temp.race.2027 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.2027)
    temp.income.2027 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.2027)
    temp.race.S1 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.S1)
    temp.income.S1 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.S1)
    temp.race.S2 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.S2)
    temp.income.S2 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.S2)
    temp.race.S3 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.S3)
    temp.income.S3 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.S3)
    temp.race.C1 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.C1)
    temp.income.C1 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.C1)
    temp.race.C2 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.C2)
    temp.income.C2 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.C2)
    temp.race.C3 <- computeAgeStdOutput(All.InputPara$InputPara_byRace,HealthOutcome$HealthOutcome_byRace.C3)
    temp.income.C3 <- computeAgeStdOutput(All.InputPara$InputPara_byIncome,HealthOutcome$HealthOutcome_byIncome.C3)
    
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

# shape the outcomes for ggplot
# race: demogrID = 1; income: demogrID=2
DFforFigure <- function(OutcomeMatrix.list,demogrID,countyID,barID){
  #test
  # demogrID = 1
  # countyID = 1
  # dbID = 1
  # barID=3
  # OutcomeMatrix.list <- RawReductionOutcome[c((demogrID*18+dbID*9-26):(demogrID*18+dbID*9-18))]
  
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
  
  OutcomeMatrix <- rbind(OutcomeMatrix.Scenario.1[countyID,],OutcomeMatrix.Scenario.2[countyID,],OutcomeMatrix.Scenario.3[countyID,])
  
  #county names
  #county <- rep(rownames(OutcomeMatrix),each=4)
  #race group names
  raceGroup <- rep(c("1.White",'2.Black','3.Hisp','4.Other'),3)
  #income group names
  incomeGroup <- rep(incomeGroupNames,3)
  
  if (demogrID==1) {
    demogrGroup = raceGroup
    #shape the outcome as data.frame
    outcome <- outcome.update<-as.data.frame(matrix(t(OutcomeMatrix),nDemoClass*nrow(OutcomeMatrix),1))
    for (i in 1:3){
      outcome.update[4*i-1,1]<-outcome[4*i,1]
      outcome.update[4*i,1]<-outcome[4*i-1,1]
    }
    
    df <- data.frame(Scenario=scenario.name,DemogrGroup=demogrGroup,v =(-outcome.update))
    
  }else{
    demogrGroup = incomeGroup
    #shape the outcome as data.frame
    outcome <- as.data.frame(matrix(t(OutcomeMatrix),nDemoClass*nrow(OutcomeMatrix),1))
    df <- data.frame(Scenario=scenario.name,DemogrGroup=demogrGroup,v =(-outcome))
    
  }
  
  return(df=df)
}

# data frame for region-wide results
DFforRegionWide <- function(ReductionOutcome,demogrID,dbID,barID){
  # TEST
  #demogrID = 1
  #dbID = 1
  #barID=1
  #ReductionOutcome <- RawReductionOutcome
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
    OutcomeMatrix.walk.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyRace.2020[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.2027[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyRace.2036[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyRace.2020[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.2027[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyRace.2036[c(countyID),c(3,5,7,9)])
    
    OutcomeMatrix.walk.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyIncome.2020[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.2027[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyIncome.2036[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyIncome.2020[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.2027[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyIncome.2036[c(countyID),c(3,5,7,9)])
    
    scenario.name <- rep(c('2012','2020','2027','2036'),each=4)
  }else if (barID==2){
    
    OutcomeMatrix.walk.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyRace.S1[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.S2[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyRace.S3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyRace.S1[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.S2[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyRace.S3[c(countyID),c(3,5,7,9)])
    
    OutcomeMatrix.walk.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyIncome.S1[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.S2[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyIncome.S3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyIncome.S1[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.S2[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyIncome.S3[c(countyID),c(3,5,7,9)])
    
    
    scenario.name <- rep(c('2012','S1','S2','S3'),each=4)
  }else if (barID==3){
    
    OutcomeMatrix.walk.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyRace.C1[c(countyID),c(2,4,6,8)],
                                       AT_Pop_MeanTimebyRace.C2[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyRace.C3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byRace <- rbind(AT_Pop_MeanTimebyRace.baseline[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyRace.C1[c(countyID),c(3,5,7,9)],
                                        AT_Pop_MeanTimebyRace.C2[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyRace.C3[c(countyID),c(3,5,7,9)])
    
    OutcomeMatrix.walk.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyIncome.C1[c(countyID),c(2,4,6,8)],
                                         AT_Pop_MeanTimebyIncome.C2[c(countyID),c(2,4,6,8)],AT_Pop_MeanTimebyIncome.C3[c(countyID),c(2,4,6,8)])
    OutcomeMatrix.cycle.byIncome <- rbind(AT_Pop_MeanTimebyIncome.baseline[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyIncome.C1[c(countyID),c(3,5,7,9)],
                                          AT_Pop_MeanTimebyIncome.C2[c(countyID),c(3,5,7,9)],AT_Pop_MeanTimebyIncome.C3[c(countyID),c(3,5,7,9)])
    
    
    scenario.name <- rep(c('2012','C1','C2','C3'),each=4)
  }
  
  #race group names
  raceGroup <- rep(c("1.White",'2.Black','3.Hisp','4.Other'),4)
  #income group names
  incomeGroup <- rep(incomeGroupNames,4)
  
  if (demogrID==1) {
    demogrGroup = raceGroup
    #shape the outcome as data.frame
    outcome.walk <- outcome.update.walk<-as.data.frame(matrix(t(OutcomeMatrix.walk.byRace),nDemoClass*nrow(OutcomeMatrix.walk.byRace),1))
    outcome.cycle <- outcome.update.cycle<-as.data.frame(matrix(t(OutcomeMatrix.cycle.byRace),nDemoClass*nrow(OutcomeMatrix.cycle.byRace),1))
    
    for (i in 1:3){
      outcome.update.walk[4*i-1,1]<-outcome.walk[4*i,1]
      outcome.update.walk[4*i,1]<-outcome.walk[4*i-1,1]
      
      outcome.update.cycle[4*i-1,1]<-outcome.cycle[4*i,1]
      outcome.update.cycle[4*i,1]<-outcome.cycle[4*i-1,1]
    }
    
    df.walk <- data.frame(Scenario=scenario.name,DemogrGroup=demogrGroup,Mode = 'walk',v1 =(outcome.update.walk))
    df.cycle <- data.frame(Scenario=scenario.name,DemogrGroup=demogrGroup,Mode = 'cycle',v1 =(outcome.update.cycle))
    df.at <- rbind(df.walk,df.cycle)
    
    
  }else{
    demogrGroup = incomeGroup
    #shape the outcome as data.frame
    outcome.walk <- as.data.frame(matrix(t(OutcomeMatrix.walk.byIncome),nDemoClass*nrow(OutcomeMatrix.walk.byIncome),1))
    outcome.cycle <- as.data.frame(matrix(t(OutcomeMatrix.cycle.byIncome),nDemoClass*nrow(OutcomeMatrix.cycle.byIncome),1))
    
    df.walk <- data.frame(Scenario=scenario.name,DemogrGroup=demogrGroup,Mode = 'walk',v1 =(outcome.walk))
    df.cycle <- data.frame(Scenario=scenario.name,DemogrGroup=demogrGroup,Mode = 'cycle',v1 =(outcome.cycle))
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
  # #test
  # demogrID = 1
  # countyID = 1
  # dbID = 1
  # barID=1
  # df.result <- df
  
  if(typeID==1){
    
    if (countyID%in%c(1:6)){
      df.result <- DFforFigure(RawReductionOutcome[c((demogrID*18+dbID*9-26):(demogrID*18+dbID*9-18))],demogrID,countyID,barID)
      plot.title <- paste0(countyNames[countyID],': Reduction in Total ',dbNames[dbID],' from Physical Activity Module')
      ggplot(data = df.result, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Health Burden Reduction')+
        geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        ggtitle(plot.title)
    }else if(countyID==7){
      df.result <- DFforRegionWide(RawReductionOutcome,demogrID = demogrID,dbID = dbID,barID = barID)
      plot.title <- paste0('Region-Wide',': Reduction in Total ',dbNames[dbID],' from Physical Activity Module')
      ggplot(data = df.result, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Health Burden Reduction')+
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_wrap(~county)+ggtitle(plot.title)
    }
    
    
    
  }else if (typeID ==2) {
    
    if (countyID%in%c(1:6)){
      df.result <- DFforFigure(AgeStdReductionOutcome[c((demogrID*18+dbID*9-26):(demogrID*18+dbID*9-18))],demogrID,countyID,barID)
      plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total ',dbNames[dbID],' from Physical Activity Module')
      ggplot(data = df.result, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Health Burden Reduction Rate (per 100,000 population)')+
        geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        ggtitle(plot.title)
    }else if(countyID==7){
      df.result <- DFforRegionWide(AgeStdReductionOutcome,demogrID = demogrID,dbID = dbID,barID = barID)
      plot.title <- paste0('Region-Wide',': Age-Standardized Reduction in Total ',dbNames[dbID],' from Physical Activity Module')
      ggplot(data = df.result, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Health Burden Reduction Rate (per 100,000 population)')+
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_wrap(~county)+ggtitle(plot.title)
    }
    
    
  }else{
    #plot for physical activity data
    if (countyID%in%c(1:6)){
      df.at <- DFforPhysicalActivity(barID,countyID,demogrID)
      plot.title <- paste0(countyNames[countyID],': Active Travel Time')
      ggplot(data = df.at, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Active Travel Time (mins per week per capita)')+
        geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_grid(Mode~.,scales = "free") +ggtitle(plot.title)
    }else if (countyID==7){
      
      df.region <- NULL
      
      for (i in 1:6){
        df.temp <- DFforPhysicalActivity(barID,i,demogrID)
        df.region <- rbind(df.region,df.temp)
      }
      
      df.region$county <- rep(countyNames,each = 32)
      
      ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Active Travel Time (mins per week per capita)')+
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_grid(Mode~county,scales = "free") +ggtitle("Region-Wide: Active Travel Time")
      
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

###################### Input Parameter ##############################

# Number of age & demographic categories
nAgeClass <- 8L
nRaceClass <- nIncomeClass <- nDemoClass <- 4L

# paramter of Physical Activity Risk Function (power)
k<-0.5

# disease names
#diseaseNames <- c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes")

# group names for race/ethnicity and income 
raceGroupNames <- c("1.NHW","2.NHB","3.NHO","4.HO")
incomeGroupNames <- c("Quant1","Quant2","Quant3","Quant4")

# disease burden
dbNames <- c('Deaths','DALYs')

# county names
countyNames <- c("El Dorado","Placer","Sacramento","Sutter","Yolo","Yuba")

# population input
Pop_Input_US <- read.csv("01_Population/01_Population_US_EA.csv")
gbd_Input_US <- read.csv("04_GBD/01_GBD_US_AllCause.csv")

Pop.file.race <- read.csv("01_Population/02_Population_byRace_2012.csv")
Pop.file.income <- read.csv("01_Population/03_Population_byIncome_2012.csv")

# input the .csv files of active travel data
AT.file.baseline.byRace <- read.csv("02_ActiveTransport/01_ActiveTransport_Baseline_EA/01_ActiveTransport_byRace.2012.csv")
AT.file.baseline.byIncome <- read.csv("02_ActiveTransport/01_ActiveTransport_Baseline_EA/02_ActiveTransport_byIncome.2012.csv")
AT_Pop_MeanTimebyRace.baseline <- read.csv("02_ActiveTransport/01_ActiveTransport_Baseline_EA/03_PopulationMeanATTimebyRace.2012.csv")
AT_Pop_MeanTimebyIncome.baseline <- read.csv("02_ActiveTransport/01_ActiveTransport_Baseline_EA/04_PopulationMeanATTimebyIncome.2012.csv")

AT.file.2020.byRace <- read.csv("02_ActiveTransport/02_ActiveTransport_2020_EA/01_ActiveTransport_byRace.2020.csv")
AT.file.2020.byIncome <- read.csv("02_ActiveTransport/02_ActiveTransport_2020_EA/02_ActiveTransport_byIncome.2020.csv")
AT_Pop_MeanTimebyRace.2020 <- read.csv("02_ActiveTransport/02_ActiveTransport_2020_EA/03_PopulationMeanATTimebyRace.2020.csv")
AT_Pop_MeanTimebyIncome.2020 <- read.csv("02_ActiveTransport/02_ActiveTransport_2020_EA/04_PopulationMeanATTimebyIncome.2020.csv")

AT.file.2036.byRace <- read.csv("02_ActiveTransport/03_ActiveTransport_2036_EA/01_ActiveTransport_byRace.2036.csv")
AT.file.2036.byIncome <- read.csv("02_ActiveTransport/03_ActiveTransport_2036_EA/02_ActiveTransport_byIncome.2036.csv")
AT_Pop_MeanTimebyRace.2036 <- read.csv("02_ActiveTransport/03_ActiveTransport_2036_EA/03_PopulationMeanATTimebyRace.2036.csv")
AT_Pop_MeanTimebyIncome.2036 <- read.csv("02_ActiveTransport/03_ActiveTransport_2036_EA/04_PopulationMeanATTimebyIncome.2036.csv")

AT.file.2027.byRace <- read.csv("02_ActiveTransport/04_ActiveTransport_2027_EA/01_ActiveTransport_byRace.2027.csv")
AT.file.2027.byIncome <- read.csv("02_ActiveTransport/04_ActiveTransport_2027_EA/02_ActiveTransport_byIncome.2027.csv")
AT_Pop_MeanTimebyRace.2027 <- read.csv("02_ActiveTransport/04_ActiveTransport_2027_EA/03_PopulationMeanATTimebyRace.2027.csv")
AT_Pop_MeanTimebyIncome.2027 <- read.csv("02_ActiveTransport/04_ActiveTransport_2027_EA/04_PopulationMeanATTimebyIncome.2027.csv")

AT.file.S1.byRace <- read.csv("02_ActiveTransport/05_ActiveTransport_S1_EA/01_ActiveTransport_byRace.S1.csv")
AT.file.S1.byIncome <- read.csv("02_ActiveTransport/05_ActiveTransport_S1_EA/02_ActiveTransport_byIncome.S1.csv")
AT_Pop_MeanTimebyRace.S1 <- read.csv("02_ActiveTransport/05_ActiveTransport_S1_EA/03_PopulationMeanATTimebyRace.S1.csv")
AT_Pop_MeanTimebyIncome.S1 <- read.csv("02_ActiveTransport/05_ActiveTransport_S1_EA/04_PopulationMeanATTimebyIncome.S1.csv")

AT.file.S2.byRace <- read.csv("02_ActiveTransport/06_ActiveTransport_S2_EA/01_ActiveTransport_byRace.S2.csv")
AT.file.S2.byIncome <- read.csv("02_ActiveTransport/06_ActiveTransport_S2_EA/02_ActiveTransport_byIncome.S2.csv")
AT_Pop_MeanTimebyRace.S2 <- read.csv("02_ActiveTransport/06_ActiveTransport_S2_EA/03_PopulationMeanATTimebyRace.S2.csv")
AT_Pop_MeanTimebyIncome.S2 <- read.csv("02_ActiveTransport/06_ActiveTransport_S2_EA/04_PopulationMeanATTimebyIncome.S2.csv")

AT.file.S3.byRace <- read.csv("02_ActiveTransport/07_ActiveTransport_S3_EA/01_ActiveTransport_byRace.S3.csv")
AT.file.S3.byIncome <- read.csv("02_ActiveTransport/07_ActiveTransport_S3_EA/02_ActiveTransport_byIncome.S3.csv")
AT_Pop_MeanTimebyRace.S3 <- read.csv("02_ActiveTransport/07_ActiveTransport_S3_EA/03_PopulationMeanATTimebyRace.S3.csv")
AT_Pop_MeanTimebyIncome.S3 <- read.csv("02_ActiveTransport/07_ActiveTransport_S3_EA/04_PopulationMeanATTimebyIncome.S3.csv")

AT.file.C1.byRace <- read.csv("02_ActiveTransport/08_ActiveTransport_C1_EA/01_ActiveTransport_byRace.C1.csv")
AT.file.C1.byIncome <- read.csv("02_ActiveTransport/08_ActiveTransport_C1_EA/02_ActiveTransport_byIncome.C1.csv")
AT_Pop_MeanTimebyRace.C1 <- read.csv("02_ActiveTransport/08_ActiveTransport_C1_EA/03_PopulationMeanATTimebyRace.C1.csv")
AT_Pop_MeanTimebyIncome.C1 <- read.csv("02_ActiveTransport/08_ActiveTransport_C1_EA/04_PopulationMeanATTimebyIncome.C1.csv")

AT.file.C2.byRace <- read.csv("02_ActiveTransport/09_ActiveTransport_C2_EA/01_ActiveTransport_byRace.C2.csv")
AT.file.C2.byIncome <- read.csv("02_ActiveTransport/09_ActiveTransport_C2_EA/02_ActiveTransport_byIncome.C2.csv")
AT_Pop_MeanTimebyRace.C2 <- read.csv("02_ActiveTransport/09_ActiveTransport_C2_EA/03_PopulationMeanATTimebyRace.C2.csv")
AT_Pop_MeanTimebyIncome.C2 <- read.csv("02_ActiveTransport/09_ActiveTransport_C2_EA/04_PopulationMeanATTimebyIncome.C2.csv")

AT.file.C3.byRace <- read.csv("02_ActiveTransport/10_ActiveTransport_C3_EA/01_ActiveTransport_byRace.C3.csv")
AT.file.C3.byIncome <- read.csv("02_ActiveTransport/10_ActiveTransport_C3_EA/02_ActiveTransport_byIncome.C3.csv")
AT_Pop_MeanTimebyRace.C3 <- read.csv("02_ActiveTransport/10_ActiveTransport_C3_EA/03_PopulationMeanATTimebyRace.C3.csv")
AT_Pop_MeanTimebyIncome.C3 <- read.csv("02_ActiveTransport/10_ActiveTransport_C3_EA/04_PopulationMeanATTimebyIncome.C3.csv")

AT_Pop_MeanTimebyRace <- read.csv("02_ActiveTransport/test/PopulationMeanATTimebyRace.csv")
#AT_Pop_MeanTimebyIncome <- read.csv("02_ActiveTransport/test/PopulationMeanATTimebyIncome.csv")

# input the matrix of Non-travel METs 
# source: CHIS2005 (Per capita weekly non-travel related physical activity expressed as metabolic equivalent tasks (kcal/kg body weight/hr of activity))
nonTravelMET_Input_byRace <- read.csv("03_nonTravelMET/01_nonTravelMET_byRace.csv")
nonTravelMET_Input_byIncome <- read.csv("03_nonTravelMET/02_nonTravelMET_byIncome.csv")

###################### Calculation ##############################

# #output as .csv file
# write.csv(cbind(write.csv.func(HealthOutcome_byRace.2020),c('NHW',rep('',18),'NHB',rep('',18),'NHO',rep('',18),'HO',rep('',18))),
#           file = '00_HealthOutcome/YUB.healthoutcome.byRace.2020.csv')
# write.csv(cbind(write.csv.func(HealthOutcome_byRace.2036),c('NHW',rep('',18),'NHB',rep('',18),'NHO',rep('',18),'HO',rep('',18))),
#           file = '00_HealthOutcome/YUB.healthoutcome.byRace.2036.csv')
# write.csv(cbind(write.csv.func(HealthOutcome_byIncome.2020),c('catQ1',rep('',18),'catQ2',rep('',18),'catQ3',rep('',18),'catQ4',rep('',18))),
#           file = '00_HealthOutcome/YUB.healthoutcome.byIncome.2020.csv')
# write.csv(cbind(write.csv.func(HealthOutcome_byIncome.2036),c('catQ1',rep('',18),'catQ2',rep('',18),'catQ3',rep('',18),'catQ4',rep('',18))),
#           file = '00_HealthOutcome/YUB.healthoutcome.byIncome.2036.csv')

# countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
# compute the raw total reduction of health burdens
RawReductionOutcome <- Reduction.output(c(1:6))
# compute the age.std reduction of health burdens
AgeStdReductionOutcome <- AgeStdHealthOutcome(c(1:6))




###################### ITHIM application for Equity Analysis - Injury Module ################
############################ Two Race Categories Version ######################
library(ggplot2)

#set your working directory
#setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/06_Equity Analysis")

# Prevent scientific notation
options(scipen = 100)

# number of traffic modes, injuried types, and road types
nTrafficModeV <- 6L #victim mode
nTrafficModeS <- 7L #striking mode (include one-party)
nInjuriedType <- 2L #fatal & serious
ModeNames <- c("bike","walk","motorcycle","car","truck","bus") #traffic mode
RoadTypes <- c("local","arterial","highway") # road type
nRoadType <- length(RoadTypes)

# disease burden
dbNames <- c('Deaths','DALYs')

# county names
countyNames <- c("El Dorado","Placer","Sacramento","Sutter","Yolo","Yuba")

# input the vehicle distance data
PersonVehicleDist.C1 <- read.csv('06_PersonVehicleDistance/07_PersonVehicleDistance_Custom1.csv')
PersonVehicleDist.C2 <- read.csv('06_PersonVehicleDistance/08_PersonVehicleDistance_Custom2.csv')
PersonVehicleDist.C3 <- read.csv('06_PersonVehicleDistance/09_PersonVehicleDistance_Custom3.csv')

# input the population data for each race group
Pop.file.race <- read.csv("01_Population/02_Population_byRace_2012.csv")
Pop.file.twoRaces <- cbind(Pop.file.race[,c(2,3)],rowSums(Pop.file.race[,c(4,6,8)]),rowSums(Pop.file.race[,c(5,7,9)]))
colnames(Pop.file.twoRaces) <- c('male.white','female.white','male.other','female.other')

# input the US population
US.pop <- read.csv("01_Population/01_Population_US_EA.csv")
US.pop <- matrix(cbind(US.pop[,2],US.pop[,3]),16,1)

# input the GBD data
GBD.injury <- read.csv("04_GBD/14_GBD_US_TrafficInjury.csv")

#distribution of age and gender group
#age.gender.dist.injury <- matrix(1/16,53,4)
#colnames(age.gender.dist.injury) <- c('male.white','female.white','male.other','female.other')

#### function for reading csv files of injury data and VMT
# data source: SWITRS 2006-2016 (Statewide Integrated Traffic Reporting System)
# data source: Transportation Injury Mapping System (TIMS by US Berkeley)
# 11-year annual average number of road traffic injuries
input.csv <- function(countyID,scenarioID){
  #scenarioID: 0-2012,1-2020,2-2036,3-2027,4-S1,5-S2,6-S3,7-C1,8-C2,9-C3
  #countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
  
  #test
  #countyID = 2
  #scenarioID = 7
  #pop.file.names <- list.files(path = "01_Population")
  
  #filenames.injury <- readLines("05_baseline injury/01_filenames_twoRaces.txt")
  #temp.Other <- read.csv(filenames.injury[2*countyID]) 
  filenames.injury <- list.files(path = '05_baseline injury')[1:6]
  
  # build a list to store the baseline injury data sets for all races
  injury.list <- rep(list(matrix(NA,36,7)),2)
  temp <- read.csv(paste0('05_baseline injury/',filenames.injury[countyID])) 
  injury.list[[1]] <- temp[1:36,2:8] # non hispanic white
  injury.list[[2]] <- temp[38:73,2:8] # other three races
  
  names(injury.list)<- c('NHW','Others')
  
  # input the person & vehicle distance matrix
  # include the distance distribution for three road types and occupancy for each traffic mode
  # filenames.vmt <- readLines("06_PersonVehicleDistance/by county/01_filenames_twoRaces.txt")
  # 
  # person.vehicle.distance_input <- read.csv(filenames.vmt[countyID])
  # person.vehicle.distance_input.list <- rep(list(matrix(NA,34,1)),2)
  # for (i in 1:2){
  #   person.vehicle.distance_input.list[[i]]<-person.vehicle.distance_input[1:34,(6*i-3):(6*i+1)]
  # }
  # names(person.vehicle.distance_input.list)<- c('NHW','Others')
  
  person.vehicle.distance_input.matrix <- matrix(NA,34,2)
  if (scenarioID<=6){
    filenames.dist <- list.files(path='06_PersonVehicleDistance')
    person.vehicle.distance_input <- read.csv(paste0('06_PersonVehicleDistance/',filenames.dist[scenarioID+1]))
  }else if (scenarioID==7){
    person.vehicle.distance_input <- PersonVehicleDist.C1
  }else if (scenarioID==8){
    person.vehicle.distance_input <- PersonVehicleDist.C2
  }else if(scenarioID==9){
    person.vehicle.distance_input <- PersonVehicleDist.C3
  }
  
  
  colnames(person.vehicle.distance_input.matrix)<- c('NHW','Others')
  
  #NHW
  person.vehicle.distance_input.matrix[,1]<-person.vehicle.distance_input[1:34,(4*countyID-1)]
  #Other three races
  person.vehicle.distance_input.matrix[,2]<-person.vehicle.distance_input[1:34,(4*countyID+1)]
  
  #GBD
  GBD.local.white <-GBD.injury[,2:5]*matrix(cbind(Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),1],Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),2]),16,1)/US.pop
  GBD.local.Other <-GBD.injury[,2:5]*matrix(cbind(Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),3],Pop.file.twoRaces[(9*countyID-8):(9*countyID-1),4]),16,1)/US.pop
  
  return(list(
    injury.list=injury.list,
    person.vehicle.distance_input.matrix=person.vehicle.distance_input.matrix,
    GBD.local.white = GBD.local.white,
    GBD.local.Other = GBD.local.Other
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
createScenarioInjuryMultiplier <- function(countyID,scenarioID){
  
  #test
  #person.vehicle.distance_input = person.vehicle.distance_input.list$NHW
  #countyID = 1
  #scenarioID=2
  
  ### for baseline ###
  # person distance for all modes (miles per day)
  # source: CHTS2012 & NHTS2009
  t.person.distance.baseline <- input.csv(countyID,scenarioID = 0)
  t.person.distance.baseline.NHW <- t.person.distance.baseline$person.vehicle.distance_input.matrix[1:6,1]
  t.person.distance.baseline.Other <- t.person.distance.baseline$person.vehicle.distance_input.matrix[1:6,2]
  
  # matrix of distance (person) distribution by road types and modes for baseline (%)
  # data source: for walk and bike, hard coded estimate; for other modes, CHTS2012
  percent.person.baseline.NHW <- percent.person.baseline.Other <- matrix(NA,nrow=6,ncol=3,dimnames=list(ModeNames,c("local","arterial","highway")))
  percent.person.baseline.NHW[,1]<-t.person.distance.baseline$person.vehicle.distance_input.matrix[8:13,1]
  percent.person.baseline.NHW[,2]<-t.person.distance.baseline$person.vehicle.distance_input.matrix[15:20,1]
  percent.person.baseline.NHW[,3]<-t.person.distance.baseline$person.vehicle.distance_input.matrix[22:27,1]
  
  percent.person.baseline.Other[,1]<-t.person.distance.baseline$person.vehicle.distance_input.matrix[8:13,2]
  percent.person.baseline.Other[,2]<-t.person.distance.baseline$person.vehicle.distance_input.matrix[15:20,2]
  percent.person.baseline.Other[,3]<-t.person.distance.baseline$person.vehicle.distance_input.matrix[22:27,2]
  
  # matrix of distance (person) by road types and modes for baseline (miles per day)
  distance.baseline.person.NHW <- t.person.distance.baseline.NHW * percent.person.baseline.NHW
  distance.baseline.person.Other <- t.person.distance.baseline.Other * percent.person.baseline.Other
  
  # matrix of occupancy
  # source: hard coded estimate & CHTS2012
  occ.baseline.NHW <- t.person.distance.baseline$person.vehicle.distance_input.matrix[29:34,1]
  occ.baseline.Other <- t.person.distance.baseline$person.vehicle.distance_input.matrix[29:34,2]
  
  ### for scenario ###
  # person distance (person) for all road types and all modes (miles per day)
  # data source: MTC Travel Model One
  t.person.distance.scenario <-input.csv(countyID,scenarioID = scenarioID)
  
  t.person.distance.scenario.NHW <- t.person.distance.scenario$person.vehicle.distance_input.matrix[1:6,1]
  t.person.distance.scenario.Other <- t.person.distance.scenario$person.vehicle.distance_input.matrix[1:6,2]
  
  # matrix of distance (person) distribution by road types and modes for scenario (%)
  # data source: for walk and bike, hard coded estimate; for other modes, MTC Travel Model One
  percent.person.scenario.NHW <- percent.person.scenario.Other <- matrix(NA,nrow=6,ncol=3,dimnames=list(ModeNames,c("local","arterial","highway")))
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
  # source: hard coded estimate & MTC Travel Model One
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
  scenario.multiplier.NHW <- scenario.multiplier.Other <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames,c(ModeNames,'one.party'))))), nRoadType)
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
    scenario.multiplier.NHW=scenario.multiplier.NHW,
    scenario.multiplier.Other=scenario.multiplier.Other
  )
  )
  
}

##### Scenario Injuries #####
#compute the scenario injuries (the product of baseline injury and scenario multiplier)
computeScenarioInjury <- function(injury.baseline,scenario.multiplier){
  
  injury.scenario.NHW <- mapply(function(x,y) x*y,injury.baseline$NHW,c(scenario.multiplier$scenario.multiplier.NHW,scenario.multiplier$scenario.multiplier.NHW),SIMPLIFY=FALSE)
  injury.scenario.Other <- mapply(function(x,y) x*y,injury.baseline$Other,c(scenario.multiplier$scenario.multiplier.Other,scenario.multiplier$scenario.multiplier.Other),SIMPLIFY=FALSE)
  
  return(list(
    injury.scenario.NHW = injury.scenario.NHW,
    injury.scenario.Other = injury.scenario.Other
  ))
}

##### Injuries results #####

# createInjuryResults <- function(injury.baseline,injury.scenario){
#   injury.number.2020 <- injury.number.2036 <- rep(list((matrix(NA,nrow=nTrafficModeV,ncol=nTrafficModeS,dimnames=list(ModeNames,c(ModeNames,'one.party'))))), 4)
#   names(injury.number.2020) <- names(injury.number.2036) <- c("baseline fatalities","baseline injuries","scenario fatalities","scenario injuries")
#   
#   #computing the total injury number
#   injury.number.2020[[1]] <- injury.baseline[[1]]+injury.baseline[[2]]+injury.baseline[[3]]#baseline fatalities
#   injury.number.2020[[2]] <- injury.baseline[[4]]+injury.baseline[[5]]+injury.baseline[[6]]#baseline serious injuries
#   injury.number.2020[[3]] <- injury.scenario$injury.scenario.2020[[1]]+injury.scenario$injury.scenario.2020[[2]]+injury.scenario$injury.scenario.2020[[3]]#scenario fatalities
#   injury.number.2020[[4]] <- injury.scenario$injury.scenario.2020[[4]]+injury.scenario$injury.scenario.2020[[5]]+injury.scenario$injury.scenario.2020[[6]]#scenario serious injuries
#   
#   injury.number.2036[[1]] <- injury.baseline[[1]]+injury.baseline[[2]]+injury.baseline[[3]]#baseline fatalities
#   injury.number.2036[[2]] <- injury.baseline[[4]]+injury.baseline[[5]]+injury.baseline[[6]]#baseline serious injuries
#   injury.number.2036[[3]] <- injury.scenario$injury.scenario.2036[[1]]+injury.scenario$injury.scenario.2036[[2]]+injury.scenario$injury.scenario.2036[[3]]#scenario fatalities
#   injury.number.2036[[4]] <- injury.scenario$injury.scenario.2036[[4]]+injury.scenario$injury.scenario.2036[[5]]+injury.scenario$injury.scenario.2036[[6]]#scenario serious injuries
#   
#   
#   # replace the NA with 0
#   injury.number.2020 <- lapply(injury.number.2020,function(x) {
#     x[is.na(x)]=0 
#     return(x)})
#   injury.number.2036 <- lapply(injury.number.2036,function(x) {
#     x[is.na(x)]=0 
#     return(x)})
#   
#   # compute the RR
#   injury.RR.2020 <- injury.RR.2036 <- rep(list((matrix(NA,nrow=nTrafficModeV+1,ncol=3,dimnames=list(c(ModeNames[1:6],"total"),c("baseline","scenario","RR"))))), 2)
#   names(injury.RR.2020) <- names(injury.RR.2036) <- c("fatalities","serious injuries") 
#   for (i in 1:2) {
#     injury.RR.2020[[i]][1:6,1] <- rowSums(injury.number.2020[[i]])
#     injury.RR.2020[[i]][1:6,2] <- rowSums(injury.number.2020[[i+2]])
#     injury.RR.2020[[i]][7,1:2] <- colSums(injury.RR.2020[[i]][1:6,1:2])
#     injury.RR.2020[[i]][,3] <- injury.RR.2020[[i]][,2]/injury.RR.2020[[i]][,1]
#     #injury.RR.2020[[i]][,3] <- replace(injury.RR.2020[[i]][,3],is.na(injury.RR.2020[[i]][,3]),1)
#     
#     injury.RR.2036[[i]][1:6,1] <- rowSums(injury.number.2036[[i]])
#     injury.RR.2036[[i]][1:6,2] <- rowSums(injury.number.2036[[i+2]])
#     injury.RR.2036[[i]][7,1:2] <- colSums(injury.RR.2036[[i]][1:6,1:2])
#     injury.RR.2036[[i]][,3] <- injury.RR.2036[[i]][,2]/injury.RR.2036[[i]][,1]
#     #injury.RR.2036[[i]][,3] <- replace(injury.RR.2036[[i]][,3],is.na(injury.RR.2036[[i]][,3]),1)
#     
#     
#   }
#   
#   return(list(
#     injury.number.2020=injury.number.2020,
#     injury.number.2036=injury.number.2036,
#     injury.RR.2020=injury.RR.2020,
#     injury.RR.2036=injury.RR.2036
#   ))
# }

createInjuryResults <- function(countyID,scenarioID){
  #test
  #countyID=2
  #scenarioID=1
  
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
  RR.fatality.NHW <- (total.injury.scenario.NHW[[1]]+total.injury.scenario.NHW[[2]]+total.injury.scenario.NHW[[3]])/(total.injury.baseline.NHW[[1]]+total.injury.baseline.NHW[[2]]+total.injury.baseline.NHW[[3]])
  RR.fatality.Other <-(total.injury.scenario.Other[[1]]+total.injury.scenario.Other[[2]]+total.injury.scenario.Other[[3]])/(total.injury.baseline.Other[[1]]+total.injury.baseline.Other[[2]]+total.injury.baseline.Other[[3]])
  
  # compute the relative risk of serious injuries for each race group
  RR.serious.NHW <-(total.injury.scenario.NHW[[4]]+total.injury.scenario.NHW[[5]]+total.injury.scenario.NHW[[6]])/(total.injury.baseline.NHW[[4]]+total.injury.baseline.NHW[[5]]+total.injury.baseline.NHW[[6]])
  RR.serious.Other <- (total.injury.scenario.Other[[4]]+total.injury.scenario.Other[[5]]+total.injury.scenario.Other[[6]])/(total.injury.baseline.Other[[4]]+total.injury.baseline.Other[[5]]+total.injury.baseline.Other[[6]])
  
  # Reduction.fatality.NHW <- (total.injury.baseline.NHW[[1]]+total.injury.baseline.NHW[[2]]+total.injury.baseline.NHW[[3]])-
  #   (total.injury.scenario.NHW[[1]]+total.injury.scenario.NHW[[2]]+total.injury.scenario.NHW[[3]])
  # 
  # Reduction.fatality.Other <- total.injury.baseline.Other[[1]]+total.injury.baseline.Other[[2]]+total.injury.baseline.Other[[3]]-
  #   (total.injury.scenario.Other[[1]]+total.injury.scenario.Other[[2]]+total.injury.scenario.Other[[3]])
  # 
  # Reduction.serious.NHW <- total.injury.baseline.NHW[[4]]+total.injury.baseline.NHW[[5]]+total.injury.baseline.NHW[[6]]-
  #   (total.injury.scenario.NHW[[4]]+total.injury.scenario.NHW[[5]]+total.injury.scenario.NHW[[6]])
  # 
  # Reduction.serious.Other <- total.injury.baseline.Other[[4]]+total.injury.baseline.Other[[5]]+total.injury.baseline.Other[[6]]-
  #   (total.injury.scenario.Other[[4]]+total.injury.scenario.Other[[5]]+total.injury.scenario.Other[[6]])
  
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
    # RR.fatality.NHW = RR.fatality.NHW,
    # RR.fatality.Other = RR.fatality.Other,
    # 
    # RR.serious.NHW =RR.serious.NHW,
    # RR.serious.Other = RR.serious.Other
    
    Reduction.Death.white=sum(Reduction.Death.white.disaggr),
    Reduction.Death.other=sum(Reduction.Death.other.disaggr),
    Reduction.DALYs.white=sum(Reduction.DALYs.white.disaggr),
    Reduction.DALYs.other=sum(Reduction.DALYs.other.disaggr),
    
    Reduction.Death.white.disaggr=Reduction.Death.white.disaggr,
    Reduction.Death.other.disaggr=Reduction.Death.other.disaggr,
    Reduction.DALYs.white.disaggr=Reduction.DALYs.white.disaggr,
    Reduction.DALYs.other.disaggr=Reduction.DALYs.other.disaggr
    
    
    
    # Reduction.fatality.NHW=Reduction.fatality.NHW,
    # Reduction.fatality.Other=Reduction.fatality.Other,
    # Reduction.serious.NHW=Reduction.serious.NHW,
    # Reduction.serious.Other=Reduction.serious.Other
  ))
}


#createInjuryResults(1,1)

# function for computing the age.std results
computeAgeStdOutput.injury <- function(scenario,countyID){
  #test
  #countyID = 2
  #scenario <- scenario.1
  
  #fatality.NHW.age.gender <- age.gender.dist.injury[((9*countyID-8):(9*countyID-1)),1:2]*scenario$Reduction.fatality.NHW/Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1:2]*100000
  #fatality.Other.age.gender <- age.gender.dist.injury[((9*countyID-8):(9*countyID-1)),3:4]*scenario$Reduction.fatality.Other/Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),3:4]*100000
  
  #serious.NHW.age.gender <- age.gender.dist.injury[((9*countyID-8):(9*countyID-1)),1:2]*scenario$Reduction.serious.NHW/Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1:2]*100000
  #serious.Other.age.gender <- age.gender.dist.injury[((9*countyID-8):(9*countyID-1)),3:4]*scenario$Reduction.serious.Other/Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),3:4]*100000
  
  fatality.NHW.age.gender <- scenario$Reduction.Death.white.disaggr/matrix(cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1],Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),2]),16,1)*100000
  fatality.Other.age.gender <- scenario$Reduction.Death.other.disaggr/matrix(cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),3],Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),4]),16,1)*100000
  
  DALYs.NHW.age.gender <- scenario$Reduction.DALYs.white.disaggr/matrix(cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1],Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),2]),16,1)*100000
  DALYs.Other.age.gender <- scenario$Reduction.DALYs.other.disaggr/matrix(cbind(Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),1],Pop.file.twoRaces[((9*countyID-8):(9*countyID-1)),2]),16,1)*100000
  
  age.std.fatality.NHW <- sum(fatality.NHW.age.gender * US.pop)/sum(US.pop)
  age.std.fatality.other <- sum(fatality.Other.age.gender * US.pop)/sum(US.pop)
  
  age.std.DALYs.NHW <- sum(DALYs.NHW.age.gender * US.pop)/sum(US.pop)
  age.std.DALYs.other <- sum(DALYs.Other.age.gender * US.pop)/sum(US.pop)
  
  return(list(
    age.std.fatality.NHW=age.std.fatality.NHW,
    age.std.fatality.other=age.std.fatality.other,
    age.std.DALYs.NHW =age.std.DALYs.NHW,
    age.std.DALYs.other=age.std.DALYs.other
  ))
  
}


# barID: 1-future years,2-Scenarios,3-customized
# typeID: 1-raw,2-age.std
DFforFigure.injury <- function(barID,countyID,typeID){
  #test
  #barID = 1
  #countyID = 2
  #typeID = 1
  
  reduction.fatality.value <- reduction.DALYs.value <-matrix(NA,6,1)
  
  if (barID == 1){#future years
    scenario.1 <- createInjuryResults(countyID = countyID,scenarioID = 1)#2020
    scenario.2 <- createInjuryResults(countyID = countyID,scenarioID = 3)#2027
    scenario.3 <- createInjuryResults(countyID = countyID,scenarioID = 2)#2036
    
    scenario.name  <- rep(c('2020','2027','2036'),each= 2)
    
  }else if (barID==2){#scenarios
    scenario.1 <- createInjuryResults(countyID = countyID,scenarioID = 4)
    scenario.2 <- createInjuryResults(countyID = countyID,scenarioID = 5)
    scenario.3 <- createInjuryResults(countyID = countyID,scenarioID = 6)
    
    scenario.name  <- rep(c('S1','S2','S3'),each= 2)
    
  }else if(barID==3){#customized
    scenario.1 <- createInjuryResults(countyID = countyID,scenarioID = 7)
    scenario.2 <- createInjuryResults(countyID = countyID,scenarioID = 8)
    scenario.3 <- createInjuryResults(countyID = countyID,scenarioID = 9)
    
    scenario.name  <- rep(c('C1','C2','C3'),each= 2)
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
  
  # reduction.fatality.value[1,1] <-scenario.1$Reduction.fatality.NHW
  # reduction.fatality.value[2,1] <-scenario.1$Reduction.fatality.Other
  # reduction.fatality.value[3,1] <-scenario.2$Reduction.fatality.NHW
  # reduction.fatality.value[4,1] <-scenario.2$Reduction.fatality.Other
  # reduction.fatality.value[5,1] <-scenario.3$Reduction.fatality.NHW
  # reduction.fatality.value[6,1] <-scenario.3$Reduction.fatality.Other
  # 
  # reduction.serious.value[1,1] <-scenario.1$Reduction.serious.NHW
  # reduction.serious.value[2,1] <-scenario.1$Reduction.serious.Other
  # reduction.serious.value[3,1] <-scenario.2$Reduction.serious.NHW
  # reduction.serious.value[4,1] <-scenario.2$Reduction.serious.Other
  # reduction.serious.value[5,1] <-scenario.3$Reduction.serious.NHW
  # reduction.serious.value[6,1] <-scenario.3$Reduction.serious.Other
  
  raceGroup <- rep(c("1.White",'2.Other'),3)
  
  # build the data frame
  df.fatality <- data.frame(Scenario=scenario.name,DemogrGroup=raceGroup,V1 =(reduction.fatality.value))
  df.DALYs <- data.frame(Scenario=scenario.name,DemogrGroup=raceGroup,V1 =(reduction.DALYs.value))
  
  return(list(
    df.fatality=df.fatality,
    df.DALYs=df.DALYs
  ))
}
# 
# output.result <- function(countyID){
#   # countyID: 1-ELD,2-PLA,3-SAC,4-SUT,5-YOL,6-YUB
#   #countyID=1
#   injury.list <- input.csv(countyID = countyID)[[1]]
#   person.vehicle.distance_input.list <- input.csv(countyID = countyID)[[2]]
#   
#   #obtain the ITHIM sheet "Baseline injuries"
#   injury.baseline.byRace <-  lapply(injury.list,function(x) createBaselineInjury(x))
#   
#   #compute the "Scenario Injury Multiplier"
#   scenario.multiplier.byRace <- lapply(person.vehicle.distance_input.list,function(x) createScenarioInjuryMultiplier(x))
#   
#   #compute the scenario injuries
#   injury.scenario.byRace <- mapply(function(x,y) computeScenarioInjury(x,y),injury.baseline.byRace,scenario.multiplier.byRace,SIMPLIFY = FALSE)
#   
#   #Summarize the injury results
#   injury.result.byRace <- mapply(function(x,y) createInjuryResults(x,y),injury.baseline.byRace,injury.scenario.byRace,SIMPLIFY = FALSE)
#   
#   #format the output file  
#   cutting.line.col <- matrix(c('2020 fatal','2020 serious','2036 fatal','2036 serious',rep('',24)),7,4,byrow = TRUE)
#   NHW <- cbind(injury.result.byRace[[1]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[1]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
#                injury.result.byRace[[1]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[1]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('NHW',rep('',6)),7,1))
#   Others <- cbind(injury.result.byRace[[2]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[2]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
#                injury.result.byRace[[2]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[2]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('NHB',rep('',6)),7,1))
#   #NHO <- cbind(injury.result.byRace[[3]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[3]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
#    #            injury.result.byRace[[3]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[3]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('NHO',rep('',6)),7,1))
#   #HO <-  cbind(injury.result.byRace[[4]]$injury.RR.2020$fatalities,cutting.line.col[,1],injury.result.byRace[[4]]$injury.RR.2020$`serious injuries`,cutting.line.col[,2],
#    #            injury.result.byRace[[4]]$injury.RR.2036$fatalities,cutting.line.col[,3],injury.result.byRace[[4]]$injury.RR.2036$`serious injuries`,cutting.line.col[,4],matrix(c('HO',rep('',6)),7,1))
#   
#   cutting.line.row <- matrix('',1,17)
#   output <- rbind(NHW,cutting.line.row,Others)
#   
#   return(output)
# }


# barID: 1-future years,2-Scenarios,3-customized
# yaxisID: 1-Death total; 2-Death age.std; 3-serious injury total; 4-serious injury age.std
# typeID : 1-raw,2-age.std
plot.shiny.app.injury <- function(countyID, barID, yaxisID){
  #test
  #countyID = 1
  #barID = 1
  #yaxisID = 1
  
  if (yaxisID == 1){ #death total
    
    
    if (countyID%in%c(1:6)){ # for county
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
      
      plot.title <- paste0(countyNames[countyID],': Reduction in Total Deaths from Traffic Injury Module')
      
      ggplot(data = df.result.injury$df.fatality, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
        geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        ggtitle(plot.title)
      
    }else if (countyID==7){ #for region wide
      
      df.region <- NULL
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
        
        df.region <- rbind(df.region,df.temp$df.fatality)
        
      }
      df.region$county <- rep(countyNames,each = 6)
      plot.title <- paste0('Region Wide: Reduction in Total Deaths from Traffic Injury Module')
      
      ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_wrap(~county)+ggtitle(plot.title)
      
      
    }
    
    
  }else if (yaxisID == 2){ # death age.std
    
    if (countyID %in% c(1:6)){ # for county
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
      
      plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total Deaths from Traffic Injury Module')
      
      ggplot(data = df.result.injury$df.fatality, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Death Reduction Rate (per 100,000 population)')+
        geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        ggtitle(plot.title)
      
    }else if (countyID==7){ # for region wide
      df.region <- NULL
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
        
        df.region <- rbind(df.region,df.temp$df.fatality)
        
      }
      df.region$county <- rep(countyNames,each = 6)
      plot.title <- paste0('Region Wide: Age-Standardized Reduction in Total Deaths from Traffic Injury Module')
      
      ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Death Reduction Rate (per 100,000 population)')+
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_wrap(~county)+ggtitle(plot.title)
    }
    
    
  }else if (yaxisID == 3){ # DALYs total
    
    if (countyID%in%c(1:6)){
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
      plot.title <- paste0(countyNames[countyID],': Reduction in Total DALYs from Traffic Injury Module')
      
      ggplot(data = df.result.injury$df.DALYs, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
        geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        ggtitle(plot.title)
      
    }else if (countyID ==7){
      df.region <- NULL
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
        
        df.region <- rbind(df.region,df.temp$df.DALYs)
        
      }
      df.region$county <- rep(countyNames,each = 6)
      plot.title <- paste0('Region Wide: Reduction in Total DALYs from Traffic Injury Module')
      
      ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_wrap(~county)+ggtitle(plot.title)
      
    }
    
    
  }else if (yaxisID ==4){ # DALYs age.std
    
    if (countyID%in%c(1:6)){
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
      
      plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total DALYs from Traffic Injury Module')
      
      ggplot(data = df.result.injury$df.DALYs, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('DALYs Reduction Rate (per 100,000 population)')+
        geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        ggtitle(plot.title)
    }else if (countyID==7){
      
      df.region <- NULL
      
      for (i in 1:6){
        df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
        
        df.region <- rbind(df.region,df.temp$df.DALYs)
        
      }
      df.region$county <- rep(countyNames,each = 6)
      plot.title <- paste0('Region Wide: Age-Standardized Reduction in Total DALYs from Traffic Injury Module')
      
      ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('DALYs Reduction Rate (per 100,000 population)')+
        #geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
        facet_wrap(~county)+ggtitle(plot.title)
      
    }
    
    
  }else{
    message('wrong input')
  }
}

# Parameter description
# countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
# barID: 1-future years,2-scenarios,3-customized
# outcomeID: 1-physical activity; 2-injury; 3-both
# demogrID: 1-Race/ethnicty; 2-household income
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data

#plot.shiny.app.PA(countyID = 1,dbID = 1,typeID = 1,demogrID = 1,barID = 1)
#plot.shiny.app.injury(countyID = 1,barID = 1,yaxisID = 6)

# function for combining two moduels (PA and injury)
integrated.shiny.app <- function(countyID,barID,outcomeID,demogrID,yaxisID){
  if (outcomeID == 1){ #PA
    if (yaxisID == 1){ # death total
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 1,demogrID = demogrID,barID = barID)
    }else if (yaxisID==2){ # death age.std
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 2,demogrID = demogrID,barID = barID)
    }else if (yaxisID==3){
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 1,demogrID = demogrID,barID = barID)
    }else if (yaxisID==4){
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 2,demogrID = demogrID,barID = barID)
    }else if (yaxisID==5){
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 3,demogrID = demogrID,barID = barID)
    }else{
      message('wrong input')
    }
    
  }else if (outcomeID==2){ #injury
    if (yaxisID%in%c(1:4)){
      plot.shiny.app.injury(countyID = countyID,barID = barID,yaxisID = yaxisID)
    }else{
      message('wrong input')
    }
    
    
  }else if (outcomeID==3){ # both
    if (countyID%in%(1:6)){
      
      value <- NULL

      if (yaxisID==1){ #total deaths
        #test
        #countyID=1
        #barID = 1
        
        df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a.physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- 'b.traffic injury'
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        plot.title <- paste0(countyNames[countyID],': Reduction in Total Deaths')
        
        # ggplot(data = df.result.integration,mapping = aes(x = factor(DemogrGroup), y = V1,color = factor(Scenario),shape = factor(type)))+
        #    geom_point(stat = 'identity',size=3,position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
        #    ggtitle(plot.title)
        # 
        # ggplot(data = df.result.integration,mapping = aes(x = factor(DemogrGroup), y = V1,color = factor(Scenario),shape = factor(type)))+
        #   geom_point(stat = 'identity',size=3,position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
        #   ggtitle(plot.title)
        
        # ggplot(data = df.result.integration,mapping = aes(x = factor(DemogrGroup), y = V1,color = factor(Scenario)))+
        #   geom_dotplot(binaxis = "y",position = 'dodge',binwidth = 0.1)+xlab('Demographic Group')+ylab('Total Death Reduction')+
        #   ggtitle(plot.title)
        
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',position = position_dodge(0.5),width = 0.5)+xlab('Demographic Group')+ylab('Total Death Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
        
      }else if (yaxisID==3){ #total DALYs
        #test
        #countyID=1
        #barID = 1
        
        df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a. physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.DALYs
        df.result.injury$type <- 'b. traffic injury'
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        plot.title <- paste0(countyNames[countyID],': Reduction in Total DALYs')
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
      }else if (yaxisID==2){ # age.std deaths
        #test
        #countyID=1
        #barID = 1
        
        df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a. physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- 'b. traffic injury'
        
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total Deaths')
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Death reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
        
      }else if (yaxisID==4){#age.std dalys
        
        df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a. physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.DALYs
        df.result.injury$type <- 'b. traffic injury'
        
        plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total DALYs')
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('DALYs reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
      }
    }else if(countyID==7){ #region wide
      #test
      #yaxisID=1
      #barID=1
      
      df.region <- NULL
      
      if (yaxisID==1){ #death total
        
        for (countyID in c(1:6)){
          
          value <- NULL
          
          df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
          df.result.injury <- df.result.injury$df.fatality
          df.result.injury$type <- 'b. traffic injury'
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          #df.result.integration.temp$type <- 'integration'
          
          #df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
          
          df.region <- rbind(df.region,df.result.integration.temp)
          
          
        }
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Reduction of Total Deaths ")
        
        #return(df.region = df.region)
      
      }else if (yaxisID==2){#death age.std
        
        for (countyID in c(1:6)){
          value <- NULL
          
          df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
  
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
          df.result.injury <- df.result.injury$df.fatality
          df.result.injury$type <- 'b. traffic injury'
          
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          #df.result.integration.temp$type <- 'integration'
          
          df.region <- rbind(df.region,df.result.integration.temp)
        }
        
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Death reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Age-Standardized Reduction in Total Deaths")
        
        #return(df.region = df.region)
        
      }else if (yaxisID==3){# total DALYs
        
        for(countyID in 1:6){
          
          df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
          
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
          df.result.injury <- df.result.injury$df.DALYs
          df.result.injury$type <- 'b. traffic injury'
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          #df.result.integration.temp$type <- 'integration'
          
          df.region <- rbind(df.region,df.result.integration.temp)
        }
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Reduction of Total DALYs ")
        
        #return(df.region = df.region)
        
      }else if (yaxisID==4){#age.std DALYs
        
        for (countyID in 1:6){
          df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
          
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
          df.result.injury <- df.result.injury$df.DALYs
          df.result.injury$type <- 'b. traffic injury'
          
          #plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total DALYs')
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          
          df.region <- rbind(df.region,df.result.integration.temp)
        }
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('DALYs reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Age-Standardized Reduction in Total DALYs")
        
        #return(df.region = df.region)
        
      }else{
        message('wrong input')
      }
    

    }
    
  }
}

# Parameter description
# countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
# barID: 1-future years,2-scenarios,3-customized
# outcomeID: 1-physical activity; 2-injury; 3-both
# demogrID: 1-Race/ethnicty; 2-household income
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data

aggr.outcome.shiny.app <- function(barID,yaxisID){
  
  #TEST
  #barID =1
  #yaxisID=1
  
  #demogr.name <- c('white','others')
  
  if(barID==1){
    scenario.name.sep <- c('2020','2027','2036')
    scenairo.name<-rep(scenario.name.sep,6)
  }else if(barID==2){
    scenario.name.sep <- c('S1','S2','S3')
    scenairo.name<-rep(scenario.name.sep,6)
  }
  
  if (yaxisID==1){#death total
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = RawReductionOutcome,demogrID = 1,dbID = 1,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
      
      #for (j in 1:3){
       # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.fatality
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demograhic Group')+ylab('Total Deaths Reduction')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Reduction in Total Deaths")
    
    
    
  }else if (yaxisID==2){#death age.std
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = AgeStdReductionOutcome,demogrID = 1,dbID = 1,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
      
      #for (j in 1:3){
      # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.fatality
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demograhic Group')+ylab('Deaths reduction rate (per 100,000 population)')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Age-Standardized Reduction in Total Deaths")
    
  }else if(yaxisID==3){# DALYs total
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = RawReductionOutcome,demogrID = 1,dbID = 2,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
      
      #for (j in 1:3){
      # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.DALYs
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Reduction in Total DALYs")
    
  }else if (yaxisID==4){#DALYs age.std
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = AgeStdReductionOutcome,demogrID = 1,dbID = 2,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
      
      #for (j in 1:3){
      # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.DALYs
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Groups')+ylab('DALYs reduction rate (per 100,000 population)')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Age-Standardized Reduction in Total DALYs")
  }else{
    message('wrong input')
  }
  
  
  
  }

# Parameter description
# countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
# barID: 1-future years,2-scenarios,3-customized
# outcomeID: 1-physical activity; 2-injury; 3-both
# demogrID: 1-Race/ethnicty; 2-household income
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
integrated.shiny.app(countyID = 2, barID = 3,outcomeID = 2,demogrID = 1, yaxisID =1)

# Parameter description
# barID: 1-future years,2-scenarios
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
aggr.outcome.shiny.app(barID = 1,yaxisID=1)


###################### ITHIM application for Equity Analysis - Web Interface - Shiny App - Server/UI ######################


#setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/06_Equity Analysis")

require(shiny)

ui <- fluidPage(
  titlePanel("ITHIM APP"),  
  navbarPage("ITHIM APP",
             
             # Pulls About page from Markdown File
             tabPanel("About",
                      fluidRow(
                        column(6, 
                               includeHTML("ITHIM_About.html")
                        )
                      )           
             ),
             # Creates simple aggregated plot
             tabPanel("Simple Aggregated Plots",
                      sidebarLayout(
                        # Creates sidebar with Radio buttons
                        sidebarPanel(
                          # Parameter description
                          # barID: 1-future years,2-scenarios
                          # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
                          radioButtons("selectbarID", label = h3("Select Scenario"), 
                                       choices = list("Future Years" = 1, "Scenarios" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID", label = h3("Select Units"), 
                                       choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                                                      "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4
                                       ), 
                                       selected = 1)
                        ),
                        mainPanel(
                          plotOutput("SimplePlot")
                        )
                      )
             ),
             
             tabPanel("Advanced Plots",
                      sidebarLayout(
                        sidebarPanel(
                          # Parameter description
                          # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
                          # barID: 1-future years,2-scenarios,3-customized
                          # outcomeID: 1-physical activity; 2-injury; 3-both
                          # demogrID: 1-Race/ethnicty; 2-household income
                          # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
                          radioButtons("selectCounty", label = h3("Select County"), 
                                       choices = list("El Dorado" = 1, "Placer" = 2, "Sacramento" = 3, "Sutter"= 4, "Yolo"= 5, "Yuba"= 6, "All"= 7), 
                                       selected = 1),
                          radioButtons("selectbarID", label = h3("Select Scenario"), 
                                       choices = list("Future Years" = 1, "Scenarios" = 2), 
                                       selected = 1),
                          radioButtons("selectoutcomeID", label = h3("Select Outcome"), 
                                       choices = list("Physical Activity" = 1, "Injury" = 2, "Both" = 3), 
                                       selected = 1),
                          radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                                       choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID", label = h3("Select Units"), 
                                       choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                                                      "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                                                      "Physical Activity Data" = 5), 
                                       selected = 1)
                          # sliderInput(inputId = "mwt",
                          #             label = "Mean Walking Time (min per week)",
                          #             value = 47.49, min = 20, max = 100),
                        ),
                        mainPanel(
                          plotOutput("AdvancedPlot")
                        )
                      )
             ),
             #Upload Panel from http://shiny.rstudio.com/gallery/upload-file.html
             # 01_Data/EQ/ActiveTransport/c1-c2-c3
             # 01_Data/EQ/PVD/c1-2-3
             
             tabPanel("Custom Scenarios", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('file1', 'Choose file to upload',
                                    multiple = TRUE,
                                    accept = c(
                                      'text/csv',
                                      'text/comma-separated-values',
                                      'text/tab-separated-values',
                                      'text/plain',
                                      '.csv',
                                      '.tsv'
                                    )
                          ),
                          tags$hr(),
                          p('Please use the following as a template for the data structure of your custom scenarios',
                            a(href = "CustomScenarioTemplate.zip", "CustomScenarioTemplate.zip")
                          ),
                          radioButtons("selectCounty", label = h3("Select County"), 
                                       choices = list("El Dorado" = 1, "Placer" = 2, "Sacramento" = 3, "Sutter"= 4, "Yolo"= 5, "Yuba"= 6, "All"= 7), 
                                       selected = 1),
                          radioButtons("selectbarID", label = h3("Select Scenario"), 
                                       choices = list("Future Years" = 1, "Scenarios" = 2, "Customized" = 3), 
                                       selected = 1),
                          radioButtons("selectoutcomeID", label = h3("Select Outcome"), 
                                       choices = list("Physical Activity" = 1, "Injury" = 2, "Both" = 3), 
                                       selected = 1),
                          radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                                       choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID", label = h3("Select Units"), 
                                       choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                                                      "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                                                      "Physical Activity Data" = 5), 
                                       selected = 1)
                        ),
                        mainPanel(
                          plotOutput("CustomizablePlot")
                        )
                        
                      )
             )
  )
)

server <- function(input, output) {
  
  # data <- reactive({
  #       (input$select)
  #     })
  
  output$CustomizablePlot <- renderPlot({
    
     inFile <- input$file1
    
     if (is.null(inFile))
       return(NULL)
  
    
    # Re - input the vehicle distance data with Custom Scenarios
    PersonVehicleDist.C1 <- read.csv(inFile$datapath)
    AT.file.C1.byRace <- read.csv(inFile$datapath)
    AT.file.C1.byIncome <- read.csv(inFile$datapath)
    AT_Pop_MeanTimebyRace.C1 <- read.csv(inFile$datapath)
    AT_Pop_MeanTimebyIncome.C1 <- read.csv(inFile$datapath)


    # Parameter description
    # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
    # barID: 1-future years,2-scenarios,3-customized
    # outcomeID: 1-physical activity; 2-injury; 3-both
    # demogrID: 1-Race/ethnicty; 2-household income
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
    integrated.shiny.app(countyID = as.integer(input$selectCounty), barID = as.integer(input$selectbarID),
                         outcomeID = as.integer(input$selectoutcomeID),demogrID = as.integer(input$selectdemogrID), 
                         yaxisID = as.integer(input$selectyaxisID))
  })
  
  output$AdvancedPlot <- renderPlot({
    
    # Parameter description
    # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
    # barID: 1-future years,2-scenarios,3-customized
    # outcomeID: 1-physical activity; 2-injury; 3-both
    # demogrID: 1-Race/ethnicty; 2-household income
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
    integrated.shiny.app(countyID = as.integer(input$selectCounty), barID = as.integer(input$selectbarID),
                         outcomeID = as.integer(input$selectoutcomeID),demogrID = as.integer(input$selectdemogrID), 
                         yaxisID = as.integer(input$selectyaxisID))
  })
  output$SimplePlot <- renderPlot({
    # Parameter description
    # barID: 1-future years,2-scenarios
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
    aggr.outcome.shiny.app(barID = as.integer(input$selectbarID),yaxisID = as.integer(input$selectyaxisID))
  })
  
}

shinyApp(ui = ui, server = server)
