############ ITHIM application for Sacramento County ##########

# function for creating total exposure matrix 
# after inputing the population Mean Walking/Cycling Time (min per week) and coefficient of variation (cv)

TotalExposure <- function(PopMeanWalkTime, PopMeanCycleTime,cv){
  # The population mean walking/cycling speed (mph)
  PopMeanWalkSpeed <- 3.0
  PopMeanCycleSpeed <- 12.0
  
  # Number of age categories
  nAgeclass<-8
  
  # the numerical matrix for the population proportion
  PopProp <- matrix(c(0.0347225,0.0332277,0.0708172,0.0677564,0.1104101,0.1078704,0.0977915,0.0988684,
                      0.1001054,0.1060743,0.041988,0.0472405,0.0224061,0.0272632,0.0126027,0.0208556),
                    byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
  
  # Numerical matrices for the relative walking time (relative to the value of "female 15-29") and the mean walking time 
  Rwt <- matrix(c(0.6918,0.7658,0.8026,0.5945,0.5787,1.0000,0.8794,0.7100,1.1115,1.2865,1.5270,0.8593,0.3089,0.7539,0.7601,0.1345),
                byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
  meanWalkTime <- Rwt/PopProp/sum(PopProp*Rwt)*PopMeanWalkTime*PopProp
  
  # Numerical matrices for the relative cycling time (relative to the value of "female 15-29") and the mean cycling time 
  Rct <- matrix(c(2.0208,1.7434,3.5787,2.3395,1.9431,1.0000,3.1866,2.7003,5.2019,0.6997,3.2352,0.2387,2.3141,0.5627,4.6880,0.0100),
                byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
  meanCycleTime <- Rct/PopProp/sum(PopProp*Rct)*PopMeanCycleTime*PopProp
  
  # Numerical matrices for the proportion of mean cycling time to total active transport time
  PropMeanCycleTime <- meanCycleTime/(meanWalkTime+meanCycleTime)
  
  # Numerical matrices for the relative walking speed (relative to the value of "female 15-29") and the mean walking speed
  Rws <- matrix(c(1.0663,0.8753,1.0663,0.8753,1.0206,1.0002,1.0590,1.0338,1.0392,0.9474,1.0302,0.9330,0.9510,0.8969,0.9510,0.8969),
                byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
  meanWalkSpeed <- Rws/PopProp/sum(PopProp*Rws)*PopMeanWalkSpeed*PopProp
  
  # Numerical matrices for the relative cycling speed (relative to the value of "female 15-29") and the mean cycling speed
  Rcs <- matrix(c(0.8474,0.8375,0.8920,0.9817,1.0721,0.9570,1.0735,0.9805,1.1565,0.9628,1.0631,0.9292,1.0293,0.8485,0.9023,0.8308),
                byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
  meanCycleSpeed <- Rcs/PopProp/sum(PopProp*Rcs)*PopMeanCycleSpeed*PopProp
  
  # Numerical matrices for the mean walking/cycling MET values
  meanWalkMET <- ifelse(1.2216*meanWalkSpeed + 0.0838 < 2.5, 2.5,  1.2216*meanWalkSpeed + 0.0838)
  meanCycleMET <- matrix(6, byrow=TRUE, ncol = 2, nrow = nAgeClass, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
  
  # Compute Quintiles of Active Transport Time #
  
  # Total Active Transport Time
  totalATTime <- meanWalkTime + meanCycleTime
  
  # Numerical matrices for the log-normal distribution (mean, sd, log mean, log sd)
  
  meanATtime <- c(totalATTime[,1],totalATTime[,2])
  sd <- meanATtime*cv
  logMean <- log(meanATtime/sqrt(1+(meanATtime*cv/meanATtime)^2))
  logSD <- sqrt(log(1+(meanATtime*cv/meanATtime)^2))
  
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
  
  # Matrix of Non-travel METs
  
  nonTravelMET <- matrix (c(0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,53.9500,31.6833,61.1000,56.7500,41.0000,55.7000,52.4000,
                            55.7500,57.0000,49.2000,46.5500,53.2000,58.2250,42.2667,44.8000,20.5000,41.0000,20.5000,33.8250,30.7500,8.7500,22.7500,6.2500,
                            4.3750,0.0000,10.0000,5.0000,5.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,35.8750,
                            45.1000,38.9500,30.7500,41.0000,41.0000,42.9000,41.0000,41.0000,41.0000,51.6000,43.8500,41.0000,41.0000,27.8000,25.5000,37.6250,
                            16.4000,21.0000,6.0000,9.3333,5.1250,0.0000,0.0000,1.2500,2.9167,0.0000,0.0000,0.0000,0.0000), 
                          byrow=TRUE, ncol = 5, nrow = nAgeClass*2,
                          dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2)))
  
  totalExposure <-ifelse(quintTotalTravelMET + nonTravelMET > 2.5, quintTotalTravelMET + nonTravelMET, 0.1)
  
  #return the matrix of total exposure
  return(list(
    totalExposure <- totalExposure
    )
    )
}


#Create the total exposure matrices by inputing parameters 
#(mean walking time, mean cycling time, and cv)
BaselineTotalExpo <-TotalExposure(32.4,5.8,1.9216)
ScenarioTotalExpo <-TotalExposure(87.2898,87.2898,1.7112)

#==========================The following part is under construction...==============================

#Create relative risks of physical activity  
diseaseNames <- c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes")
nAgeClass <- 8
  
RR.lit <- exposure <- rep(list((matrix(NA,nrow=nAgeClass,ncol=2,dimnames=list(paste0("agClass",1:nAgeClass),c("F","M"))))), length(diseaseNames))
  
names(RR.lit) <- names(exposure) <- diseaseNames
  
exposure[["BreastCancer"]][1:nAgeClass,"F"] <- 4.5
RR.lit[["BreastCancer"]][1:nAgeClass,"F"] <- 0.944
  
exposure[["BreastCancer"]][1:nAgeClass,"M"] <- 1
RR.lit[["BreastCancer"]][1:nAgeClass,"M"] <- 1
  
exposure[["ColonCancer"]][1:nAgeClass,"M"] <- 30.9
RR.lit[["ColonCancer"]][1:nAgeClass,"M"] <- 0.8
  
exposure[["ColonCancer"]][1:nAgeClass,"F"] <- 30.1
RR.lit[["ColonCancer"]][1:nAgeClass,"F"] <- 0.86
  
exposure[["CVD"]][1:nAgeClass,1:2] <- 7.5
RR.lit[["CVD"]][1:nAgeClass,1:2] <- 0.84
  
exposure[["Dementia"]][1:nAgeClass,1:2] <- 31.5
RR.lit[["Dementia"]][1:nAgeClass,1:2] <- 0.72
  
exposure[["Diabetes"]][1:nAgeClass,1:2] <- 10
RR.lit[["Diabetes"]][1:nAgeClass,1:2] <- 0.83
  
exposure[["Depression"]][1:3,1:2] <- 11.25
RR.lit[["Depression"]][1:3,1:2] <- 0.927945490148335
  
exposure[["Depression"]][4:nAgeClass,1:2] <- 11.25
RR.lit[["Depression"]][4:nAgeClass,1:2] <- 0.859615572255727
  
# exposure[["Stroke"]] <- exposure[["CVD"]]
# RR.lit[["Stroke"]] <- RR.lit[["CVD"]]
#   
# exposure[["HHD"]] <- exposure[["CVD"]]
# RR.lit[["HHD"]] <- RR.lit[["CVD"]]
  
reshapeRR <- function(RR, nQuantiles = 5){
  nAgeClass <- 8
  list( M = matrix(RR[,"M"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))),F = matrix(RR[,"F"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))))
}


k <- 0.5
RR.PA <- mapply(function(x,y,k) x^(1/y)^k, RR.lit, exposure, 0.5, SIMPLIFY=FALSE)
RR.PA <- lapply(RR.PA, reshapeRR, nQuantiles = 5)


#Compute RR for an exposure of x MET
MET2RR <- function(RR,MET){
  mapply(FUN = function(x, y) x^(y^0.5), RR, MET, SIMPLIFY = FALSE)
}

RR.Baseline <- lapply(RR.PA,MET2RR,BaselineTotalExpo)



