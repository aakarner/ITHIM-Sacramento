#####Understand the ITHIM package#####

#create a ITHIM objective
#It is a list which contains three elements: parameters, means, quintiles
createITHIM <- function(){
  return(
    list(
      parameters=parameters<-createParametersList(),
      means=means<-computeMeanMatrices(parameters),
      quintiles = quintiles <- getQuintiles(means, parameters)
      )
    )
}

#Define ITHIM model parameters
#Rwt:A numerical matrix for the walking time, relative to the value of "female 15-29"
#Rct:A numerical matrix for the cycling time, relative to the value of "female 15-29"
#Rws:A numerical matrix for the walking speed, relative to the value of "female 15-29"
#muwt:A numerical value for the mean walking time
#muws:A numerical value for the mean walking speed
#muct:A numerical value for the mean cycling time
#cv:A numerical value for the coefficient of variation for active transport time
#cvNonTravel: A numerical value for the coefficient of variation for leisure activity
#GBD:ratio of regional disease-specific mortality to national disease-specific mortality (GBD)
#muNonTravel:non-travel related physical activity means by age and sex 
#cvNonTravel:A numerical value for the coefficient of variation for leisure activity


createParametersList <- function(){
  
  nAgeClass <- 8
  
  #Sacramento Case
  Rwt <- matrix(c(0.6918,0.7658,0.8026,0.5945,0.5787,1.0000,0.8794,0.7100,1.1115,1.2865,1.5270,0.8593,0.3089,0.7539,0.7601,0.1345),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
  
  Rct <- matrix(c(2.0208,1.7434,3.5787,2.3395,1.9431,1.0000,3.1866,2.7003,5.2019,0.6997,3.2352,0.2387,2.3141,0.5627,4.6880,0.0100),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
  
  Rws <- matrix(c(0.5803,1.1543,1.0108,0.7786,0.9484,1.0000,0.9641,0.7758,0.8002,0.7350,0.6790,0.6474,0.6585,0.5325,0.4675,0.7308),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
  
  muNonTravelMatrix <- matrix(c(0.0000,0.0000,0.0000,0.0000,1.7301,1.0000,1.8052,1.0017,1.5407,1.0068,0.6156,0.2400,0.0500,0.0133,0.0089,0.0022), byrow=TRUE,ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
  
  meanType <- "referent"
  n <- 100
  quantiles <- seq(1/n, (n-1)/n, by = 1/n)
  GBDFile <- "gbd.csv"
  GBD <- readGBD(file = GBDFile)
  muwt <- 49.1543 # min per week
  muws <- 2.4608 # mph
  muct <- 15.0010 # min per week
  cv <- 1.85 # coefficient of variation for active transport time
  
  muNonTravel <- 2 # MET-hrs./week leisure activity
  cvNonTravel <- 1 # coefficient of variation for leisure activity
  
  return( list(
    Rwt = Rwt,
    Rct = Rct,
    Rws = Rws,
    muwt = muwt,
    muws = muws,
    muct = muct,
    cv = cv,
    cvNonTravel = cvNonTravel,
    nAgeClass = nAgeClass,
    muNonTravel = muNonTravel,
    muNonTravelMatrix = muNonTravelMatrix,
    GBD = GBD,
    meanType = meanType,
    quantiles = quantiles
  ))
}


#Compute Matrices of Active Transport Means

computeMeanMatrices <- function(parList){
  with(parList, {
    if( meanType == "overall" ){
      alphawt <- sum(F*Rwt)
      alphact <- sum(F*Rct)
      alphaws <- sum(F*Rws)
      meanWalkTime <- muwt/alphawt*Rwt
      meanCycleTime <- muct/alphact*Rct
      meanWalkSpeed <- muws/alphaws*Rws
    } else if (meanType == "referent" ){
      meanWalkTime <- muwt*Rwt
      meanCycleTime <- muct*Rct
      meanWalkSpeed <- muws*Rws
    }else{
      message("Wrong mean type.")
    }
    propTimeCycling <-  meanCycleTime/(meanCycleTime+meanWalkTime)
    
    #        meanWalkMET <- ifelse(1.2216*meanWalkSpeed + 0.0838 < 2.5, 2.5,  1.2216*meanWalkSpeed + 0.0838)
    #        meanCycleMET <- matrix(6, byrow=TRUE, ncol = 2, nrow = nAgeClass, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
    meanActiveTransportTime <- meanWalkTime + meanCycleTime
    sdActiveTransportTime <- meanActiveTransportTime*cv
    
    pWalk <- 1 - propTimeCycling #meanWalkTime/(meanWalkTime + meanCycleTime)
    
    return(list(meanWalkTime = meanWalkTime, meanCycleTime = meanCycleTime, meanWalkSpeed = meanWalkSpeed, meanActiveTransportTime = meanActiveTransportTime, sdActiveTransportTime = sdActiveTransportTime, propTimeCycling = propTimeCycling, pWalk = pWalk)) # meanWalkMET = meanWalkMET, meanCycleMET = meanCycleMET,
  })
}

#Compute Quintiles of Active Transport Time
getQuintiles <- function(means, parameters){
  
  ActiveTransportTime <- computeQuintiles(means$meanActiveTransportTime, means$sdActiveTransportTime, parameters$quantiles)
  WalkingTime <- list(M = ActiveTransportTime[["M"]] * (1-means$propTimeCycling[,"M"]), F = ActiveTransportTime[["F"]] * (1-means$propTimeCycling[,"F"]))
  CyclingTime <- list(M = ActiveTransportTime[["M"]] * (means$propTimeCycling[,"M"]), F = ActiveTransportTime[["F"]] * (means$propTimeCycling[,"F"]))
  #WalkingMET <- list(M = means$meanWalkMET[,"M"]*WalkingTime[["M"]]/60, F = means$meanWalkMET[,"F"]*WalkingTime[["F"]]/60)
  #CyclingMET <- list(M = means$meanCycleMET[,"M"]*CyclingTime[["M"]]/60, F = means$meanCycleMET[,"F"]*CyclingTime[["F"]]/60)
  #TravelMET <- list(M = WalkingMET[["M"]] + CyclingMET[["M"]], F = WalkingMET[["F"]] + CyclingMET[["F"]])
  
  muNonTravel <- parameters$muNonTravel
  muNonTravelMatrix <- parameters$muNonTravelMatrix
  
  TotalMETSample <- mapply(getTotalDistribution,
                           muTravel = means$meanActiveTransportTime,
                           cvTravel = parameters$cv,
                           muNonTravel = muNonTravelMatrix*muNonTravel,
                           cvNonTravel = parameters$cvNonTravel,
                           pWalk = means$pWalk, # parameters$pWalk
                           vWalk = means$meanWalkSpeed,
                           size = 1e5, SIMPLIFY = FALSE)
  TotalMETQuintiles <- lapply(TotalMETSample,function(x) quantile(x, parameters$quantiles, na.rm = TRUE))
  
  TotalMET <- list( M = matrix(unlist(TotalMETQuintiles[1:8]),ncol = length(parameters$quantiles), byrow = TRUE), F = matrix(unlist(TotalMETQuintiles[9:16]),ncol = length(parameters$quantiles), byrow = TRUE ) )
  
  TotalMET <- mapply(function(x,y) ifelse(x < 0.1, 0.1, x), TotalMET, SIMPLIFY=FALSE)
  
  #TotalMET <- mapply(function(x,y) ifelse(x+y<2.5,0.1,x+y),TravelMET,parameters$NonTravelMETs,SIMPLIFY=FALSE) # This is the old way of doing things.
  
  return(list(ActiveTransportTime=ActiveTransportTime, WalkingTime=WalkingTime, CyclingTime=CyclingTime, TotalMET = TotalMET)) # WalkingMET=WalkingMET, CyclingMET = CyclingMET, TravelMET = TravelMET,
}


#Compute Quintiles of the Lognormal Distribution
computeQuintiles <- function( mean, sd, quantiles ){
  
  nAgeClass <- nrow(mean)
  ncol <- length(quantiles)
  
  logMean <- log(mean)-1/2*log(1+(sd/mean)^2)
  logSD <- sqrt(log(1+(sd/mean)^2))
  
  quintVec <- c()
  
  for( quant in quantiles ){
    
    quintVec <- c(quintVec, mapply(qlnorm, logMean, logSD, p = quant))
    
  }
  
  quintMat <- matrix(quintVec, nrow = 2*nAgeClass, ncol = ncol, dimnames = list(paste0("ageClass", rep(1:nAgeClass,2)),paste0("q",1:ncol)))
  
  quintList = list(M = quintMat[1:nAgeClass,], F = quintMat[nAgeClass+1:8,])
  
  return(quintList)
}

#Set Risk Ratios for Avctive Transport
createActiveTransportRRs <- function(nQuantiles = 5){
  
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
  
  exposure[["Stroke"]] <- exposure[["CVD"]]
  RR.lit[["Stroke"]] <- RR.lit[["CVD"]]
  
  exposure[["HHD"]] <- exposure[["CVD"]]
  RR.lit[["HHD"]] <- RR.lit[["CVD"]]
  
  k <- 0.5
  RR <- mapply(function(x,y,k) x^(1/y)^k, RR.lit, exposure, 0.5, SIMPLIFY=FALSE)
  RR <- lapply(RR, reshapeRR, nQuantiles = nQuantiles)
  
  return(RR)
  
}


compareModels <- function(baseline, scenario){
  
  ## if( identical(baseline$parameters$GBD,scenario$parameters$GBD) ){
  ##     GBD <- baseline$parameters$GBD # GBD must be the same between baseline and scenario
  ##     }else{
  ##         #error message
  ##         }
  
  GBD <- baseline$parameters$GBD
  
  RR <- createActiveTransportRRs(nQuantiles = length(baseline$parameters$quantiles))
  RR.baseline <- lapply(RR, MET2RR, baseline$quintiles$TotalMET)
  RR.scenario <- lapply(RR, MET2RR, scenario$quintiles$TotalMET)
  
  RRnormalizedToBaseline.scenario <- mapply(ratioForList,RR.baseline, RR.scenario, SIMPLIFY = FALSE) # ratioForList simply computes the ratio
  RRnormalizedToBaseline.baseline <- mapply(ratioForList,RR.baseline, RR.baseline, SIMPLIFY = FALSE) # What!  Always 1!
  
  #    AF <- mapply(AFForList, RRnormalizedToBaseline.scenario,RRnormalizedToBaseline.baseline, SIMPLIFY = FALSE) # Neil and Geoff compute AF diifferently.  This is Neil's way
  AF <- mapply(AFForList2, RR.scenario,RR.baseline, SIMPLIFY = FALSE) # Neil and Geoff compute AF diifferently.  This is Geoff's way.
  
  normalizedDiseaseBurden <- lapply(RR.scenario, normalizeDiseaseBurden)
  normalizedDiseaseBurden.baseline <- lapply(RR.baseline, normalizeDiseaseBurden)
  
  NewBurden <- lapply(AF,function(x) 1-x)
  NewBurdenList <- lapply(NewBurden,function(x) list(M = x[,"M"], F = x[,"F"]))
  denom <- lapply(normalizedDiseaseBurden, function(x) lapply(x, rowSums))
  denom.baseline <- lapply(normalizedDiseaseBurden.baseline, function(x) lapply(x, rowSums))
  
  
  # diseases <- intersect(intersect(names(NewBurdenList),names(GBD)),names(normalizedDiseaseBurden))
  diseases <- c("BreastCancer","ColonCancer","Depression","Dementia","Diabetes")
  
  GBD <- GBD[diseases]
  NewBurdenList <- NewBurdenList[diseases]
  denom <- denom[diseases]
  denom.baseline <- denom.baseline[diseases]
  normalizedDiseaseBurden <- normalizedDiseaseBurden[diseases]
  normalizedDiseaseBurden.baseline <- normalizedDiseaseBurden.baseline[diseases]
  
  dproj <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "dproj"), SIMPLIFY = FALSE)
  dproj.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "dproj", baseline = TRUE), SIMPLIFY = FALSE)
  dprojBurden <- calculateBurden(dproj, normalizedDiseaseBurden)
  dprojBurden.baseline <- calculateBurden(dproj.baseline, normalizedDiseaseBurden.baseline)
  dproj.delta <- mapply(function(x,y){
    mapply("-",x,y, SIMPLIFY = FALSE)
  },dprojBurden,dprojBurden.baseline, SIMPLIFY = FALSE)
  
  yll <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "yll"), SIMPLIFY = FALSE)
  yll.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "yll", baseline = TRUE), SIMPLIFY = FALSE)
  yllBurden <- calculateBurden(yll, normalizedDiseaseBurden)
  yllBurden.baseline <- calculateBurden(yll.baseline, normalizedDiseaseBurden.baseline)
  yll.delta <- mapply(function(x,y){
    mapply("-",x,y, SIMPLIFY = FALSE)
  },yllBurden,yllBurden.baseline, SIMPLIFY = FALSE)
  
  yld <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "yld"), SIMPLIFY = FALSE)
  yld.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "yld", baseline = TRUE), SIMPLIFY = FALSE)
  yldBurden <- calculateBurden(yld, normalizedDiseaseBurden)
  yldBurden.baseline <- calculateBurden(yld.baseline, normalizedDiseaseBurden.baseline)
  yld.delta <- mapply(function(x,y){
    mapply("-",x,y, SIMPLIFY = FALSE)
  },yldBurden,yldBurden.baseline, SIMPLIFY = FALSE)
  
  daly <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom, MoreArgs = list(burden = "daly"), SIMPLIFY = FALSE)
  daly.baseline <- mapply(FUN = burdenFunction, GBD, NewBurdenList, denom.baseline, MoreArgs = list(burden = "daly", baseline = TRUE), SIMPLIFY = FALSE)
  dalyBurden <- calculateBurden(daly, normalizedDiseaseBurden)
  dalyBurden.baseline <- calculateBurden(daly.baseline, normalizedDiseaseBurden.baseline)
  daly.delta <- mapply(function(x,y){
    mapply("-",x,y, SIMPLIFY = FALSE)
  },dalyBurden,dalyBurden.baseline, SIMPLIFY = FALSE)
  
  #    APRR <- createAirPollutionRRs(baseline,scenario)
  
  return(list(RR.baseline = RR.baseline,
              RR.scenario = RR.scenario,
              RRnormalizedToBaseline = RRnormalizedToBaseline.scenario,
              AF = AF,
              normalizedDiseaseBurden = normalizedDiseaseBurden,
              dproj.delta = dproj.delta,
              yll.delta = yll.delta,
              yld.delta = yld.delta,
              daly.delta = daly.delta
  ))
  
}

updateITHIM <- function( ITHIM, parName, parValue){
  ITHIM$parameters[[parName]] <- parValue
  ITHIM <- list(
    parameters = parameters <- ITHIM$parameters,
    means = means <- computeMeanMatrices(parameters),
    quintiles = quintiles <- getQuintiles(means, parameters)
  )
  return(ITHIM)
}

readGBD <- function(file = "gbd.csv"){
  filePath <- system.file(file, package="ITHIM")
  gbd <- read.csv(file=filePath)
  gbdList <- split(gbd,gbd$disease)
  gbdList2 <- lapply(gbdList,function(x) split(x,as.factor(x$sex)))
  gbdList2 <- lapply(gbdList2, function(x) list(M=x$M,F=x$F))
  return(gbdList2)
}

getNonTravelDistribution <- function(mu, cv, size = 1e4){
  mu <- ifelse(mu == 0, 0.01, mu)
  sd <- mu*cv
  simLogNorm <- rlnorm(size, log(mu/sqrt(1+sd^2/mu^2)), sqrt(log(1+sd^2/mu^2)))
  #simData <- ifelse(sample(0:1, size = size, prob = c(1-p,p), replace = TRUE) == 1, simLogNorm, 0)
  simData <- simLogNorm
  return(simData)
}

getTravelDistribution <- function(mu, cv, pWalk, vWalk, size = 1e4){
  mu <- ifelse(mu == 0, 0.01, mu)
  sd <- mu*cv
  activeTransportTime <- rlnorm(size, log(mu/sqrt(1+sd^2/mu^2)), sqrt(log(1+sd^2/mu^2)))
  
  walkingTime <- activeTransportTime*pWalk
  cyclingTime <- activeTransportTime*(1-pWalk)
  
  walkingMETs <- computeWalkingMETs(vWalk)*walkingTime/60
  cyclingMETs <- computeCyclingMETs()*cyclingTime/60
  
  travelMETs <- walkingMETs + cyclingMETs
  
  return(travelMETs)
}

computeWalkingMETs <- function(v){
  
  METs <- 1.2216*v + 0.0838
  
  return(ifelse( METs < 2.5, 2.5, METs ))
  
}

computeCyclingMETs <- function(){
  
  return(6)
  
}

getTotalDistribution <- function( muTravel, cvTravel, muNonTravel, cvNonTravel, pWalk, vWalk, size ){
  
  return(getTravelDistribution( mu = muTravel, cv=cvTravel, pWalk = pWalk, vWalk = vWalk, size = size) + getNonTravelDistribution(mu = muNonTravel, cv = cvNonTravel, size = size))
  
}



