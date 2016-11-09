############ ITHIM application for Sacramento County ##########

#set your working directory
setwd("~/Documents/02_Work/13_ITHIM/03_Data/01_GBD")

# Number of age categories
nAgeClass <- 8

# the numerical matrix for the population proportion
PopProp <- matrix(c(0.0347225,0.0332277,0.0708172,0.0677564,0.1104101,0.1078704,0.0977915,0.0988684,
                    0.1001054,0.1060743,0.041988,0.0472405,0.0224061,0.0272632,0.0126027,0.0208556),
                  byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))

# paramter of Physical Activity Risk Function (power)  								
k<-0.5

# disease names
diseaseNames <- c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes")

##############################function definition######################################

# function for creating total exposure matrix 
# after inputing the population Mean Walking/Cycling Time (min per week) and coefficient of variation (cv)
TotalExposure <- function(PopMeanWalkTime, PopMeanCycleTime,cv){
  # The population mean walking/cycling speed (mph)
  PopMeanWalkSpeed <- 3.0
  PopMeanCycleSpeed <- 12.0
 
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
  return(
    totalExposure <- totalExposure
    )
}

#function for computing relative risks of physical activity  
create.PA.RR <- function(){
  
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
  
  #reshape RR matrix
  reshapeRR <- function(RR, nQuantiles = 5){
    matrix(c(RR[,"M"],RR[,"F"]),nrow=nAgeClass*2,ncol=nQuantiles,dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2)))
    #list( M = matrix(RR[,"M"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))),F = matrix(RR[,"F"], nrow = nAgeClass, ncol = nQuantiles, dimnames = list(paste0("ageClass",1:nAgeClass), seq(0.1,0.9,by=0.2))))
  }
  
  #compute RR matrix
  RR <- mapply(function(x,y,z) x^(1/y)^z, RR.lit, exposure, k, SIMPLIFY=FALSE)
  RR <- lapply(RR, reshapeRR, nQuantiles = 5)
  return(RR)
}

#function for computing local disease burden
computeLocalGBD <- function (){
  #Read the gbd data
  gbd <- read.csv("gbd.csv")
  gbdList <- split(gbd,gbd$Disease)
  
  #Input the population
  allPop <- matrix (c(10319427,20969500,32953433,30432499,31666007,13930047,7426360,4084053,9881935,20056351,31774758,30600206,33005514,15323140,9169601,7152707), 
                    byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  
  SacPop <- matrix (PopProp*2377554, byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  
  gbd.local <- sapply(gbdList, function(x){x$RR * x[,c(3:6)]/allPop*SacPop}, simplify = FALSE)
  
  #update the colon cancer data with the parameter "colon % of colorectal cancer Male 79% Female 81% "
  gbd.local$ColonCancer[c(1:8),] <- gbd.local$ColonCancer[c(1:8),]*0.7878193
  gbd.local$ColonCancer[c(9:16),] <- gbd.local$ColonCancer[c(9:16),]*0.814
  
  return(gbd.local)
}

#function for computing health outcome
computeHealthOutcome <- function (RR.PA,BaselineTotalExpo,ScenarioTotalExpo,gbd.local){
  #Compute RR for an exposure of x MET
  RR.Baseline <- sapply(RR.PA, function(x) {x^(BaselineTotalExpo^k)}, simplify = FALSE)
  RR.Scenario <- sapply(RR.PA, function(x) {x^(ScenarioTotalExpo^k)}, simplify = FALSE)
  
  
  #Compute Ratio of DB relative to group 1
  RatioDB.Baseline <- lapply(RR.Baseline,function(x) x/x[,1])
  RatioDB.Scenario <- lapply(RR.Scenario,function(x) x/x[,1])
  sum.RatioDB.Baseline <-lapply(RatioDB.Baseline,rowSums)
  sum.RatioDB.Scenario <-lapply(RatioDB.Scenario,rowSums)
  
  #Compute New Burden and AF
  sum.RR.Baseline<-lapply(RR.Baseline,rowSums)
  sum.RR.Scenario<-lapply(RR.Scenario,rowSums)
  new.burden <- mapply(function(x,y) x/y,sum.RR.Scenario,sum.RR.Baseline,SIMPLIFY=FALSE)
  AF <- sapply(new.burden, function(x) 1-x, simplify=FALSE)
  
  #Compute the health outcomes
  #Define a function for outputing health outcomes
  fun.outcome <- function(x,y){
    x[,1] <- y
    x[,c(2:5)] <- x[,c(2:5)]*y
    return(x)}
  
  #Compute deaths per group
  dproj.scenario.firstCol <- mapply(function(x,y,z) x*y$deaths/z, new.burden,gbd.local,sum.RatioDB.Scenario,SIMPLIFY=FALSE)
  dproj.scenario <- mapply(fun.outcome,RatioDB.Scenario,dproj.scenario.firstCol,SIMPLIFY=FALSE)
  
  dproj.baseline.firstCol <- mapply(function(x,y) x$deaths/y, gbd.local,sum.RatioDB.Baseline,SIMPLIFY=FALSE)
  dproj.baseline <- mapply(fun.outcome,RatioDB.Baseline,dproj.baseline.firstCol,SIMPLIFY=FALSE)
  
  #Compute YLL per group
  yll.scenario.firstCol <- mapply(function(x,y,z) x*y$yll/z, new.burden,gbd.local,sum.RatioDB.Scenario,SIMPLIFY=FALSE)
  yll.scenario <- mapply(fun.outcome,RatioDB.Scenario,yll.scenario.firstCol,SIMPLIFY=FALSE)
  
  yll.baseline.firstCol <- mapply(function(x,y) x$yll/y, gbd.local,sum.RatioDB.Baseline,SIMPLIFY=FALSE)
  yll.baseline <- mapply(fun.outcome,RatioDB.Baseline,yll.baseline.firstCol,SIMPLIFY=FALSE)
  
  #Compute YLD per group
  yld.scenario.firstCol <- mapply(function(x,y,z) x*y$yld/z, new.burden,gbd.local,sum.RatioDB.Scenario,SIMPLIFY=FALSE)
  yld.scenario <- mapply(fun.outcome,RatioDB.Scenario,yld.scenario.firstCol,SIMPLIFY=FALSE)
  
  yld.baseline.firstCol <- mapply(function(x,y) x$yld/y, gbd.local,sum.RatioDB.Baseline,SIMPLIFY=FALSE)
  yld.baseline <- mapply(fun.outcome,RatioDB.Baseline,yld.baseline.firstCol,SIMPLIFY=FALSE)
  
  #Compute the ∆Burden, total ∆Burden, and the proportion
  delta.Burden <- rep(list((matrix(NA,nrow=nAgeClass*2,ncol=4,dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),c("∆Deaths","∆YLL","∆YLD","DALYS"))))), length(diseaseNames))
  names(delta.Burden) <- diseaseNames
  
  delta.Burden <- mapply(function (x,a,b,c,d,e,f) {
    x[,1] <- rowSums(a)-rowSums(b) #deaths
    x[,2] <- rowSums(c)-rowSums(d) #yll
    x[,3] <- rowSums(e)-rowSums(f) #yld
    x[,4] <- x[,2] + x[,3]         #dalys
    return(x)
  },delta.Burden, dproj.scenario,dproj.baseline,yll.scenario,yll.baseline,yld.scenario,yld.baseline, SIMPLIFY=FALSE)
  
  total.delta.Burden <- lapply(delta.Burden, colSums)
  total.gbd.local <- lapply(gbd.local,function(x){
    ifelse(colSums(x)!=0,colSums(x),0.0001)
  })
  
  prop.delta.Burden <- mapply(function(x,y) x/y, total.delta.Burden,total.gbd.local,SIMPLIFY=FALSE)
  
  return(list(
    AF=AF,
    new.burden=new.burden,
    delta.Burden=delta.Burden,
    prop.delta.Burden= prop.delta.Burden
    ))
}

############################calculation example#################################

#Create the total exposure matrices by inputing parameters 
#(mean walking time, mean cycling time, and cv)
BaselineTotalExpo <-TotalExposure(32.4,5.8,1.9216)
ScenarioTotalExpo <-TotalExposure(87.2898,87.2898,1.7112)

#compute the relative risks of Physical Activity (1MET)
RR.PA <- create.PA.RR()

#compute local disease burden
gbd.local <- computeLocalGBD()

#compute health outcomes
HealthOutcome <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo,gbd.local)






