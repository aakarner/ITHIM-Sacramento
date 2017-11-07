############ ITHIM application for Sacramento County ##########

##### Physical Activity Module #####

#set your working directory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/05_Sacramento Case")

# Prevent scientific notation
options(scipen = 100)

# Number of age categories
nAgeClass <- 8L

# the numerical matrix for the population and its proportion for Sacramento Area (source: US Census/Finance Department)
Pop_Input <- read.csv("01_Population.csv")
# population matrix of Sacramento area
Pop_Sac <- as.matrix(Pop_Input[1:8,2:3])
dimnames(Pop_Sac) = list(paste0("ageClass ",1:nAgeClass),c("M","F"))
# the proportion of population
PopProp <- Pop_Sac / sum(Pop_Sac)

# paramter of Physical Activity Risk Function (power)
k<-0.5

# disease names
diseaseNames <- c("BreastCancer","ColonCancer","CVD","Dementia","Depression","Diabetes")

##############################function definition######################################

# function for creating total exposure matrix 
# after inputing the population Mean Walking/Cycling Time (min per week) and coefficient of variation (cv)
TotalExposure <- function(PopMeanWalkTime, PopMeanCycleTime,cv){
  # The population mean walking/cycling speed (mph)
  #"It is common practice in MPOs to assume average walk speed of 3 mph and bicycle speed of 12 mph" from ITHIM user's manual
  PopMeanWalkSpeed <- 3.0
  PopMeanCycleSpeed <- 12.0
 
  # Input the parameters of active transport (include relative walking/cycling time and speed)
  AT_Input <- read.csv("02_ActiveTransport.csv")
  
  # Numerical matrices for the relative walking time (relative to the value of "female 15-29") and the mean walking time
  # Source: CHTS2012 (Per capita mean daily travel time by mode)
  Rwt <- as.matrix(AT_Input[1:8,2:3])
  dimnames(Rwt) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))

  meanWalkTime <- Rwt/PopProp/sum(PopProp*Rwt)*PopMeanWalkTime*PopProp
  
  # Numerical matrices for the relative cycling time (relative to the value of "female 15-29") and the mean cycling time 
  # Source: CHTS2012 (Per capita mean daily travel time by mode)
  Rct <- as.matrix(AT_Input[10:17,2:3])
  dimnames(Rct) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  
  meanCycleTime <- Rct/PopProp/sum(PopProp*Rct)*PopMeanCycleTime*PopProp
    
  # Numerical matrices for the proportion of mean cycling time to total active transport time
  PropMeanCycleTime <- meanCycleTime/(meanWalkTime+meanCycleTime)
  
  # Numerical matrices for the relative walking speed (relative to the value of "female 15-29") and the mean walking speed
  # Hard code in spreadsheet with a comment from James W "these will be fixed"
  Rws <- as.matrix(AT_Input[19:26,2:3])
  dimnames(Rws) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
  meanWalkSpeed <- Rws/PopProp/sum(PopProp*Rws)*PopMeanWalkSpeed*PopProp
  
  # Numerical matrices for the relative cycling speed (relative to the value of "female 15-29") and the mean cycling speed
  # Hard code in spreadsheet with a comment from James W "these will be fixed"
  Rcs <- as.matrix(AT_Input[28:35,2:3])
  dimnames(Rcs) =list(paste0("ageClass ",1:nAgeClass),c("M","F"))
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
  # source: CHIS2009 (Per capita weekly non-travel related physical activity expressed as metabolic equivalent tasks (kcal/kg body weight/hr of activity))
  nonTravelMET_input <- read.csv("03_NonTravelMET.csv")
  
  nonTravelMET <- as.matrix(nonTravelMET_input[1:16,2:6])
  dimnames(nonTravelMET) = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),seq(0.1,0.9,by=0.2))
  
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
  
  #physical 
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
  gbd <- read.csv("04_GBD.csv")
  gbdList <- split(gbd,gbd$Disease)
  
  #Input the whole US population in 2010
  #source: US Census 2010
  allPop <- matrix (c(Pop_Input[10:17,2],Pop_Input[10:17,3]), 
                    byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  
  # the population in Sacramento area
  # source: US Census 2010
  # in the spread version, 2377554 is used as the whole Sac population, which is not the right number. 
  # Here I use the actual sum of population
  SacPop <- matrix (PopProp*sum(Pop_Sac), byrow=TRUE, ncol = 1, nrow = nAgeClass*2, dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),"Population"))
  
  gbd.local <- sapply(gbdList, function(x){x$RR * x[,c(3:6)]/allPop*SacPop}, simplify = FALSE)
  
  # update the colon cancer data with the parameter "colon % of colorectal cancer Male 79% Female 81% "
  # The source of data is the CDPH Death Statistical Master file for the years 2009 to 2011.
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
#(mean walking time(min per week), mean cycling time(min per week), and cv)
BaselineTotalExpo <-TotalExposure(32.4,5.8,1.9216)
ScenarioTotalExpo <-TotalExposure(60,20,1.7112)

#compute the relative risks of Physical Activity (1MET)
RR.PA <- create.PA.RR()

#compute local disease burden
gbd.local <- computeLocalGBD()

#compute health outcomes
HealthOutcome <- computeHealthOutcome(RR.PA,BaselineTotalExpo,ScenarioTotalExpo,gbd.local)


