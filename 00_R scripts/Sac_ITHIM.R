# ITHIM application for Sacramento County

# Population Mean Walking/Cycling Time (min per week), speed (mph) and coefficient of variation (cv)
PopMeanWalkTime <- 32.4
PopMeanCycleTime <- 5.8
PopMeanWalkSpeed <- 3.0
PopMeanCycleSpeed <- 12.0
cv <- 1.9216

# Number of age categories
nAgeclass<-8

# A numerical matrix for the population proportion
PopProp <- matrix(c(0.0347225,0.0332277,0.0708172,0.0677564,0.1104101,0.1078704,0.0977915,0.0988684,
                       0.1001054,0.1060743,0.041988,0.0472405,0.0224061,0.0272632,0.0126027,0.0208556),
                     byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))

# Numerical matrices for the relative walking time (relative to the value of "female 15-29")
# and the mean walking time 
Rwt <- matrix(c(0.6918,0.7658,0.8026,0.5945,0.5787,1.0000,0.8794,0.7100,1.1115,1.2865,1.5270,0.8593,0.3089,0.7539,0.7601,0.1345),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
meanWalkTime <- Rwt/PopProp/sum(PopProp*Rwt)*PopMeanWalkTime*PopProp

# Numerical matrices for the relative cycling time (relative to the value of "female 15-29")
# and the mean cycling time 
Rct <- matrix(c(2.0208,1.7434,3.5787,2.3395,1.9431,1.0000,3.1866,2.7003,5.2019,0.6997,3.2352,0.2387,2.3141,0.5627,4.6880,0.0100),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
meanCycleTime <- Rct/PopProp/sum(PopProp*Rct)*PopMeanCycleTime*PopProp

# Numerical matrices for the proportion of mean cycling time to total active transport time
PropMeanCycleTime <- meanCycleTime/(meanWalkTime+meanCycleTime)

# Numerical matrices for the relative walking speed (relative to the value of "female 15-29")
# and the mean walking speed
Rws <- matrix(c(1.0663,0.8753,1.0663,0.8753,1.0206,1.0002,1.0590,1.0338,1.0392,0.9474,1.0302,0.9330,0.9510,0.8969,0.9510,0.8969),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))
meanWalkSpeed <- Rws/PopProp/sum(PopProp*Rws)*PopMeanWalkSpeed*PopProp


# Numerical matrices for the relative cycling speed (relative to the value of "female 15-29")
# and the mean cycling speed
Rcs <- matrix(c(0.8474,0.8375,0.8920,0.9817,1.0721,0.9570,1.0735,0.9805,1.1565,0.9628,1.0631,0.9292,1.0293,0.8485,0.9023,0.8308),byrow=TRUE, ncol = 2, dimnames = list(paste0("ageClass",1:nAgeClass),c("M","F")))
meanCycleSpeed <- Rcs/PopProp/sum(PopProp*Rcs)*PopMeanCycleSpeed*PopProp

# Numerical matrices for the mean walking/cycling MET values
meanWalkMET <- ifelse(1.2216*meanWalkSpeed + 0.0838 < 2.5, 2.5,  1.2216*meanWalkSpeed + 0.0838)
meanCycleMET <- matrix(6, byrow=TRUE, ncol = 2, nrow = nAgeClass, dimnames = list(paste0("ageClass ",1:nAgeClass),c("M","F")))

############### Compute Quintiles of Active Transport Time ###############

# Total Active Transport Time
totalATTime <- meanWalkTime + meanCycleTime

# Numerical matrices for the log-normal distribution (mean, sd, log mean, log sd)

meanATtime <- c(totalATTime[,1],totalATTime[,2])
sd <- meanATtime*cv
logMean <- log(meanATtime/sqrt(1+(meanATtime*cv/meanATtime)^2))
logSD <- sqrt(log(1+(meanATtime*cv/meanATtime)^2))

lognorm <- matrix(c(meanATtime,sd,logMean,logSD),
                      byrow=FALSE, ncol = 4, nrow = nAgeClass*2,dimnames = list(c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass)),c("Mean","SD","log Mean","log sd")))

#lognormMale <- matrix(c(totalATTime[,1],totalATTime[,1]*cv,log(totalATTime[,1]/sqrt(1+(totalATTime[,1]*cv/totalATTime[,1])^2)),sqrt(log(1+(totalATTime[,1]*cv/totalATTime[,1])^2))),
                      byrow=FALSE, ncol = 4, nrow = nAgeClass,dimnames = list(paste0("ageClass",1:nAgeClass),c("Mean","SD","log Mean","log sd")))
#lognormFemale <- matrix(c(totalATTime[,2],totalATTime[,2]*cv,log(totalATTime[,2]/sqrt(1+(totalATTime[,2]*cv/totalATTime[,2])^2)),sqrt(log(1+(totalATTime[,2]*cv/totalATTime[,2])^2))),
                    byrow=FALSE, ncol = 4, nrow = nAgeClass,dimnames = list(paste0("ageClass",1:nAgeClass),c("Mean","SD","log Mean","log sd")))

# Numerical matrices for quintiles
quintiles <- seq(0.1,0.9,by=0.2)

quintVec <- c()
for (quant in quintiles) {
  quintVec <- c(quintVec,mapply(qlnorm,lognorm[,3],lognorm[,4],p=quant))
}

quinTotalACTime <- matrix (quintVec, byrow=FALSE, ncol = 5, nrow = nAgeClass*2,dimnames = list((c(paste0("maleAgeClass ",1:nAgeClass),paste0("femaleAgeClass ",1:nAgeClass))),c("10%","30%","50%","70%","90%")))

