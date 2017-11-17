# This file is part of ITHIM Sacramento.

# File: DataProcess_PUMS.R
# Purpose: use the PUMS data to do the hot-deck imputation. 
#          impute the race/ethnicity information for SACSIM synthetic population
#          impute the income information for CDPH vital statistics

setwd("/Users/Yizheng/Documents/02_Work/13_ITHIM/03_Data")

##### library definition
library(StatMatch)
library(foreign)

##### function definition
# in the output of SACSIM,the unit of location is TAZ
# here we extract the TAZ list for each county in order to obtain the county-level data
getTAZ <- function(parc.input){
  ELD.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="ELD")])
  PLA.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="PLA")])
  SAC.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="SAC")])
  SUT.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="SUT")])
  YOL.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="YOL")])
  YUB.TAZ <- unique(parc.input$TAZ[which(substr(parc.input$PIDSTR,1,3)=="YUB")])
  
  return(list(
    ELD.TAZ = ELD.TAZ,
    PLA.TAZ = PLA.TAZ,
    SAC.TAZ = SAC.TAZ,
    SUT.TAZ = SUT.TAZ,
    YOL.TAZ = YOL.TAZ,
    YUB.TAZ = YUB.TAZ
  ))
}

# function of recode countyID and household income ID
recode <- function(pop,taz){
  #recode countyID (only 5 countyID since Sutter and Yuba are counted as one based on Puma No. in PUMS data)
  pop$countyID <- ifelse(pop$HHTAZ%in%taz$ELD.TAZ,1,
                         ifelse(pop$HHTAZ%in%taz$PLA.TAZ,2,
                                ifelse(pop$HHTAZ%in%taz$SAC.TAZ,3,
                                       ifelse(pop$HHTAZ%in%c(taz$SUT.TAZ,taz$YUB.TAZ),4,
                                              ifelse(pop$HHTAZ%in%taz$YOL.TAZ,5,99)))))
  # recode household income ID based on quartiles
  # 1: household income < 25%
  # 2: household income 25-50%
  # 3: household income 50-75%
  # 4: household income 75-100%
  pop$hincID<-
    ifelse(pop$HINC<=quantile(pop$HINC)[2],1,
           ifelse(pop$HINC<=quantile(pop$HINC)[3],2,
                  ifelse(pop$HINC<=quantile(pop$HINC)[4],3,4)))
  
  # rename the variable "age" in order to match it with PUMA 
  names(pop)[16]<-"AGEP"
  
  return(pop)
}

# function to apply hot deck imputation 
hotdeck.imputation <- function(pop){
  # matching variable - age
  X.mtc <- c("AGEP")
  
  # store the result
  pop.cmplt<-NULL
  
  # 'for' loop to do the Hot-Deck imputation
  for (i in 1:5){ #countyID
    #print(i)
    for (j in 1:2){ #gender
      #print(j)
      for (k in 1:4){ #income
        #print(k)
        
        rnd <- RANDwNND.hotdeck(data.rec = pop[which(pop$countyID==i&pop$SEX==j&pop$hincID==k),],data.don = pums.h.p[which(pums.h.p$countyID==i&pums.h.p$SEX==j&pums.h.p$hincID==k),],weight.don = "PWGTP",match.vars = X.mtc)
        output.temp <- create.fused(data.rec = pop[which(pop$countyID==i&pop$SEX==j&pop$hincID==k),],data.don = pums.h.p[which(pums.h.p$countyID==i&pums.h.p$SEX==j&pums.h.p$hincID==k),],mtc.ids = rnd$mtc.ids,z.vars = "raceID")
        
        pop.cmplt <- rbind(pop.cmplt,output.temp)
      }
      
    }
  }
  return(pop.cmplt)
}

update.countyID <- function(pop.cmplt,taz){
  #recode countyID (from 5 "counties" to 6 counties)
  pop.cmplt$countyID <- ifelse(pop.cmplt$HHTAZ%in%taz$ELD.TAZ,1,
                               ifelse(pop.cmplt$HHTAZ%in%taz$PLA.TAZ,2,
                                      ifelse(pop.cmplt$HHTAZ%in%taz$SAC.TAZ,3,
                                             ifelse(pop.cmplt$HHTAZ%in%taz$SUT.TAZ,4,
                                                    ifelse(pop.cmplt$HHTAZ%in%taz$YOL.TAZ,5,6)))))
  return(pop.cmplt)
}

############### data process for PUMS #####################
# read the pums data
# ss12hca.csv - 2012 housing record
# ss12pca.csv - 2012 person record
pums.h <- read.csv('07_SacSim/PUMS/csv_hca_12/ss12hca.csv')
pums.p <- read.csv('07_SacSim/PUMS/csv_pca_12/ss12pca.csv')

# keep key variables in each record
keep.pums.p <- c("SERIALNO","PUMA","AGEP","PWGTP","SEX","SCHL","RAC1P","HISP")
pums.p.c <- pums.p[,names(pums.p)%in%keep.pums.p]

keep.pums.h <- c("SERIALNO","HINCP")
pums.h.c <- pums.h[,names(pums.h)%in%keep.pums.h]

# merge two data sets by "SERIALNO"
pums.h.p <- merge(pums.p.c,pums.h.c,by.x = 'SERIALNO',by.y = 'SERIALNO')
#head(pums.h.p)

# define puma no. for six counties
# source: https://www.census.gov/geo/maps-data/maps/2010puma/st06_ca.html
pumano.eld <- 1700         #countyID=1
pumano.pla <- c(6101:6103) #countyID=2
pumano.sac <- c(6701:6712) #countyID=3
pumano.sut.yub <- 10100    #countyID=4 include two counties (Sutter and Yuba)
pumano.yol <- 11300        #countyID=5

# recode county ID
pums.h.p$countyID <- ifelse(pums.h.p$PUMA==pumano.eld,1,
                            ifelse(pums.h.p$PUMA%in%pumano.pla,2,
                                   ifelse(pums.h.p$PUMA%in%pumano.sac,3,
                                          ifelse(pums.h.p$PUMA==pumano.sut.yub,4,
                                                 ifelse(pums.h.p$PUMA==pumano.yol,5,99)))))

# recode race ID
# 1: non-Hispanic White
# 2: non-Hispanic Black
# 3: non-Hispanic Other Races 
# 4: Hispanic Races
# 99: unknown
pums.h.p$raceID <- ifelse(pums.h.p$HISP==1&pums.h.p$RAC1P==1,1,
                          ifelse(pums.h.p$HISP==1&pums.h.p$RAC1P==2,2,
                                 ifelse(pums.h.p$HISP==1,3,
                                        ifelse(pums.h.p$HISP!=1,4,99))))

#recode age and sex ID (age.sex.ID)
pums.h.p$age.sex.ID <- ifelse(pums.h.p$AGEP<=4,1,
                              ifelse(pums.h.p$AGEP<=14,2,
                                     ifelse(pums.h.p$AGEP<=29,3,
                                            ifelse(pums.h.p$AGEP<=44,4,
                                                   ifelse(pums.h.p$AGEP<=59,5,
                                                          ifelse(pums.h.p$AGEP<=69,6,
                                                                 ifelse(pums.h.p$AGEP<=79,7,8
                                                                        )))))))

#recode educational level ID (eduID)
# 1: 8th grade or less
# 2: 9th through 12th grade
# 3: high school graduate / ged completed
# 4: some college credit but no degree
# 5: associate degree
# 6: bachelor's
# 7: master's
# 8: doctoral degree / professional degree
# 9: unknown
pums.h.p$eduID <- ifelse(pums.h.p$SCHL%in%c(1:11),1,
                         ifelse(pums.h.p$SCHL%in%c(12:15),2,
                                ifelse(pums.h.p$SCHL%in%c(16:17),3,
                                       ifelse(pums.h.p$SCHL%in%c(18,19),4,
                                              ifelse(pums.h.p$SCHL==20,5,
                                                     ifelse(pums.h.p$SCHL==21,6,
                                                            ifelse(pums.h.p$SCHL==22,7,
                                                                   ifelse(pums.h.p$SCHL%in%c(23,24),8,99))))))))
# add a column of '1'
pums.h.p$ones <- 1

# recode hincID
# get the quantiles for the household income in all six counties
svy.pums <- svydesign(ids = ~1,weights = ~PWGTP,data = pums.h.p)
inc.quan <- svyquantile(~HINCP,subset(svy.pums,countyID%in%c(1:5)&is.na(HINCP)==FALSE),c(0.25,0.5,0.75))

# define the income group
# 1: household income < 25%
# 2: household income 25-50%
# 3: household income 50-75%
# 4: household income 75-100%
pums.h.p$hincID <- ifelse(pums.h.p$HINCP<=inc.quan[1],1,
                          ifelse(pums.h.p$HINCP<=inc.quan[2],2,
                                 ifelse(pums.h.p$HINCP<=inc.quan[3],3,4)))

############### Imputation process for SACSIM #####################
# read the data
# pop - population
# parc - parcel information 
pop.2012 <- read.dbf('07_SacSim/2012_pop_parc_AM1/2012_pop.dbf')
parc.2012 <- read.dbf('07_SacSim/2012_pop_parc_AM1/2012_parc.dbf')

pop.2020 <- read.dbf('07_SacSim/pa20_pop_parc_AM1/pa20_pop.dbf')
parc.2020 <- read.dbf('07_SacSim/pa20_pop_parc_AM1/pa20_parc.dbf')

pop.2036 <- read.dbf('07_SacSim/pa36_pop_parc_AM1/pa36_pop.dbf')
parc.2036 <- read.dbf('07_SacSim/pa36_pop_parc_AM1/pa36_parc.dbf')

# extrac the taz list for each county
taz.2012 <- getTAZ(parc.2012)
taz.2020 <- getTAZ(parc.2020)
taz.2036 <- getTAZ(parc.2036)

# recode countyID (1:5) and household income ID
pop.2012 <- recode(pop.2012,taz.2012)
pop.2020 <- recode(pop.2020,taz.2020)
pop.2036 <- recode(pop.2036,taz.2036)

# apply the function of hot deck imputation 
pop.2012.cmplt <- hotdeck.imputation(pop.2012)
pop.2020.cmplt <- hotdeck.imputation(pop.2020)
pop.2036.cmplt <- hotdeck.imputation(pop.2036)

# update the countyID from 5 counties to 6 counties
pop.2012.cmplt <- update.countyID(pop.2012.cmplt,taz.2012)
pop.2020.cmplt <- update.countyID(pop.2020.cmplt,taz.2020)
pop.2036.cmplt <- update.countyID(pop.2036.cmplt,taz.2036)

# save the output
save(pop.2012.cmplt,file = "pop.2012.cmplt")
save(pop.2020.cmplt,file = "pop.2020.cmplt")
save(pop.2036.cmplt,file = "pop.2036.cmplt")


############### imputation process for CDPH #####################
cdph <- read.spss('04_CDPH/TEMPB.sav',to.data.frame = TRUE)

#recode countyID (only 5 countyID since Sutter and Yuba are counted as one based on Puma No.)
cdph$countyID <- ifelse(cdph$county3=="009",1,
                        ifelse(cdph$county3=="031",2,
                               ifelse(cdph$county3=="034",3,
                                      ifelse(cdph$county3%in%c("051","058"),4,
                                             ifelse(cdph$county3=="057",5,99)))))

# obtain the data for Sacramento region
cdph.sac <- cdph[which(cdph$countyID%in%c(1,2,3,4,5)),]

#recode raceID
# 1: non-Hispanic White
# 2: non-Hispanic Black
# 3: non-Hispanic Other Races 
# 4: Hispanic Races
# 99: unknown
cdph.sac$raceID <- ifelse(cdph.sac$hisp == "1" & cdph.sac$race1 == "10",1,
                               ifelse(cdph.sac$hisp == "1" & cdph.sac$race1 == "20",2,
                                      ifelse(cdph.sac$hisp == "1",3,
                                             ifelse(!(cdph.sac$hisp %in% c("1","9")),4,99))))

#recode educational level ID (eduID)
# 1: 8th grade or less
# 2: 9th through 12th grade
# 3: high school graduate / ged completed
# 4: some college credit but no degree
# 5: associate degree
# 6: bachelor's
# 7: master's
# 8: doctoral degree / professional degree
# 9: unknown
cdph.sac$eduID <- cdph.sac$educ

# rename the variables "age" and "sex" in order to match it with PUMA 
names(cdph.sac)[3]<-"AGEP"
names(cdph.sac)[8]<-"SEX"

# matching variables - age, education level, sex, and race
X.mtc <- c("AGEP","eduID","SEX","raceID")

# store the result
cdph.sac.cmplt <- NULL

# 'for' loop to apply hot deck imputation 
for (i in 1:5){ # counties
  print(i)
  rnd <- RANDwNND.hotdeck(data.rec = cdph.sac[which(cdph.sac$countyID==i),],data.don = pums.h.p[which(pums.h.p$countyID==i&is.na(pums.h.p$hincID)==FALSE),],weight.don = "PWGTP",match.vars = X.mtc)
  output.temp <- create.fused(data.rec = cdph.sac[which(cdph.sac$countyID==i),],data.don = pums.h.p[which(pums.h.p$countyID==i&is.na(pums.h.p$hincID)==FALSE),],mtc.ids = rnd$mtc.ids,z.vars = "hincID")
  
  cdph.sac.cmplt <- rbind(cdph.sac.cmplt,output.temp)
}

# save the output
save(cdph.sac.cmplt,file = "cdph.sac.cmplt")
