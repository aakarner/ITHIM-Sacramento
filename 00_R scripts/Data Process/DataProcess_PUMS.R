setwd("/Users/Yizheng/Documents/02_Work/13_ITHIM/03_Data")

library(StatMatch)
library(foreign)

##### function definition
getTAZ <- function(parc.input){
  ELD.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="ELD")])
  PLA.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="PLA")])
  SAC.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="SAC")])
  SUT.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="SUT")])
  YOL.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="YOL")])
  YUB.TAZ <- unique(parc.input$TAZ[which(substr(parc.2012$PIDSTR,1,3)=="YUB")])
  
  return(list(
    ELD.TAZ = ELD.TAZ,
    PLA.TAZ = PLA.TAZ,
    SAC.TAZ = SAC.TAZ,
    SUT.TAZ = SUT.TAZ,
    YOL.TAZ = YOL.TAZ,
    YUB.TAZ = YUB.TAZ
  ))
}
############### data process for PUMS
pums.h <- read.csv('07_SacSim/PUMS/csv_hca_12/ss12hca.csv')
pums.p <- read.csv('07_SacSim/PUMS/csv_pca_12/ss12pca.csv')
class(pums.h)
head(pums.h)
head(pums.p)

keep.pums.p <- c("SERIALNO","PUMA","AGEP","PWGTP","SEX","SCHL","RAC1P","HISP")
pums.p.c <- pums.p[,names(pums.p)%in%keep.pums.p]

keep.pums.h <- c("SERIALNO","HINCP")
pums.h.c <- pums.h[,names(pums.h)%in%keep.pums.h]

pums.h.p <- merge(pums.p.c,pums.h.c,by.x = 'SERIALNO',by.y = 'SERIALNO')
head(pums.h.p)


# puma no. for six counties
# source: https://www.census.gov/geo/maps-data/maps/2010puma/st06_ca.html
pumano.eld <- 1700         #countyID=1
pumano.pla <- c(6101:6103) #countyID=2
pumano.sac <- c(6701:6712) #countyID=3
pumano.sut.yub <- 10100    #countyID=4
pumano.yol <- 11300        #countyID=5

# recode county ID
pums.h.p$countyID <- ifelse(pums.h.p$PUMA==pumano.eld,1,
                            ifelse(pums.h.p$PUMA%in%pumano.pla,2,
                                   ifelse(pums.h.p$PUMA%in%pumano.sac,3,
                                          ifelse(pums.h.p$PUMA==pumano.sut.yub,4,
                                                 ifelse(pums.h.p$PUMA==pumano.yol,5,99)))))

# recode race ID
pums.h.p$raceID <- ifelse(pums.h.p$HISP==1&pums.h.p$RAC1P==1,1,
                          ifelse(pums.h.p$HISP==1&pums.h.p$RAC1P==2,2,
                                 ifelse(pums.h.p$HISP==1,3,
                                        ifelse(pums.h.p$HISP!=1,4,99))))

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



# recode hincID
svy.pums <- svydesign(ids = ~1,weights = ~PWGTP,data = pums.h.p)
inc.quan <- svyquantile(~HINCP,subset(svy.pums,countyID%in%c(1:5)&is.na(HINCP)==FALSE),c(0.25,0.5,0.75))

pums.h.p$hincID <- ifelse(pums.h.p$HINCP<=inc.quan[1],1,
                          ifelse(pums.h.p$HINCP<=inc.quan[2],2,
                                 ifelse(pums.h.p$HINCP<=inc.quan[3],3,4)))

head(pums.h.p)


############### data process for SACOG ABM
pop.2012 <- read.dbf('07_SacSim/2012_pop_parc_AM1/2012_pop.dbf')
parc.2012 <- read.dbf('07_SacSim/2012_pop_parc_AM1/2012_parc.dbf')
head(pop.2012)

taz <- getTAZ(parc.2012)

#recode countyID
pop.2012$countyID <- ifelse(pop.2012$HHTAZ%in%taz$ELD.TAZ,1,
                            ifelse(pop.2012$HHTAZ%in%taz$PLA.TAZ,2,
                                   ifelse(pop.2012$HHTAZ%in%taz$SAC.TAZ,3,
                                          ifelse(pop.2012$HHTAZ%in%c(taz$SUT.TAZ,taz$YUB.TAZ),4,
                                                 ifelse(pop.2012$HHTAZ%in%taz$YOL.TAZ,5,99)))))

pop.2012$hincID<-
  ifelse(pop.2012$HINC<=quantile(pop.2012$HINC)[2],1,
         ifelse(pop.2012$HINC<=quantile(pop.2012$HINC)[3],2,
                ifelse(pop.2012$HINC<=quantile(pop.2012$HINC)[4],3,4)))


names(pop.2012)[16]<-"AGEP"
names(pums.h.p)
names(pop.2012)

X.mtc <- c("AGEP")


# rnd <- RANDwNND.hotdeck(data.rec = pop.2012[which(pop.2012$countyID==3&pop.2012$SEX==1&pop.2012$hincID==1),],data.don = pums.h.p[which(pums.h.p$countyID==3&pums.h.p$SEX==1&pums.h.p$hincID==1),],weight.don = "PWGTP",match.vars = X.mtc)
# output.temp <- create.fused(data.rec = pop.2012[which(pop.2012$countyID==3&pop.2012$SEX==1&pop.2012$hincID==1),],data.don = pums.h.p[which(pums.h.p$countyID==3&pums.h.p$SEX==1&pums.h.p$hincID==1),],mtc.ids = rnd$mtc.ids,z.vars = "raceID")

pop.2012.cmplt<-NULL

for (i in 1:5){ #countyID
  print(i)
  for (j in 1:2){ #gender
    print(j)
    for (k in 1:4){ #income
      print(k)
      
      rnd <- RANDwNND.hotdeck(data.rec = pop.2012[which(pop.2012$countyID==i&pop.2012$SEX==j&pop.2012$hincID==k),],data.don = pums.h.p[which(pums.h.p$countyID==i&pums.h.p$SEX==j&pums.h.p$hincID==k),],weight.don = "PWGTP",match.vars = X.mtc)
      output.temp <- create.fused(data.rec = pop.2012[which(pop.2012$countyID==i&pop.2012$SEX==j&pop.2012$hincID==k),],data.don = pums.h.p[which(pums.h.p$countyID==i&pums.h.p$SEX==j&pums.h.p$hincID==k),],mtc.ids = rnd$mtc.ids,z.vars = "raceID")
      
      pop.2012.cmplt <- rbind(pop.2012.cmplt,output.temp)
    }
    
  }
  
  
}

save(pop.2012.cmplt,file = "pop.2012.cmplt")

#head(output)
#table(output$raceID[which(output$countyID==5)])
#svytable(~raceID,subset(svy.pums,countyID==5))


################data process for CDPH
cdph <- read.spss('04_CDPH/TEMPB.sav',to.data.frame = TRUE)

head(cdph)

#recode countyID
cdph$countyID <- ifelse(cdph$county3=="009",1,
                        ifelse(cdph$county3=="031",2,
                               ifelse(cdph$county3=="034",3,
                                      ifelse(cdph$county3%in%c("051","058"),4,
                                             ifelse(cdph$county3=="057",5,99)))))

cdph.sac <- cdph[which(cdph$countyID%in%c(1,2,3,4,5)),]

#recode raceID
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

names(cdph.sac)[3]<-"AGEP"
names(cdph.sac)[8]<-"SEX"
names(cdph.sac)
names(pums.h.p)


X.mtc <- c("AGEP","eduID","SEX","raceID")

cdph.sac.cmplt <- NULL

for (i in 1:5){
  print(i)
  rnd <- RANDwNND.hotdeck(data.rec = cdph.sac[which(cdph.sac$countyID==i),],data.don = pums.h.p[which(pums.h.p$countyID==i),],weight.don = "PWGTP",match.vars = X.mtc)
  output.temp <- create.fused(data.rec = cdph.sac[which(cdph.sac$countyID==i),],data.don = pums.h.p[which(pums.h.p$countyID==i),],mtc.ids = rnd$mtc.ids,z.vars = "hincID")
  
  cdph.sac.cmplt <- rbind(cdph.sac.cmplt,output.temp)
}

save(cdph.sac.cmplt,file = "cdph.sac.cmplt")

# svy.pums <- svydesign(ids = ~1,weights = ~PWGTP,data = pums.h.p)
# 
# table(cdph.sac.inc$hincID[which(cdph.sac.inc$countyID==5)])
# svytable(~hincID,subset(svy.pums,countyID==5&AGEP>62))
