# Author: Alex Karner and Yizheng Wu
# File: DataProcess_CDPH.R
# Purpose: Calculate age-sex-race specific deathrates by county for traffic deaths and diseases related to
# physical activity and air pollution.
# NB: The input files specified here must be obtained for a fee from CDPH and are confidential. 
# Additionally, we have heavily processed them to combine diseases into relevant categories from the 
# Global Burden of Disease Study. 

# Library definitions
library(foreign)

# Set working drectory
# since the CDPH file is larger than 100MB, we store it in local server.
setwd("~/Documents/02_Work/13_ITHIM/03_Data/04_CDPH")

# -----------------------------------
# Data preparation
# -----------------------------------

# use read.spss() in package 'foreign' to input the .sav data
cdph <- read.spss('TEMPB.sav',to.data.frame = TRUE)


# race group names
raceGroupNames <- c("NHW","NHB","NHO","HO")

# -----------------------------------
# Analysis
# -----------------------------------


processCDPH <- function(local.counties,year){
  # obtain the local data
  cdph.local.year <- cdph[which(cdph$county3%in%local.counties & cdph$yod%in%year),]
  
  # race categories
  cdph.local.year$race <- ifelse(cdph.local.year$hisp == "1" & cdph.local.year$race1 == "10",1,
                                 ifelse(cdph.local.year$hisp == "1" & cdph.local.year$race1 == "20",2,
                                        ifelse(cdph.local.year$hisp == "1",3,
                                               ifelse(!(cdph.local.year$hisp %in% c("1","9")),4,99))))
  
  # numeirc matrix for storing the local deaths
  local.gdb.race <- matrix(NA,nrow = 96,ncol = 4)
  
  # for loops for compute the death numbers of each disease
  for (k in 1:4){ # race and ethnicity
    for (j in 1:2){ #sex
      for (i in 1:8){ # age categories
        temp <- cdph.local.year[which(cdph.local.year$sex == j & cdph.local.year$age8cat == i & cdph.local.year$race == k),]
        local.gdb.race[(8*(j-1)+i),k] <- sum(temp$GBDGRP2) # Breast cancer
        local.gdb.race[(8*(j-1)+i)+16,k] <- sum(temp$GBDGRP1) # Colon cancer
        local.gdb.race[(8*(j-1)+i)+32,k] <- sum(temp$GBDGRP4) # CVD
        local.gdb.race[(8*(j-1)+i)+48,k] <- sum(temp$GBDGRP6) # Dementia
        local.gdb.race[(8*(j-1)+i)+64,k] <- sum(temp$GBDGRP8) # Depression
        local.gdb.race[(8*(j-1)+i)+80,k] <- sum(temp$GBDGRP7) # Diabetes
      }
    }
  }
  
  
  
  # adjust the format for inputting into equity analysis module
  dieaseNamesList <- c(rep("BreastCancer",16),rep("ColonCancer",16),rep("CVD",16),rep("Dementia",16),
                       rep("Depression",16),rep("Diabetes",16))
  
  dimnames(local.gdb.race) = list(dieaseNamesList,paste0("deaths_",raceGroupNames))
  
  return(local.gdb.race = local.gdb.race)
  
}

# -----------------------------------
# Output
# -----------------------------------

# define the required areas and year of datasets
#local.counties <- c("010") #Fresno (validation purpose)
#local.counties <- c("009","031","034","051","057","058") # SACOG six counties
local.counties <- c("058")
year <- c("2009")

local.gdb.race <- processCDPH(local.counties,year)

local.gdb.race

Col2 <- matrix(rep(c(paste0("maleAgeClass ",1:8),paste0("femaleAgeClass ",1:8)),6),96,1)



write.csv(cbind(Col2,local.gdb.race),file = "test_local gbd.csv")

