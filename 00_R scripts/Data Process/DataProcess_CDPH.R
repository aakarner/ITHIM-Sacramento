# Author: Alex Karner and Yizheng Wu
# File: DataProcess_CDPH.R
# Purpose: Calculate age-sex-race specific deathrates by county for traffic deaths and diseases related to
# physical activity and air pollution.
# NB: The input files specified here must be obtained for a fee from CDPH and are confidential. 
# Additionally, we have heavily processed them to combine diseases into relevant categories from the 
# Global Burden of Disease Study. 

# Library definitions
#library(foreign)

# Set working drectory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/02_CDPH")

# -----------------------------------
# Data preparation
# -----------------------------------

# use read.spss() in package 'foreign' to input the .sav data
#cdph <- read.spss('TEMPB.sav',to.data.frame = TRUE)
#head(cdph)

load('cdph.sac.cmplt')
cdph <- cdph.sac.cmplt

# race group names
raceGroupNames <- c("NHW","NHB","NHO","HO")
incomeGroupNames <- c("cat1","cat2","cat3","cat4")

# -----------------------------------
# Analysis
# -----------------------------------

processCDPH <- function(local.counties,year){
  
  # obtain the local data
  cdph.local.year <- cdph[which(cdph$county3%in%local.counties & cdph$yod%in%year),]
  
  # recode the race ID
  # 1: non-Hispanic White
  # 2: non-Hispanic Black
  # 3: non-Hispanic Other Races 
  # 4: Hispanic Races
  # 99: unknown
  # cdph.local.year$race <- ifelse(cdph.local.year$hisp == "1" & cdph.local.year$race1 == "10",1,
  #                                ifelse(cdph.local.year$hisp == "1" & cdph.local.year$race1 == "20",2,
  #                                       ifelse(cdph.local.year$hisp == "1",3,
  #                                              ifelse(!(cdph.local.year$hisp %in% c("1","9")),4,99))))
  
  # numeirc matrix for storing the local deaths
  local.gdb.race <- matrix(NA,nrow = 16,ncol = 4)
  local.gdb.income <- matrix(NA,nrow = 16,ncol = 4)
  
  # for loops for compute the death numbers of each disease classified by race, sex, and age
  for (k in 1:4){ # race and ethnicity
    for (j in 1:2){ #sex
      for (i in 1:8){ # age categories
        temp <- cdph.local.year[which(cdph.local.year$SEX == j & cdph.local.year$age8cat == i & cdph.local.year$raceID == k),]
        local.gdb.race[8*(j-1)+i,k] <- sum(temp$GBDGRP2,temp$GBDGRP1,temp$GBDGRP4,temp$GBDGRP6,temp$GBDGRP8,temp$GBDGRP7)
        # local.gdb.race[(8*(j-1)+i),k] <- sum(temp$GBDGRP2) # Breast cancer
        # local.gdb.race[(8*(j-1)+i)+16,k] <- sum(temp$GBDGRP1) # Colon cancer
        # local.gdb.race[(8*(j-1)+i)+32,k] <- sum(temp$GBDGRP4) # CVD
        # local.gdb.race[(8*(j-1)+i)+48,k] <- sum(temp$GBDGRP6) # Dementia
        # local.gdb.race[(8*(j-1)+i)+64,k] <- sum(temp$GBDGRP8) # Depression
        # local.gdb.race[(8*(j-1)+i)+80,k] <- sum(temp$GBDGRP7) # Diabetes
      }
    }
  }
  
  # for loops for compute the death numbers of each disease classified by income, sex, and age
  for (k in 1:4){ # income
    for (j in 1:2){ #sex
      for (i in 1:8){ # age categories
        temp <- cdph.local.year[which(cdph.local.year$SEX == j & cdph.local.year$age8cat == i & cdph.local.year$hincID == k),]
        local.gdb.income[8*(j-1)+i,k] <- sum(temp$GBDGRP2,temp$GBDGRP1,temp$GBDGRP4,temp$GBDGRP6,temp$GBDGRP8,temp$GBDGRP7)
        # local.gdb.income[(8*(j-1)+i),k] <- sum(temp$GBDGRP2) # Breast cancer
        # local.gdb.income[(8*(j-1)+i)+16,k] <- sum(temp$GBDGRP1) # Colon cancer
        # local.gdb.income[(8*(j-1)+i)+32,k] <- sum(temp$GBDGRP4) # CVD
        # local.gdb.income[(8*(j-1)+i)+48,k] <- sum(temp$GBDGRP6) # Dementia
        # local.gdb.income[(8*(j-1)+i)+64,k] <- sum(temp$GBDGRP8) # Depression
        # local.gdb.income[(8*(j-1)+i)+80,k] <- sum(temp$GBDGRP7) # Diabetes
      }
    }
  }
  
  # adjust the format for inputting into equity analysis module
  #dieaseNamesList <- c(rep("BreastCancer",16),rep("ColonCancer",16),rep("CVD",16),rep("Dementia",16),
                       #rep("Depression",16),rep("Diabetes",16))
  
  dimnames(local.gdb.race) = list(c(paste0("maleAgeClass ",1:8),paste0("femaleAgeClass ",1:8)),paste0("deaths_",raceGroupNames))
  dimnames(local.gdb.income) = list(c(paste0("maleAgeClass ",1:8),paste0("femaleAgeClass ",1:8)),paste0("deaths_",incomeGroupNames))
  
  return(list(
    local.gdb.race = local.gdb.race,
    local.gdb.income=local.gdb.income)
    )
  
}

# -----------------------------------
# Output
# -----------------------------------

# define the required areas and year of datasets
# county ID:
# 009 - ElD
# 031 - PLA
# 034 - SAC
# 051 - SUT
# 057 - YOL
# 058 - YUB
local.counties <- c("058")
year <- c("2008","2009","2010")

local.gdb.3ys <- processCDPH(local.counties,year)

# output
write.csv(local.gdb.3ys$local.gdb.race/3,file = "YUB_local gbd_race.csv")
write.csv(local.gdb.3ys$local.gdb.income/3,file = "YUB_local gbd_income.csv")
