# This file is part of ITHIM Sacramento.

# File: DataProcess_CDPH.R
# Purpose: Calculate age-sex-race and age-sex-income specific all-cause deathrates by county.
# NB: The input files specified here must be obtained for a fee from CDPH and are confidential. 

# Library definitions
library(foreign)

# Set working drectory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/02_CDPH")

# Part 1 Data preparation ----------------------------------------------------------------------------

# load the pre-processed data which includes the income variable estimated by hot-deck imputation
# processed by the R script "DataProcess_PUMS.R"
load('cdph.sac.cmplt')
cdph <- cdph.sac.cmplt

# define group names
raceGroupNames <- c("NHW","NHB","NHO","HO")
incomeGroupNames <- c("Quint1","Quint2","Quint3","Quint4")

# Part 2 Function Definition -------------------------------------------------------------------------

processCDPH <- function(local.counties,year){
  
  # obtain the target data according to county of resident and year of death
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
  
  # build the numeirc matrix for storing the local deaths by race and income
  local.gdb.race <- matrix(NA,nrow = 16,ncol = 4)
  local.gdb.income <- matrix(NA,nrow = 16,ncol = 4)
  
  # 'for' loops for compute the all-cause motality classified by race, sex, and age
  for (k in 1:4){ # race and ethnicity
    for (j in 1:2){ #sex
      for (i in 1:8){ # age categories
        temp <- cdph.local.year[which(cdph.local.year$SEX == j & cdph.local.year$age8cat == i & cdph.local.year$raceID == k),]
        #obtain the all-cause mortality for each group
        local.gdb.race[8*(j-1)+i,k] <- sum(temp$GBDGRP1,temp$GBDGRP2,temp$GBDGRP3,temp$GBDGRP4,
                                           temp$GBDGRP5,temp$GBDGRP6,temp$GBDGRP7,temp$GBDGRP8,temp$GBDGRP9,
                                           temp$GBDGRP10,temp$GBDGRP11,temp$GBDGRP12,temp$GBDGRP13)
      }
    }
  }
  
  # 'for' loops for compute the all-cause motality classified by income, sex, and age
  for (k in 1:4){ # income group
    for (j in 1:2){ #sex
      for (i in 1:8){ # age categories
        temp <- cdph.local.year[which(cdph.local.year$SEX == j & cdph.local.year$age8cat == i & cdph.local.year$hincID == k),]
        #ontain the all-cause mortality for each group
        local.gdb.income[8*(j-1)+i,k] <- sum(temp$GBDGRP1,temp$GBDGRP2,temp$GBDGRP3,temp$GBDGRP4,
                                             temp$GBDGRP5,temp$GBDGRP6,temp$GBDGRP7,temp$GBDGRP8,temp$GBDGRP9,
                                             temp$GBDGRP10,temp$GBDGRP11,temp$GBDGRP12,temp$GBDGRP13)
      }
    }
  }
  
  # defina the dimension names for both matrices
  dimnames(local.gdb.race) = list(c(paste0("maleAgeClass ",1:8),
                                    paste0("femaleAgeClass ",1:8)),paste0("deaths_",raceGroupNames))
  dimnames(local.gdb.income) = list(c(paste0("maleAgeClass ",1:8),
                                      paste0("femaleAgeClass ",1:8)),paste0("deaths_",incomeGroupNames))
  
  # return the matrices
  return(list(
    local.gdb.race = local.gdb.race,
    local.gdb.income = local.gdb.income)
    )
  
}

# Part 3 Output the result in .csv file --------------------------------------------------------

# define the required areas and year of death
# county ID:
# 009 - ElD
# 031 - PLA
# 034 - SAC
# 051 - SUT
# 057 - YOL
# 058 - YUB
local.counties <- c("009","031","034","051","057","058")
year <- c("2008","2009","2010")

# apply the function "processCDPH" after inputing the county and year
local.gdb.3ys <- processCDPH(local.counties,year)

# output the annual average all-cause mortality into .csv files
write.csv(local.gdb.3ys$local.gdb.race/3,file = "Region_local gbd_race.allcause.csv")
write.csv(local.gdb.3ys$local.gdb.income/3,file = "Region_local gbd_income.allcause.csv")
