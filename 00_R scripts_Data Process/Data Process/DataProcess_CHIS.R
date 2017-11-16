# This file is part of ITHIM Sacramento.

# File: DataProcess_CHIS.R
# Purpose: Process data from the 2005 California Heath Interview Survey

# Library definitions
library(survey)
library(reshape)
library(foreign)

# Set working drectory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data")

# Part 1 Data preparation -------------------------------------------------------------

# use read.spss() in package 'foreign' to input the .sav data
chis.2005 <- read.spss('01_CHIS2005/ADULT.sav',to.data.frame = TRUE)
names(chis.2005) <- tolower(names(chis.2005))

# Create age categories;
chis.2005$age8cat =
  ifelse(chis.2005$srage_p <= 4, 1,
         ifelse(chis.2005$srage_p <= 14, 2,
                ifelse(chis.2005$srage_p <= 29, 3,
                       ifelse(chis.2005$srage_p <= 44, 4,
                              ifelse(chis.2005$srage_p <= 59, 5,
                                     ifelse(chis.2005$srage_p <= 69, 6,
                                            ifelse(chis.2005$srage_p <= 79, 7, 8)))))))


# Create walk for fun, walk for transport, job-related, moderate and vigorous physical activity

# Walk for fun
chis.2005$minwk_walk4fun <- chis.2005$ad42 * chis.2005$ad41 #min/week
chis.2005$minwk_walk4fun[chis.2005$ad40 %in% c("INAPPLICABLE", "NO")] <- 0
chis.2005$minwk_walk4fun[chis.2005$ad42unt == "HOURS"] <- chis.2005$minwk_walk4fun[chis.2005$ad42unt == "HOURS"] * 60
chis.2005$minday_walk4fun <- chis.2005$minwk_walk4fun / 7 # min/day

# Walk for transport
chis.2005$minwk_walk4transport <- chis.2005$ad39 * chis.2005$ad38 # min/week
chis.2005$minwk_walk4transport[chis.2005$ad37 %in% c("UNABLE TO WALK", "NO")] <- 0
chis.2005$minwk_walk4transport[chis.2005$ad39unt == "HOURS"] <- chis.2005$minwk_walk4transport[chis.2005$ad39unt == "HOURS"] * 60
chis.2005$minday_walk4transport <- chis.2005$minwk_walk4transport / 7 # min/day

# Moderate Physical Activity
chis.2005$minwk_mod <- chis.2005$ae27a * chis.2005$ae27 # min/week
chis.2005$minwk_mod[chis.2005$ae26 == "NO"] <- 0
chis.2005$minwk_mod[chis.2005$ae27aunt == "HOURS"] <- chis.2005$minwk_mod[chis.2005$ae27aunt == "HOURS"] * 60
chis.2005$minday_mod <- chis.2005$minwk_mod /7 # min/day

# Vigorous Activity
chis.2005$minwk_vig <- chis.2005$ae25a * chis.2005$ae25; # min/week
chis.2005$minwk_vig[chis.2005$ae24 == "NO"] <- 0
chis.2005$minwk_vig[chis.2005$ae25aunt == "HOURS"] <- chis.2005$minwk_vig[chis.2005$ae25aunt == "HOURS"] * 60
chis.2005$minday_vig <- chis.2005$minwk_vig /7 # min/day

# Make MET assignments
# Walking for fun
chis.2005$MET_walk_hrs_wk <-
  ifelse(chis.2005$age8cat < 6, 3.8 * chis.2005$minwk_walk4fun / 60,
         ifelse(chis.2005$age8cat == 6, 3.0 * chis.2005$minwk_walk4fun / 60,
                ifelse(chis.2005$age8cat == 7, 2.5 * chis.2005$minwk_walk4fun / 60,
                       2.5 * chis.2005$minwk_walk4fun / 60)))

# Moderate exercise
chis.2005$MET_mod_hrs_wk <-
  ifelse(chis.2005$age8cat < 6, 3.8 * chis.2005$minwk_mod / 60,
         ifelse(chis.2005$age8cat == 6, 3.0 * chis.2005$minwk_mod / 60,
                ifelse(chis.2005$age8cat == 7, 2.5 * chis.2005$minwk_mod / 60,
                       2.5 * chis.2005$minwk_mod / 60)))

# Vigorous exercise
chis.2005$MET_vig_hrs_wk <-
  ifelse(chis.2005$age8cat < 6, 9 * chis.2005$minwk_vig / 60,
         ifelse(chis.2005$age8cat == 6, 8 * chis.2005$minwk_vig / 60,
                ifelse(chis.2005$age8cat == 7, 7 * chis.2005$minwk_vig / 60,
                       7 * chis.2005$minwk_vig / 60)))


# Occupational physical activity for people < 60 years of age;

# Asterisk out next line to set minimum day time activity hours
# On-job physical activity
# AK3 is hours worked. Assumption that non-job PA is 0 METS

# Initialize hours worked and non-job activity for unemployed
chis.2005$hours_worked <- 0
chis.2005$hours_worked[chis.2005$ak3 > 0] <- chis.2005$ak3[chis.2005$ak3 > 0]

# Use Woodcock's rule that 5 of 8 hours/day of work are at the
# nominal MET rating

chis.2005$hours_worked <- 5 / 8 * chis.2005$hours_worked

chis.2005$MET_occ_hrs_wk = 0

chis.2005$MET_occ_hrs_wk <-
  ifelse(chis.2005$occmain %in% c("MANAGEMENT, BUSINESS AND FINALCIAL", "PROFESSIONAL AND RELATED", "OFFICE AND ADMIN SUPPOR"), 
         1.5 * chis.2005$hours_worked,
         ifelse(chis.2005$occmain %in% c("SERVICE", "SALES AND RELATED"), 
                2.3 * chis.2005$hours_worked,
                # Farming, forestry
                ifelse(chis.2005$occmain == "FARMING, FORESTRY, AND FISHING", 
                       5.8 * chis.2005$hours_worked,
                       # Construction
                       ifelse(chis.2005$occmain == "CONSTRUCTIOIN AND EXTRACTION", 
                              5.5 * chis.2005$hours_worked,
                              ifelse(chis.2005$occmain == "INSTALLATION, MAINTENANCE AND REPAIR", 
                                     3.5 * chis.2005$hours_worked,
                                     ifelse(chis.2005$occmain == "PRODUCTION", 
                                            3.0 * chis.2005$hours_worked,
                                            ifelse(chis.2005$occmain == "TRANS AND MATERIAL MOVING", 
                                                   6.5 * chis.2005$hours_worked,
                                                   ifelse(chis.2005$occmain == "ARMED FORCES", 
                                                          4.0 * chis.2005$hours_worked,
                                                          # Missing or refused, assign 2.5
                                                          ifelse(chis.2005$occmain == "OCC UNCODABLE-RF, BLNK" & chis.2005$hours_worked > 0,
                                                                 2.5 * chis.2005$hours_worked, 0)))))))))

# Total - non transport related physical activity
# Inclusion of walking is double counting
# MET hours/week of non-transport physical activity
chis.2005$MET_hrwk_nt_pa <- chis.2005$MET_mod_hrs_wk + chis.2005$MET_vig_hrs_wk + chis.2005$MET_occ_hrs_wk

# Adding variables about race/ethnicity and income
# srh -> self-reported hispanic; srw -> self-reported white
# sraa -> self-reported African American
# 1: Non-Hispanic White
# 2: Non-Hispanic Black
# 3: Non-Hispanic Other Races
# 4: Hispanic
# 99: Other
chis.2005$race.cat <- ifelse(chis.2005$srh == "NO" & chis.2005$srw =="YES",1,
                             ifelse(chis.2005$srh == "NO" & chis.2005$sraa == "YES",2,
                                    ifelse(chis.2005$srh == "NO",3,
                                           ifelse(chis.2005$srh == "YES",4,99))))


# ak22_p -> Annual HH income ($)
# quartiles: 25%-25,000; 50%-50,000; 75%-95,000
chis.2005$inc.cat <- ifelse(chis.2005$ak22_p>0 & chis.2005$ak22_p<25000,1,
                            ifelse(chis.2005$ak22_p>25000 & chis.2005$ak22_p<50000,2,
                                   ifelse(chis.2005$ak22_p>50000 & chis.2005$ak22_p<75000,3,
                                          ifelse(chis.2005$ak22_p>75000 & chis.2005$ak22_p<300000,4,99))))

# Part 2 output -------------------------------------------------------------

# define a function for shape the output file into the required format
shape.output <- function(output){
  output.matrix <- matrix(0,64,6,dimnames = 
                            list(rep(c(paste0("maleAgeClass ",1:8),paste0("femaleAgeClass ",1:8)),4),c(paste0("p",1:5),"Demo.Cat")) )
  for (i in c(1:8)){
    output.matrix[(8*i-5):(8*i),1:5] <- output[(6*i-5):(6*i),4]
  }
  
  output.matrix[,6] <- c("cat1",rep(" ",15),"cat2",rep(" ",15),"cat3",rep(" ",15),"cat4",rep(" ",15))
  
  return(
    output.matrix=output.matrix
  )
}

# Create a table of median MET hours/week for non-work physical activity by age-sex-income category
# Use as.data.frame.table() to coerce the 'by' object to a data frame
# Ref: http://tolstoy.newcastle.edu.au/R/e8/help/09/11/6227.html

a.s.table.byInc <- as.data.frame.table(
  by(chis.2005, list(chis.2005$age8cat, chis.2005$srsex,chis.2005$inc.cat), 
     function(x) median(x$MET_hrwk_nt_pa, na.rm = TRUE), simplify = TRUE))
names(a.s.table.byInc) <- c("age8cat", "gender","income", "METS_Median")

# Create a table of median MET hours/week for non-work physical activity by age-sex-race category
a.s.table.byRace <- as.data.frame.table(
  by(chis.2005, list(chis.2005$age8cat, chis.2005$srsex,chis.2005$race.cat), 
     function(x) median(x$MET_hrwk_nt_pa, na.rm = TRUE), simplify = TRUE))
names(a.s.table.byRace) <- c("age8cat", "gender","race/ethnicity", "METS_Median")

# adjust the format
output.byRace <- shape.output(a.s.table.byRace)
output.byIncome <- shape.output(a.s.table.byInc)

# out as .csv files 
cuttingline <- matrix(" ",1,6)

write.csv(rbind(
  output.byRace[1:16,],cuttingline, #NHW
  output.byRace[17:32,],cuttingline, # NHB
  output.byRace[33:48,],cuttingline, # NHO
  output.byRace[49:64,] # HO
),file = "01_nonTravelMET_byRace.csv")

write.csv(rbind(
  output.byIncome[1:16,],cuttingline, #Low Income
  output.byIncome[17:32,],cuttingline, # Median Income
  output.byIncome[33:48,],cuttingline, # High Income
  output.byIncome[49:64,] # Other
),file = "02_nonTravelMET_byIncome.csv")
