# File: Process_CHTS_data.R
# Purpose: Process the raw data from the 2010-2012 California Statewide Travel Survey.
# Impute some missing values.
# Create smaller files to be used as input for the Sacramento County ITHIM.

# Library definitions

# Turn off scientific notation
options(scipen=999)

# Function definitions

# Set working directory
setwd("~/Documents/02_Work/13_ITHIM/03_Data/00_CHTS/00_Original")

# -----------------------------------
# Data preparation
# -----------------------------------

# Read in the full data files and prepare smaller versions for easier HIA analysis
# Note that an account with NREL is necessary to access the data
# See instructions here: http://www.nrel.gov/vehiclesandfuels/secure_transportation_data.html
# Unzip and place all files in your working directory

place <- read.csv("survey_place.csv")
persons <- read.csv("survey_person.csv")
hhs <- read.csv("survey_households.csv")

# Hot deck imputation of missing ages
# According to the 2010-2012 California Household Travel Survey Final Report, hot deck imputation was used on 
# respondents that refused to provide their age before calculating final weights (p. 91). Unfortunately, the imputed
# ages are not included here. Fortunately, the report states that average values by education level (educa: 1-6 =
# relevant, 7 = other, 8/9 = DK/refused), work status (emply: 1 = Yes, 2 = No, Other = NA/Refused) and student 
# status (wkstat: 6 = Student, Other = Not student, 98/99 = DK/refused) were used as the basis for imputation. 
# Further, "If education level was refused or missing, a mean age of relevant work status and student status 
# category was applied. If all the variables used for imputation are refusals, and the overall average age was 
# applied."

# Create a unique person ID
persons$ID <- paste0(persons$sampno, persons$perno)

# Recode missing variables
persons$age[persons$age > 900] <- NA
persons$educa[persons$educa > 7] <- NA
persons$emply[persons$emply > 2] <- NA
persons$wkstat[persons$wkstat > 97] <- NA







