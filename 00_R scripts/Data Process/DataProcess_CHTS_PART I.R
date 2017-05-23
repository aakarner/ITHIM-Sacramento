# Author: Alex Karner and Yizheng Wu
# File: DataProcess_CHTS_PART I.R

# Purpose: Process the raw data from the 2010-2012 California Household Travel Survey.
# Impute some missing values
# Divide data into groups by race/ethnicity and household income
# Create smaller files to be used as input for ITHIM_EquityAnalysis. 

library(survey)
library(StatMatch)

# Prevent scientific notation
options(scipen = 100)

# Function definitions
rbind.data.frame.NA <- function(...) {
  # This function allows rbind to function when columns are not equivalent.
  # Missing columns are filled with NAs.
  # Courtesy of: http://marc.info/?l=r-help&m=108333217907421
  N <- unique(unlist(lapply(list(...), names)))
  result <- NULL
  for (DF in list(...)) {
    x <- as.data.frame(lapply(N, function(x) 
      if (x %in% names(DF)) DF[,x] 
      else NA))
    names(x) <- N
    result <- rbind(result,x)
  }
  result
}

# Set your working directory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data")

# -----------------------------------
# Data preparation
# -----------------------------------

# Read in the full data files and prepare smaller versions for easier ITHIM implementation
# Note that an account with NREL is necessary to access the data
# See instructions here: http://www.nrel.gov/vehiclesandfuels/secure_transportation_data.html
# Unzip and place all files in your working directory

place <- read.csv("00_CHTS2010-2012/place.csv")
persons <- read.csv("00_CHTS2010-2012/persons.csv")
hhs <- read.csv("00_CHTS2010-2012/households.csv")

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
persons$ID <- paste0(persons$sampn, persons$perno)

# Recode missing variables
persons$age[persons$age > 900] <- NA
persons$educa[persons$educa > 7] <- NA
persons$emply[persons$emply > 2] <- NA
persons$wkstat[persons$wkstat > 97] <- NA

# Note: Students may not be employed
# Create a new categorical variable, employ_stat, that takes on four values:
# 1 = student, 2 = employed, 3 = non-student, not employed, 4 = DK/refused
employ_stat <- ifelse(persons$emply == 1, 1, NA)  # Employed
employ_stat[persons$wkstat != 6 & persons$emply == 2] <- 3  # Not student and not employed
employ_stat[is.na(persons$wkstat) & persons$emply != 1] <- 4  # employed people have a NA wkstat
employ_stat[is.na(persons$emply)] <- 4  # Some students have an NA for emply
employ_stat[persons$wkstat == 6] <- 2  # Student
persons$employ_stat <- employ_stat

# Hot deck imputation of missing ages

# Split the data into donor and recipient 
# There are 201 records with age but no education or work status
# These are not useful as donors, but will be merged into the final dataset
to.merge <- persons[!is.na(persons$age) & persons$employ_stat == 4 & is.na(persons$educa), 
                    c("ID", "age", "employ_stat", "educa", "expperwgt")]

# Donors with both education and work status
donor.1 <- persons[!is.na(persons$age) & persons$employ_stat < 4 & !is.na(persons$educa), 
                   c("ID", "age", "employ_stat", "educa", "expperwgt")]

# Donors with work status but no education
donor.2 <- persons[!is.na(persons$age) & persons$employ_stat < 4 & is.na(persons$educa), 
                   c("ID", "age", "employ_stat", "expperwgt")]

# Donors with education but no work status
donor.3 <- persons[!is.na(persons$age) & persons$employ_stat > 3 & !is.na(persons$educa), 
                   c("ID", "age", "educa", "expperwgt")]

# Recipients with no education or work status - will have overall mean imputed
recip.0 <- persons[is.na(persons$age) & persons$employ_stat > 3 & is.na(persons$educa), 
                   c("ID", "age", "employ_stat", "educa", "expperwgt")]

# Recipients with both education and work status
recip.1 <- persons[is.na(persons$age) & persons$employ_stat < 4 & !is.na(persons$educa), 
                   c("ID", "employ_stat", "educa", "expperwgt")]

# Recipients with work status but no education
recip.2 <- persons[is.na(persons$age) & persons$employ_stat < 4 & is.na(persons$educa), 
                   c("ID", "employ_stat", "expperwgt")]

# Recipients with education but no work status
recip.3 <- persons[is.na(persons$age) & persons$employ_stat > 3 & !is.na(persons$educa), 
                   c("ID", "educa", "expperwgt")]

# Total number of to.merge, donor, and recipient records equals the total number in the persons file
stopifnot(
  nrow(to.merge) + nrow(donor.1) + nrow(donor.2) + nrow(donor.3) + 
    nrow(recip.0) + nrow(recip.1) + nrow(recip.2) + nrow(recip.3) ==
    nrow(persons)
)

# All variables
# Use population weights since we're solving backwards from an unpublished method in which ages were
# imputed before weights were calculated.
imp.NND.1 <- RANDwNND.hotdeck(data.rec = recip.1, data.don = donor.1,	
                              match.vars = NULL, don.class = c("employ_stat", "educa"), weight.don = "expperwgt")
rec.imp.1 <- create.fused(data.rec = recip.1, data.don = donor.1, mtc.ids = imp.NND.1$mtc.ids, z.vars="age")

# Work status but no education
imp.NND.2 <- RANDwNND.hotdeck(data.rec = recip.2, data.don = rbind(donor.1[, -4], donor.2),	
                              match.vars = NULL, don.class = c("employ_stat"), weight.don = "expperwgt")
rec.imp.2 <- create.fused(data.rec = recip.2, data.don = rbind(donor.1[, -4], donor.2), 
                          mtc.ids = imp.NND.2$mtc.ids, z.vars="age")

# Education but no work status
imp.NND.3 <- RANDwNND.hotdeck(data.rec = recip.3, data.don = rbind(donor.1[, -3], donor.3),	
                              match.vars = NULL, don.class = c("educa"), weight.don = "expperwgt")
rec.imp.3 <- create.fused(data.rec = recip.3, data.don = rbind(donor.1[, -3], donor.3), 
                          mtc.ids = imp.NND.3$mtc.ids, z.vars="age")

# Rebuild the person records
final <- rbind.data.frame.NA(to.merge, donor.1, donor.2, donor.3, recip.0, rec.imp.1, rec.imp.2, rec.imp.3)

# Finally, impute the mean age for records with no education or work status
final$age[is.na(final$age)] <- 
  weighted.mean(subset(final, !is.na(age))$age, final$expperwgt[!is.na(final$age)])

# Merge the imputed age with the person records
persons.t <- merge(persons, final[, c("ID", "age")], by.x = "ID", by.y = "ID")

names(persons.t)[151] <- "age_imputed"
names(persons.t)[6] <- "age"

# Recode races into categories relevant for the HIA 
# (1: non-Hispanic White / 2: non-Hispanic Black / 3: non-Hispanic other races / 4: Hispanic / 99: DK&RF)
persons.t$race <- ifelse(persons.t$hisp==2 & persons.t$race1==1,1,
                         ifelse(persons.t$hisp==2 & persons.t$race1 == 2, 2,
                                ifelse(persons.t$hisp==2 & persons.t$race1 %in% c(3,4,5,97),3,
                                ifelse(persons.t$hisp==1,4,99))))

# Fields needed for the implementation:
# HH: home county, tract, zip,income
# Person: age, sex, race
# Trips: trip distance, trip duration,mode
# All: weights

keep.place <- c("sampn","plano","perno","mode","tripdistance","tripdur","tcf","tcfperwgt","exptcfperwgt")
place.c <- place[,names(place)%in%keep.place]
  
keep.persons <- c("sampn","perno","ID","gend","age","age_imputed","ptrips","perwgt","expperwgt","race")
persons.c <- persons.t[,names(persons.t)%in%keep.persons]

keep.hhs <- c("sampno","ctfip","hzip","hctract","incom","htrips","hhwgt","exphhwgt")
hhs.c <- hhs[,names(hhs)%in%keep.hhs]

# Subset to create files containing only the needed columns
# with(hhs, write.csv(cbind(sampno, ctfip, hzip, hctract, incom, htrips, hhwgt, exphhwgt), "hh_sm.csv", row.names = FALSE))
# with(persons, write.csv(cbind(sampn, perno, ID, gend, age, age_imputed, ptrips, perwgt, expperwgt,race), "person_sm.csv", row.names = FALSE))
# with(place, write.csv(cbind(sampn, plano, perno, mode, tripdistance, tripdur, tcf, tcfperwgt, exptcfperwgt), "trip_sm.csv", row.names = FALSE))

# Remove the home anchor location	
place.c <- place.c[place.c$plano>1,]

# Remove missing cases
place.c <- place.c[!is.na(place.c$exptcfperwgt), ] # This shouldn't be necessary, but one record has no weight

# Create an ID variable for matching to person records
place.c$ID <- paste0(place.c$sampn, place.c$perno)

# Recode modes into categories relevant for the implementation
place.c$mode_recode <-
  ifelse(place.c$mode == 1, "Walk",
         ifelse(place.c$mode == 2, "Bike",
                ifelse(place.c$mode %in% c(3, 4), "Other non-motorized",
                       ifelse(place.c$mode == 5, "Auto driver",
                              ifelse(place.c$mode %in% c(6, 7), "Auto passenger",
                                     ifelse(place.c$mode %in% 8:14, "Private transit",
                                            ifelse(place.c$mode %in% c(15:20,22,23), "Bus and express bus",
                                                   ifelse(place.c$mode == 21, "Paratransit",
                                                          ifelse(place.c$mode == 24, "Heavy rail",
                                                                 ifelse(place.c$mode == 25, "Commuter rail",
                                                                        ifelse(place.c$mode %in% 26:28, "Light rail", "Ferry")))))))))))

# Full mode categories and from the technical appendix pp. 263-4
# 1=Walk
# 2=Bike
# 
# Other
# 3=Wheelchair / Mobility Scooter
# 4=Other Non-Motorized
#
# Auto driver
# 5=Auto / Van / Truck Driver
# Auto passenger
# 6=Auto / Van / Truck Passenger
# 7=Carpool / Vanpool
# 
# Private transit
# 8=Motorcycle / Scooter / Moped
# 9=Taxi / Hired Car / Limo
# 10=Rental Car/Vehicle
# 11=Private shuttle (SuperShuttle, employer, hotel, etc.)
# 12=Greyhound Bus
# 13=Plane
# 14=Other Private Transit

# Bus/express bus
# 15=Local Bus, Rapid Bus
# 16=Express Bus / Commuter Bus (AC Transbay, Golden Gate Transit, etc)
# 17=Premium Bus ( Metro Orange / Silver Line )
# 18=School Bus
# 19=Public Transit Shuttle (DASH, Emery Go Round, etc.)
# 20=AirBART / LAX FlyAway
# 22=Amtrak Bus
# 23=Other Bus

# Paratransit
# 21=Dial-a-Ride / Paratransit (Access Services, etc.)

# Heavy rail
# 24=BART, Metro Red / Purple Line

# Commuter rail
# 25=ACE, Amtrak, Caltrain, Coaster, Metrolink

# Light rail/other
# 26=Metro Blue / Green / Gold Line, Muni Metro, Sacramento Light Rail, San Diego Sprinter / Trolley / Orange/Blue/Green, VTA Light Rail
# 27=Street Car / Cable Car
# 28=Other Rail

# Ferry
# 29=Ferry / Boat

# Remove improbably slow (< 0.1 mph) and fast(> 10 mph) walking trips
place.c$speed <- place.c$tripdist / (place.c$tripdur / 60) # calculation of speed
sum((place.c$speed < 0.1 | place.c$speed > 10) & place.c$mode == 1, na.rm = TRUE)
place.c$exptcfperwgt[(place.c$speed < 0.1 | place.c$speed > 10) & place.c$mode == 1] <- NA

# Remove improbably slow (< 0.1 mph) and fast (> 24 mph) cycling trips
sum((place.c$speed < 0.1 | place.c$speed > 24) & place.c$mode == 2, na.rm = TRUE)
place.c$exptcfperwgt[(place.c$speed < 0.1 | place.c$speed > 24) & place.c$mode == 2] <- NA

# Persons file
# Recode age into eight categories, consistent with ITHIM
persons.c$age8cat <-
  ifelse(persons.c$age_imputed <= 4, 1,
         ifelse(persons.c$age_imputed <= 14, 2,
                ifelse(persons.c$age_imputed <= 29, 3,
                       ifelse(persons.c$age_imputed <= 44, 4,
                              ifelse(persons.c$age_imputed <= 59, 5,
                                     ifelse(persons.c$age_imputed <= 69, 6,
                                            ifelse(persons.c$age_imputed <= 79, 7, 8)))))))

# Add a column of ones
persons.c$ones <- rep(1, nrow(persons.c))

# Create a data frame containing merged household and person data
persons.hhs <- merge(persons.c, hhs.c, by.x = "sampn", by.y = "sampno")

# Combing gender & race into one variable (gender.race)
# 1, male, non-Hispanic white
# 2, female, non-Hispanic white
# 3, male, non-Hispanic black
# 4, female, non-Hispanic black
# 5, male, non-Hispanic other races
# 6, female, non-Hispanic other races
# 7, male, Hispanic
# 8, female, Hispanic
# 99, DK/RF 
persons.hhs$gender.race <-
  ifelse(persons.hhs$gend == 1 & persons.hhs$race == 1,1,
         ifelse(persons.hhs$gend == 2 & persons.hhs$race == 1,2,
                ifelse(persons.hhs$gend == 1 & persons.hhs$race == 2,3,
                       ifelse(persons.hhs$gend == 2 & persons.hhs$race == 2,4,
                              ifelse(persons.hhs$gend == 1 & persons.hhs$race == 3,5,
                                     ifelse(persons.hhs$gend == 2 & persons.hhs$race == 3,6,
                                            ifelse(persons.hhs$gend == 1 & persons.hhs$race == 4,7,
                                                   ifelse(persons.hhs$gend == 2 & persons.hhs$race == 4,8,99))))))))

# Combing gender & income into one variable (gender.inc)
# 1, male, low income hh (incom group 1 + group 2; <$25,000)
# 2, female, low income hh (incom group 1 + group 2; <$25,000)
# 3, male, other income categories
# 4, female, other income categories
# 5,
# 6,
# 7,
# 8,
# 99, DK/RF
persons.hhs$gender.inc <-
  ifelse(persons.hhs$gend == 1 & persons.hhs$incom %in% c(1:3),1,
         ifelse(persons.hhs$gend == 2 & persons.hhs$incom %in% c(1:3),2,
                ifelse(persons.hhs$gend == 1 & persons.hhs$incom %in% c(4:5),3,
                       ifelse(persons.hhs$gend == 2 & persons.hhs$incom %in% c(4:5),4,
                              ifelse(persons.hhs$gend == 1 & persons.hhs$incom %in% c(6:7),5,
                                     ifelse(persons.hhs$gend == 2 & persons.hhs$incom %in% c(6:7),6,
                                            ifelse(persons.hhs$gend == 1 & persons.hhs$incom %in% c(8:10),7,
                                                   ifelse(persons.hhs$gend == 2 & persons.hhs$incom %in% c(8:10),8,99))))))))


# Create a data frame containing merged trip, household, and person data
# CA.trips.c <- merge(persons.c, hhs.c, by.x = "sampn", by.y = "sampno")
CA.trips <- merge(persons.hhs, place.c, by.x = "ID", by.y = "ID", all = TRUE)

# We're only interested in trips
# Remove table elements that don't represent trips
sum(is.na(CA.trips$mode))
CA.trips <- CA.trips[!is.na(CA.trips$exptcfperwgt), ]

# Create requisite complex survey objects
CA.trips.svy <- svydesign(id = ~ID, weights = ~exptcfperwgt, data = CA.trips)
CA.persons.svy <- svydesign(id = ~ID, weights = ~expperwgt, data = persons.hhs)
save(CA.trips.svy, CA.persons.svy, file = "00_CHTS2010-2012/TestInc_Processed_CHTS_2010-2012.RData")
# Save the complex survey objects to an .RData file
# Just load this in the future instead of running the data preparation section of the script
#save(CA.trips.svy, CA.persons.svy, file = "00_CHTS2010-2012/Processed_CHTS_2010-2012.RData")







