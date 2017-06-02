# Author: Alex Karner and Yizheng Wu
# File: DataProcess_CHTS_PART II.R
# Purpose: output the .csv file which is required in Equity Anlysis module
 
library(survey)

# Prevent scientific notation
options(scipen = 100)

# Set your working directory
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data")

# load the data processed by part I
load("00_CHTS2010-2012/TestInc_Processed_CHTS_2010-2012.RData")

#################################### Function Definition ######################################

# calculate the relative values (relative to the value of "female 15-29")
CalRelativeMatrix <- function(x){
  for (i in 1:4){
    ref <- x[3,2*i] #female in the category of 15~29 years old
    x[,c((2*i-1):(2*i))] <- x[,c((2*i-1):(2*i))]/ref
  }
  return(x)
}

# main function for output the active travel data after inputting the county code and demographic categories
ActiveTravelDataOutput <- function(Demo,CountyCode){
  # 2 genders, 12 mode categories, 8 age categories, 4 race categories, 4 income categories
  
  # numeric matrix for travel time
  travel.times.by.demo <- matrix(nrow = 96, ncol = 8)
  # travel.times.err.by.race <- matrix(nrow = 96, ncol = 4)
  
  # numeric matrix for travel distance
  travel.distance.by.demo <- matrix(nrow = 96, ncol = 8)
  # travel.distance.err.by.race <- matrix(nrow = 96, ncol = 4)
  
  # set the demo ID
  demo.ID<- ifelse(Demo=="Race",demo.ID<- 20,
                   ifelse(Demo=="Income", demo.ID <- 21,
                          message("Please type 'Race' or 'Income'")))
  
  for(i in 1:8) { # gender and demo
    print(paste0("i is ", i))
    for (j in 1:12) { # mode category
      print(paste0("j is ", j))
      for (k in 1:8) { # age category
        time <- try(svytotal(~tripdur, subset(CA.trips.svy, 
                                              CA.trips.svy$variables[demo.ID] == i & age8cat == k & ctfip %in% CountyCode
                                              & mode_recode == levels(factor(CA.trips.svy$variables$mode_recode))[j]), 
                             na.rm = TRUE), silent = TRUE)
        # if there are no trips in this category, return 0, otherwise return the total trip duration by
        # age-sex-demo category
        travel.times.by.demo[k + 8 * (j - 1), i] <- 
          ifelse(class(time) == "try-error", 0, coef(time))
        #travel.times.err.by.race[k + 8 * (j - 1), i] <-
        #ifelse(class(time) == "try-error", 0, SE(time))
        
        dist <- try(svytotal(~tripdistance, subset(CA.trips.svy, 
                                                   CA.trips.svy$variables[demo.ID] == i & age8cat == k & ctfip %in% CountyCode 
                                                   & mode_recode == levels(factor(CA.trips.svy$variables$mode_recode))[j]), 
                             na.rm = TRUE), silent = TRUE) 
        # if there are no trips in this category, return 0, otherwise return the total trip distance by
        # age-sex-demo category
        travel.distance.by.demo[k + 8 * (j - 1), i] <- 
          ifelse(class(dist) == "try-error", 0, coef(dist))
        #travel.distance.err.by.race[k + 8 * (j - 1), i] <-
        #ifelse(class(dist) == "try-error", 0, SE(dist))
      }
    }
  }
  
  # Create a table for population counts
  if (demo.ID==20){
    pop.age.gender.demo <- svytable(~age8cat + gender.race, subset(CA.persons.svy, ctfip %in% CountyCode))
  } else{
    pop.age.gender.demo <- svytable(~age8cat + gender.inc, subset(CA.persons.svy, ctfip %in% CountyCode))
  }
  
  # compute the travel time and distance per capita
  walk.time.byDemo <- travel.times.by.demo[c(89:96),]/pop.age.gender.demo[,c(1:8)]
  walk.distance.byDemo <- travel.distance.by.demo[c(89:96),]/pop.age.gender.demo[,c(1:8)]
  cycle.time.byDemo <- travel.times.by.demo[c(25:32),]/pop.age.gender.demo[,c(1:8)]
  cycle.distance.byDemo <- travel.distance.by.demo[c(25:32),]/pop.age.gender.demo[,c(1:8)]
  pop.age.gender.demo <- pop.age.gender.demo[,c(1:8)]
  
  
  # compute the relative values
  re.walk.time.byDemo <- CalRelativeMatrix(walk.time.byDemo)
  re.walk.distance.byDemo <- CalRelativeMatrix(walk.distance.byDemo)
  re.cycle.time.byDemo <- CalRelativeMatrix(cycle.time.byDemo)
  re.cycle.distance.byDemo <- CalRelativeMatrix(cycle.distance.byDemo)
  
  # compute the speed (mph) 
  walk.speed.byDemo <- walk.distance.byDemo/(walk.time.byDemo/60)
  walk.speed.byDemo[is.na(walk.speed.byDemo)] <- 0
  cycle.speed.byDemo <- cycle.distance.byDemo/(cycle.time.byDemo/60)
  cycle.speed.byDemo[is.na(cycle.speed.byDemo)] <- 0
  
  # compute the relative value for speed
  re.walk.speed.byDemo <- CalRelativeMatrix(walk.speed.byDemo)
  re.cycle.speed.byDemo <- CalRelativeMatrix(cycle.speed.byDemo)
  
  # add dimnames
  temp.gender <- rep(c("male","female"),4)
  dimname <- list(c(paste0("ageCat",1:8)),paste0(temp.gender,".Demo.",rep(1:4,each=2)))
  dimnames(re.walk.time.byDemo)<-dimnames(re.cycle.time.byDemo)<-
  dimnames(re.walk.speed.byDemo)<-dimnames(re.cycle.speed.byDemo)<-dimnames(pop.age.gender.demo)<-dimname
  
  return(list(
    travel.distance.by.demo = travel.distance.by.demo,
    travel.times.by.demo = travel.times.by.demo,
    pop.age.gender.demo = pop.age.gender.demo,
    re.walk.time.byDemo = re.walk.time.byDemo,
    re.cycle.time.byDemo = re.cycle.time.byDemo,
    re.walk.speed.byDemo = re.walk.speed.byDemo,
    re.cycle.speed.byDemo = re.cycle.speed.byDemo
  ))
  
}

#################################### Output ######################################

# Define geographic identifiers
SJV.counties <- c(6107, 6047, 6039, 6019, 6077, 6031, 6029, 6099)
SAC.counties <- c(6017,6061,6067,6101,6113,6115)

Travel.Output.byRace <- ActiveTravelDataOutput("Race",SJV.counties)
Travel.Output.byIncome <- ActiveTravelDataOutput("Income",SAC.counties)


# Output the .csv files for active transport data and population
cuttingline <- matrix(" ",1,9)

write.csv(rbind(
  cbind(Travel.Output.byRace$re.walk.time.byDemo,c("relative.walk.time",rep("",7))),cuttingline, #relative walking time
  cbind(Travel.Output.byRace$re.cycle.time.byDemo,c("relative.cycle.time",rep("",7))),cuttingline, # relative cycling time
  cbind(Travel.Output.byRace$re.walk.speed.byDemo,c("relative.walk.speed",rep("",7))),cuttingline, # relative walking speed
  cbind(Travel.Output.byRace$re.cycle.speed.byDemo,c("relative.cycle.speed",rep("",7))) # relative cycling speed
  ),file = "04_Equity Analysis/test_at_race.csv")

write.csv(rbind(
  cbind(Travel.Output.byIncome$re.walk.time.byDemo,c("relative.walk.time",rep("",7))),cuttingline, #relative walking time
  cbind(Travel.Output.byIncome$re.cycle.time.byDemo,c("relative.cycle.time",rep("",7))),cuttingline, # relative cycling time
  cbind(Travel.Output.byIncome$re.walk.speed.byDemo,c("relative.walk.speed",rep("",7))),cuttingline, # relative walking speed
  cbind(Travel.Output.byIncome$re.cycle.speed.byDemo,c("relative.cycle.time",rep("",7))) # relative cycling speed
),file = "04_Equity Analysis/test_at_income.csv")

write.csv(Travel.Output.byRace$pop.age.gender.demo,file = "04_Equity Analysis/test_pop_race.csv")
write.csv(Travel.Output.byIncome$pop.age.gender.demo,file = "04_Equity Analysis/test_pop_income.csv")

