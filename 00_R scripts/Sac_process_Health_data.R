# Author: Alex Karner & Yizheng Wu
# File: Sac_process_GBD_data.R
# Purpose: Process data from the Global Burden of Disease study to be used as input
# for the Sacramento County implementation. 
# NB: This script uses a pre-processed data file as input that's availabel on github:
# https://github.com/aakarner/fresno-hia/tree/master/Data/WHO%20Global%20Burden%20of%20Disease

# Library definitions
library(Rmisc)


# Set working directory
setwd("~/Documents/02_Work/13_ITHIM/03_Data/01_GDB")

# -----------------------------------
# Data preparation
# -----------------------------------
gbd.2010 <- read.table("IHME_USA_GBD_2010_ITHIM diseases, non-overlapping ages.csv", header = TRUE, 
                       sep = ",", stringsAsFactors = TRUE)
levels(gbd.2010$age_name)

# Recode age levels in the age factor

# The categories we want are:
# 0-4
# 5-14
# 15-29
# 30-44
# 45-59
# 60-69
# 70-79
# 80+

levels(gbd.2010$age_name) <- 
  c("15-29", "15-29", "15-29", "30-44", "30-44", "30-44", "45-59", "5-14", "45-59", "45-59", 
    "60-69", "60-69", "70-79", "70-79", "80+", "All ages", "0-4")

# Recode disease levels in the disease factor

levels(gbd.2010$cause_medium) <- c("Alzheimers disease", "Asthma and COPD", "Breast cancer", 
                                   "Colorectal cancer", "Asthma and COPD", "Diabetes", "Isch Stroke", 
                                   "Hypertensive HD", "IHD", "Hemor Stroke", 
                                   "Upper and lower respiratory", "Lung cancer", "Asthma and COPD", 
                                   "Road injury", "Road injury", "Rheumatic HD", "Road injury", 
                                   "Stroke", "Depression", "Upper and lower respiratory")

# Create a new table using the updated categories
gbd.2010.agg <- summarySE(gbd.2010, measurevar="nm_mean", groupvars=c("cause_medium", "year","age_name", "sex", "measure"),
               .drop=FALSE)
gbd.2010.agg$sums <- gbd.2010.agg$N*gbd.2010.agg$nm_mean
gbd.2010.agg <- gbd.2010.agg[, c(1:5,11)]


# require(plyr)
# gbd.2010.agg = ddply(gbd.2010, .(cause_medium, year, age_name, sex, measure), 
#                      function(x) c(nm = sum(x$nm_mean)),.drop=FALSE)
# 
# attach(gbd.2010)
# gbd.2010.agg <- tapply(nm_mean,list(cause_medium, age_name, sex, measure),sum,na.rm = TRUE)
# test<-as.data.frame(gbd.2010.agg)
# 
# library(reshape)
# head(test)
# mdata <- melt(test,id=c("","",""))
# # Create a new table using the updated categories
# agg.results <- with(gbd.2010, tapply(nm_mean, list(cause_medium, age_name, sex, measure), 
#                                      sum, na.rm = TRUE))
# 
# # Write the output to a csv file
# write.csv(as.data.frame(agg.results), "GBD2010_clean.csv", row.names = FALSE)






