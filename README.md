# ITHIM_Sacramento

Application of ITHIM for Sacramento County

## Source code ##

### Core functions ###

Physical activity module: */00_R Scripts/EquityAnalysis_ITHIM_PA.R*

Traffic injury module: */00_R Scripts/EquityAnalysis_ITHIM_Injuries_TwoRaces.R*

### Data preparation ###

California Department of Public Health ([CDPH](http://https://www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx)) Statistic Vitals: */00_R scripts/Data Process/DataProcess_CDPH.R*

California Health Interview Survey ([CHIS](http://http://healthpolicy.ucla.edu/chis/Pages/default.aspx)): */00_R scripts/Data Process/DataProcess_CHIS.R*

Hot Deck Imputation of CDPH and SACSIM based on [PUMS](https://www.census.gov/programs-surveys/acs/data/pums.html): */00_R scripts/Data Process/DataProcess_PUMS.R*

Process output from [SACSIM](http://www.sacog.org/sites/main/files/file-attachments/plnrscmte_sacog_travel_model_wkshp_27mar2014.pdf): */00_R scripts/Data Process/DataProcess_CustomizeSACSIM.R*

Internet Statewide Integrated Traffic Records System ([SWITRS](https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system)): */00_R scripts/Data Process/DataProcess_SWITRS.R*

## Data for a future update ##

Population: *01_Data/06_Equity Analysis/01_Population/*

Active Transport: *01_Data/06_Equity Analysis/02_ActiveTransport/*

non Travel MET: *01_Data/06_Equity Analysis/03_nonTravelMET/*

Global Burden of Disease ([GBD](http://www.healthdata.org/gbd)): *01_Data/06_Equity Analysis/04_GBD/*

Baseline injury: *01_Data/06_Equity Analysis/05_baseline injury/*

Travel distance by each traffic mode: *01_Data/06_Equity Analysis/06_PersonVehicleDistance/*







