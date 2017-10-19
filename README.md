# ITHIM_Sacramento

Application of ITHIM for Sacramento County

## Source code ##

### Core functions ###

Physical activity module: */00_R Scripts/EquityAnalysis_ITHIM_PA.R*

Traffic injury module: */00_R Scripts/EquityAnalysis_ITHIM_Injuries_TwoRaces.R*

### Data preparation ###

1. California Department of Public Health ([CDPH](http://https://www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx)) Statistic Vitals: */00_R scripts/Data Process/DataProcess_CDPH.R*

2. California Health Interview Survey ([CHIS](http://http://healthpolicy.ucla.edu/chis/Pages/default.aspx)): */00_R scripts/Data Process/DataProcess_CHIS.R*

3. Hot Deck Imputation of CDPH and SACSIM based on [PUMS](https://www.census.gov/programs-surveys/acs/data/pums.html): */00_R scripts/Data Process/DataProcess_PUMS.R*

4. Process output from [SACSIM](http://www.sacog.org/sites/main/files/file-attachments/plnrscmte_sacog_travel_model_wkshp_27mar2014.pdf): */00_R scripts/Data Process/DataProcess_CustomizeSACSIM.R*

5. Internet Statewide Integrated Traffic Records System ([SWITRS](https://www.chp.ca.gov/programs-services/services-information/switrs-internet-statewide-integrated-traffic-records-system)): */00_R scripts/Data Process/DataProcess_SWITRS.R*

## Data for a future update ##

For a future update, the user is required to update the data files in the following folders:

1. Population: *01_Data/06_Equity Analysis/01_Population/*

2. Active Transport: *01_Data/06_Equity Analysis/02_ActiveTransport/* (Processed by the script *DataProcess_CustomizeSACSIM.R*)

3. non Travel MET: *01_Data/06_Equity Analysis/03_nonTravelMET/* (Processed by the script *DataProcess_CHIS.R*)

4. Global Burden of Disease ([GBD](http://www.healthdata.org/gbd)): *01_Data/06_Equity Analysis/04_GBD/*

5. Baseline injury: *01_Data/06_Equity Analysis/05_baseline injury/* (Processed by the script *DataProcess_SWITRS.R*)

6. Travel distance by each traffic mode: *01_Data/06_Equity Analysis/06_PersonVehicleDistance/* (Processed by the script *DataProcess_CustomizeSACSIM.R*)







