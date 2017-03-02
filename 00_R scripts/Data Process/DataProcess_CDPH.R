# Author: Alex Karner and Yizheng Wu
# File: DataProcess_CDPH.R
# Purpose: Calculate age-sex-race specific deathrates by county for traffic deaths and diseases related to
# physical activity and air pollution.
# NB: The input files specified here must be obtained for a fee from CDPH and are confidential. 
# Additionally, we have heavily processed them to combine diseases into relevant categories from the 
# Global Burden of Disease Study. 

# Library definitions
library(lattice)
library(R2HTML)

