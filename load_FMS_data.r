#loads data
#1. all PIT tagged FMS in GCMRC database for mark-recapture analysis
#2. all flannelmouth to look at size structure, maturity, etc.
#Author: Jan Boyer, AGFD, jboyer@azgfd.gov
#Inputs: most recent version of big boy (access database)
#Outputs: "all_PIT_tagged_flannelmouth.csv"
#         "all_flannelmouth.csv"
#Dependencies:
#Note: MUST USE 32-bit R (and version 1.1.x or older of Rstudio, newer versions
#      of Rstudio only run 64-bit R) to use RODBC to connect to access database


library(RODBC) #database interface
library(tidyverse)

#load data ######