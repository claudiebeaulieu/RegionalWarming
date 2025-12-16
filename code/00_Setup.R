################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# Script to load libraries and functions that will be used in this project.

# load libraries #####

library(ncdf4)
library(terra)
library(oce)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(forecast)
library(nlme)
library(oceanmap)
library(RColorBrewer)
library(viridis)


# load functions #####

source('./code/ImportncData.R')
source('./code/RegridData.R')
source('./code/method/PELTtrendARpJOIN.R')
source('./code/ProcessGriddedTemp.R')
source('./code/method/CptDifficulty.R')
source('./code/method/SimsFunctions.R')
source('./code/PlotsFunctions.R')

