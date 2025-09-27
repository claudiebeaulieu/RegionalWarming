############################
# Regional warming project #
############################

This repository contains the code required to perform the data analysis and create
the figures for Beaulieu, Johnson, Killick, Lanzante and Knutson, currently under
review at NCC.

The main goal of this project is to determine regions that have undergone a 
warming acceleration since 1970.

Code to reproduce the results and figures is in the /code folder. Steps are as follow:

(0) The script to setting up libraries and load functions to be used in the main scripts in 00_Setup.R

(1) The script to process the raw monthly gridded surface temperature files and computing
annual anomalies, regrid the fine resolution ones to  5x5 degrees, as well as load the global mean surface 
temperature time series is in 01_CleanRawData.R.

(2) The script to process monthly gridded annual surface temperature anomalies and 
analyze for changes in therate of warming via changepoint detection and via fitting 
quadratic trends is in 02_AnalysisGriddedDatasets.R. 

(3) The script to create and analyze regional surface temperature time series as 
illustrated in Figure 4 is in 03_AnalysisRegionalTS.R

(4) The script to analyze global surface temperature time series as illustrated in Figure S1 
is in 04_AnalysisGlobalTS.R. Note that this code isn't necessary to recreate results presented in the main paper.

(5) The script to conduct a simulation study (Supplementary) to eveluate the true and 
false positives is in 05_SimsBerkeley.R

(6) Figures showing results from analysing temperature observations are produced in
06_PlotsTempResults.R

(7) Figure showing simulation results are produced in 07_PlotsSims.R


Please contact Claudie Beaulieu (beaulieu@ucsc.edu) for comments, suggestions, etc.

Credits:

This package was developed in collaboration with Rebecca Killick, Lancaster University.

