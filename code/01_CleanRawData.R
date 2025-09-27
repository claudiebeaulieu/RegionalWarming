################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# Script to import the raw data and clean up into 3-D arrays for the gridded 
# datasets and a data table for the global mean time series.

## Gridded datasets #####

dataset_name=c("NASA","HadCRUT","NOAA","Berkeley","DCENT")
Tas_varname = c("tempanomaly","tas_mean","air","temperature","temperature") # The variables we want from the different gridded datasets
lon_varname = c("lon","longitude","lon","longitude","lon")
lat_varname = c("lat","latitude","lat","latitude","lat")
time_varname = "time"
start_year = c(1880,1850,1850,1850,1850)
end_year = 2024

file_paths=c("./data/raw/gistemp1200_GHCNv4_ERSSTv5.nc",
             "./data/raw/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc",
             "./data/raw/air.mon.anom.nc",
             "./data/raw/Land_and_Ocean_LatLong1.nc",
             "./data/raw/DCENT_ensemble_1850_2024_ensemble_mean.nc")

for(i in 1:5){
ImportncData(file_paths[i],dataset_name[i],Tas_varname[i],lon_varname[i],lat_varname[i],time_varname,start_year[i],end_year)
}
# For each dataset listed above, a file named annual_*dataset_name*_anom.RData containing annual anomalies has been created 
# in /data/processed. Those are the intermediate datasets used in this study, not saved in the repository as they are too large.


## Regridding the finer resolution datasets to coarse resolution #####


# Regrid Berkeley and NASA to a 5x5 degree grid
target_lat = seq(from=-87.5,by=5,to=87.5)
target_lon = seq(from=-177.5,by=5,to=177.5)
load('./data/processed/annual_Berkeley_anom.RData')
tas_regridded = RegridData(lat,lon,time,tas_annual)
save(file='./data/processed/annual_Berkeley_anom_regridded.RData',target_lat,target_lon,time,tas_regridded)

load('./data/processed/annual_NASA_anom.RData')
tas_regridded = RegridData(lat,lon,time,tas_annual)
save(file='./data/processed/annual_NASA_anom_regridded.RData',target_lat,target_lon,time,tas_regridded)


## GMST time series #####

##### Gather the GMST from different institutions and create a dataframe.

Tanom_annual_df = matrix(data=NA,nrow=175,ncol=6)

#HadCRUT
data = read.csv("./data/raw/HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")
#https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/download.html

tmp = data[1:175,2]

Tanom_annual_df[,1] = 1850:2024
Tanom_annual_df[,3] = tmp

#NASA
data = read.table("./data/raw/GMST_NASA_1850_2024.txt")
#https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt
Tanom_annual_df[31:175,2] = data[,2]

#NOAA
data=read.table("./data/raw/aravg.ann.land_ocean.90S.90N.v6.0.0.202507.asc")
#https://www.ncei.noaa.gov/data/noaa-global-surface-temperature/v6/access/timeseries/
Tanom_annual_df[,4] = data[1:175,2]

#Berkeley
data=read.table("./data/raw/GMST_Berk_1850_2024.txt")
#https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_summary.txt
Tanom_annual_df[,5] = data[1:175,2]

#DCENT
data = read.table("./data/raw/GMST_DCENT_1850_2024.txt",sep=",")
#https://duochanatharvard.github.io/research_01_DCENT.html
Tanom_annual_df[,6] = data[1:175,2]

Tanom_annual_df = as.data.frame(Tanom_annual_df)
names(Tanom_annual_df) = c("year","NASA","HadCRUT","NOAA","Berkeley","DCENT")

save(file="./data/processed/temperature_anomalies_1850_2024_global.RData",Tanom_annual_df)