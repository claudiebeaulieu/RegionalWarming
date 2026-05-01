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
Tas_varname = c("tempanomaly","tas_mean","air","temperature","ts_mean") # The variables we want from the different gridded datasets
lon_varname = c("lon","longitude","lon","longitude","lon")
lat_varname = c("lat","latitude","lat","latitude","lat")
time_varname = "time"
start_year = c(1880,1850,1850,1850,1850)
end_year = c(2024,2024,2024,2024,2024)
# We keep end_year in 2024 for all datasets to be consistent. 
# Most of them have been updated this year (2026) and have data until 2025 except for Berkeley.

file_paths=c("./data/raw/gistemp1200_GHCNv4_ERSSTv5.nc",
             "./data/raw/HadCRUT.5.1.0.0.analysis.anomalies.ensemble_mean.nc",
             "./data/raw/air.mon.anom.nc",
             "./data/raw/Land_and_Ocean_LatLong1.nc",
             "./data/raw/DCENT_I_1.1.0.0_mean_spread_tas.nc")

for(i in 1:5){
ImportncData(file_paths[i],dataset_name[i],Tas_varname[i],lon_varname[i],lat_varname[i],time_varname,start_year[i],end_year[i])
}
# For each dataset listed above, a file named annual_*dataset_name*_anom.RData containing annual anomalies has been created 
# in /data/processed. Those are the intermediate datasets used in this study, not saved in the repository as they are too large.

## Get the Berkeley dataset landmask

# Open the file
nc = nc_open("./data/raw/Land_and_Ocean_LatLong1.nc")

# Extract the land mask (latitude x longitude matrix)
land_mask = ncvar_get(nc, "land_mask")
lat = ncvar_get(nc, "latitude")
lon = ncvar_get(nc, "longitude")
nc_close(nc)
# Create a binary mask: 1 = land (>50% land fraction), 0 = ocean
land_binary = ifelse(land_mask > 0.5, 1, 0)
save(file='./data/processed/Berkeley_landmask.RData',land_mask,land_binary,lat,lon)


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


## Ensembles ####

# DCENT ensemble members
Tas_varname = "ts" # The variables we want from the different gridded datasets
lon_varname = "lon"
lat_varname = "lat"
time_varname = "time"
start_year = 1850
end_year = 2025

for(i in 1:200){
  filepath = sprintf("./data/raw/DCENTI_Ensemble_Members/DCENT_I_1.1.0.0_member_%03d.nc", i)
  dataset_name = sprintf("DCENT_I_1.1.0.0_member_%03d", i)
  ImportncData(filepath,dataset_name,Tas_varname,lon_varname,lat_varname,time_varname,start_year,end_year)
}

# HadCRUT ensemble members
Tas_varname = "tas" 
lon_varname = "longitude"
lat_varname = "latitude"
time_varname = "time"
start_year = 1850
end_year = 2025

for(i in 1:200){
  filepath = paste0("./data/raw/HadCRUT_Ensemble_Members/HadCRUT.5.1.0.0.analysis.anomalies.", i, ".nc")
  dataset_name = paste0("HadCRUT.5.1.0.0_member_", i)
  ImportncData(filepath,dataset_name,Tas_varname,lon_varname,lat_varname,time_varname,start_year,end_year)
}


## GMST time series #####

##### Gather the GMST from different institutions and create a dataframe. 
year = 1850:2025
n = length(year)
Tanom_annual_df = matrix(data=NA,nrow=n,ncol=6)

#HadCRUT
data = read.csv("./data/raw/HadCRUT.5.1.0.0.analysis.summary_series.global.annual.csv")
#https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/download.html

tmp = data[1:n,2] # we get temperature anomaly
Tanom_annual_df[,1] = year
Tanom_annual_df[,3] = tmp

#NASA
data = read.table("./data/raw/GMST_NASA_1850_2025.txt",header=T)
#https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt
Tanom_annual_df[31:n,2] = data$J.D/100

#NOAA
data=read.table("./data/raw/aravg.ann.land_ocean.90S.90N.v6.1.0.202603.asc")
#https://www.ncei.noaa.gov/data/noaa-global-surface-temperature/v6.1/access/timeseries/aravg.ann.land_ocean.90S.90N.v6.1.0.202603.asc
Tanom_annual_df[,4] = data[1:n,2]

#Berkeley
data=read.table("./data/raw/GMST_Berk_1850_2024.txt")
#https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_summary.txt
Tanom_annual_df[1:(n-1),5] = data[1:(n-1),2] # This data has not been updated till 2025.

#DCENT
data = read.table("./data/raw/DCENT_DCENT_I_GMST_1850_2025.txt",sep=",",header=T)
#https://dcent-i.github.io/
Tanom_annual_df[,6] = data[1:n,2]

Tanom_annual_df = as.data.frame(Tanom_annual_df)
names(Tanom_annual_df) = c("year","NASA","HadCRUT","NOAA","Berkeley","DCENT")

save(file="./data/processed/temperature_anomalies_1850_2025_global.RData",Tanom_annual_df)

# We also save until 2024 to be consistent with gridded datasets - Berkeley gridded data has not been updated to include 2025
Tanom_annual_df = Tanom_annual_df[-n,]
save(file="./data/processed/temperature_anomalies_1850_2024_global.RData",Tanom_annual_df)
