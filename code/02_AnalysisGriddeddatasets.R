################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# Script that goes through the different processed gridded datasets and analyze
# for the presence of changes in the rate of warming. For each dataset, this 
# is done with changepoint analysis, then by fitting a quadratic trend.

year = seq(1970,2024)# we conduct the analysis until 2024 to be consistent across dataset

## Analyze HadCRUT dataset #####

load('./data/processed/annual_HadCRUT_anom.RData')
data = tas_annual[,,which(time == 1970):which(time == 2024)]#we extract the data over the 1970-2024 period
results = st.cpts(data,lon,lat,year,"BIC")
save(file='./results/ResultsHadCRUT.RData',results)

results = st.quad(data,lon,lat,year)
save(file='./results/ResultsquadHadCRUT.RData',results)

## Analyze NASA dataset #####

load('./data/processed/annual_NASA_anom.RData')
data = tas_annual[,,which(time == 1970):which(time == 2024)]#we extract the data over the 1970-2024 period
results = st.cpts(data,lon,lat,year,"BIC")
save(file='./results/ResultsNASA.RData',results)

results = st.quad(data,lon,lat,year)
save(file='./results/ResultsquadNASA.RData',results)

## Analyze NOAA dataset #####

load('./data/processed/annual_NOAA_anom.RData')
data = tas_annual[,,which(time == 1970):which(time == 2024)]#we extract the data over the 1970-2024 period
results = st.cpts(data,lon,lat,year,"BIC")
save(file='./results/ResultsNOAA.RData',results)

results = st.quad(data,lon,lat,year)
save(file='./results/ResultsquadNOAA.RData',results)

## Analyze Berkeley dataset #####

load('./data/processed/annual_Berkeley_anom.RData') #BIC penalty
data = tas_annual[,,which(time == 1970):which(time == 2024)]#we extract the data over the 1970-2024 period
results = st.cpts(data,lon,lat,year,"BIC")
save(file='./results/ResultsBerkeley.RData',results)

results = st.cpts(data,lon,lat,year,3*log(length(year))) #reduced penalty
save(file='./results/ResultsBerkeley_3Pen.RData',results)

results = st.quad(data,lon,lat,year) #quadratic trend
save(file='./results/ResultsquadBerkeley.RData',results)

## Analyze DCENT dataset #####

load('./data/processed/annual_DCENT_anom.RData')
data = tas_annual[,,which(time == 1970):which(time == 2024)]#we extract the data over the 1970-2024 period
results = st.cpts(data,lon,lat,year,"BIC")
save(file='./results/ResultsDCENT.RData',results)

results = st.quad(data,lon,lat,year)
save(file='./results/ResultsquadDCENT.RData',results)

## Analyze regridded datasets to a coarse resolution #####

load('./data/processed/annual_Berkeley_anom_regridded.RData')
data = tas_regridded[,,which(time == 1970):which(time == 2024)]#we extract the data over the 1970-2024 period
results = st.cpts(data,target_lon,target_lat,year,"BIC")
save(file='./results/ResultsBerkeleyRegridded.RData',results)

load('./data/processed/annual_NASA_anom_regridded.RData')
data = tas_regridded[,,which(time == 1970):which(time == 2024)]#we extract the data over the 1970-2024 period
results = st.cpts(data,target_lon,target_lat,year,"BIC")
save(file='./results/ResultsNASARegridded.RData',results)

## Combine results from all datasets in coarse resolution #####
names=c("NOAA","DCENT","HadCRUT","BerkeleyRegridded","NASARegridded")
ycpts_array = array(data=NA,dim=c(72,36,10))
dift_array = array(data=NA,dim=c(72,36,10))
ncpts_array = array(data=NA,dim=c(72,36,5))
load('./data/processed/annual_DCENT_anom.RData')#this is to have the longitude for NOAA and DCENT to be shifted
for(k in 1:5){
  load(paste0("./results/Results",names[k],".RData"))
  ycpts_array[,,(k*2-1):(k*2)] = results$ycpts[,,1:2]
  dift_array[,,(k*2-1):(k*2)] = results$dift[,,1:2]
  ncpts_array[,,k] = results$ncpts
  
  if(k <= 2){ #datasets need to be shifted to lon -180-180
    ycpt1_shifted = matrixShiftLongitude((results$ycpts[,,1]),lon)
    ycpt2_shifted = matrixShiftLongitude((results$ycpts[,,2]),lon)
    ycpts_array[,,(k*2-1)] = ycpt1_shifted$m
    ycpts_array[,,(k*2)] = ycpt2_shifted$m
    
    dift1_shifted = matrixShiftLongitude(results$dift[,,1],lon)
    dift2_shifted = matrixShiftLongitude(results$dift[,,2],lon)
    dift_array[,,(k*2-1)] = dift1_shifted$m
    dift_array[,,(k*2)] = dift2_shifted$m
    
    ncpts_shifted = matrixShiftLongitude(results$ncpts,lon)
    ncpts_array[,,k] = ncpts_shifted$m
  }
}
na_counts = apply(ncpts_array,1:2,function(x) sum(x>0,na.rm=T))
na_counts[na_counts == 0] = NA

my = apply(ycpts_array,1:2,mean,na.rm=T)
mt = apply(dift_array,1:2,mean,na.rm=T)

combined_results = list(na_counts,my,mt)
names(combined_results) = c("agree","ycpts","dift")
save(file="./results/ResultsCombined.RData",combined_results)


## Analyze DCENT ensemble members ####
year = seq(1970,2024)
for(i in 1:200){
  filepath = sprintf("./data/processed/annual_DCENT_I_1.1.0.0_member_%03d_anom.RData", i)
  load(filepath)
  data = tas_annual[,,which(time == 1970):which(time == 2024)]
  results = st.cpts(data,lon,lat,year,"BIC")
  save(file=sprintf("./results/ResultsDCENT_member_%03d.RData",i),results)
}

# Loop that goes through and stack results
year = seq(1970,2024)
nlon = dim(results$ncpts)[1]
nlat = dim(results$ncpts)[2]
ncpts = array(data=NA,dim=c(nlon,nlat,200))
ycpts = array(data=NA,dim=c(nlon,nlat,200))
for(k in 1:200){
  load(file=sprintf("./results/ResultsDCENT_member_%03d.RData",k))
  ncpts[,,k] = results$ncpts
  ycpts[,,k] = results$ycpts[,,1]
}

#compute the number of cp detected per grid cell
p_cp = apply(ncpts, c(1,2), function(x) sum(x > 0, na.rm = TRUE))
# median changepoint year
med_yr = apply(ycpts, c(1,2), function(x) median(x, na.rm = TRUE))
# IQR of changepoint year
iqr_yr = apply(ycpts, c(1,2), function(x) IQR(x, na.rm = TRUE))

ensemble_results=list(p_cp,med_yr,iqr_yr)
save(file="./results/ResultsDCENTSummary.RData",ensemble_results)



## Analyze HadCRUT ensemble members ####
year = seq(1970,2024)
for(i in 1:200){
  filepath = paste0("./data/processed/annual_HadCRUT.5.1.0.0_member_", i, "_anom.RData")
  load(filepath)
  data = tas_annual[,,which(time == 1970):which(time == 2024)]
  results = st.cpts(data,lon,lat,year,"BIC")
  save(file=paste0("./results/ResultsHadCRUT_member_", i,".RData"),results)
}

# Loop that goes through and stack results
year = seq(1970,2024)
nlon = dim(results$ncpts)[1]
nlat = dim(results$ncpts)[2]
ncpts = array(data=NA,dim=c(nlon,nlat,200))
ycpts = array(data=NA,dim=c(nlon,nlat,200))
for(k in 1:200){
  load(file= paste0("./results/ResultsHadCRUT_member_",k,".RData"))
  ncpts[,,k] = results$ncpts
  ycpts[,,k] = results$ycpts[,,1]
}

#compute the number of cp detected per grid cell
p_cp = apply(ncpts, c(1,2), function(x) sum(x > 0, na.rm = TRUE))
# median changepoint year
med_yr = apply(ycpts, c(1,2), function(x) median(x, na.rm = TRUE))
# IQR of changepoint year
iqr_yr = apply(ycpts, c(1,2), function(x) IQR(x, na.rm = TRUE))

ensemble_results=list(p_cp,med_yr,iqr_yr)
save(file="./results/ResultsHadCRUTSummary.RData",ensemble_results)

