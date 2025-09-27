################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# Script that goes through the different processed gridded datasets and analyze
# for the presence of changes in the rate of warming. For each dataset, this 
# is done with changepoint analysis, then by fitting a quadratic trend.

year = seq(1970,2024)

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
names=c("HadCRUT","NOAA","DCENT","BerkeleyRegridded","NASARegridded")
ycpts_array = array(data=NA,dim=c(72,36,5))
dift_array = array(data=NA,dim=c(72,36,5))
for(k in 1:5){
  load(paste0("./results/Results",names[k],".RData"))
  ycpts_array[,,k] = results$ycpts[,,1]
  dift_array[,,k] = results$dift
  
  if(k == 5){
    ycpts_shifted = matrixShiftLongitude((results$ycpts[,,1]),lon)
    ycpts_array[,,k] = ycpts_shifted$m
    
    dift_shifted = matrixShiftLongitude(results$dift,lon)
    dift_array[,,k] = dift_shifted$m
  }
}
na_counts = apply(ycpts_array,1:2,function(x) sum(!is.na(x)))
na_counts[na_counts == 0] = NA
my = apply(ycpts_array,1:2,mean,na.rm=T)
mt = apply(dift_array,1:2,mean,na.rm=T)

combined_results = list(na_counts,my,mt)
names(combined_results) = c("agree","ycpts","dift")
save(file="./results/ResultsCombined.RData",combined_results)
