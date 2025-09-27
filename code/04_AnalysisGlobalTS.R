################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# The script is used to analyze global surface temperature time series as 
# illustrated in Figure S1.


# load datasets
load('./data/processed/temperature_anomalies_1850_2024_global.RData')
# Tanom_annual_df is a data frame with GMST from 1850-2026 from five different sources

# Compute anomalies wrt to 1850-1900
n = dim(Tanom_annual_df)[1]
nvar = dim(Tanom_annual_df)[2]
m50 = colMeans(Tanom_annual_df[1:51,2:nvar],na.rm=T)
Tanom_annual_df[,2:nvar] = Tanom_annual_df[,2:nvar]-matrix(rep(m50,n),nrow=n,ncol=nvar-1,byrow=T)

##### Continuous model AR(1) fitted to full dataset until 2024

trendarjoin=list()
mresiduals=list()
fits=list()
fitsAR=list()
dates=list()

for(i in 2:nvar){
  data=Tanom_annual_df[,c(1,i)][!is.na(Tanom_annual_df[,i]),]
  n=nrow(data)
  
  #Model fit
  trendarjoin[[i]]=PELT.trendARpJOIN(data[,2],p=1,pen=4*log(n),minseglen=10)
  fittrend = fit.trendARpJOIN(data[,2],trendarjoin[[i]],p=1,dates=data[,1],plot=T,add.ar=F,fit=T,
                              title=names(Tanom_annual_df)[i],pred=F)# get fit without AR - to visualize trend segments
  fits[[i]] = fittrend$fit
  dates[[i]] = fittrend$dates
  fittrendAR = fit.trendARpJOIN(data[,2],trendarjoin[[i]],p=1,dates=data[,1],plot=F,add.ar=T,fit=T,
                                title=names(Tanom_annual_df)[i]) #get fit with AR - to compute residuals 
  fitsAR[[i]] = fittrendAR$fit
  trendarjoin[[i]]=data[trendarjoin[[i]],1]  # put cpts in terms of year
  mresiduals[[i]] = data[,2]-fittrendAR$fit  # get residuals
}
save(Tanom_annual_df,trendarjoin,mresiduals,fits,fitsAR,dates,file="./results/results_trendar1join_global.Rdata")

# NOTE: we get same results if we reduce the penalty to 3*log(n)
