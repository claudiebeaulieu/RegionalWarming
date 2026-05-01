# Changepoint uncertainty for Regional warming paper

# Load in the regional averages for the boxes
#load("./data/processed/RegionalAverages.RData")
# regional_averages is a 55x9 matrix for the 8 regions (first col is dates)

ConfidenceIntervals = function(regional_averages){

# for each region we have a single changepoint.  We want to look at the changepoint uncertainty
# So we will get the test statistic for each of the potential changepoint times
# then for a specific alpha level (0.05) we look for the minimum in the test statistic
# the confidence set is those points that are within the c_alpha bound away from the minimum.

# we will use the test statistic used by PELT to fit a change in joined trend with AR(1) errors
secondseg=function(data,start,previousbeta,p=1){
  # assumes that data is the data for the segment
  n=length(data)
  t=start:(start+n-1)
  filtered=data-previousbeta+previousbeta*(t-start)/n
  X=(t-start)/n
  trendfit=lm(filtered~-1+X)
  arfit=try(arima(resid(trendfit),order=c(p,0,0),include.mean=FALSE, method="ML"),silent=TRUE)
  if(class(arfit)=="try-error"){
    return(c(10^{16},0)) # large number so PELT doesn't pick as min
  }
  #loglik=n*log(arfit$sigma2)-log(1-arfit$coef^2)+
  #  (1-arfit$coef^2)*(resid(trendfit)[1])^2/arfit$sigma2+ # first obs
  #  (1/arfit$sigma2)*sum((resid(trendfit)[-1]-arfit$coef*resid(trendfit)[-n])^2) # remaining obs
  return(c(-2*logLik(arfit),coef(trendfit)[1])) # 1 is the trend estimate as no intercept
}
firstseg=function(data,start,p=1){
  # assumes that data is the data for the segment
  n=length(data)
  t=start:(start+n-1)
  X=(t-start)/n
  trendfit=lm(data~X)
  arfit=try(arima(resid(trendfit),order=c(p,0,0),include.mean=FALSE, method="ML"))
  if(class(arfit)=="try-error"){
    return(c(10^{16},0)) # large number so PELT doesn't pick as min
  }
  return(c(-2*logLik(arfit),coef(trendfit)[2]+coef(trendfit)[1])) # 2 is the trend estimate
}
runmin <- function(v,k){
  # running minimum function needed for the conf set bound
  # v = vector of numbers
  # k = window width. Needs to be at least 2.
  out = NULL
  for(i in 1:(length(v)-k+1)){
    out = c(out,min(v[i:(i+k-1)],na.rm=TRUE))
  }
  return(out)
}


minseglen=10
n=nrow(regional_averages)
test.stat=matrix(NA,nrow=n, ncol=ncol(regional_averages)-1)
for(i in 2:ncol(regional_averages)){
  for(tau in minseglen:(n-minseglen)){
    first=firstseg(regional_averages[1:tau,i],start=0)
    test.stat[tau,i-1]=first[1]+secondseg(regional_averages[(tau+1):n,i],start=tau,previousbeta=first[2])[1]
  }
}
# above is fast despite being a double loop :-)

# changepoint times
regional_averages[apply(test.stat,MARGIN=2,FUN=which.min),1]

# get the confidence set
alpha=0.05
apply(test.stat,MARGIN=2,FUN=function(x){
  # Get threshold from Owen's work
  cpts=c(0,which.min(x),length(x))
  nj.maxi = max(runmin(diff(cpts),2))
  CS.thresh = 3/2 + 2*log(2*nj.maxi)-2*log(alpha) - log(n)
  CI = regional_averages[which(x<min(x,na.rm=T)+CS.thresh),1]
  return(CI)
})

}

