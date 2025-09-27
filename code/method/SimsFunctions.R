#Functions for power & FPR
sim_one=function(cpt,cpt_difficulty,n,beta,sigma2,phi,penalty,type){
  
  # simulate AR data with long-term trend of 0.02
  y=arima.sim(model=list(ar=phi),n=n,sd=sqrt(sigma2)) + beta[1]*seq(1,n)
  #y = arima.sim(list(order=c(1,0,0), ar=phi), n=n, innov = rnorm(n,mean=0,sd=sqrt(sigma2))) + beta*seq(1,n)
  
  if (type == 1){ #simulating power - need to add changepoints
    
    cptm = sqrt((cpt-1)*(n-cpt)/n)
    diff_beta=cpt_difficulty*sqrt(sigma2/(1-phi^2))/cptm 
  
    #add an acceleration
    y = y+c(rep(0,cpt),(diff_beta+beta[1])*seq(from=1,to=(n-cpt))) 
  }
  
    # fit model to data
    out=PELT.trendARpJOIN(y,p=1,pen=penalty,minseglen=10)
  
  # Did we find a cpt?
  if(length(out)==0){
    return(0)
  }
  else(return(1))
}

sim_many=function(reps,cpt, cpt_difficulty,n,beta,sigma2,phi,penalty,type){
  out=replicate(reps,sim_one(cpt, cpt_difficulty,n,beta,sigma2,phi,penalty,type))
  return(sum(out,na.rm=T)) # how many out of reps found a cpt
}
