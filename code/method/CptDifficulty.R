# Code to calculate the energy of a changepoint
# Assumes change in join-pin trend (so just the change in trend is important, not the intercept)
# Assumes AR1 errors

cpt_difficulty=function(data,cpts, coefs){
  # both arguments are assumed to be for a single series.
  # cpts is a vector (not including 0 and n)
  # coefs is a matrix with 3 columns, beta, phi, sigma2 and nrows=length(cpts)+1
  
  # the coefs for the join-pin are not interpretable "as-is", 
  #    the beta_1 given is actually alpha+beta_1
  #    the subsequent beta's are actually beta*n_i so you need to divide by the segment length
  
  # thus to retrieve the correct parameters, we fit a trend to the first parameters to recover beta_1
  beta1=lm(data[1:cpts[1]]~c(1:cpts[1]))$coef[2]
  cpts=c(0,cpts,length(data)) # add the 0 and n to get the full set
  ni=cpts[-1]-cpts[-length(cpts)] # the number of observations per segment
  n2=ni[-1]+ni[-length(ni)] # the rolling number of observations across two segments (ignoring the changepoint in the middle)
  
  betas=c(beta1,(coefs[-1,1]-coefs[-nrow(coefs),1])/ni[-1])
  segvar=coefs[,3]/(1-coefs[,2]^2) # sigma2/(1-phi^2)
  
  #cpt_difficulty=c(betas,abs(diff(betas/sqrt(segvar)))*sqrt((ni[-1]*ni[-length(ni)])/n2))
  cpt_difficulty=c(betas,abs(diff(betas/sqrt(segvar)))*sqrt((ni[-1]*(ni[-length(ni)]-1)/n2)))
  print(cpt_difficulty)
  print(ni)
  print(sqrt(segvar))
  return(cpt_difficulty)
}
