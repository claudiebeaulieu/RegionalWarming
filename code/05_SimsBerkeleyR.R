################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# The script is used to conduct simulations to assess the true and false positive rates,
# based on characteristics of the berkeley time series.

# load data
load('./Datasets/temperature_anomalies_1850_2024_global.RData')

# We take Berkeley time series over 1970-2024 and extract characteristics to simulate synthetic series
tmp = Tanom_annual_df[121:175,5]
n = length(tmp)
time = seq(1,n)
linmod = gls(tmp~time,correlation = corAR1())
beta = linmod$coefficients[2]
phi = 0.23 #from summary(linmod)
vare = var(residuals(linmod))
varw = vare*(1-(phi^2))


##### Simulations to compute FPR

#sims with no acceleration pen=4longn
reps = 1000
set.seed(123)
FPR=sim_many(reps,0,0,n,beta,varw,phi,4*log(n),0)/reps
#1.8% with set.seed(123)

#sims with no acceleration pen=3longn
reps = 1000
set.seed(123)
FPR=sim_many(reps,0,0,n,beta,varw,phi,3*log(n),0)/reps
#9.4% with set.seed(123)


##### Simulations to compute TP

min=0.25 #0.0005  # minimum difficulty seen in the data analysis
max=3 # maximum difficulty seen in the data analysis is 4.5, but we stop at 3 since power is 1 after that
cpt_difficulty=seq(from=min, to=max, by=0.25)
cpts=c(10,20,30,40) # 1980, 1990, 2000, 2010
reps=1000

set.seed(234089)
power=matrix(0,nrow=length(cpt_difficulty),ncol=length(cpts))
for(i in 1:length(cpt_difficulty)){
  for(j in 1:length(cpts)){
    power[i,j]=sim_many(reps,cpts[j],cpt_difficulty[i],n,beta,varw,phi,4*log(n),1)
  }
  print(paste("Completed",i))
}
power=power/reps # proportion of times a cpt is detected when one exists

save(file='./Results/power4logn_1000reps.Rdata',power)
#change the penalty to 3*log(n) for reduced penalty
