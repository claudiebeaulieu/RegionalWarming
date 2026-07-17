################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# This script is used to create and analyze regional surface temperature time series as 
# illustrated in Figure 4 and presented in Table 2.

# load data
load('./data/processed/annual_Berkeley_anom.RData')
# Gridded Berkeley data

#Define coordinates of time series to compute averages
box_coords = matrix(c(-100, -90, 16, 21, 
                      -90, -78, 21, 30, 
                      -68,-56,-21,-10,
                      -32,-18,68,80,
                      15,38,28,45,
                      100,120,26,35,
                      165,178,-45,-28,
                      160,180,35,45,
                      -180,-168,35,45), nrow=9, byrow = T)#contains the limits for boxes in regions above
box_names = c("Southeast Mexico","Gulf of Mexico","Bolivia", "East Greenland",
              "East Mediterranean","Southeast China","New Zealand",
              "North Pacific 1","North Pacific 2", "North Pacific")

nregion = length(box_names)
year = seq(1970,2024)
n=length(year)
box_ts = list()

for (i in 1:nregion){
  
  if(i == 10){# need to merge boxes 8-9 and then average
    box1 = tas_annual[(lon > box_coords[9,1]) & (lon < box_coords[9,2]),(lat > box_coords[9,3]) & (lat < box_coords[9,4]),121:175]
    box2 = tas_annual[(lon > box_coords[8,1]) & (lon < box_coords[8,2]),(lat > box_coords[8,3]) & (lat < box_coords[8,4]),121:175]
    box = abind(box1,box2,along=1)
    mbox = apply(box, 3, mean, na.rm=T) 
    box_ts[[i]] = mbox
     }else
  
  #get the data in the regional box and compute mean
  box = tas_annual[(lon > box_coords[i,1]) & (lon < box_coords[i,2]),(lat > box_coords[i,3]) & (lat < box_coords[i,4]),121:175]
  mbox = apply(box, 3, mean, na.rm=T) 
  box_ts[[i]] = mbox
}

#save as a data frame
regional_averages = data.frame(year,box_ts[[1]], box_ts[[2]],box_ts[[3]],box_ts[[4]],box_ts[[5]],
                                box_ts[[6]],box_ts[[7]],box_ts[[10]])
 names(regional_averages)= c("Year","Southeast Mexico","Gulf of Mexico","Bolivia", "East Greenland",
                             "East Mediterranean","Southeast China","New Zealand","North Pacific")
 save(file="./data/processed/RegionalAverages.RData",regional_averages,year,box_ts)


#repeat cp analysis in those regions
 box_cpt=list()
 box_energy=list()
 box_fits=list()

for (i in 1:nregion){
  mbox = box_ts[[i]]
  #run cp analysis
  cpt.trendARpJOIN = PELT.trendARpJOIN(mbox,p=1,pen=4*log(n),minseglen=10)
  fittrend = fit.trendARpJOIN(data=mbox,cpts=cpt.trendARpJOIN,p=1,dates=year,add.ar=F)
  fittrendAR = fit.trendARpJOIN(data=mbox,cpts=cpt.trendARpJOIN,p=1,dates=year,add.ar=T)
  cpt.difficulty = cpt_difficulty(mbox,cpt.trendARpJOIN, fittrend$coeffs)
  difficulty = cpt.difficulty[length(cpt.difficulty)]
  #create a data frame
  mbox_fits = as.data.frame(unname(cbind(year,mbox,fittrend$fit,fittrendAR$fit)))
  #store results in a list
  box_fits[[i]] = mbox_fits#this is a list of data frames with obs + models
  box_cpt[[i]]=year[cpt.trendARpJOIN]#year of cpt
  box_energy[[i]]=difficulty
}
save(file='./results/ResultsRegionalAverages.RData',box_fits,box_cpt,box_energy,box_names,box_coords)


# Calculate CI conditional on one changepoint for the regional averages

# Load in the regional averages for the boxes
load("./data/processed/RegionalAverages.RData")
# regional_averages is a 55x9 matrix for the 8 regions (first col is dates)

CI = ConfidenceIntervals(regional_averages)
save(file='./results/CIRegionalAverages.RData',CI)

