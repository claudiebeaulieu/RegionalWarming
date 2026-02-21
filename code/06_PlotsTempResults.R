################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# The script is used to plot results obtained on all gridded datasets

## Plots cp timings and magnitudes #####

#Berkeley
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsBerkeley.RData')
timing_p1 = timing_plots(results,lon,lat,"Berkeley")
mag_p1 = mag_plots(results,lon,lat,"Berkeley")

#Berkeley regridded
load('./data/processed/annual_Berkeley_anom_regridded.RData')
load(file='./results/ResultsBerkeleyRegridded.RData')
timing_p1r = timing_plots(results,lon,lat,"Berkeley")
mag_p1r = mag_plots(results,lon,lat,"Berkeley")

#NASA
load('./data/processed/annual_NASA_anom.RData')
load(file='./results/ResultsNASA.RData')
timing_p2 = timing_plots(results,lon,lat,"NASA")
mag_p2 = mag_plots(results,lon,lat,"NASA")

#NASA
load('./data/processed/annual_NASA_anom_regridded.RData')
load(file='./results/ResultsNASARegridded.RData')
timing_p2r = timing_plots(results,lon,lat,"NASA")
mag_p2r = mag_plots(results,lon,lat,"NASA")

# NOAA
load('./data/processed/annual_NOAA_anom.RData')
load(file='./results/ResultsNOAA.RData')
timing_p3 = timing_plots(results,lon,lat,"NOAA")
mag_p3 = mag_plots(results,lon,lat,"NOAA")

#HadCRUT
load('./data/processed/annual_HadCRUT_anom.RData')
load(file='./results/ResultsHadCRUT.RData')
timing_p4 = timing_plots(results,lon,lat,"HadCRUT")
mag_p4 = mag_plots(results,lon,lat,"HadCRUT")

#DCENT
load('./data/processed/annual_DCENT_anom.RData')
load(file='./results/ResultsDCENT.RData')
timing_p5 = timing_plots(results,lon,lat,"DCENT")
mag_p5 = mag_plots(results,lon,lat,"DCENT")

#Combined datasets
load('./data/processed/annual_Berkeley_anom_regridded.RData')
load(file="./results/ResultsCombined.RData")
timing_p6 = timing_combined_plots(combined_results,target_lon,target_lat,"Aggregated")
mag_p6 = mag_combined_plots(combined_results,target_lon,target_lat,"Aggregated")

# Fig1 in paper 
fig1 = ggarrange(timing_p6,timing_p1,timing_p2,timing_p3,timing_p4,timing_p5,nrow=3,ncol=2,labels=c("A","B","C","D","E","F"),align="v",common.legend = TRUE, legend = "top")
ggsave('./figures/trendcpt_all.png', fig1, bg = "white",width=8, height=10) 

# Alternative to fig 1 that shows results on regridded data
fig1a = ggarrange(timing_p6,timing_p1r,timing_p2r,timing_p3,timing_p4,timing_p5,nrow=3,ncol=2,labels=c("A","B","C","D","E","F"),align="v",common.legend = TRUE, legend = "top")
ggsave('./figures/trendcpt_all_alt.png', fig1a, bg = "white",width=8, height=10) 

#Plot with magnitudes all datasets - main paper Figure 2
fig2 = ggarrange(mag_p6,mag_p1,mag_p2,mag_p3,mag_p4,mag_p5,nrow=3,ncol=2,labels=c("A","B","C","D","E","F"),align="v",common.legend = TRUE, legend = "top")
ggsave('./figures/trendmag_all.png', fig2, bg = "white",width=8, height=10) 

#Alternative to fig2 with regridded data
fig2a = ggarrange(mag_p6,mag_p1r,mag_p2r,mag_p3,mag_p4,mag_p5,nrow=3,ncol=2,labels=c("A","B","C","D","E","F"),align="v",common.legend = TRUE, legend = "top")
ggsave('./figures/trendmag_all_alt.png', fig2a, bg = "white",width=8, height=10) 

#Plot the timing and magnitude for Berkeley with reduced penalty - Figure S2 Supp
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsBerkeley_3Pen.RData')
timing_3pen = timing_plots(results,lon,lat,"Berkeley")
mag_3pen = mag_plots(results,lon,lat,"Berkeley")

figs2 = ggarrange(timing_3pen+labs(title=""),mag_3pen+labs(title=""),nrow=2,ncol=1,labels=c("A","B"),align="v")
ggsave('./figures/trendcpt_Berkeley_3Pen.png', figs2, bg = "white",width=8, height=10) 


## Plots cp difficulty #####

#Berkeley dataset Figure 5 Main paper
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsBerkeley.RData')
diff_p1 = difficulty_plots(results,lon,lat,"Berkeley") + labs(title=" ")
ggsave('./figures/difficulty_Berkeley.png', diff_p1, bg = "white",width=8, height=4) 


## PLots quadratic trends #####

#Berkeley dataset Figure S4 Supp
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsquadBerkeley.RData')
quad_p1 = quadtrend_plots(results,lon,lat,"Berkeley") + labs(title=" ")
ggsave('./figures/quadtrends_Berkeley.png', quad_p1, bg = "white",width=8, height=4) 


## Plots of changepoints per latitude #####

# Berkeley latitudina changepoints - Figure 3 in Main
load('./data/processed/annual_Berkeley_anom.RData')
load('./results/ResultsBerkeley.RData')
lat_p1 = latitudinal_plots(lat,results$ncpts,results$dift,results$ycpts[,,1],3) +
  annotate("text", label = "Acceleration", x = 6, y = 5, size = 3, color = "lightpink2") +
  annotate("text", label = "Deceleration", x = 6, y = -5, size = 3, color = "steelblue") +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = -10),color="steelblue",
              arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10),color="lightpink2",
             arrow = arrow(length = unit(0.3, "cm")))
ggsave(file='./figures/latitudes_Berkeley.png',lat_p1,bg = "white",width=4, height=4) 

# Berkeley latitudinal changepoints - Land only
load('./data/processed/annual_Berkeley_anom.RData')
load('./data/processed/Berkeley_landmask.RData')#1 is land and 0 is ocean
load('./results/ResultsBerkeley.RData')

# Ocean
ncpts = results$ncpts
ncpts[land_binary == 1] = NA

dift = results$dift
dift[land_binary == 1] = NA

ycpts = results$ycpts[,,1]
ycpts[land_binary == 1] = NA

latoce_p1 = latitudinal_plots(lat,ncpts,dift,ycpts,3) +
  annotate("text", label = "Acceleration", x = 6, y = 5, size = 3, color = "lightpink2") +
  annotate("text", label = "Deceleration", x = 6, y = -5, size = 3, color = "steelblue") +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = -10),color="steelblue",
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10),color="lightpink2",
               arrow = arrow(length = unit(0.3, "cm")))

# Land
ncpts = results$ncpts
ncpts[land_binary == 0] = NA

dift = results$dift
dift[land_binary == 0] = NA

ycpts = results$ycpts[,,1]
ycpts[land_binary == 0] = NA

latland_p1 = latitudinal_plots(lat,ncpts,dift,ycpts,3) +
  annotate("text", label = "Acceleration", x = 6, y = 5, size = 3, color = "lightpink2") +
  annotate("text", label = "Deceleration", x = 6, y = -5, size = 3, color = "steelblue") +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = -10),color="steelblue",
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10),color="lightpink2",
               arrow = arrow(length = unit(0.3, "cm")))

lat_landoce = ggarrange(latland_p1,latoce_p1,nrow=1,ncol=2,labels=c("A","B"),align="v",common.legend = TRUE, legend = "bottom")
ggsave(file='./figures/latitudes_landoce_Berkeley.png',lat_landoce,bg = "white",width=8, height=4) 


## Plots regional average time series #####

# With Berkeley dataset - Figure 3 in the Main

# load regional averages time series and analysis results - Berkeley
load('./results/ResultsRegionalAverages.RData')

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

# plot of Berkeley cp timings plus white boxes around regions of interest
timing_p1_boxes = timing_p1 + 
  geom_rect(aes(xmin=box_coords[1,1],xmax=box_coords[1,2],ymin=box_coords[1,3],ymax=box_coords[1,4]),color="firebrick",alpha=0,linewidth=1)+#Mexico
  geom_rect(aes(xmin=box_coords[2,1],xmax=box_coords[2,2],ymin=box_coords[2,3],ymax=box_coords[2,4]),color="firebrick",alpha=0,linewidth=1)+#Gulf of Mexico
  geom_rect(aes(xmin=box_coords[3,1],xmax=box_coords[3,2],ymin=box_coords[3,3],ymax=box_coords[3,4]),color="firebrick",alpha=0,linewidth=1)+#Bolivia
  geom_rect(aes(xmin=box_coords[4,1],xmax=box_coords[4,2],ymin=box_coords[4,3],ymax=box_coords[4,4]),color="firebrick",alpha=0,linewidth=1)+#Eastern Greenland
  geom_rect(aes(xmin=box_coords[5,1],xmax=box_coords[5,2],ymin=box_coords[5,3],ymax=box_coords[5,4]),color="firebrick",alpha=0,linewidth=1)+#Eastern mediterranean
  geom_rect(aes(xmin=box_coords[6,1],xmax=box_coords[6,2],ymin=box_coords[6,3],ymax=box_coords[6,4]),color="firebrick",alpha=0,linewidth=1)+#Southeast China 
  geom_rect(aes(xmin=box_coords[7,1],xmax=box_coords[7,2],ymin=box_coords[7,3],ymax=box_coords[7,4]),color="firebrick",alpha=0,linewidth=1)+#SNew Zealand
  geom_rect(aes(xmin=box_coords[8,1],xmax=box_coords[8,2],ymin=box_coords[8,3],ymax=box_coords[8,4]),color="firebrick",alpha=0,linewidth=1)+#NP1
  geom_rect(aes(xmin=box_coords[9,1],xmax=box_coords[9,2],ymin=box_coords[9,3],ymax=box_coords[9,4]),color="firebrick",alpha=0,linewidth=1)+#NP2
  geom_text(aes(x=-108,y=20),label="B",color="firebrick")+
  geom_text(aes(x=-70,y=30),label="C",color="firebrick")+
  geom_text(aes(x=-48,y=-10),label="D",color="firebrick")+
  geom_text(aes(x=-38,y=72),label="E",color="firebrick")+
  geom_text(aes(x=45,y=26),label="F",color="firebrick")+
  geom_text(aes(x=93,y=36),label="G",color="firebrick")+
  geom_text(aes(x=160,y=-25),label="H",color="firebrick")+
  geom_text(aes(x=168,y=49),label="I",color="firebrick")+
  geom_text(aes(x=-170,y=49),label="I",color="firebrick") + 
  labs(title=" ")

box_p = list()
for (i in 1:(nregion)){
  mbox_df=box_ts[[i]]
  p = ggplot(data=mbox_df,aes(x=year,y=mbox))+geom_line()+
    geom_line(data=mbox_df,aes(x=year,y=V3),col="red")+
    labs(title = box_names[i], x = "Year",y = "Anomaly (°C)")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45,hjust=1))
  box_p[[i]]=p
}

fig4 = ggarrange(timing_p1_boxes,                                                 # First row with map
          ggarrange(box_p[[1]], box_p[[2]], box_p[[3]],box_p[[4]],box_p[[5]], box_p[[6]], box_p[[7]], box_p[[10]],ncol = 4, nrow=2,labels = c("B", "C","D","E","F","G","H","I")), # following rows with time series
          nrow = 2, 
          labels = "A")                                        
ggsave(file='./figures/regional_averages_Berkeley.png',fig4,bg = "white",width=8, height=8) 

## Plots global mean time series and fitted models #####

# This is figure S1 in Supp

# load results + data
load("./results/results_trendar1join_global.Rdata")

# Initialise figure params
cols = c("darkblue","orange","red","darkgrey","cyan")
names = c("NASA","HadCRUT","NOAA","Berkeley","DCENT")
index = c(2,3,4,5,6)

# Make fits and fitsAR data frames for plotting
fits = fits[-1]
names(fits)=names
fitsAR = fitsAR[-1]
names(fitsAR)=names
dates = dates[-1]
names(dates)=names

fits_long = data.frame(unlist(dates),unlist(fits),c(rep("NASA",145),rep("HadCRUT",175),rep("NOAA",175),rep("Berkeley",175),rep("DCENT",175)))
names(fits_long) = c("year","value","dataset")

fitsAR_long = data.frame(unlist(dates),unlist(fitsAR),c(rep("NASA",145),rep("HadCRUT",175),rep("NOAA",175),rep("Berkeley",175),rep("DCENT",175)))
names(fitsAR_long) = c("year","value","dataset")

#plot the observations
data_long = melt(Tanom_annual_df,id="year")
pobs = ggplot(data=data_long,aes(x=year,y=value,color=variable)) +
  geom_line(linewidth=0.25)+
  labs( x = "Year",y = "Anomaly (°C)",color=" ")+
  scale_color_manual(values=cols)+
  theme_bw() 

#superpose fits to obs
pfits = pobs + geom_line(data=fits_long,aes(x=year,y=value,color=dataset),linewidth=1)

#superpose fits+AR to obs
pfitsAR = pobs + geom_line(data=fitsAR_long,aes(x=year,y=value,color=dataset),linewidth=1)

pdf(file='./figures/globalfits.pdf',width=8, height=8)
ggarrange(pfits,pfitsAR,nrow=2,ncol=1,labels=c("A","B"),align="v",common.legend = TRUE, legend = "bottom")
dev.off()