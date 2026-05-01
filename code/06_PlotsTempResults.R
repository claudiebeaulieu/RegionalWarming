################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# The script is used to plot results obtained on all temperature datasets

## Plots cp timings and magnitudes #####

#Berkeley
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsBerkeley.RData')
timing_p1 = timing_plots(results,lon,lat,"Berkeley")
mag_p1 = mag_plots(results,lon,lat,"Berkeley") 

#Berkeley regridded
load('./data/processed/annual_Berkeley_anom_regridded.RData')
load(file='./results/ResultsBerkeleyRegridded.RData')
timing_p1r = timing_plots(results,lon,lat,"Berkeley regridded")
mag_p1r = mag_plots(results,lon,lat,"Berkeley regridded")

#NASA
load('./data/processed/annual_NASA_anom.RData')
load(file='./results/ResultsNASA.RData')
timing_p2 = timing_plots(results,lon,lat,"NASA")
mag_p2 = mag_plots(results,lon,lat,"NASA")

#NASA
load('./data/processed/annual_NASA_anom_regridded.RData')
load(file='./results/ResultsNASARegridded.RData')
timing_p2r = timing_plots(results,lon,lat,"NASA regridded")
mag_p2r = mag_plots(results,lon,lat,"NASA regridded")

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
timing_p5 = timing_plots(results,lon,lat,"DCENT-I")
mag_p5 = mag_plots(results,lon,lat,"DCENT-I")

#Combined datasets
load('./data/processed/annual_Berkeley_anom_regridded.RData')
load(file="./results/ResultsCombined.RData")
timing_p6 = timing_combined_plots(combined_results,target_lon,target_lat,"Aggregated")
mag_p6 = mag_combined_plots(combined_results,target_lon,target_lat,"Aggregated")

# Fig1 in paper - timings
fig1 = ggarrange(timing_p6,timing_p3,timing_p4,timing_p5,timing_p2r,timing_p1r,timing_p2,timing_p1,nrow=4,ncol=2,labels=c("A","B","C","D","E","F","G","H"),align="v",common.legend = TRUE, legend = "top")
ggsave('./figures/trendcpt_all.png', fig1, bg = "white",width=8, height=10) 

# Fig2 in paper - magnitudes
fig2 = ggarrange(mag_p6,mag_p3,mag_p4,mag_p5,mag_p2r,mag_p1r,mag_p2,mag_p1,nrow=4,ncol=2,labels=c("A","B","C","D","E","F","G","H"),align="v",common.legend = TRUE, legend = "top")
ggsave('./figures/trendmag_all.png', fig2, bg = "white",width=8, height=10) 

#Plot the timing and magnitude for Berkeley with reduced penalty - Figure S4 Supp
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsBerkeley_3Pen.RData')
timing_3pen = timing_plots(results,lon,lat,"Berkeley")
mag_3pen = mag_plots(results,lon,lat,"Berkeley")

figs2 = ggarrange(timing_3pen+labs(title=""),mag_3pen+labs(title=""),nrow=2,ncol=1,labels=c("A","B"),align="v")
ggsave('./figures/trendcpt_Berkeley_3Pen.png', figs2, bg = "white",width=8, height=8) 


## Plots cp SNR #####

#Berkeley dataset Figure 5 Main paper
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsBerkeley.RData')
diff_p1 = difficulty_plots(results,lon,lat,"Berkeley") + labs(title=" ")
ggsave('./figures/difficulty_Berkeley.png', diff_p1, bg = "white",width=8, height=4) 


## PLots quadratic trends #####

#Berkeley dataset Figure S6 Supp
load('./data/processed/annual_Berkeley_anom.RData')
load(file='./results/ResultsquadBerkeley.RData')
quad_p1 = quadtrend_plots(results,lon,lat,"Berkeley") + labs(title=" ")
ggsave('./figures/quadtrends_Berkeley.png', quad_p1, bg = "white",width=8, height=4) 


## Plots of changepoints per latitude #####

# Berkeley latitudinal changepoints - Land and ocean separated
load('./data/processed/annual_Berkeley_anom.RData')
load('./data/processed/Berkeley_landmask.RData')#1 is land and 0 is ocean
load('./results/ResultsBerkeley.RData')

# Ocean
ncpts = results$ncpts
ncpts[land_binary == 1] = NA

dift = results$dift
dift[land_binary == 1] = NA

ycpts = results$ycpts
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

ycpts = results$ycpts
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
load('./results/CIRegionalAverages.RData')

# plot of Berkeley cp timings plus white boxes around regions of interest
timing_p1_boxes = timing_p1 + 
  geom_rect(aes(xmin=box_coords[1,1],xmax=box_coords[1,2],ymin=box_coords[1,3],ymax=box_coords[1,4]),color="firebrick",alpha=0,linewidth=1)+#Mexico
  geom_rect(aes(xmin=box_coords[2,1],xmax=box_coords[2,2],ymin=box_coords[2,3],ymax=box_coords[2,4]),color="firebrick",alpha=0,linewidth=1)+#Gulf of Mexico
  geom_rect(aes(xmin=box_coords[3,1],xmax=box_coords[3,2],ymin=box_coords[3,3],ymax=box_coords[3,4]),color="firebrick",alpha=0,linewidth=1)+#Bolivia
  #geom_rect(aes(xmin=box_coords[4,1],xmax=box_coords[4,2],ymin=box_coords[4,3],ymax=box_coords[4,4]),color="firebrick",alpha=0,linewidth=1)+#Eastern Greenland
  geom_rect(aes(xmin=box_coords[5,1],xmax=box_coords[5,2],ymin=box_coords[5,3],ymax=box_coords[5,4]),color="firebrick",alpha=0,linewidth=1)+#Eastern mediterranean
  geom_rect(aes(xmin=box_coords[6,1],xmax=box_coords[6,2],ymin=box_coords[6,3],ymax=box_coords[6,4]),color="firebrick",alpha=0,linewidth=1)+#Southeast China 
  geom_rect(aes(xmin=box_coords[7,1],xmax=box_coords[7,2],ymin=box_coords[7,3],ymax=box_coords[7,4]),color="firebrick",alpha=0,linewidth=1)+#SNew Zealand
  geom_rect(aes(xmin=box_coords[8,1],xmax=box_coords[8,2],ymin=box_coords[8,3],ymax=box_coords[8,4]),color="firebrick",alpha=0,linewidth=1)+#NP1
  geom_rect(aes(xmin=box_coords[9,1],xmax=box_coords[9,2],ymin=box_coords[9,3],ymax=box_coords[9,4]),color="firebrick",alpha=0,linewidth=1)+#NP2
  geom_text(aes(x=-108,y=20),label="B",color="firebrick")+
  geom_text(aes(x=-70,y=30),label="C",color="firebrick")+
  geom_text(aes(x=-48,y=-10),label="D",color="firebrick")+
  #geom_text(aes(x=-38,y=72),label="E",color="firebrick")+
  geom_text(aes(x=45,y=26),label="E",color="firebrick")+
  geom_text(aes(x=93,y=36),label="F",color="firebrick")+
  geom_text(aes(x=160,y=-25),label="G",color="firebrick")+
  geom_text(aes(x=168,y=49),label="H",color="firebrick")+
  geom_text(aes(x=-170,y=49),label="H",color="firebrick") + 
  labs(title=" ")

#plot individual time series
box_p = list()
ind_region = c(seq(1:7),10)
for (i in 1:length(ind_region)){
  tmp = box_fits[[ind_region[i]]]
  limits = CI[[i]]
  p = ggplot(data=tmp,aes(x=V1,y=V2))+geom_line()+
    annotate("rect",
             xmin = min(limits), xmax = max(limits),
             ymin = -Inf, ymax = Inf,
             fill = "grey70", alpha = 0.4) +
    geom_line(data=tmp,aes(x=V1,y=V3),col="red")+
    labs(title = box_names[ind_region[i]], x = "Year",y = "Anomaly (°C)")+
    scale_y_continuous(labels = function(x) sprintf("%.1f", x))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45,hjust=1),
          panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8))
  box_p[[i]]=p
  print(limits)
}

fig4 = ggarrange(timing_p1_boxes,                                                 # First row with map
          ggarrange(box_p[[1]], box_p[[2]], box_p[[3]],box_p[[5]], box_p[[6]], box_p[[7]], box_p[[8]],ncol = 4, nrow=2,labels = c("B", "C","D","E","F","G","H")), # following rows with time series
          nrow = 2, 
          labels = "A")                                        
ggsave(file='./figures/regional_averages_Berkeley.png',fig4,bg = "white",width=8, height=8) 


## Plots global mean time series and fitted models #####

# This is figure S1 in Supp

# load results + data
load("./results/results_trendar1join_global_1850_2024.Rdata")

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
nlong = dim(Tanom_annual_df)[1]

fits_long = data.frame(unlist(dates),unlist(fits),c(rep("NASA",nlong-30),rep("HadCRUT",nlong),rep("NOAA",nlong),rep("Berkeley",nlong),rep("DCENT",nlong)))
names(fits_long) = c("year","value","dataset")

fitsAR_long = data.frame(unlist(dates),unlist(fitsAR),c(rep("NASA",nlong-30),rep("HadCRUT",nlong),rep("NOAA",nlong),rep("Berkeley",nlong),rep("DCENT",nlong)))
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

figs1=ggarrange(pfits,pfitsAR,nrow=2,ncol=1,labels=c("A","B"),align="v",common.legend = TRUE, legend = "bottom")
ggsave(file='./figures/globalfits.png',figs1,bg = "white",width=8, height=8) 

## Plot the DCENT ensemble analysis ####
# - Figure S2 in Supp 
load('./data/processed/annual_DCENT_anom.RData')
load(file='./results/ResultsDCENTSummary.RData')
results = ensemble_results
plots = ensemble_plots(results, lon, lat, "")

figs2 = ggarrange(plots[[2]],plots[[1]],plots[[3]],nrow=3,ncol=1,labels=c("A","B","C"),align="v")
ggsave('./figures/DCENT_ensemble.png', figs2, bg = "white",width=8, height=10) 

## Plot the hadCRUT ensemble analysis ####
# Figure S3 in Supp
load('./data/processed/annual_HadCRUT_anom.RData')
load(file='./results/ResultsHadCRUTSummary.RData')
results = ensemble_results
plots = ensemble_plots(results, lon, lat, "")

figs3 = ggarrange(plots[[2]],plots[[1]],plots[[3]],nrow=3,ncol=1,labels=c("A","B","C"),align="v")
ggsave('./figures/HadCRUT_ensemble.png', figs3, bg = "white",width=8, height=10) 