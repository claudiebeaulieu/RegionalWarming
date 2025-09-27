################################################################################
######################## Regional warming project ##############################
################################################################################
# The main goal of this project is to determine regions that have undergone a 
# warming acceleration since 1970.

################################################################################
# The script is used to plot simulation results - Figure S3 in Supp.


timings = c(1980,1990,2000,2010)
energy = seq(from=0.25, to=3, by=0.25)

#4logn penalty

load("./results/power4logn_1000reps.RData")

data = data.frame(cbind(power))
names(data)=c("1980","1990","2000","2010")

datalong = melt(data)
names(datalong)=c("timing","power")

datalong$energy = rep(energy,4)

p = ggplot(datalong,aes(x=energy, y=power, colour=timing)) 

p1 = p + geom_line(linewidth=1) +
  scale_color_viridis_d() + 
  labs(x="SNR",y="Power",color="Timing") +
  scale_x_continuous(expand=c(0,0),limits=c(0,3)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.05)) +
  theme_linedraw() 

#3logn penalty
load("./results/power3logn_1000reps.RData")

data = data.frame(cbind(power))
names(data)=c("1980","1990","2000","2010")

datalong = melt(data)
names(datalong)=c("timing","power")

datalong$energy = rep(energy,4)

p = ggplot(datalong,aes(x=energy, y=power, colour=timing)) 

p2 = p + geom_line(linewidth=1) +
  scale_color_viridis_d() + 
  labs(x="SNR",y="Power",color="Timing") +
  scale_x_continuous(expand=c(0,0),limits=c(0,3)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.05)) +
  theme_linedraw() 

pdf(file='./figures/sims_power.pdf',width=8, height=4)
ggarrange(p1,p2,nrow=1,ncol=2,labels=c("A","B"),align="v",common.legend = TRUE, legend = "bottom")
dev.off()
