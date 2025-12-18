##Plots

#Define lat and lon cutoffs for plotting maps
latcutoff = c(-90,90)
#latcutoff = c(-70.5,70.5) #for -80-80 
longcutoff = c(-163,163)

##############Timing Plot######################### 
timing_plots = function(results, lon, lat, dataname){
  #returns a map plot of cpts timings 
  landmass = geom_polygon(data = map_data("world", wrap=c(-180,180)),
                         aes(x = long, y = lat, group = group),
                         fill = NA, color = "black",linewidth=0.2)
  
  if(max(lon) < 180){# longitude is -180-180
    
    cpts_raster = matrix2raster((results$ycpts[,,1]),x=lon,y=lat,layer=2)
    cpts_df = as.data.frame(cpts_raster,xy=T)
    
  }else{#longitude is 0-360, we need to shift it
    # this dataset has 0-360 longitude, so we need to reorganize into -180-180
    diff_shifted = matrixShiftLongitude((results$ycpts[,,1]),lon)
    
    cpts_raster = matrix2raster(diff_shifted$m,
                               x=diff_shifted$longitude,
                               y=lat,layer=2)
    cpts_df = as.data.frame(cpts_raster,xy=T)
  }
  
  plt = ggplot() + 
    geom_raster(data = cpts_df, aes(x = x, y = y, fill = layer)) + 
    landmass + 
    labs(title = dataname,
         x = "Longitude",
         y = "Latitude", 
         fill = "Year") + 
    # coord_cartesian(xlim=c(-163,163),ylim=c(-55,55)) + 
    coord_cartesian(xlim=longcutoff,ylim=latcutoff) + 
    scale_fill_viridis_c(na.value = "lightgrey")+
    theme(aspect.ratio = 1.0) +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border= element_blank())
    
  return(plt)
}


timing_combined_plots = function(results, lon, lat, dataname){
  
  landmass = geom_polygon(data = map_data("world", wrap=c(-180,180)),
                           aes(x = long, y = lat, group = group),
                           fill = NA, color = "black",linewidth=0.2)
    
    cpts_raster = matrix2raster(results$ycpts,x=lon,y=lat,layer=2)
    cpts_df = as.data.frame(cpts_raster,xy=T)
    
    ag_raster = matrix2raster(results$agree,x=lon,y=lat,layer=2)
    ag_df = as.data.frame(ag_raster,xy=T)
    ag_df$layer = as.factor(ag_df$layer)
  
    plt = ggplot() + 
    geom_raster(data = cpts_df, aes(x = x, y = y, fill = layer)) + 
    landmass + 
      coord_cartesian(xlim=longcutoff,ylim=latcutoff) + 
    scale_fill_viridis_c(na.value = "lightgrey") +
    geom_point(data=na.omit(ag_df),aes(x = x, y = y, size = layer),na.rm=T,shape=21,fill="white",border="black",stroke=0.25) + 
    scale_size_discrete(range = c(0.5, 2)) +
    labs(title = dataname,
           x = "Longitude",
           y = "Latitude", 
           fill = "Year",
           size = "Agreement") + 
    theme(aspect.ratio = 1.0) +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border= element_blank())
  
  return(plt)
}

##############Magnitude Plot#############################
mag_plots = function(results, lon, lat, dataname){
  #returns a map plot of magnitude of cpts in results 
  landmass = geom_polygon(data = map_data("world", wrap=c(-180,180)),
                         aes(x = long, y = lat, group = group),
                         fill = NA, color = "black",linewidth=0.2)

  if(max(lon) < 180){# longitude is -180-180
    
    results$dift[results$ncpts==0]=NA
    mag_raster = matrix2raster(results$dift*10,x=lon,y=lat,layer=2)
    mag_df = as.data.frame(mag_raster,xy=T)
    
  }else{#longitude is 0-360, we need to shift it
    # this dataset has 0-360 longitude, so we need to reorganize into -180-180
    results$dift[results$ncpts==0]=NA
    diff_shifted = matrixShiftLongitude(results$dift,lon)
    
    mag_raster = matrix2raster(diff_shifted$m*10,
                               x=diff_shifted$longitude,
                               y=lat,layer=2)
    mag_df = as.data.frame(mag_raster,xy=T)
  }

  plt <- ggplot() + 
    geom_raster(data = mag_df, aes(x = x, y = y, fill = layer)) + 
    landmass + 
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "firebrick",
                         midpoint = 0,
                         name = "°C/decade",
                         limits = range(c(-1, 2)),
                         oob = scales::squish,
                         space = "Lab", 
                         guide = "colorbar", 
                         na.value="lightgrey") + 
    labs(title = dataname, x = "Longitude",
         y = "Latitude", fill = "°C/decade") + 
    coord_cartesian(xlim=longcutoff,ylim=latcutoff) + 
    theme(aspect.ratio = 1.0) +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border= element_blank())
  
  return(plt)
}

mag_combined_plots = function(results, lon, lat, dataname){

  landmass = geom_polygon(data = map_data("world", wrap=c(-180,180)),
                           aes(x = long, y = lat, group = group),
                           fill = NA, color = "black",linewidth=0.2)
  
    mag_raster = matrix2raster(results$dift*10,x=lon,y=lat,layer=2)
    mag_df = as.data.frame(mag_raster,xy=T)
    
    ag_raster = matrix2raster(results$agree,x=lon,y=lat,layer=2)
    ag_df = as.data.frame(ag_raster,xy=T)
    ag_df$layer = as.factor(ag_df$layer)
    
  plt = ggplot() + 
    geom_raster(data = mag_df, aes(x = x, y = y, fill = layer)) + 
    landmass + 
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "firebrick",
                         midpoint = 0,
                         name = "°C/decade",
                         limits = range(c(-1, 2)),
                         oob = scales::squish,
                         space = "Lab", 
                         guide = "colorbar", 
                         na.value="lightgrey") + 
    coord_cartesian(xlim=longcutoff,ylim=latcutoff) + 
    geom_point(data=na.omit(ag_df),aes(x = x, y = y, size = layer),na.rm=T,shape=21,fill="black",border="black") +
    scale_size_discrete(range = c(0.5, 2)) +
    labs(title = dataname, x = "Longitude",
         y = "Latitude", fill = "°C/decade",size="Agreement") + 
    theme(aspect.ratio = 1.0) +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border= element_blank())
  
  return(plt)
}


##############Difficulty Plot#############################
difficulty_plots = function(results,lon,lat,dataname){
  #difficulty plots
  
  if(max(lon) < 180){# longitude is -180-180
    
    snr_raster = matrix2raster(results$difficulty,x=lon,y=lat,layer=2)
    snr_df = as.data.frame(snr_raster,xy=T)
    
  }else{
    diff_shifted = matrixShiftLongitude(results$difficulty,lon)
    
    snr_raster = matrix2raster(diff_shifted$m,
                               x=diff_shifted$longitude,
                               y=lat,layer=2)
    snr_df = as.data.frame(snr_raster,xy=T)
  }
  
  plt = ggplot() +
    geom_raster(data = snr_df, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = map_data("world"),
                 aes(x = long, y = lat, group = group), 
                 fill = NA, 
                 color = "black",
                 linewidth=0.2) +
    coord_cartesian(xlim=longcutoff,
                    ylim=latcutoff)+
    scale_fill_gradient(low = "white", 
                        high = "darkred",
                         limits = c(0,4.5),
                         guide = "colorbar",
                         na.value="lightgrey")+
    labs(title = dataname, 
         x = "Longitude",
         y = "Latitude",
         fill = "SNR")+
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border= element_blank())
  
  return(plt)
}

##############Quadratic Plot#############################
quadtrend_plots = function(results,lon,lat,dataname){
  #quadfit plots
  
  if(max(lon) < 180){
  
  quad_raster = matrix2raster(results[,,2],x=lon,y=lat,layer=2)
  quad_df = as.data.frame(quad_raster,xy=T)
  
  pval_raster = matrix2raster(results[,,4],x=lon,y=lat,layer=2)
  pval_df = as.data.frame(pval_raster,xy=T)
  sig_df = subset(pval_df,layer > 0.05)
  
  }else{
    mag2_shifted = matrixShiftLongitude(results[,,2],lon)
    pval_shifted = matrixShiftLongitude(results[,,4],lon)
    
    quad_raster = matrix2raster(mag2_shifted$m,
                                x=mag2_shifted$longitude,
                                y=lat,
                                layer=2)
    quad_df = as.data.frame(quad_raster,xy=T)
    
    pval_raster = matrix2raster(pval_shifted$m,
                                x=pval_shifted$longitude,
                                y=lat,
                                layer=2)
    pval_df = as.data.frame(pval_raster,xy=T)
    sig_df = subset(pval_df,layer > 0.05)
  }
  
  plt = ggplot() +
    geom_raster(data = quad_df, aes(x = x, y = y, fill = layer)) +
    geom_polygon(data = map_data("world"),
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black",linewidth=0.2) +
    coord_cartesian(xlim=longcutoff,ylim=latcutoff)+
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
                         midpoint = 0,
                         limits = c(-0.001,0.002),
                         space = "Lab",
                         guide = "colorbar",
                         na.value="lightgrey")+
    labs(title = dataname, x = "Longitude",
         y = "Latitude",fill = latex2exp::TeX('°C/decade$^2$'))+
    geom_point(data = sig_df, aes(x = x, y = y),
               alpha=0.2, size = 0.5, shape = 15)+
    guides(alpha="none")+
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border= element_blank())
  
  return(plt)
}

##############Latitudinal Plots#############################
latitudinal_plots = function(lats,ncpts,dift,ycpts,plottype){
  
  # Function to produce latitudinal plots that show percentage of grid cells showing changepoints
  # Plot types are to show (1) all changes, (2) acceleleration vs deceleration or (3) accelerations only and their decade
  
  # lats is the range of latitudes to plot
  # plottype is 1,2 or 3 as described above
  # ncpts is a matrix with number of changepoints detected nlonxnlat
  # dift is a matrix with magnitudes of changepoints lonxnlat
  # ycpts is a matrix with year of changepoints detected nlonxnlat
  # plottype is 1,2 or 3:
  # to show (1) all changes, (2) acceleleration vs deceleration or (3) decade changes
  
  nlon = dim(ncpts)[1]
  
  #prepare the data frames
  
  #all changes confounded
  pcpts_lat = (colSums(ncpts,na.rm=T)/nlon)*100
  dfl = as.data.frame(cbind(lats,pcpts_lat))
  names(dfl) = c("lat","pc")
  
  #with acceleration vs deceleration per latitude
  acc_lat = (colSums(dift > 0 & ncpts > 0,na.rm=T)/nlon)*100
  dec_lat = (colSums(dift < 0 & ncpts > 0,na.rm=T)/nlon)*100
  dfs = as.data.frame(cbind(lats,acc_lat,dec_lat))
  names(dfs) = c("lat","acc","dec")
  #df_sign = melt( data = dfs, id.vars = 'lat')   # melt data into long format
  
  #with acceleration per latitude per decade
  acp2010s = (colSums(ycpts >= 2010 & dift > 0,na.rm=T)/nlon)*100
  acp2000s = (colSums(ycpts >= 2000 & ycpts < 2010 & dift > 0,na.rm=T)/nlon)*100
  acp1990s = (colSums(ycpts >= 1990 & ycpts < 2000 & dift > 0,na.rm=T)/nlon)*100
  acp1980s = (colSums(ycpts < 1990 & dift >0,na.rm=T)/nlon)*100
  dfa = as.data.frame(cbind(lats,acp2010s,acp2000s,acp1990s,acp1980s))
  names(dfa) = c("lat","2010s","2000s","1990s","1980s")
  dfa_long = melt(data = dfa, id.vars = 'lat') 
  
  #with deceleration per latitude per decade
  dcp2010s = (colSums(ycpts >= 2010 & dift < 0,na.rm=T)/nlon)*100
  dcp2000s = (colSums(ycpts >= 2000 & ycpts < 2010 & dift < 0,na.rm=T)/nlon)*100
  dcp1990s = (colSums(ycpts >= 1990 & ycpts < 2000 & dift < 0,na.rm=T)/nlon)*100
  dcp1980s = (colSums(ycpts < 1990 & dift < 0,na.rm=T)/nlon)*100
  dfd = as.data.frame(cbind(lats,dcp2010s,dcp2000s,dcp1990s,dcp1980s))
  names(dfd) = c("lat","2010s","2000s","1990s","1980s")
  dfd_long = melt(data = dfd, id.vars = 'lat') 
  
  if(plottype ==1){#basic plot of all changes detected per latitude
    
    plt = ggplot(dfl,aes(x=lat,y=pc)) +
      geom_col(linewidth=0.1,col="darkblue",fill="lightblue") + coord_flip() +
      labs(y = "Changepoints detected (%)",
           x = "Latitude") +
      scale_y_continuous(expand = c(0,0),limits = c(0,ceiling(max(dfl$pc)))) +
      scale_x_continuous(expand = c(0,0))+
      theme_linedraw() 
  }
  
  if(plottype == 2){#plots of changes detected per latitude, separated by acc vs deceleration
    
    plt = ggplot(data = dfs) +
      geom_col(aes(x=lat,y=acc,fill="acc")) +
      geom_col(aes(x=lat,y=-dec,fill="dec")) +
      coord_flip() +
      geom_hline(yintercept = 0)+
      labs(y = "Changepoints detected (%)",
           x = "Latitude",
           fill = " ") +
      scale_fill_manual(values = c("lightpink3", "steelblue"),labels = c("Acceleration","Deceleration"))+
      theme_linedraw() 
  }
  
  if(plottype == 3){#we plot accelerations only and organize per decade
    
    plt = ggplot() +
      geom_col(data = dfa_long,aes(x=lat,y=value,fill=variable)) +
      geom_col(data = dfd_long,aes(x=lat,y=-value,fill=variable)) +
      scale_y_continuous(expand = c(0,0),breaks=seq(-10,20,10),labels=c("10","0","10","20"),limits = c(-10,25)) +
      scale_x_continuous(expand = c(0,0),limits=c(-79,79)) +
      coord_flip() +
      geom_hline(yintercept = 0) +
      labs(y = "Changepoints detected (%)",
           x = "Latitude",
           fill = "Decade") +
      scale_fill_viridis_d(direction = -1)+
      theme_linedraw()+
      theme(legend.position="bottom")
  }
return(plt)
}



