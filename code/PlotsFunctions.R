##Plots

# Define lat and lon cutoffs for plotting maps
latcutoff  = c(-90, 90)
longcutoff = c(-180, 180)

# Define the land lines
landmass = geom_polygon(data = map_data("world", wrap = c(-180, 180)),
  aes(x = long, y = lat, group = group),
  fill = NA, color = "black", linewidth = 0.2)

# Ordinal labels for legend: "2nd changepoint", "3rd changepoint", etc.
ordinal_label = function(i) {
  suffix = c("th", "st", "nd", "rd")
  mod    = i %% 100
  if (mod %in% 11:13) paste0(i, "th changepoint")
  else paste0(i, suffix[min(i %% 10 + 1, 4)], " changepoint")
}

# Helper: convert one slice of ycpts to a dataframe, handling lon shift if needed
slice_to_df = function(mat, lon, lat) {
  if (max(lon) < 180) {
    r = matrix2raster(mat, x = lon, y = lat, layer = 2)
  } else {
    shifted = matrixShiftLongitude(mat, lon)
    r = matrix2raster(shifted$m, x = shifted$longitude, y = lat, layer = 2)
  }
  as.data.frame(r, xy = TRUE)
}

# Timing Plots for individual datasets
timing_plots = function(results, lon, lat, dataname) {
  
  n_cpts  = dim(results$ycpts)[3]
  cpt_list = lapply(seq_len(n_cpts), function(i) slice_to_df(results$ycpts[,,i], lon, lat))
  
  NA_df = slice_to_df(results$NAs, lon, lat)
  NA_df[NA_df == 0] = NA
  
  all_vals      = range(unlist(lapply(cpt_list, `[[`, "layer")), na.rm = TRUE)
  n_overlays    = n_cpts - 1
  overlay_names = sapply(seq_len(n_overlays) + 1, ordinal_label)
  point_shapes  = c(3, 2, 1, 4, 5)  
  shape_values  = setNames(point_shapes[seq_len(n_overlays)], overlay_names)
  
  plt = ggplot() +
    geom_tile(data = cpt_list[[1]], aes(x = x, y = y, fill = layer)) +
    scale_fill_viridis_c(na.value = "white", name = "Year", limits = all_vals) +
    new_scale_color() +
    landmass +
    labs(title = dataname, x = "Longitude", y = "Latitude") +
    geom_tile(data = subset(NA_df, is.na(layer)),
              aes(x = x, y = y), fill = "grey70", alpha = 0.6) +
    coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE)
  
  # Add one geom_point per overlay changepoint
  for (i in seq_len(n_overlays)) {
    df   = subset(cpt_list[[i + 1]], !is.na(layer))
    name = overlay_names[[i]]
    plt  = plt + geom_point(data = df,
      aes(x = x, y = y, color = layer, shape = !!name),
      size = 0.5, stroke = 0.5)
  }
  
  plt = plt +
    scale_color_viridis_c(name = "Year", limits = all_vals, guide = "none") +
    scale_shape_manual(name = NULL, values = shape_values,
                       guide = guide_legend(
                         override.aes = list(size = 2, stroke = 1, color = "black"))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = "right")
  return(plt)
}

# Timing plots for combined dataset - with extra symbol showing agreement
timing_combined_plots = function(results, lon, lat, dataname) {
  
  if (length(dim(results$ycpts)) == 2) {
    results$ycpts = array(results$ycpts, dim = c(dim(results$ycpts), 1))
  }
  
  n_cpts     = dim(results$ycpts)[3]
  n_overlays = n_cpts - 1
  
  cpts_df     = slice_to_df(results$ycpts[,,1], lon, lat)
  ag_df       = slice_to_df(results$agree, lon, lat)
  ag_df$layer = as.factor(ag_df$layer)
  
  plt = ggplot() +
    geom_tile(data = cpts_df, aes(x = x, y = y, fill = layer)) +
    landmass +
    scale_fill_viridis_c(na.value = "white", name = "Year",
                         limits = range(cpts_df$layer, na.rm = TRUE),
                         guide  = guide_colorbar(order = 2)) +
    coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
    geom_point(data = na.omit(ag_df),
               aes(x = x, y = y, size = layer),
               shape = 21, fill = "transparent", color = "darkgrey", stroke = 0.4) +
    scale_size_discrete(range = c(0.5, 2), name = "Agreement",
                        guide = guide_legend(order = 1)) +
    labs(title = dataname, x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  if (n_overlays > 0) {
    overlay_names = sapply(seq_len(n_overlays) + 1, ordinal_label)
    color_values  = setNames(rep("black", n_overlays), overlay_names)
    point_shapes  = c(3, 2, 1, 4, 5)
    shape_values  = setNames(point_shapes[seq_len(n_overlays)], overlay_names)
    legend_df     = data.frame(x = 200, y = 200,
                                label = factor(overlay_names, levels = overlay_names))
    
    plt = plt +
      new_scale_color() +
      geom_point(data = legend_df,
                 aes(x = x, y = y, color = label, shape = label),
                 size = 2, stroke = 1) +
      scale_color_manual(name = NULL, values = color_values,
                         guide = guide_legend(order = 3)) +
      scale_shape_manual(name = NULL, values = shape_values,
                         guide = guide_legend(order = 3))
    
  } else {
    # Always add dummy cross so legend is consistent when combined with ggarrange
    dummy_df = data.frame(x = 200, y = 200)
    
    plt = plt +
      new_scale_color() +
      geom_point(data = dummy_df,
                 aes(x = x, y = y, color = "2nd changepoint"),
                 shape = 3, size = 2, stroke = 1) +
      scale_color_manual(name = NULL,
                         values = c("2nd changepoint" = "black"),
                         guide = guide_legend(order = 3,
                                              override.aes = list(shape = 3, size = 2, stroke = 1, linetype = 0)))
  }
  
  return(plt)
}

# latcutoff = c(-90,90)
# longcutoff = c(-180,180)
# 
# landmass = geom_polygon(data = map_data("world", wrap=c(-180,180)),
#                         aes(x = long, y = lat, group = group),
#                         fill = NA, color = "black",linewidth=0.2)
# 
# ##############Timing Plot######################### 
# timing_plots = function(results, lon, lat, dataname){
#   #returns a map plot of cpts timings 
#   
#   if(max(lon) < 180){# longitude is -180-180
#     
#     cpt1_raster = matrix2raster((results$ycpts[,,1]),x=lon,y=lat,layer=2)
#     cpt1_df = as.data.frame(cpt1_raster,xy=T)
#     cpt2_raster = matrix2raster((results$ycpts[,,2]),x=lon,y=lat,layer=2)
#     cpt2_df = as.data.frame(cpt2_raster,xy=T)
#     
#     NA_raster = matrix2raster((results$NAs),x=lon,y=lat,layer=2)
#     NA_df = as.data.frame(NA_raster,xy=T)
#     NA_df[NA_df == 0] = NA # zeros indicate missing values so we replace those with NA symbols
#       
#   }else{#longitude is 0-360, we need to shift it to -180-180
#     cpt1_shifted = matrixShiftLongitude((results$ycpts[,,1]),lon)
#     cpt1_raster = matrix2raster(cpt1_shifted$m,
#                                x=cpt1_shifted$longitude,
#                                y=lat,layer=2)
#     cpt1_df = as.data.frame(cpt1_raster,xy=T)
#     
#     cpt2_shifted = matrixShiftLongitude((results$ycpts[,,2]),lon)
#     cpt2_raster = matrix2raster(cpt2_shifted$m,
#                                  x=cpt2_shifted$longitude,
#                                  y=lat,layer=2)
#     cpt2_df = as.data.frame(cpt2_raster,xy=T)
#     
#     NA_shifted = matrixShiftLongitude(results$NAs,lon)
#     NA_raster = matrix2raster(NA_shifted$m,
#                                 x=NA_shifted$longitude,
#                                 y=lat,layer=2)
#     NA_df = as.data.frame(NA_raster,xy=T)
#     NA_df[NA_df == 0] = NA 
#   }
#   
#   plt = ggplot() +
#     geom_tile(data = cpt1_df, aes(x = x, y = y, fill = layer)) +
#     scale_fill_viridis_c(na.value = "white",
#       name = "Year",
#       limits = range(c(cpt1_df$layer, cpt2_df$layer), na.rm = TRUE)) +
#     new_scale_color() +
#     landmass +
#     labs(title = dataname,
#          x = "Longitude",
#          y = "Latitude") +
#     geom_tile(data = subset(NA_df, is.na(layer)),
#               aes(x = x, y = y), fill = "grey70", alpha = 0.6) +
#     coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
#     geom_point(data = subset(cpt2_df, !is.na(layer)),
#                aes(x = x, y = y, color = layer, shape = "2nd changepoint"),
#                size = 0.5, stroke = 0.5) +
#     scale_color_viridis_c(name = "Year",
#       limits = range(c(cpt1_df$layer, cpt2_df$layer), na.rm = TRUE),
#       guide = "none") +  # Color handled by fill legend; shape gets its own legend
#     scale_shape_manual(name = NULL,
#       values = c("2nd changepoint" = 3),
#       guide = guide_legend(
#         override.aes = list(size = 2, stroke = 1, color = "black"))) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
#           legend.position = "right")
#   return(plt)
# }
# 
# 
# timing_combined_plots = function(results, lon, lat, dataname){
#     
#     cpts_raster = matrix2raster(results$ycpts,x=lon,y=lat,layer=2)
#     cpts_df = as.data.frame(cpts_raster,xy=T)
#     
#     ag_raster = matrix2raster(results$agree,x=lon,y=lat,layer=2)
#     ag_df = as.data.frame(ag_raster,xy=T)
#     ag_df$layer = as.factor(ag_df$layer)
#     
#     cross_legend_df = data.frame(x = 200, y = 200)
#   
#     plt = ggplot() + 
#     geom_tile(data = cpts_df, aes(x = x, y = y, fill = layer)) + 
#     landmass +
#     scale_fill_viridis_c(na.value = "white",
#                          name     = "Year",
#                          limits   = range(cpts_df$layer, na.rm = TRUE),
#                          guide    = guide_colorbar(order = 2)) +
#     coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
#     geom_point(data=na.omit(ag_df),
#                aes(x = x, y = y, size = layer),
#                shape=21,fill="transparent",color="darkgrey",stroke=0.4) + 
#     scale_size_discrete(range = c(0.5, 2),
#                         name = "Agreement",
#                         guide=guide_legend(order=1)) +
#     new_scale_color() +
#     geom_point(data = cross_legend_df,
#                 aes(x = x, y = y, color = "2nd changepoint"),
#                 shape = 3, size = 2, stroke = 1) +
#     scale_color_manual(name = NULL,
#         values = c("2nd changepoint" = "black"),
#         guide  = guide_legend(order=3,override.aes = list(shape = 3, size = 2, stroke = 1, linetype = 0)))+
#     labs(title = dataname,
#            x = "Longitude",
#            y = "Latitude", 
#            fill = "Year") + 
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           panel.border= element_blank(),
#           plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
#   return(plt)
# }

##############Magnitude Plot#############################

mag_plots = function(results, lon, lat, dataname) {
  
  if (length(dim(results$dift)) == 2) {
    results$dift = array(results$dift, dim = c(dim(results$dift), 1))
  }
  
  n_cpts     = dim(results$dift)[3]
  n_overlays = n_cpts - 1
  
  mag_list = lapply(seq_len(n_cpts), function(i) slice_to_df(results$dift[,,i] * 10, lon, lat))
  
  NA_df = slice_to_df(results$NAs, lon, lat)
  NA_df[NA_df == 0] = NA
  
  overlay_names = sapply(seq_len(n_overlays) + 1, ordinal_label)
  point_shapes  = c(3, 2, 1, 4, 5)
  shape_values  = setNames(point_shapes[seq_len(n_overlays)], overlay_names)
  
  plt = ggplot() +
    geom_tile(data = mag_list[[1]], aes(x = x, y = y, fill = layer)) +
    landmass +
    scale_fill_gradient2(low = "darkblue", mid = "ivory", high = "firebrick",
                         midpoint = 0, name = "°C/decade",
                         limits = c(-1, 2), oob = scales::squish,
                         na.value = "white") +
    scale_color_gradient2(low = "darkblue", mid = "ivory", high = "firebrick",
                          midpoint = 0, name = "°C/decade",
                          limits = c(-1, 2), oob = scales::squish,
                          na.value = "white", guide = "none") +
    geom_tile(data = subset(NA_df, is.na(layer)),
              aes(x = x, y = y), fill = "grey70", alpha = 0.6) +
    coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
    labs(title = dataname, x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = "right")
  
  for (i in seq_len(n_overlays)) {
    df   = subset(mag_list[[i + 1]], !is.na(layer))
    name = overlay_names[[i]]
    plt  = plt + geom_point(
      data = df,
      aes(x = x, y = y, color = layer, shape = !!name),
      size = 0.5, stroke = 0.5)
  }
  
  if (n_overlays > 0) {
    plt = plt +
      scale_shape_manual(name = NULL, values = shape_values,
                         guide = guide_legend(
                           override.aes = list(size = 2, stroke = 1, color = "black")))
  }
  
  return(plt)
}


mag_combined_plots <- function(results, lon, lat, dataname) {
  
  if (length(dim(results$dift)) == 2) {
    results$dift = array(results$dift, dim = c(dim(results$dift), 1))
  }
  
  n_cpts     = dim(results$dift)[3]
  n_overlays = n_cpts - 1
  
  mag_df      = slice_to_df(results$dift[,,1] * 10, lon, lat)
  ag_df       = slice_to_df(results$agree, lon, lat)
  ag_df$layer = as.factor(ag_df$layer)
  
  plt = ggplot() +
    geom_tile(data = mag_df, aes(x = x, y = y, fill = layer)) +
    landmass +
    scale_fill_gradient2(low = "darkblue", mid = "ivory", high = "firebrick",
                         midpoint = 0, name = "°C/decade",
                         limits = c(-1, 2), oob = scales::squish,
                         space = "Lab", na.value = "white",
                         guide = guide_colorbar(order = 2)) +
    coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
    geom_point(data = na.omit(ag_df),
               aes(x = x, y = y, size = layer),
               shape = 21, fill = "transparent", color = "black", stroke = 0.3) +
    scale_size_discrete(range = c(0.5, 2), name = "Agreement",
                        guide = guide_legend(order = 1)) +
    labs(title = dataname, x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  if (n_overlays > 0) {
    overlay_names = sapply(seq_len(n_overlays) + 1, ordinal_label)
    color_values  = setNames(rep("black", n_overlays), overlay_names)
    point_shapes  = c(3, 2, 1, 4, 5)
    shape_values  = setNames(point_shapes[seq_len(n_overlays)], overlay_names)
    legend_df     = data.frame(x = 200, y = 200,
                                label = factor(overlay_names, levels = overlay_names))
    
    plt = plt +
      new_scale_color() +
      geom_point(data = legend_df,
                 aes(x = x, y = y, color = label, shape = label),
                 size = 2, stroke = 1) +
      scale_color_manual(name = NULL, values = color_values,
                         guide = guide_legend(order = 3)) +
      scale_shape_manual(name = NULL, values = shape_values,
                         guide = guide_legend(order = 3))
    
  } else {
    # Always add dummy cross for consistent legend across ggarrange panels
    dummy_df = data.frame(x = 200, y = 200)
    
    plt = plt +
      new_scale_color() +
      geom_point(data = dummy_df,
                 aes(x = x, y = y, color = "2nd changepoint"),
                 shape = 3, size = 2, stroke = 1) +
      scale_color_manual(name = NULL,
                         values = c("2nd changepoint" = "black"),
                         guide = guide_legend(order = 3,
                                              override.aes = list(shape = 3, size = 2, stroke = 1, linetype = 0)))
  }
  
  return(plt)
}


# mag_plots = function(results, lon, lat, dataname){
# 
#   if(max(lon) < 180){# longitude is -180-180
#     
#     #results$dift[results$ncpts==0]=NA
#     mag1_raster = matrix2raster(results$dift[,,1]*10,x=lon,y=lat,layer=2)
#     mag1_df = as.data.frame(mag1_raster,xy=T)
#     
#     mag2_raster = matrix2raster(results$dift[,,2]*10,x=lon,y=lat,layer=2)
#     mag2_df = as.data.frame(mag2_raster,xy=T)
#     
#     NA_raster = matrix2raster((results$NAs),x=lon,y=lat,layer=2)
#     NA_df = as.data.frame(NA_raster,xy=T)
#     NA_df[NA_df == 0] = NA # zeros indicate missing values so we replace those with NA symbols
#     
#   }else{#longitude is 0-360, we need to shift it
#     # this dataset has 0-360 longitude, so we need to reorganize into -180-180
#     #results$dift[results$ncpts==0]=NA
#     mag1_shifted = matrixShiftLongitude(results$dift[,,1],lon)
#     mag1_raster = matrix2raster(mag1_shifted$m*10,
#                                x=mag1_shifted$longitude,
#                                y=lat,layer=2)
#     mag1_df = as.data.frame(mag1_raster,xy=T)
#     
#     mag2_shifted = matrixShiftLongitude(results$dift[,,2],lon)
#     mag2_raster = matrix2raster(mag2_shifted$m*10,
#                                 x=mag2_shifted$longitude,
#                                 y=lat,layer=2)
#     mag2_df = as.data.frame(mag2_raster,xy=T)
#     
#     NA_shifted = matrixShiftLongitude(results$NAs,lon)
#     NA_raster = matrix2raster(NA_shifted$m,
#                               x=NA_shifted$longitude,
#                               y=lat,layer=2)
#     NA_df = as.data.frame(NA_raster,xy=T)
#     NA_df[NA_df == 0] = NA # zeros indicate missing values so we replace those with NA symbols
#   }
#   
#   plt <- ggplot() + 
#     geom_tile(data = mag1_df, aes(x = x, y = y, fill = layer)) + 
#     landmass + 
#     scale_fill_gradient2(low = "darkblue", mid = "ivory", high = "firebrick",
#                          midpoint = 0,
#                          name = "°C/decade",
#                          limits = c(-1, 2),
#                          oob = scales::squish,
#                          na.value = "white") + 
#     scale_color_gradient2(low = "darkblue", mid = "ivory", high = "firebrick",
#                           midpoint = 0,
#                           name = "°C/decade",
#                           limits = c(-1, 2),
#                           oob = scales::squish,
#                           na.value = "white",
#                           guide = "none") +          # suppress duplicate color bar
#     geom_point(data = subset(mag2_df, !is.na(layer)),
#                aes(x = x, y = y, color = layer, shape = "2nd changepoint"),  # shape inside aes()
#                size = 0.5, stroke = 0.5) +
#     scale_shape_manual(name = NULL,
#                        values = c("2nd changepoint" = 3),
#                        guide = guide_legend(
#                          override.aes = list(size = 2, stroke = 1, color = "black"))) +
#     labs(title = dataname, x = "Longitude", y = "Latitude") + 
#     geom_tile(data = subset(NA_df, is.na(layer)), 
#               aes(x = x, y = y), fill = "grey70", alpha = 0.6) +
#     coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
#           legend.position = "right")
#   
#   return(plt)
# }
# 
# mag_combined_plots = function(results, lon, lat, dataname){
#   
#     mag_raster = matrix2raster(results$dift*10,x=lon,y=lat,layer=2)
#     mag_df = as.data.frame(mag_raster,xy=T)
#     
#     ag_raster = matrix2raster(results$agree,x=lon,y=lat,layer=2)
#     ag_df = as.data.frame(ag_raster,xy=T)
#     ag_df$layer = as.factor(ag_df$layer)
#     
#     cross_legend_df = data.frame(x = 200, y = 200)
#     
#     plt = ggplot() +
#       geom_tile(data = mag_df, aes(x = x, y = y, fill = layer)) +
#       landmass +
#       scale_fill_gradient2(
#         low      = "darkblue", mid = "ivory", high = "firebrick",
#         midpoint = 0,
#         name     = "°C/decade",
#         limits   = c(-1, 2),
#         oob      = scales::squish,
#         space    = "Lab",
#         na.value = "white",
#         guide    = guide_colorbar(order = 2)) +
#       coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
#       geom_point(data = na.omit(ag_df),
#                  aes(x = x, y = y, size = layer),
#                  shape = 21, fill = "transparent", color = "black", stroke = 0.3) +
#       scale_size_discrete(range = c(0.5, 2),
#         name  = "Agreement",
#         guide = guide_legend(order = 1)) +
#       new_scale_color() +
#       geom_point(data = cross_legend_df,
#                  aes(x = x, y = y, color = "2nd changepoint"),
#                  shape = 3, size = 2, stroke = 1) +
#       scale_color_manual(name   = NULL,
#         values = c("2nd changepoint" = "black"),
#         guide  = guide_legend(order        = 3,
#           override.aes = list(shape = 3, size = 2, stroke = 1, linetype = 0))) +
#       labs(title = dataname,
#            x     = "Longitude",
#            y     = "Latitude") +
#       theme_bw() +
#       theme(panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank(),
#             panel.border      = element_blank(),
#             plot.margin       = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
#     return(plt)
# }
# 

##############Difficulty Plot#############################

difficulty_plots = function(results, lon, lat, dataname) {
  
  if (length(dim(results$difficulty)) == 2) {
    results$difficulty <- array(results$difficulty, dim = c(dim(results$difficulty), 1))
  }
  
  n_cpts     = dim(results$difficulty)[3]
  n_overlays = n_cpts - 1
  
  snr_list = lapply(seq_len(n_cpts), function(i) slice_to_df(results$difficulty[,,i], lon, lat))
  
  NA_df = slice_to_df(results$NAs, lon, lat)
  NA_df[NA_df == 0] = NA
  
  overlay_names = sapply(seq_len(n_overlays) + 1, ordinal_label)
  point_shapes  = c(3, 2, 1, 4, 5)
  shape_values  = setNames(point_shapes[seq_len(n_overlays)], overlay_names)
  
  plt = ggplot() +
    geom_tile(data = snr_list[[1]], aes(x = x, y = y, fill = layer)) +
    landmass +
    scale_fill_gradientn(name = "SNR", colors = c("#FFFFCC", "#FD8D3C", "#800026"),
                         trans = "log1p", limits = c(0, 5), na.value = "white") +
    scale_color_gradientn(name = "SNR", colors = c("#FFFFCC", "#FD8D3C", "#800026"),
                          trans = "log1p", limits = c(0, 5),
                          na.value = "white", guide = "none") +
    geom_tile(data = subset(NA_df, is.na(layer)),
              aes(x = x, y = y), fill = "grey70", alpha = 0.6) +
    coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
    labs(title = dataname, x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = "right")
  
  for (i in seq_len(n_overlays)) {
    df   = subset(snr_list[[i + 1]], !is.na(layer))
    name = overlay_names[[i]]
    plt  = plt + geom_point(
      data = df,
      aes(x = x, y = y, color = layer, shape = !!name),
      size = 0.5, stroke = 0.5)
  }
  
  if (n_overlays > 0) {
    plt = plt +
      scale_shape_manual(name = NULL, values = shape_values,
                         guide = guide_legend(
                           override.aes = list(size = 2, stroke = 1, color = "black")))
  }
  
  return(plt)
}

# difficulty_plots = function(results,lon,lat,dataname){
#   #difficulty plots
#   
#   if(max(lon) < 180){# longitude is -180-180
#     
#     snr1_raster = matrix2raster(results$difficulty[,,1],x=lon,y=lat,layer=2)
#     snr1_df = as.data.frame(snr1_raster,xy=T)
#     
#     snr2_raster = matrix2raster(results$difficulty[,,2],x=lon,y=lat,layer=2)
#     snr2_df = as.data.frame(snr2_raster,xy=T)
#     
#     NA_raster = matrix2raster((results$NAs),x=lon,y=lat,layer=2)
#     NA_df = as.data.frame(NA_raster,xy=T)
#     NA_df[NA_df == 0] = NA # zeros indicate missing values so we replace those with NA symbols
#     
#   }else{
#     snr1_shifted = matrixShiftLongitude(results$difficulty[,,1],lon)
#     snr1_raster = matrix2raster(snr1_shifted$m,
#                                x=snr1_shifted$longitude,
#                                y=lat,layer=2)
#     snr1_df = as.data.frame(snr1_raster,xy=T)
#     
#     snr2_shifted = matrixShiftLongitude(results$difficulty[,,2],lon)
#     snr2_raster = matrix2raster(snr2_shifted$m,
#                                 x=snr2_shifted$longitude,
#                                 y=lat,layer=2)
#     snr2_df = as.data.frame(snr2_raster,xy=T)
#     
#     NA_shifted = matrixShiftLongitude(results$NAs,lon)
#     NA_raster = matrix2raster(NA_shifted$m,
#                               x=NA_shifted$longitude,
#                               y=lat,layer=2)
#     NA_df = as.data.frame(NA_raster,xy=T)
#     NA_df[NA_df == 0] = NA # zeros indicate missing values so we replace those with NA symbols
#   }
#   
#   plt = ggplot() +
#     geom_tile(data = snr1_df, aes(x = x, y = y, fill = layer)) +
#     landmass +
#     scale_fill_gradientn(name = "SNR",colors = c("#FFFFCC", "#FD8D3C", "#800026"),
#       trans = "log1p", limits = c(0, 5), na.value = "white") +
#     scale_color_gradientn(name = "SNR", colors = c("#FFFFCC", "#FD8D3C", "#800026"),
#       trans = "log1p", limits = c(0, 5), na.value = "white") +
#     geom_point(data = subset(snr2_df, !is.na(layer)), aes(x = x, y = y, color = layer),
#                shape = 3, size = 0.5, stroke = 0.5) +
#     guides(color="none") +
#     coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
#     geom_tile(data = subset(NA_df, is.na(layer)), 
#                 aes(x = x, y = y), fill = "grey70", alpha = 0.6) +
#     labs(title = dataname, 
#          x = "Longitude",
#          y = "Latitude",
#          fill = "SNR")+
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           panel.border= element_blank())
#   
#   
#   return(plt)
# }

##############Quadratic Plot#############################
quadtrend_plots = function(results,lon,lat,dataname){
  #quadfit plots
  
  if(max(lon) < 180){
  
  quad_raster = matrix2raster(results$quadfit[,,2],x=lon,y=lat,layer=2)
  quad_df = as.data.frame(quad_raster,xy=T)
  
  pval_raster = matrix2raster(results$quadfit[,,4],x=lon,y=lat,layer=2)
  pval_df = as.data.frame(pval_raster,xy=T)
  sig_df = subset(pval_df,layer > 0.05)
  
  NA_raster = matrix2raster((results$NAs),x=lon,y=lat,layer=2)
  NA_df = as.data.frame(NA_raster,xy=T)
  NA_df[NA_df == 0] = NA # zeros indicate missing values so we replace those with NA symbols
  
  }else{
    mag2_shifted = matrixShiftLongitude(results$quadfit[,,2],lon)
    pval_shifted = matrixShiftLongitude(results$quadfit[,,4],lon)
    
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
    
    NA_shifted = matrixShiftLongitude(results$NAs,lon)
    NA_raster = matrix2raster(NA_shifted$m,
                              x=NA_shifted$longitude,
                              y=lat,layer=2)
    NA_df = as.data.frame(NA_raster,xy=T)
    NA_df[NA_df == 0] = NA # zeros indicate missing values so we replace those with NA symbols
  }
  
  plt = ggplot() +
    geom_tile(data = quad_df, aes(x = x, y = y, fill = layer)) +
    landmass +
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
                         midpoint = 0,
                         limits = c(-0.001,0.002),
                         space = "Lab",
                         guide = "colorbar",
                         na.value="white")+
    coord_fixed(ratio = 1, xlim = longcutoff, ylim = latcutoff, expand = FALSE) +
    geom_tile(data = subset(NA_df, is.na(layer)), 
                aes(x = x, y = y), fill = "grey70", alpha = 0.6) +
    labs(title = dataname, x = "Longitude",
         y = "Latitude",fill = latex2exp::TeX('°C/decade$^2$'))+
    geom_point(data = sig_df, aes(x = x, y = y),
               alpha=0.2, size = 0.5, shape = 15)+
    guides(alpha="none")+
    theme_bw() +
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
  # dift is a matrix with magnitudes of changepoints lonxnlatx2
  # ycpts is a matrix with year of changepoints detected nlonxnlatx2
  # plottype is 1,2 or 3:
  # to show (1) all changes, (2) acceleleration vs deceleration or (3) decade changes
  
  nlon = dim(ncpts)[1]
  
  #prepare the data frames
  
  #all changes confounded
  pcpts_lat = (colSums(ncpts,na.rm=T)/nlon)*100
  dfl = as.data.frame(cbind(lats,pcpts_lat))
  names(dfl) = c("lat","pc")
  
  #with acceleration vs deceleration per latitude
  acc_lat = (colSums(dift[,,1] > 0 & ncpts > 0,na.rm=T)/nlon)*100 + (colSums(dift[,,2] > 0 & ncpts > 0,na.rm=T)/nlon)*100
  dec_lat = (colSums(dift[,,1] < 0 & ncpts > 0,na.rm=T)/nlon)*100 + (colSums(dift[,,2] < 0 & ncpts > 0,na.rm=T)/nlon)*100
  dfs = as.data.frame(cbind(lats,acc_lat,dec_lat))
  names(dfs) = c("lat","acc","dec")
  #df_sign = melt( data = dfs, id.vars = 'lat')   # melt data into long format
  
  #with acceleration per latitude per decade
  acp2010s = (colSums(ycpts[,,1] >= 2010 & dift[,,1] > 0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] >= 2010 & dift[,,2] > 0,na.rm=T)/nlon)*100
  acp2000s = (colSums(ycpts[,,1] >= 2000 & ycpts[,,1] < 2010 & dift[,,1] > 0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] >= 2000 & ycpts[,,2] < 2010 & dift[,,2] > 0,na.rm=T)/nlon)*100
  acp1990s = (colSums(ycpts[,,1] >= 1990 & ycpts[,,1] < 2000 & dift[,,1] > 0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] >= 1990 & ycpts[,,2] < 2000 & dift[,,2] > 0,na.rm=T)/nlon)*100
  acp1980s = (colSums(ycpts[,,1] < 1990 & dift[,,1] >0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] < 1990 & dift[,,2] >0,na.rm=T)/nlon)*100
  dfa = as.data.frame(cbind(lats,acp2010s,acp2000s,acp1990s,acp1980s))
  names(dfa) = c("lat","2010s","2000s","1990s","1980s")
  dfa_long = melt(data = dfa, id.vars = 'lat') 
  
  #with deceleration per latitude per decade
  dcp2010s = (colSums(ycpts[,,1] >= 2010 & dift[,,1] < 0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] >= 2010 & dift[,,2] < 0,na.rm=T)/nlon)*100
  dcp2000s = (colSums(ycpts[,,1] >= 2000 & ycpts[,,1] < 2010 & dift[,,1] < 0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] >= 2000 & ycpts[,,2] < 2010 & dift[,,2] < 0,na.rm=T)/nlon)*100
  dcp1990s = (colSums(ycpts[,,1] >= 1990 & ycpts[,,1] < 2000 & dift[,,1] < 0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] >= 1990 & ycpts[,,2] < 2000 & dift[,,2] < 0,na.rm=T)/nlon)*100
  dcp1980s = (colSums(ycpts[,,1] < 1990 & dift[,,1] < 0,na.rm=T)/nlon)*100 + (colSums(ycpts[,,2] < 1990 & dift[,,2] < 0,na.rm=T)/nlon)*100
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
  
  if(plottype == 3){#we plot accelerations/decelations and color per decade
    
    plt = ggplot() +
      geom_col(data = dfa_long,aes(x=lat,y=value,fill=variable),width=1) +
      geom_col(data = dfd_long,aes(x=lat,y=-value,fill=variable),width=1) +
      scale_y_continuous(expand = c(0,0),breaks=seq(-10,20,10),labels=c("10","0","10","20"),limits = c(-10,20)) +
      scale_x_continuous(expand = c(0,0),limits=c(-89,89)) +
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



