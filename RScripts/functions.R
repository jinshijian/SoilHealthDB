
#*****************************************************************************************************************
# Basic functions for link external data and get attributes
#*****************************************************************************************************************
# get climate information
get_koeppon <- function (sdata1, sdata2) {
  
  # sdata1 = SoilHealthDB
  # sdata2 = koeppen
  # i = 1
  for (i in 1:nrow(sdata1) ) {
    
    # get the lat and lon from sdata1
    target_lat <- sdata1$Latitude[i]
    target_lon <- sdata1$Longitude[i]
    
    if (!is.na(target_lat) & !is.na(target_lon)) {
      # get the closest lat and lon from koeppen
      ilat <- sdata2$Lat[which.min(abs(sdata2$Lat - target_lat))]
      ilon <- sdata2$Lon[which.min(abs(sdata2$Lon - target_lon))]
      
      # get the koeppen information
      target_koeppon <- sdata2 %>% filter(Lat == ilat, Lon == ilon) %>% dplyr::select(Cls)
      
      sdata1[i, "Koeppen"] <- target_koeppon$Cls
    }
    
    else {next}
    
    print(paste0("====================", i))
  }
  
  return (sdata1)
}



# get MAT MAP
get_Del <- function (sdata1, sdata2) {
  
  # sdata1 = SoilHealthCC
  # sdata2 = DelClimate
  for (i in 1:nrow(sdata1) ) {
    
    # get the lat and lon from sdata1
    target_lat <- sdata1$Latitude[i]
    target_lon <- sdata1$Longitude[i]
    
    if (!is.na(target_lat) & !is.na(target_lon)) {
      # get the closest lat and lon from koeppen
      ilat <- sdata2$Latitude[which.min(abs(sdata2$Latitude - target_lat))]
      ilon <- sdata2$Longitude[which.min(abs(sdata2$Longitude - target_lon))]
      
      # get the koeppen information
      target_Del <- sdata2 %>% filter(Latitude == ilat, Longitude == ilon) %>% dplyr::select(MAT, MAP)
      
      if (nrow(target_Del) == 1) {
        sdata1[i, "MAT_del"] <- target_Del$MAT
        sdata1[i, "MAP_del"] <- target_Del$MAP
      }
      else {next}
    }
    
    else {next}
    
    print(paste0("====================", i))
  }
  return (sdata1)
}

# get soil classfication
get_soil <- function (sdata1, sdata2) {
  # sdata1 = SoilHealthCC
  # sdata2 = DelClimate
  for (i in 1:nrow(sdata1) ) {
    # get the lat and lon from sdata1
    target_lat <- sdata1$Latitude[i]
    target_lon <- sdata1$Longitude[i]
    
    if (!is.na(target_lat) & !is.na(target_lon)) {
      # get the closest lat and lon from koeppen
      ilat <- sdata2$Latitude[which.min(abs(sdata2$Latitude - target_lat))]
      ilon <- sdata2$Longitude[which.min(abs(sdata2$Longitude - target_lon))]
      
      # get the koeppen information
      target_soil <- sdata2 %>% filter(Latitude == ilat, Longitude == ilon) %>% dplyr::select(RASTERVALU)
      
      if (nrow(target_soil) == 1) {
        sdata1[i, "nDSMW"] <- target_soil$RASTERVALU
      }
      else {next}
    }
    else {next}
    print(paste0("====================", i))
  }
  return (sdata1)
}

# get soil CEC
get_cec <- function (sdata1) {
  # sdata1 = SoilHealthCC
  # sdata2 = DelClimate
  for (i in 1:nrow(sdata1) ) {
    
    # get the lat and lon from sdata1
    target_lat <- sdata1$Latitude[i]
    target_lon <- sdata1$Longitude[i]
    
    if (!is.na(target_lat) & !is.na(target_lon)) {
      # get the closest lat and lon from koeppen
      
      ilat <- which.min(abs(lat$X.89.975 - target_lat))
      ilon <- which.min(abs(lon$X.179.97 - target_lon))
      
      # get the koeppen information
      target_soil <- cec[ilon, ilat]
      sdata1[i, "cec"] <- target_soil
    }
    
    else {next}
    print(paste0("====================", i))
  }
  return (sdata1)
}


# get_wrb
get_wrb <- function (sdata) {
  for (i in 1:nrow(sdata) ) {
    # get the lat and lon from sdata
    # sdata = SoilHealthDB
    # i = 3300
    target_lat <- sdata$Latitude[i]
    target_lon <- sdata$Longitude[i]
    
    if (!is.na(target_lat) & !is.na(target_lon)) {
      # get the closest lat and lon from koeppen
      
      ilat <- wrb_point$y[which.min(abs(wrb_point$y - target_lat))]
      ilon <- wrb_point$x[which.min(abs(wrb_point$x - target_lon))]
      
      # get the wrb information
      target_wrb <- wrb_point %>% filter(y == ilat, x == ilon) 
      if (nrow(target_wrb) == 0) {
        sdata[i, "wrb"] <- NA
      } else {sdata[i, "wrb"] <- target_wrb$wsrc_}
    }
    else {next}
    print(paste0("====================", i))
  }
  return (sdata)
}



#*****************************************************************************************************************
# function for sites spatial distribution plot
plot_site <- function (sdata) {
  # sdata <- SoilHealthDB
  sub_AFS <- sdata[sdata$Conservation_Type == "AF",]
  sub_OGF <- sdata %>% filter(Conservation_Type %in% c("OF", "OF-CC", "OF-NT")) 
  sub_NT <- sdata[sdata$Conservation_Type == "CC-NT",]
  sub_CC <- sdata %>% filter(!Conservation_Type %in% c("OF", "OF-CC", "OF-NT", "AF", "CC-NT"))
   
  
  # aggregate for sub_CC
  siteInfor_CC <- sub_CC %>% dplyr::select(Latitude, Longitude) %>% group_by(Latitude, Longitude) %>% tally()
  siteInfor_CC <- siteInfor_CC %>% filter(!is.na(Latitude)) %>% mutate(N = ifelse(n >= 75, 75, n))
  siteInfor_CC$var_size <- mean(siteInfor_CC$N)*0.15 + (siteInfor_CC$N)*0.05
  obs_cc <- nrow(siteInfor_CC)
  
  # aggregate for NT
  siteInfor_NT <- sub_NT %>% dplyr::select(Latitude, Longitude) %>% group_by(Latitude, Longitude) %>% tally()
  siteInfor_NT <- siteInfor_NT %>% filter(!is.na(Latitude)) %>% mutate(N = ifelse(n >= 75, 75, n))
  siteInfor_NT$var_size <- mean(siteInfor_NT$N)*0.15 + (siteInfor_NT$N)*0.05
  obs_nt <- nrow(siteInfor_NT)
  
  # aggregate for OGF
  siteInfor_OGF <- sub_OGF %>% dplyr::select(Latitude, Longitude) %>% group_by(Latitude, Longitude) %>% tally()
  siteInfor_OGF <- siteInfor_OGF %>% filter(!is.na(Latitude)) %>% mutate(N = ifelse(n >= 75, 75, n))
  siteInfor_OGF$var_size <- mean(siteInfor_OGF$N)*0.15 + (siteInfor_OGF$N)*0.05
  obs_OF <- nrow(siteInfor_OGF)
  
  # aggregate for AFS
  siteInfor_AFS <- sub_AFS %>% dplyr::select(Latitude, Longitude) %>% group_by(Latitude, Longitude) %>% tally()
  siteInfor_AFS <- siteInfor_AFS %>% filter(!is.na(Latitude)) %>% mutate(N = ifelse(n >= 75, 75, n))
  siteInfor_AFS$var_size <- mean(siteInfor_AFS$N)*0.15 + (siteInfor_AFS$N)*0.05
  obs_AF <- nrow(siteInfor_AFS)
  
  counties <- map_data("world", region = ".", exact = FALSE)
  
  globMap <- ggplot(data = counties) + 
    geom_polygon(aes(x = long, y = lat , group = group), color = "white", fill = 'gray') + 
    guides(fill=FALSE)+
    theme(legend.position="none")
  
  sitemap1 <- globMap + 
    # CC
    geom_point(data = siteInfor_CC, aes(x=Longitude, y=Latitude)
               , shape=1, col="orange", size = siteInfor_CC$var_size
               , show.legend = TRUE) +
    # NT
    geom_point(data = siteInfor_NT, aes(x=Longitude, y=Latitude)
               , shape=2, col = "red" 
               , size = siteInfor_NT$var_size, alpha = 7/10
               , show.legend = TRUE)+
    # OGF
    geom_point(data = siteInfor_OGF, aes(x=Longitude, y=Latitude)
               , shape=3, col = "black" 
               , size = siteInfor_OGF$var_size, alpha = 7/10
               , show.legend = TRUE)+
    # AFS
    geom_point(data = siteInfor_AFS, aes(x=Longitude, y=Latitude)
               , shape=4, col = "cyan" 
               , size = siteInfor_AFS$var_size, alpha = 7/10
               , show.legend = TRUE)+
    
    theme(axis.text.y   = element_text(size=12),
          axis.text.x   = element_text(size=12),
          
          axis.title.y   = element_text(size=15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x   = element_text(size=15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          
          # axis.title.y  = element_blank(),
          # axis.title.x  = element_blank(),
          
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
    scale_x_continuous(name="Longitude", breaks=seq(-180,180, 30),labels = seq(-180,180, 30))+
    scale_y_continuous(limits = c(-60, 90),name="Latitude", breaks=seq(-60,90,20),labels = seq(-60,90,20))+
    annotate("text", x = -170, y = 10, label = "Legend", size = 4, adj = 0)+
    annotate("text", x = -150, y = -5, label = paste0("CC (", obs_cc, ")"), size = 4, adj = 0)+
    annotate("text", x = -150, y = -20, label = paste0("NT (", obs_nt, ")"), size = 4, adj = 0)+
    annotate("text", x = -150, y = -35, label = paste0("OF (", obs_OF, ")"), size = 4, adj = 0)+
    annotate("text", x = -150, y = -50, label = paste0("AF (", obs_AF,")"), size = 4, adj = 0)+
    # Add legend sign
    geom_point( aes(x=-170, y=-5)
                , shape=1, col="orange", size = 5
                , show.legend = TRUE) +
    geom_point( aes(x=-170, y=-20)
                , shape=2, col="red", size = 5
                , show.legend = TRUE) +
    geom_point( aes(x=-170, y=-35)
                , shape=3, col="black", size = 5
                , show.legend = TRUE) +
    geom_point( aes(x=-170, y=-50)
                , shape=4, col="cyan", size = 5
                , show.legend = TRUE) 
  # plot figure 1
  sitemap1
}


