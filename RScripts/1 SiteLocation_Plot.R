
#*****************************************************************************************************************
# Step 1.1: Prepare data
#*****************************************************************************************************************

library(ggplot2)
# install.packages("ggmap")
library(ggmap)
# install.packages("maps")
library(maps)
# install.packages("mapdata")
library(mapdata)
# library("gridExtra")


data_loc <- paste(here::here(), "Data", sep = "/")
outputs_loc <- paste(here::here(), "outputs", sep = "/")

source(paste(here::here(),"RScripts/functions.R", sep = "/"))

soilhealthData <- read.csv(paste(data_loc, "SoilHealthDB_V1.csv", sep="/"), header = T)


# Subset

unique(soilhealthData$Conservation_Type)

sub_AFS <- soilhealthData[soilhealthData$Conservation_Type == "AFS",]
sub_OGF <- soilhealthData[soilhealthData$Conservation_Type == "OGF"|soilhealthData$Conservation_Type == "OGF-CC"
                          |soilhealthData$Conservation_Type == "OGF-NT",]
sub_NT <- soilhealthData[soilhealthData$Conservation_Type == "CC-NT",]
sub_CC <- soilhealthData[soilhealthData$Conservation_Type != "OGF"&soilhealthData$Conservation_Type != "OGF-CC"
                          &soilhealthData$Conservation_Type != "OGF-NT"&soilhealthData$Conservation_Type != "AFS"
                         &soilhealthData$Conservation_Type != "CC-NT",]

# aggregate for sub_CC
siteInfor_CC <- summarySE (data=sub_CC, measurevar='YearPublication', groupvars=c("Latitude","Longitude"))

siteInfor_CC <- siteInfor_CC[, c(1:3)]
siteInfor_CC <- siteInfor_CC[which(!is.na(siteInfor_CC$Latitude)),]

sort(siteInfor_CC$N)
siteInfor_CC[siteInfor_CC$N >= 75,3] <- 75

# plot(siteInfor$Latitude~siteInfor$Longitude)

siteInfor_CC$var_size <- mean(siteInfor_CC$N)*0.15 + (siteInfor_CC$N)*0.05

sort(unique(siteInfor_CC$var_size))

length(unique(soilhealthData$StudyID) )
length(unique(soilhealthData$ExperimentID) )


# aggregate for NT
siteInfor_NT <- summarySE (data=sub_NT, measurevar='YearPublication', groupvars=c("Latitude","Longitude"))
siteInfor_NT <- siteInfor_NT[, c(1:3)]
siteInfor_NT <- siteInfor_NT[which(!is.na(siteInfor_NT$Latitude)),]
siteInfor_NT$var_size <- mean(siteInfor_NT$N)*0.15 + (siteInfor_NT$N)*0.05

# aggregate for OGF
siteInfor_OGF <- summarySE (data=sub_OGF, measurevar='YearPublication', groupvars=c("Latitude","Longitude"))
siteInfor_OGF <- siteInfor_OGF[, c(1:3)]
siteInfor_OGF <- siteInfor_OGF[which(!is.na(siteInfor_OGF$Latitude)),]
siteInfor_OGF$var_size <- mean(siteInfor_OGF$N)*0.15 + (siteInfor_OGF$N)*0.05

# aggregate for AFS
siteInfor_AFS <- summarySE (data=sub_AFS, measurevar='YearPublication', groupvars=c("Latitude","Longitude"))
siteInfor_AFS <- siteInfor_AFS[, c(1:3)]
siteInfor_AFS[siteInfor_AFS$N >= 75,3] <- 75
siteInfor_AFS <- siteInfor_AFS[which(!is.na(siteInfor_AFS$Latitude)),]
siteInfor_AFS$var_size <- mean(siteInfor_AFS$N)*0.15 + (siteInfor_AFS$N)*0.05


#*****************************************************************************************************************
# Step 2: Plot
#*****************************************************************************************************************
# global map
counties <- map_data("world", region = ".", exact = FALSE)
sort(unique(counties$region))

globMap <- ggplot(data = counties) + 
  geom_polygon(aes(x = long, y = lat , group = group), color = "white", fill = 'gray') + 
  guides(fill=FALSE)+
  theme(legend.position="none")
  # ylim(-57, 90)
globMap


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
  annotate("text", x = -150, y = -5, label = "CC (177)", size = 4, adj = 0)+
  annotate("text", x = -150, y = -20, label = "NT (58)", size = 4, adj = 0)+
  annotate("text", x = -150, y = -35, label = "OF (87)", size = 4, adj = 0)+
  annotate("text", x = -150, y = -50, label = "AS (35)", size = 4, adj = 0)+
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


# ?scale_y_continuous()

sitemap1 


#*****************************************************************************************************************
# output figure

tiff(paste(outputs_loc, "Figure 1. site_plot.tiff", sep = "/"), width = 8, height = 4, pointsize = 1/300, units = 'in', res = 300)

sitemap1

dev.off()


# grid.arrange(sitemap1, meta_plot,ncol=1, nrow = 2)




