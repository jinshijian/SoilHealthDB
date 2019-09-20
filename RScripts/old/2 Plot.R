
#*****************************************************************************************************************
# Prepare data
#*****************************************************************************************************************
# library("gridExtra")
# install.packages("here")
library(here)
library(agricolae)
# install.packages('multcompView')
library(multcompView)
# install.packages("ncdf4")
library(ncdf4)
# install.packages("bootstrap")
# install.packages("metafor")
library("metafor")
library("bootstrap")
library(cowplot)
library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(kableExtra)
library(knitr)
library("ggpubr")
library(reshape)
# install.packages("ggmap")
library(ggmap)
# install.packages("maps")
library(maps)
# install.packages("mapdata")
library(mapdata)
# library(tidyr)
# install.packages('car')
library(car)
library(dplyr)
library(stringr)

data_loc <- paste(here::here(), "Data", sep = "/")
outputs_loc <- paste(here::here(), "outputs", sep = "/")

source(paste(here::here(),"RScripts/functions.R", sep = "/"))

soilhealthData <- read.csv(paste(data_loc, "SoilHealthDB_V1.csv", sep="/"), header = T)

# Subset

unique(soilhealthData$Conservation_Type)

sub_AFS <- soilhealthData[soilhealthData$Conservation_Type == "AF",]
sub_OGF <- soilhealthData[soilhealthData$Conservation_Type == "OF"|soilhealthData$Conservation_Type == "OF-CC"
                          |soilhealthData$Conservation_Type == "OF-NT",]
sub_NT <- soilhealthData[soilhealthData$Conservation_Type == "CC-NT",]
sub_CC <- soilhealthData[soilhealthData$Conservation_Type != "OF"&soilhealthData$Conservation_Type != "OF-CC"
                          &soilhealthData$Conservation_Type != "OF-NT"&soilhealthData$Conservation_Type != "AF"
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

lat <- read.csv("data/lat.csv")
lon <- read.csv("data/lon.csv")
cec <- read.csv("data/T_Clay_CEC.csv")

#*****************************************************************************************************************
# Plot Figure 1
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

# output figure

tiff(paste(outputs_loc, "Figure 1.tiff", sep = "/"), width = 8, height = 4, pointsize = 1/300, units = 'in', res = 300)

sitemap1

dev.off()

# grid.arrange(sitemap1, meta_plot,ncol=1, nrow = 2)

#*****************************************************************************************************************
# Plot Figure 4
#*****************************************************************************************************************
# load climate koeppon data
koeppen <- read.table("../data/Koeppen-Geiger-ASCII.txt", header = TRUE, sep = "")
IGBP <- read.table("../data/IGBP2010Point.txt", header = TRUE, sep = ",")
ClimateDel <- read.csv("../data/summarized_climate.csv")
# f <- "../data/ISSOIL.nc4"
# file_cec <- "G:/My Drive/MyResearch/12.SoilHealthDB/SoilHealthDB/data/T_CEC_CLAY.nc4"
# f2 <- "../data/MU_GLOBAL.nc4"
# HWSD_soil <- nc_open(f)
# HWSD_cec <- nc_open(file_cec)
# HWSD_unit <- nc_open(f2)
DSMW_point <- read.csv("../data/DSMW_Point.csv")
DSMW_soiltype <- read.csv("../data/DSMW_SoilType.csv")

lat <- read.csv("../data/lat.csv")
lon <- read.csv("../data/lon.csv")
cec <- read.csv("../data/T_Clay_CEC.csv")

# print(HWSD_cec)
koeppen %>% count(Cls)

# SoilHealthCC <- get_koeppon(SoilHealthCC, koeppen)
SoilHealthCC %>% count(Koeppen)

SoilHealthCC %>% mutate(KoeppenGroup = case_when(Koeppen %in% c("Af", "Am", "As", "Aw") ~ "Equat"
                                                 , Koeppen %in% c("BSh", "BSk", "BWh") ~ "Arid"
                                                 , Koeppen %in% c("Cfa", "Cfb", "Csa", "Csb", "Cwa", "Cwb") ~ "Temp"
                                                 , Koeppen %in% c("Dfa", "Dfb", "Dfc", "Dwa", "Dwb") ~ "Snow"
)) -> SoilHealthCC

SoilHealthCC %>% filter(!is.na(KoeppenGroup)) %>% 
  ggplot(aes(KoeppenGroup)) + geom_bar() +
  labs(x=expression(Koeppen~climate), y="Count") -> plot_climate

# SoilHealthCC <- get_Del(SoilHealthCC, ClimateDel)
ClimateDel %>% filter(MAT > -5) %>% ggplot(aes(MAT)) +
  geom_density(stat = 'density', col = "black", fill = "gray") +
  geom_density(data = SoilHealthCC, aes(MAT_del), stat = 'density', col = "lightblue", fill = rgb(0,1,1, alpha = 0.5)) +
  labs(x=expression(MAT~"("~degree~C~")"), y="Density") -> plot_MAT

sub_SlilHealthDB <- SoilHealthCC %>% filter(MAP_del > 300 & MAP_del<3000)

ClimateDel %>% filter(MAP > 300 & MAP < 3000) %>% ggplot(aes(MAP)) +
  geom_density(stat = 'density', col = "black", fill = "gray") +
  geom_density(data = sub_SlilHealthDB
               , aes(MAP_del), stat = 'density', col = "lightblue", fill = rgb(0,1,1, alpha = 0.5)) +
  labs(x=expression(MAP~"(mm)"), y="Density") -> plot_MAP

# SoilHealthCC <- get_soil(SoilHealthCC, DSMW_point)
SoilHealthCC %>% count(FAOSOIL)
# left_join(SoilHealthCC, DSMW_soiltype %>% select(VALUE_, FAOSOIL), by = c("nDSMW" = "VALUE_")) -> SoilHealthCC


SoilHealthCC %>% filter(str_detect(FAOSOIL, "^A")) %>% count() #867
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^B")) %>% count() #764
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^C")) %>% count() #40
# SoilHealthCC %>% filter(str_detect(FAOSOIL, "^D")) %>% count(FAOSOIL)
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^E")) %>% count() #16
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^F")) %>% count() #705
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^G")) %>% count() #175
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^H")) %>% count() #534
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^I")) %>% count() #480
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^J")) %>% count() #230
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^K")) %>% count() #297
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^L")) %>% count() #935
# SoilHealthCC %>% filter(str_detect(FAOSOIL, "^M")) %>% count(FAOSOIL)
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^N")) %>% count() #155
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^O")) %>% count() #2
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^P")) %>% count() #116
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^Q")) %>% count() #28
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^R")) %>% count() #72
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^S")) %>% count() #13
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^T")) %>% count() #35
# SoilHealthCC %>% filter(str_detect(FAOSOIL, "^U")) %>% count(FAOSOIL)
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^V")) %>% count() #7
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^W")) %>% count() #235
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^X")) %>% count() #86
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^Y")) %>% count() #8
SoilHealthCC %>% filter(str_detect(FAOSOIL, "^Z")) %>% count() #18
SoilHealthCC %>% filter(str_detect(FAOSOIL, "WAT")) %>% count() #183

tibble( SoilClass = c("Acrisols(867)", "Cambisols(764)", "Chernozems(40)" ,"Rendzinas(16)","Ferralsols(705)"
                      ,"Gleysols(175)","Phaeozems(534)","Lithosols(438)","Fluvisols(230)"
                      ,"Kastanozems(297)","Luvisols(935)","Nitosols(155)","Histosols(2)","Podzols(116)","Arenosols(28)","Regosols(72)"
                      ,"Solonetz(13)","Andosols(35)","Vertisols(7)","Planosols(235)","Xerosols(86)","Yermosols(8)","Solonchaks(18)","Wetland(183)"),
        obs = c(867,764,40,16,705,175,534,480,230,297,935,155,2,116,28,72,13,35,7,235,86,8,18,183) ) %>% 
  ggplot(aes(x=SoilClass, y=obs)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) +
  labs(x=expression(Soil~group), y="Samples (n)") -> plot_soilgroup

# Figure 4
plot_grid(plot_climate, plot_MAT, plot_MAP, nrow = 1, labels = c("a", "b", "c"), hjust = c(-7.5, -7, -8.5), vjust = 2) -> p1
plot_grid(p1, plot_soilgroup, ncol = 1, labels = c("", "d"), hjust = -7, vjust = 2) -> Fig4

Fig4
ggsave("../outputs/Figure 4.jpg", width = 8, height = 8, dpi = 300, units = "in")

#*****************************************************************************************************************
# Plot Figure 5
#*****************************************************************************************************************
tiff("G:/My Drive/MyResearch/12.SoilHealthDB/SoilHealthDB/outputs/Figure 5.tiff", width = 6, height = 8, pointsize = 1/300, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.3, 0.6, 0.1, 0.1)  # by inches, inner margin
     , omi = c(1.6, 0.3, 0.3, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     , cex.axis = 1
     , las = 1
     , mfrow=c(2,1))

# plot soilhealthDB
cec_db <- density(SoilHealthCC[SoilHealthCC$cec>0,]$cec, na.rm = TRUE)
plot(cec_db, main = ""
     , xaxt = "n", xlab = "" , col = "blue"
     , yaxt = "n", ylab = ""
     , xlim = c(0, 150), ylim = c(0, 0.030)
)
polygon(cec_db, col = "lightblue")

# mtext(1,text = expression(CEC), line = 1.75, cex=1)
mtext(2,text = expression("Density"), line = 2.75, cex=1, las = 0)
text(10, 0.028,"a", cex=1, lwd = 2)
axis(side=1,las=1,cex=1,tcl=0.4, at = seq(0,150,30))
axis(side=2,las=1,cex=1,tcl=0.4, at = seq(0,0.03,0.01))

# SoilHealthCC <- get_cec(SoilHealthCC)
matrix_cec <- as.matrix(cec)
cec_dens <- density(matrix_cec, na.rm = TRUE)
plot(cec_dens, main = ""
     , xaxt = "n", xlab = ""
     , yaxt = "n", ylab = ""
     , xlim = c(0, 150), ylim = c(0, 0.030)
)
polygon(cec_dens, col = "gray")

mtext(1,text = expression(CEC), line = 1.75, cex=1)
mtext(2,text = expression("Density"), line = 2.75, cex=1, las = 0)
text(10, 0.028,"b", cex=1, lwd = 2)
axis(side=1,las=1,cex=1,tcl=0.4, at = seq(0,150,30))
axis(side=2,las=1,cex=1,tcl=0.4, at = seq(0,0.03,0.01))

dev.off()




