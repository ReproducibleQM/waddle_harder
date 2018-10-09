# # Map of Archipelego with inset using ggplot
# 
# #install.packages(c("ggplot2", "maps", "grid", "gridExtra"))
# library("ggplot2")
# library("maps")
# library("grid")
# library("gridExtra")
# library("rgdal")
# library("RColorBrewer")
# library("plotly")
# 
# shapefile <- readOGR(dsn = ".", "Tristan") 
# shapefile_df <- fortify(shapefile)
# 
# #define coords for the sst points - these are the points used in the study
# sst_points = data.frame("long" = c(-12,-10), "lat" = c(-38,-40))
# 
# 
# worldmap <- data.frame(map("world", plot = FALSE)[c("x", "y")])
# 
# smallerworld <- worldmap[worldmap$x < 30 & worldmap$x > -80 & worldmap$y < 30 & worldmap$y > -60,]
# TristanGoughSmall <- worldmap[worldmap$x < -8 & worldmap$x > -14 & worldmap$y < -36 & worldmap$y > -42, ]
# 
# #make little box in the inset map
# insetrect <- data.frame(xmin = -13, xmax = -9, ymin = -41, ymax = -37)
# 
# #Read in sst csv so that we can find the domain average
# sstdat <- read.csv(file="data/SST_Data_All.csv", header=F, sep=",")
# sstlatlong <- read.csv(file="data/SST_Lat_Longs.csv", header=F, sep=",")
# 
# #Calculate the gridpoint averages, then reshape the data so that the contours can be plotted easily
# avgs = colMeans(sstdat, na.rm = T, dims = 1)
# meansst_grid = matrix(avgs,nrow=8,byrow = T)
# meansst_grid_smaller = meansst_grid[3:5,3:6]
# newlat = as.data.frame(seq(from = -40, to = -36, by = 2))
# newlong = as.data.frame(seq(from = -14, to = -8, by = 2))
# dat = as.data.frame(meansst_grid_smaller)
# 
# #Make data frame for sst because apparently contours can only be made from dataframes
# 
# sst_df = as.data.frame(avgs)
# names(sst_df)=c('sst')
# sst_df$lat = t(sstlatlong[1,])
# sst_df$long = t(sstlatlong[2,]-360)
# 
# sst_df2 = sst_df[which(sst_df$lat >= -42 & sst_df$lat <= -36 & sst_df$long >= -14 & sst_df$long <= -8),]
# ptheme <- theme(panel.border = element_rect(colour = 'black', size = 2, linetype = 2),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'white'),legend.key = element_blank())
# 
# 
# #Inset
# a <- ggplot(smallerworld) +
#   coord_fixed(1)+
#   theme_bw(base_size = -1) +
#   geom_path(data = smallerworld, aes(x, y), colour = 'black',fill = 'grey80') +
#   geom_rect(data = insetrect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "red", size = 1, linetype = 1) +
#   theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) +
#   labs(x = '', y = '')
# 
# #Islands
# b <- ggplot(TristanGoughSmall) +
#   theme_bw() +
#   #theme(panel.background = element_blank())+
#   geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),colour = 'black', fill = 'black', size = 1)+
#   annotate("text", x = -11.8, y = -37.1, label = "Tristan")+
#   annotate("text", x = -9.6, y = -40.1, label = "Gough")+
#   annotate("text", x = -13.1, y = -37, label = "Inaccessible")+
#   annotate("text", x = -12.1, y = -37.6, label = "Nightingale")+
#   geom_point(data = sst_points, aes(x = long, y = lat))+
#   labs(x = 'lon', y = 'lat')+
#   #coord_fixed(xlim = c(-14, -8),  ylim = c(-42, -36), ratio = 1)
# 
# 
# #sst
# c <- ggplot(data = sst_df2, aes(x = long, y = lat, fill = sst))+ 
#   geom_raster(interpolate = T)+
#   geom_polygon(data = shapefile_df, aes(x=long, y = lat, group = group), color = 'black', fill = 'grey80')+
#   scale_fill_gradientn(colours = (topo.colors(20)),na.value = NA) +
#   theme_bw()+
#   coord_fixed(xlim = c(-13, -9),  ylim = c(-41, -37), ratio = 1)+
#   annotate("text", x = -11.9, y = -37.1, label = "Tristan", size = 3.6)+
#   annotate("text", x = -9.6, y = -40.3, label = "Gough",size = 3.6)+
#   annotate("text", x = -12.5, y = -38.1, label = "Inaccessible",size = 3.6)+
#   annotate("segment", x = -12.68, xend = -12.68, y = -38, yend = -37.4, colour = "black",size = .5)+
#   annotate("text", x = -12, y = -37.4,label = "Nightingale", size = 3.6)+
#   
#   annotate("point", x = -12, y = -38,colour = "red", size = 2)+
#   annotate("point", x = -10, y = -40,colour = "red", size = 2)+
#   annotate("text", x = -11.35, y = -38,label = "north grid point", colour = "red")+
#   annotate("text", x = -9.35, y = -40,label = "south grid point", colour = "red")+
#   
# 
#   #labs(x = 'lon', y = 'lat')
#   
#   #scale_color_brewer(palette = "Purples")
# 
# #c1 = direct.label(c, list("bottom.pieces", colour='black'))
#              
# #grid.newpage()
# 
# #vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
# vpc_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
# vpa_ <- viewport(width = 0.3, height = 0.3, x = 0.60, y = 0.80)  # the inset in upper right
# 
# 
# print(c, vp = vpc_)
# #print(b, vp = vpb_)
# print(a, vp = vpa_)

library(sf)
library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles

# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
shapefile <- readOGR(dsn = './shapefile', layer = "Tristan")

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <-ggplot(data = shapefile_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'black', fill = 'gray', size = .2)+
  stat_contour(aes(long,lat,z = sst),data = sst_df)+
  coord_fixed(xlim = c(-13, -9),  ylim = c(-41, -37), ratio = 1)+
  annotate("text", x = -11.9, y = -37.1, label = "Tristan", size = 3.6)+
  annotate("text", x = -9.6, y = -40.3, label = "Gough",size = 3.6)+
  annotate("text", x = -12.5, y = -38.1, label = "Inaccessible",size = 3.6)+
  annotate("segment", x = -12.68, xend = -12.68, y = -38, yend = -37.4, colour = "black",size = .5)+
  annotate("text", x = -12, y = -37.4,label = "Nightingale", size = 3.6)+
  #
  annotate("point", x = -12, y = -38,colour = "red", size = 2)+
  annotate("point", x = -10, y = -40,colour = "red", size = 2)+
  annotate("text", x = -11.35, y = -38,label = "north grid point", colour = "red")+
  annotate("text", x = -9.35, y = -40,label = "south grid point", colour = "red")


map + theme_nothing()

print(map) 

# Using the ggplot2 function coord_map will make things look better and it will also let you change
# the projection. But sometimes with large shapefiles it makes everything blow up.
map_projected <- map 
  coord_map()

print(map_projected)



