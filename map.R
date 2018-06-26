# Map of Archipelego with inset using ggplot

#install.packages(c("ggplot2", "maps", "grid", "gridExtra"))
library("ggplot2")
library("maps")
library("grid")
library("gridExtra")
library("rgdal")

shapefile <- readOGR(dsn = ".", "Tristan") 
shapefile_df <- fortify(shapefile)

worldmap <- data.frame(map("world", plot = FALSE)[c("x", "y")])

smallerworld <- worldmap[worldmap$x < 30 & worldmap$x > -80 & worldmap$y < 30 & worldmap$y > -60,]
TristanGoughSmall <- worldmap[worldmap$x < -8 & worldmap$x > -14 & worldmap$y < -36 & worldmap$y > -42, ]

#make little box in the inset map
insetrect <- data.frame(xmin = -14, xmax = -8, ymin = -42, ymax = -36)

#Read in sst csv so that we can find the domain average
sstdat <- read.csv(file="SST_Data_All.csv", header=F, sep=",")
sstlatlong <- read.csv(file="SST_Lat_Longs.csv", header=F, sep=",")

#Calculate the gridpoint averages, then reshape the data so that the contours can be plotted easily
avgs = colMeans(sstdat, na.rm = T, dims = 1)
meansst_grid = matrix(avgs,nrow=8,byrow = T)
meansst_grid_smaller = meansst_grid[3:5,3:6]
newlat = as.data.frame(seq(from = -40, to = -36, by = 2))
newlong = as.data.frame(seq(from = -14, to = -8, by = 2))
dat = as.data.frame(meansst_grid_smaller)

#Make data frame for sst because apparently contours can only be made from dataframes

sst_df = as.data.frame(avgs)
names(sst_df)=c('sst')
sst_df$lat = t(sstlatlong[1,])
sst_df$long = t(sstlatlong[2,]-360)

ptheme <- theme(panel.border = element_rect(colour = 'black', size = 2, linetype = 2),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'white'),legend.key = element_blank())

a <- ggplot(smallerworld) +
  theme_bw(base_size = 22) +
  geom_path(data = smallerworld, aes(x, y), colour = "black") +
  geom_rect(data = insetrect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "blue", size = 1, linetype = 1) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) +
  labs(x = '', y = '')



#mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")


b <- ggplot(TristanGoughSmall) +
  theme_bw() +
  #geom_polygon(data = TristanGoughSmall, aes(x, y), colour = "green", fill = "white") +
  #geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),colour = 'black', fill = 'black', size = 1)+
  annotate("text", x = -11.8, y = -37.1, label = "Tristan")+
  annotate("text", x = -9.6, y = -40.1, label = "Gough")+
  annotate("text", x = -13.1, y = -37, label = "Inaccessible")+
  annotate("text", x = -12.1, y = -37.6, label = "Nightingale")+
  labs(x = 'lon', y = 'lat')+
  xlim(-14,-8)+
  ylim(-42,-36)+
  

# c <- ggplot(sst_df)+
#   #aes(x = long, y = lat, z = sst)+
#   geom_tile(aes(fill=z))+
#   coord_equal()+
#   stat_contour(aes(fill=..level..), geom='polygon', binwidth=0.005)+
#   geom_contour(color='white',alpha=0.5)+
#   scale_fill_distiller(palette='Spectral', na.value='white')+
#   theme_bw()

  stat_contour(aes(x = long, y = lat, z = sst, colour=..level..), data = sst_df, breaks = round(quantile(sst_df$sst,seq(10,20,1)),0), size=1)+

  #color = 'black', bins=5)+


grid.newpage()

vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
#vpc_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.8)  # the inset in upper right

print(b, vp = vpb_)
#print(c, vp = vpc_)
print(a, vp = vpa_)

####troubloushoot
# 
# ggplot(data = sst_df, aes(x = long, y = lat, z = sst))+
#   stat_contour(geom = 'polygon',aes(fill=..level..))+
#   geom_tile(aes(fill=sst))+
#   geom_raster(interpolate = TRUE)+
#   scale_fill_gradientn(colours=rev(rainbow(7)))+
#   stat_contour()
  

