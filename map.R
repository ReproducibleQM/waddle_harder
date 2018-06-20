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
TristanGoughSmall <- worldmap[worldmap$x < -4 & worldmap$x > -18 & worldmap$y < -32 & worldmap$y > -48, ]

#make little box in the inset map
insetrect <- data.frame(xmin = -14.5, xmax = -8.5, ymin = -41.5, ymax = -35.5)

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



ptheme <- theme(panel.border = element_rect(colour = 'black', size = 1, linetype = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'white'),legend.key = element_blank())

a <- ggplot(smallerworld) +
  theme_bw(base_size = 22) +
  geom_path(data = smallerworld, aes(x, y), colour = "black", fill = "white") +
  geom_rect(data = insetrect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "blue", size = 1, linetype = 1) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) +
  labs(x = '', y = '')

b <- ggplot(TristanGoughSmall) +
  theme_bw(base_size = 22) +
  #geom_polygon(data = TristanGoughSmall, aes(x, y), colour = "green", fill = "white") +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),colour = 'black', fill = 'black', size = 1)+
  annotate("text", x = -12, y = -37.1, label = "Tristan")+
  annotate("text", x = -9.8, y = -40.1, label = "Gough")+
  annotate("text", x = -13, y = -37.2, label = "Inaccessible")+
  annotate("text", x = -12.25, y = -37.5, label = "Nightingale")+
  labs(x = 'lon', y = 'lat')+
  xlim(-13.5,-9)
  geom_contour(mapping = aes(newlong,newlat), data = dat, stat = "contour",
             position = "identity", ..., lineend = "butt", linejoin = "round",
             linemitre = 10, na.rm = FALSE, show.legend = T, inherit.aes = TRUE)
  
grid.newpage()

vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.8)  # the inset in upper right
#vpc_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # adding the sst data
print(b, vp = vpb_)
print(a, vp = vpa_)
#print(c, vp = vpc_)
