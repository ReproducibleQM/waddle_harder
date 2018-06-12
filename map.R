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
TristanGoughSmall <- worldmap[worldmap$x < -8.5 & worldmap$x > -14.5 & worldmap$y < -35.5 & worldmap$y > -38, ]

#smallerworld <- na.omit(smallerworld)
#TristanGoughSmall <- na.omit(TristanGoughSmall)

insetrect <- data.frame(xmin = -14.5, xmax = -8.5, ymin = -41.5, ymax = -35.5)

ptheme <- theme(panel.border = element_rect(colour = 'black', size = 1, linetype = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'white'),legend.key = element_blank())

a <- ggplot(smallerworld) +
  theme_bw(base_size = 22) +
  geom_path(data = smallerworld, aes(x, y), colour = "black", fill = "white") +
  geom_rect(data = insetrect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "blue", size = 1, linetype = 1) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) +
  labs(x = '', y = '')

b <- ggplot(TristanGoughSmall) +
  theme_bw(base_size = 22) +
  geom_path(data = TristanGoughSmall, aes(x, y), colour = "black", fill = "white") +
  geom_path(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'white', size = 1)+
  annotate("text", x = -12, y = -37.1, label = "Tristan")+
  annotate("text", x = -10, y = -40, label = "Gough")+
  annotate("text", x = -12.7, y = -37.1, label = "Inaccessbile")+
  annotate("text", x = -12.5, y = -37.6, label = "Nightengale")
  labs(x = 'lon', y = 'lat')

  
grid.newpage()

vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.8)  # the inset in upper right
print(b, vp = vpb_)
print(a, vp = vpa_)
