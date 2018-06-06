# Map with inset

# library(maps)
# library(mapdata)
# 
# map("world", fill=TRUE, col="lightgreen", bg="white", ylim=c(-60, 60),xlim=c(-100,30), mar=c(0,0,0,0))
# 
# #map('state', fill = FALSE, xlim = c(-125, -114), ylim = c(32.2, 42.5), xlab = "lon", ylab = "lat")
# map.axes(cex.axis=1)
# 
# #points(-121.6945, 39.36708, bg = "black", pch = 21)
# 
# #maps::map.scale(x=-124, y=34, ratio=FALSE, relwidth=0.3)
# #north.arrow(xb=-122, yb=34, len=0.22, lab="N") 
# 
# # Inmap
# par(usr=c(-216, -63, 20, 144))
# #rect(xleft =-100,ybottom = 0,xright = 30,ytop = 100,col = "white")
# map("world", xlim=c(-100,30), ylim=c(-60,60),fill = T,col = "green",add=T)
# #map("world", fill=TRUE, col="lightgreen", bg="white", ylim=c(-60, 60),xlim=c(-100,30), mar=c(0,0,0,0)
# points(-121.6945, 39.36708, bg = "white", pch = 21)

#install.packages(c("ggplot2", "maps", "grid", "gridExtra"))
library("ggplot2")
library("maps")
library("grid")
library("gridExtra")

dat <- data.frame(ecosystem = rep(c("oak", "steppe", "prairie"), each = 8),lat = rnorm(24, mean = 51, sd = 1), lon = rnorm(24, mean = -113, sd = 5))
#head(dat)

worldmap <- data.frame(map("world", plot = FALSE)[c("x", "y")])

smallerworld <- worldmap[worldmap$x < 30 & worldmap$x > -80 & worldmap$y < 30 & worldmap$y > -60,]
TristanGoughSmall <- worldmap[worldmap$x < -9 & worldmap$x > -14 & worldmap$y < -36 & worldmap$y > -41, ]

#smallerworld <- na.omit(smallerworld)
#TristanGoughSmall <- na.omit(TristanGoughSmall)

insetrect <- data.frame(xmin = -14, xmax = -9, ymin = -41, ymax = -36)

ptheme <- theme(
  panel.border = element_rect(colour = 'black', size = 1, linetype = 1),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = 'white'),
  legend.key = element_blank()
)

a <- ggplot(smallerworld) +
  theme_bw(base_size = 22) +
  geom_path(data = smallerworld, aes(x, y), colour = "black", fill = "white") +
  geom_rect(data = insetrect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "blue", size = 1, linetype = 1) +
  ptheme %+% theme(
    legend.position = c(0.15, 0.80),
    axis.ticks = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = '', y = '')

b <- ggplot(TristanGoughSmall, aes(lon, lat)) +
  theme_bw(base_size = 22) +
  #geom_jitter(size = 4, alpha = 0.6) +
  geom_path(data = TristanGoughSmall, aes(x, y), colour = "black", fill = "white") +
  scale_size(guide = "none") +
  theme(
    legend.position = c(0.1, 0.20),
    legend.text = element_text(size = 12, face = 'bold'),
    legend.title = element_text(size = 12, face = 'bold'),
    axis.ticks = element_line(size = 1)
  ) +
  
  grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.8)  # the inset in upper right
print(b, vp = vpb_)
print(a, vp = vpa_)
  labs(x = '', y = '')