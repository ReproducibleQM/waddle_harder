# Map with inset

library(maps)
library(mapdata)

#map("world", fill=TRUE, col="lightgreen", bg="white", ylim=c(-60, 60),xlim=c(-100,30), mar=c(0,0,0,0))

map('state', fill = FALSE, xlim = c(-125, -114), ylim = c(32.2, 42.5), xlab = "lon", ylab = "lat")
map.axes(cex.axis=1)

#points(-121.6945, 39.36708, bg = "black", pch = 21)

#maps::map.scale(x=-124, y=34, ratio=FALSE, relwidth=0.3)
north.arrow(xb=-116, yb=41, len=0.22, lab="N") 

# Inmap
par(usr=c(-216, -63, 22, 144))
rect(xleft =-126.2,ybottom = 23.8,xright = -65.5,ytop = 50.6,col = "white")
map("world", xlim=c(-100,30), ylim=c(-60,60),fill = T,col = "lightgreen",add=T)
#map("world", fill=TRUE, col="lightgreen", bg="white", ylim=c(-60, 60),xlim=c(-100,30), mar=c(0,0,0,0)
#points(-121.6945, 39.36708, bg = "white", pch = 21)