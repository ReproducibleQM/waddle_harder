# SST plots

plot(SST$index,SST$Tristan,type = 'l',col = 'red')
lines(SST$index,SST$Gough, col = 'blue')
tristanlm = lm(SST$Tristan~SST$index)
goughlm = lm(SST$Gough~SST$index)

#add trendlines for both
abline(tristanlm,col = 'black',lwd = '3')
abline(goughlm,col='black',lwd='3')
