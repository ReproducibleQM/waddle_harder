# SST plots

library(zoo)
library(ggplot2)

cma_tristan = rollmean(SST$Tristan,12,na.pad=TRUE)
cma_gough = rollmean(SST$Gough,12,na.pad=TRUE)
#tris_filt = filter(SST$Tristan,f,sides=1)
#tris_filt2 = filter(SST$Tristan,f,sides=2)

# gou_filt = filter(SST$Gough,f,sides=1)
# gou_filt2 = filter(SST$Gough,f,sides = 2)

plot(SST$index,cma_tristan,type = 'l',col='red',lwd='2'ylim = c(0,20))
lines(SST$index,cma_gough,col = 'blue',lwd='2')



#plot(SST$index,SST$Tristan,type = 'l',col = 'red')
#lines(SST$index,SST$Gough, col = 'blue')

#compute simple linear trends
# tristanlm = lm(cma_tristan~SST$year)
# goughlm = lm(cma_gough~SST$year)



#add trendlines for both
# abline(tristanlm,col = 'black',lwd = '3')
# abline(goughlm,col='black',lwd='3')
#lines(SST$index,m_sim,col='red',lwd='2')


# For loop for center moving average for sst
# for (loop in 1:nrow(SST)-12){
#   new_dat[,1]= mean(SST$Tristan[loop:(loop+11)]
# }
