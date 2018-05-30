# SST plots

library(zoo)
library(ggplot2)

#12 month rolling mean
cma_tristan = rollmean(SST$Tristan,12,na.pad=TRUE)
cma_gough = rollmean(SST$Gough,12,na.pad=TRUE)


#plot away!
plot(SST$index,cma_tristan,type = 'l',col='red',lwd='2',ylim = c(11,17),width=5,height=2)
lines(SST$index,cma_gough,col = 'blue',lwd='2')


#compute simple linear trends
tristanlm = lm(cma_tristan~SST$index)
goughlm = lm(cma_gough~SST$index)



#add trendlines for both
abline(tristanlm,col = 'black',lwd = '3')
abline(goughlm,col='black',lwd='3')


# For loop for center moving average for sst
# for (loop in 1:nrow(SST)-12){
#   new_dat[,1]= mean(SST$Tristan[loop:(loop+11)]
# }
