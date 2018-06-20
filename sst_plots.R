# SST plots
# RUN penguinR.R FIRST!!

library(zoo)
library(ggplot2)

# loading the data
SST<-read.csv(file="SST_TG.csv", header=T)
View(SST)



#12 month rolling mean
cma_tristan = rollmean(SST$Tristan,12,na.pad=TRUE)
cma_gough = rollmean(SST$Gough,12,na.pad=TRUE)

#ggplot for timeseries


ggplot(data=SST, aes(x=dose, y=len)) +
  geom_line(linetype = "dashed")+
  geom_point()


# # Base plot with date axis
# p <- ggplot(data = cma_tristan) +
#   geom_line(color = "#00AFBB", size = 1)
# p
# # Set axis limits c(min, max)
# min <- as.Date("2002-1-1")
# max <- NA
# p + scale_x_date(limits = c(min, max))


# #plot away!
# xaxes = SST$Year
# plot(cma_tristan,type = 'l',col='red',lwd='2',ylim = c(11,17), width=5,height=2, xlab = 'Years', ylab = 'temperature (deg C)')
# lines(SST$index,cma_gough,col = 'blue',lwd='2')
# 
# 
# #compute simple linear trends
# tristanlm = lm(cma_tristan~SST$index)
# goughlm = lm(cma_gough~SST$index)


# 
# #add trendlines for both
# abline(tristanlm,col = 'black',lwd = '3')
# abline(goughlm,col='black',lwd='3')
# 
# #add a legend
# legend(1,17,legend=c("Tristan (North Islands)", "Gough (South Island)"),
#        col=c("red", "blue"), lty=1, cex=1)



## working on a timeseries of sst data
ggplot(data=SST, aes(x=Year, y=Tristan, group=1)) +
  geom_line()



ggplot(data=SST, aes(x=Year, y=Gough, group=1)) +
  geom_line()


## working on boxplots of penguin egg sizes
final.b=Final_All[Final_All$Decision=="B",]

bp <- ggplot(final.b, aes(x=Zone, y=Volume)) + geom_boxplot()
bp

#need to make these pretty and overlay them on top of one another...
