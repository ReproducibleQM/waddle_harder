# SST plots
# RUN penguinR.R FIRST!!
#also oil_spill_etc_analysis.R

library(zoo)
library(ggplot2)

# loading the data
SST<-read.csv(file="SST_TG.csv", header=T)
View(SST)


SST.means<-ddply(SST, "Year", summarize, 
                North=mean(Tristan), South=mean(Gough))

summary.penguin.year.Zone<-ddply(penguin2, c("Zone", "Decision", "Year"), summarise,
                            mean.Volume=mean(Volume), N=length(Volume),  SE=sd(Volume)/sqrt(N))

#subset the data so we can manually add it as layers to the graph
summary.penguin.year.North<-summary.penguin.year.Zone[which(summary.penguin.year.Zone$Zone=="Other"),]
summary.penguin.year.South<-summary.penguin.year.Zone[which(summary.penguin.year.Zone$Zone=="Gough"),]

summary.penguin.year.North.A<-summary.penguin.year.North[which(summary.penguin.year.North$Decision=="A"),]
summary.penguin.year.North.B<-summary.penguin.year.North[which(summary.penguin.year.North$Decision=="B"),]

summary.penguin.year.South.A<-summary.penguin.year.South[which(summary.penguin.year.South$Decision=="A"),]
summary.penguin.year.South.B<-summary.penguin.year.South[which(summary.penguin.year.South$Decision=="B"),]
#ggplot for timeseries


Northplot<-ggplot(data=SST.means, aes(x=Year, y=North)) +
  geom_line(linetype = "solid", colour="darkred")+
  theme_bw()+
  ylim(c(13,17))+
  #put the other measures on the secondary axis
  geom_errorbar(data=summary.penguin.year.North.A, aes(x=Year, y=(mean.Volume/50+13.2), 
                                                       ymin=(mean.Volume-SE)/50+13.2,
                                                       ymax=(mean.Volume+SE)/50+13.2), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.North.A, aes(x=Year, y=(mean.Volume/50+13.2)),
             color="black", size=4, fill="blue", pch=24)+
  geom_errorbar(data=summary.penguin.year.North.B, aes(x=Year, y=(mean.Volume/50+13.2), 
                                                       ymin=(mean.Volume-SE)/50+13.2,
                                                       ymax=(mean.Volume+SE)/50+13.2), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.North.B, aes(x=Year, y=(mean.Volume/50+13.2)),
             color="black", size=4, fill="orange", pch=22)+
  scale_y_continuous(sec.axis=sec_axis(~(.-13.2)*50, name=expression("Mean egg volume, "~cm^3)))+
    labs(y=expression("Mean annual sea surface temperature, "~degree~C), x="Year")
Northplot


Southplot<-ggplot(data=SST.means, aes(x=Year, y=South)) +
  geom_line(linetype = "solid", colour="darkblue")+
  theme_bw()+
  ylim(c(13,17))+
  #put the other measures on the secondary axis
  geom_errorbar(data=summary.penguin.year.South.A, aes(x=Year, y=(mean.Volume/50+13.2), 
                                                       ymin=(mean.Volume-SE)/50+13.2,
                                                       ymax=(mean.Volume+SE)/50+13.2), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.South.A, aes(x=Year, y=(mean.Volume/50+13.2)),
             color="black", size=4, fill="blue", pch=24)+
  geom_errorbar(data=summary.penguin.year.South.B, aes(x=Year, y=(mean.Volume/50+13.2), 
                                                       ymin=(mean.Volume-SE)/50+13.2,
                                                       ymax=(mean.Volume+SE)/50+13.2), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.South.B, aes(x=Year, y=(mean.Volume/50+13.2)),
             color="black", size=4, fill="orange", pch=22)+
  scale_y_continuous(sec.axis=sec_axis(~(.-13.2)*50, name=expression("Mean egg volume, "~cm^3)))+
  labs(y=expression("Mean annual sea surface temperature, "~degree~C), x="Year")
Southplot



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
