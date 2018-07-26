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
  #put the other measures on the secondary axis
  geom_errorbar(data=summary.penguin.year.North.A, aes(x=Year, y=(mean.Volume/31+11.7), 
                                                       ymin=(mean.Volume-SE)/31+11.7,
                                                       ymax=(mean.Volume+SE)/31+11.7), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.North.A, aes(x=Year, y=(mean.Volume/31+11.7)),
             color="black", size=4, fill="blue", pch=24)+
  geom_errorbar(data=summary.penguin.year.North.B, aes(x=Year, y=(mean.Volume/31+11.7), 
                                                       ymin=(mean.Volume-SE)/31+11.7,
                                                       ymax=(mean.Volume+SE)/31+11.7), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.North.B, aes(x=Year, y=(mean.Volume/31+11.7)),
             color="black", size=4, fill="orange", pch=22)+
  scale_y_continuous(limits=c(11.7,16), sec.axis=sec_axis(~(.-11.7)*31, name=NULL))+
    labs(y=expression("Mean annual sea surface temperature, "~degree~C), x="Year")+
  annotate("text", x=1850, y=15.7, label="A", size=6)
Northplot


Southplot<-ggplot(data=SST.means, aes(x=Year, y=South)) +
  geom_line(linetype = "solid", colour="darkblue")+
  theme_bw()+
  #put the other measures on the secondary axis
  geom_errorbar(data=summary.penguin.year.South.A, aes(x=Year, y=(mean.Volume/31+11.7), 
                                                       ymin=(mean.Volume-SE)/31+11.7,
                                                       ymax=(mean.Volume+SE)/31+11.7), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.South.A, aes(x=Year, y=(mean.Volume/31+11.7)),
             color="black", size=4, fill="blue", pch=24)+
  geom_errorbar(data=summary.penguin.year.South.B, aes(x=Year, y=(mean.Volume/31+11.7), 
                                                       ymin=(mean.Volume-SE)/31+11.7,
                                                       ymax=(mean.Volume+SE)/31+11.7), 
                width=0.05, color="black")+
  geom_point(data=summary.penguin.year.South.B, aes(x=Year, y=(mean.Volume/31+11.7)),
             color="black", size=4, fill="orange", pch=22)+
  scale_y_continuous(limits=c(11.7,16), sec.axis=sec_axis(~(.-11.7)*31, name=NULL))+
  labs(y=expression("Mean annual sea surface temperature, "~degree~C), x="Year")+
  annotate("text", x=1850, y=15.7, label="B", size=6)
Southplot


#stack the plots together
library(grid)
library(gridExtra)

#remove axis labels so we can just use common ones
Northplot1<-Northplot+
  ylab(NULL)+
  xlab(NULL)

Southplot1<-Southplot+
  ylab(NULL)+
  xlab(NULL)

grid.arrange(arrangeGrob(Northplot1, Southplot1, heights=c(1,1)),
             left=textGrob(expression("Mean annual sea surface temperature, "~degree~C), rot=90, hjust=-0.15),
             right=textGrob(expression("Mean egg volume, "~cm^3), rot=270, hjust=1.7),
             sub=textGrob("Year", vjust=-13))

pdf("figs/SST_timeseries_with_eggs.pdf", height=8, width=8)
grid.arrange(arrangeGrob(Northplot1, Southplot1, heights=c(1,1)),
             left=textGrob(expression("Mean annual sea surface temperature, "~degree~C), rot=90, hjust=-0.15),
             right=textGrob(expression("Mean egg volume, "~cm^3), rot=270, hjust=1.7),
             sub=textGrob("Year", vjust=-13))
dev.off()
