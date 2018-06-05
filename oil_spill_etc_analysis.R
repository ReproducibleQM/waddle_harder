#read in the data for the oil spill and harvesting analyses
penguin<-read.csv("penguin.data.csv", header= TRUE)

#we are interested in testing if the egg size patterns correspond to predictions associated with two
#events- specifically, we want to test if the 2011 oil spill negatively affected egg size, or if
#other variations eggs affected size of eggs over time with any discernable pattern 

#first, let's check if the oil spill affected egg size. The oil spill occured in 2011, and if it  
#was a driver, we would expect egg size to be lower after the spill, and MORE lower closer to the spill
# so let's test if that happened

#first we need to create a variable for the oil spill, which happened
penguin$oilspill<-as.factor(ifelse(penguin$Year>=2011, "After", "Before"))

#also, because data are spotty and weirdly distributed prior to 1970, let's cut it out for this analysis
#and cut out a couple outlying egg sizes
penguin1<-penguin[which(penguin$Year>1970 & penguin$Decision != "U" &
                          penguin$Volume<130),]

#fiddle with transformations to get us closer to normality of residuals

penguin1$logvolume<-log(penguin1$Volume)
penguin1$sqrtvolume<-sqrt(penguin1$Volume)

#now create a model to describe egg volume
oil.model<-lm(Volume~Zone*oilspill*Decision, data=penguin1)

summary(oil.model)
AIC(oil.model)
anova(oil.model)
TukeyHSD(aov(oil.model))
#data are very non normal
shapiro.test(resid(oil.model))

#ok, now we need to get the summary stats out of it
library(plyr)

summary.penguin.oilspill<-ddply(penguin1, c("Zone", "Decision", "oilspill"), summarise,
                                mean.Volume=mean(Volume), N=length(Volume), SE=sd(Volume)/sqrt(N))
summary.penguin.oilspill$all.lines<-with(summary.penguin.oilspill, interaction(Zone,  Decision))


#insert plot here
library(ggplot2)
oil.plot<-ggplot(summary.penguin.oilspill, aes(oilspill, mean.Volume, 
                                               group=Zone, shape=Zone, fill=Decision))+
  scale_fill_manual(values=c("blue", "orange"), name="Egg type")+
  scale_shape_manual(values=c(21,24))+
  geom_line(aes(group=all.lines), color="black")+
  geom_errorbar(aes(ymin=(mean.Volume-SE), ymax=(mean.Volume+SE)), width=0.05, color="black")+
  geom_point(color="black", size=4)+
    theme_bw()
oil.plot

# now we want to examine the finer scale patterns in variation of egg size.
#instead of lumping by time period and island group, we'll just break it out so that readers can see the patterns
#This will be a very similar analysis as above, 
# but there will be an interaction with year


#and because we want to look at a year effect, and we want to see if there's anything going on
#relative to time, but there's not a ton of data so we'll just need to look at years directly and ANOVA them


#we're interested in change with time in these two periods, and by island, but otherwise the analysis is 
#very similar to the above
year.model<-lm(Volume~Location2*as.factor(Year)*Decision, data=penguin1)

summary(year.model)
AIC(year.model)
anova(year.model)
TukeyHSD(aov(year.model))
#data are very non normal
shapiro.test(resid(year.model))


summary.penguin.year<-ddply(penguin1, c("Location2", "Decision", "Year"), summarise,
                                mean.Volume=mean(Volume), N=length(Volume), SE=sd(Volume)/sqrt(N))