#read in the data for the oil spill and harvesting analyses
penguin<-read.csv("penguin.data.csv", header= TRUE)

#we are interested in testing if the egg size patterns correspond to predictions associated with two
#events- specifically, we want to test if the 2011 oil spill negatively affected egg size, or if
#human harvesting of eggs affected size of eggs over time. 

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
oil.model<-lm(Volume~Zone*oilspill+Decision*oilspill+Year, data=penguin1)

summary(oil.model)
AIC(oil.model)
anova(oil.model)
#data are very non normal
shapiro.test(resid(oil.model))

#ok, now we need to get the summary stats out of it
library(plyr)

summary.penguin.oilspill<-ddply(penguin1, c("Zone", "Decision", "oilspill"), summarise,
                                mean.Volume=mean(Volume), N=length(Volume), SE=sd(Volume)/sqrt(N))

#insert plot here


# now we want to examine the effect of harvesting on egg size. This will be a very similar analysis as above, 
# but there will be an interaction with year, corresponding to specific regulation periods


# 2006- Legislation to only harvest A eggs
# 2011- harvesting ban enacted

# we know that harvesting was strongest on Nightingale and Tristan, and practically nonexistent on the other
# islands, and we're assuming that penguins are relatively site-loyal and don't just randomly choose islands
# every year

# NOTE there is no data between 1999 and 2013 so we can't get all of this, but let's classify the data as we would if 
# we had these data, anyway

penguin1$legislate<-as.factor(ifelse(penguin1$Year>=2011, "Ban", 
                                     ifelse(penguin1$Year>=2006, "Aonly", "NoBan")))
