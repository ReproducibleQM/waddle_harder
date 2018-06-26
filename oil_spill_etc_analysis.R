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
                                mean.Volume=mean(Volume), N=length(Volume), 
                                SE=sd(Volume)/sqrt(N))
summary.penguin.oilspill$all.lines<-with(summary.penguin.oilspill, interaction(Decision, Zone))

#reorder factor levels for oilspill
summary.penguin.oilspill$oilspill<-factor(summary.penguin.oilspill$oilspill, levels=c("Before", "After"))


#insert plot here
library(ggplot2)
library(ggrepel)


oil.plot<-ggplot(summary.penguin.oilspill, aes(oilspill, mean.Volume, 
                                               shape=Zone, fill=Decision, label=N))+
  scale_fill_manual(values=c("blue", "orange"), name="Egg type", 
                    guide=guide_legend(override.aes=aes(shape=21)))+
  scale_shape_manual(values=c(21,24), labels=c("North", "South"))+
  geom_line(aes(group=all.lines), color="black", position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=(mean.Volume-SE), ymax=(mean.Volume+SE)), width=0.05, color="black",
                position=position_dodge(width=0.3))+
  geom_point(color="black", size=4, position=position_dodge(width=0.3))+
  geom_text(aes(label=N),hjust=1.4, vjust=-1.2, position=position_dodge(width=0.3))+
  theme_bw()+
  labs(y=expression("Mean egg volume, "~cm^3), x="Oil Spill")

oil.plot
#save to pdf
pdf("figs/oilspillplot.pdf", height=5, width=7)
oil.plot
dev.off()


# now we want to examine the finer scale patterns in variation of egg size.
#instead of lumping by time period and island group, we'll just break it out so that readers can see the patterns
#This will be a very similar analysis as above, 
# but there will be an interaction with year


#and because we want to look at a year effect, and we want to see if there's anything going on
#relative to time, but there's not a ton of data so we'll just need to look at years directly and ANOVA them

#bring back the pre-1970 data
penguin2<-penguin[which(penguin$Decision != "U"),]

#we're interested in change with time in these two periods, and by island, but otherwise the analysis is 
#very similar to the above
year.model<-lm(Volume~Location2*as.factor(Year)*Decision, data=penguin2)

summary(year.model)
AIC(year.model)
anova(year.model)
TukeyHSD(aov(year.model))
#data are very non normal
shapiro.test(resid(year.model))


summary.penguin.year<-ddply(penguin2, c("Location2", "Decision", "Year"), summarise,
                                mean.Volume=mean(Volume), N=length(Volume),  SE=sd(Volume)/sqrt(N))


#reorder factor levels for islands
summary.penguin.year$Location2<-factor(summary.penguin.year$Location2, levels=c("Tristan", "Nightingale", "Inaccessible", "Gough"))

#create interaction
summary.penguin.year$all.lines<-with(summary.penguin.year, interaction(Location2,  Decision))

year.plot<-ggplot(summary.penguin.year, aes(as.factor(Year), mean.Volume, 
                                               shape=Location2, fill=Decision, label=N))+
  scale_fill_manual(values=c("blue", "orange"), name="Egg type", 
                    guide=guide_legend(override.aes=aes(shape=21), order=1))+
  scale_shape_manual(values=c(23,22,21,24), name="Island", labels=c("Tristan", "Nightingale", "Inaccessible", "Gough"))+
  geom_line(aes(group=all.lines), color="black",  position=position_dodge(width=0.2))+
  geom_errorbar(aes(ymin=(mean.Volume-SE), ymax=(mean.Volume+SE)), width=0.05, color="black",
                position=position_dodge(width=0.2))+
  geom_point(color="black", size=4,  position=position_dodge(width=0.2))+
  geom_text_repel(aes(label=N),hjust=1.7, vjust=-0.3,  position=position_dodge(width=0.2))+
  theme_bw()+
  labs(y=expression("Mean egg volume, "~cm^3), x="Year")

year.plot

#save to pdf
pdf("figs/yearplot.pdf", height=5, width=7)
year.plot
dev.off()

#now, let's do some correlation analyses to see how A and B eggs are correlated with each other by zone
summary.penguin.zone<-ddply(penguin2, c("Zone", "Decision", "Year"), summarise,
                            mean.Volume=mean(Volume), N=length(Volume),  SE=sd(Volume)/sqrt(N))

#cut out years that there was only 1 egg of a given type collected
summary.penguin.zone<-summary.penguin.zone[which(summary.penguin.zone$N>1),]

# first let's split it up and then line the data back up together 

A.eggs<-summary.penguin.zone[which(summary.penguin.zone$Decision=="A"),]

B.eggs<-summary.penguin.zone[which(summary.penguin.zone$Decision=="B"),]

#fiddle with it so we can remerge it all
A.eggs$all.lines<-NULL
B.eggs$all.lines<-NULL
A.eggs$Decision<-NULL
B.eggs$Decision<-NULL
names(A.eggs)<-c("Zone","Year","A.mean.Volume", "A.N", "A.SE")
names(B.eggs)<-c("Zone","Year","B.mean.Volume", "B.N", "B.SE")

crosstab.eggs<-merge(A.eggs, B.eggs, by=c("Zone", "Year"))

#reorder factor levels for islands
crosstab.eggs$Zone<-factor(crosstab.eggs$Zone, levels=c("Other", "Gough"))

#now let's do a correlation analysis

cor.test(crosstab.eggs$B.mean.Volume, crosstab.eggs$A.mean.Volume, method="pearson")

#not much there, but we might be able to see things a little better if we look at this by Zone

egg.cor.lm<-lm(B.mean.Volume~A.mean.Volume*Zone, data=crosstab.eggs)
summary(egg.cor.lm)

#lets's also do variability

egg.SE.lm<-lm(B.SE~A.SE*Zone, data=crosstab.eggs)
summary(egg.SE.lm)

#not a lot significant, but let's see what we can visualize

eggcorr.plot<-ggplot(crosstab.eggs, aes(A.mean.Volume, B.mean.Volume, 
                                            shape=Zone, fill=Zone, linetype=Zone, label=Year))+
  scale_shape_manual(values=c(21,24), labels=c("North", "South"))+
  scale_linetype_manual(values=c("twodash", "dashed"), labels=c("North", "South"))+
  scale_fill_manual(values=c("pink", "yellow"), name="Zone", labels=c("North", "South"))+
  geom_smooth(method="lm", se=FALSE, colour="grey")+
  geom_errorbar(aes(ymin=(B.mean.Volume-B.SE), ymax=(B.mean.Volume+B.SE)), width=0.05, color="black", linetype="solid")+
  geom_errorbarh(aes(xmin=(A.mean.Volume-A.SE), xmax=(A.mean.Volume+A.SE)), height=0.05, color="black", linetype="solid")+
  geom_point(color="black", size=4)+
  geom_text_repel(aes(label=Year), size=3, nudge_y=1, nudge_x=2)+
  theme_bw()+
  labs(y=expression("Mean B egg volume "~cm^3), x=expression("Mean A egg volume "~cm^3))

eggcorr.plot

#save to pdf
pdf("figs/AvsBvol.pdf", height=5, width=7)
eggcorr.plot
dev.off()

eggvar.plot<-ggplot(crosstab.eggs, aes(A.SE, B.SE, 
                                        shape=Zone, fill=Zone, label=Year))+
  scale_shape_manual(values=c(21,24), labels=c("North", "South"))+
  scale_fill_manual(values=c("pink", "yellow"), name="Zone", labels=c("North", "South"))+
  geom_smooth(method="lm", se=FALSE)+
  geom_point(color="black", size=4)+
  geom_text_repel(aes(label=Year),hjust=1.5, vjust=-0.3, size=3)+
  theme_bw()+
  labs(y=expression("Mean B egg volume variation"~cm^3), x=expression("Mean A egg volume variation "~cm^3))

eggvar.plot


#not much there, so let's leave it alone.

