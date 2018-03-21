#rockhopper penguin data from Alex Bond

#created new variables: Location2 =gough and alex islands are merged. changed the name alex to gough
#                     : Zone =gough island separated from all other islands.


#needed packages
library(nlme)
library(ggplot2)
library(car)
library(plyr)
library(lme4)
library(doBy)
library(reshape2)
library(AICmodavg)
library(vegan)


#loading the data
penguin.data<-read.csv("penguin.csv")
names(penguin.data)
dim(penguin.data)
View(penguin.data)
summary(penguin.data)

#to get rid of na
#penguin<-subset(penguin.data,!is.na(penguin.data$))


#subset by egg size

penguin.a=penguin.data[penguin.data$A.or.B.egg=="A",]
penguin.b=penguin.data[penguin.data$A.or.B.egg=="B",]
penguin.u=penguin.data[penguin.data$A.or.B.egg=="U",]
penguin.no.u=penguin.data[penguin.data$A.or.B.egg!="U",]


#subset by location
#Nightingale and Alex grouped together
penguin.Nightingale=penguin.data[penguin.data$Location=="Nightingale",]
penguin.Tristan=penguin.data[penguin.data$Location=="Tristan",]
penguin.Gough=penguin.data[penguin.data$Location2=="Gough",]
penguin.Inaccessible=penguin.data[penguin.data$Location=="Inaccessible",]


#subset a or b egg for each island
penguin.Nightingale.a=penguin.data[penguin.a$Location=="Nightingale",]
penguin.Tristan.a=penguin.data[penguin.a$Location=="Tristan",]
penguin.Gough.a=penguin.data[penguin.a$Location2=="Gough",]
penguin.Inaccessible.a=penguin.data[penguin.a$Location=="Inaccessible",]



#subset by climate zone
penguin.north=penguin.new[penguin.new$Zone=="Other",]
penguin.south=penguin.new[penguin.new$Zone=="Gough",]

#Subset by years

#cutting out all of the years with a single specimen
penguin.n1<-read.csv("penguin.n1.csv")
#cutting out all of the years where there's only one island with specimens


#cut out years before 2002
penguin.new=penguin.data[penguin.data$Year>2001,]
View(penguin.new)

penguin.Gough.new=penguin.new[penguin.new$Location=="Gough",]
penguin.Nightingale.new=penguin.new[penguin.new$Location2=="Nightingale",]
penguin.Tristan.new=penguin.new[penguin.new$Location=="Tristan",]
penguin.Inaccessible.new=penguin.new[penguin.new$Location=="Inaccessible",]


#linear models

#giving me trouble with there being only one location for some years. also doesn't like using Day as a random factor

penguin.vol.location=lme(fixed=Volume~Location, random=~1|Length, data=penguin.data)
anova(penguin.vol.location)



penguins2all=lm(Volume~Year*Zone, data=penguin.data)
anova(penguins2all)


penguin2all = aov(Volume~Location, data=penguin.data)

TukeyHSD(penguin2all, "Location", ordered=FALSE)

#looks like there's a big difference between gough and the other islands, but the other islands aren't that much different from each other

#now do it for just a-eggs and b-eggs

penguin2a = aov(Volume~Location , data=penguin.a)

TukeyHSD(penguin2a, "Location", ordered=FALSE)


penguin2b = aov(Volume~Location , data=penguin.b)

TukeyHSD(penguin2b, "Location", ordered=FALSE)


#looking for difference over years on each island separately for all years

Goughlm=lm(Volume~Year, data=penguin.Gough)
anova(Goughlm)

Tristanlm=lm(Volume~Year, data=penguin.Tristan)
anova(Tristanlm)

Nightingalelm=lm(Volume~Year, data=penguin.Nightingale)
anova(Nightingalelm)
#   nightingale island shows significant change over time for all years

Inaccessiblelm=lm(Volume~Year, data=penguin.Inaccessible)
anova(Inaccessiblelm)

#looking for difference over years on each island separately from 2002 to 2015

#only worked for gough, which is significant
Goughnewlm=lm(Volume~Year, data=penguin.Gough.new)
anova(Goughnewlm)

Tristannewlm=lm(Volume~Year, data=penguin.Tristan.new)
anova(Tristannewlm)

Nightingalenewlm=lm(Volume~Year, data=penguin.Nightingale.new)
anova(Nightingalenewlm)

Inaccessiblenewlm=lm(Volume~Year, data=penguin.Inaccessible.new)
anova(Inaccessiblenewlm)


# Plot Year and Length
plot(penguin.data$Length~penguin.data$Year)


qplot(Year,Volume, data=penguin.a)
qplot(Year,Volume, data=penguin.b)
qplot(Year,Volume, data=penguin.u)
qplot(Year,Volume, data=penguin.Tristan)


#Matts Experimental plots
penguinPlot <- ggplot(penguin.data, aes(Year, Volume, color = AorB)) + 
  geom_point(aes(fill = AorB), pch = 21) +
    scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()


#Call Plot
penguinPlot



#Regression, Find the Slope
linearModel <- lm(Volume~AorB*Year, data = penguin.data)

linearModel

summary(linearModel)

#Same plot as above, but for each hisland separately

#tristan
penguinPlot.Tristan <- ggplot(penguin.Tristan, aes(Year, Volume, color = AorB)) + 
  geom_point(aes(fill = AorB), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Tristan
#Regression
Tristan.linearModel <- lm(Volume~AorB*Year, data = penguin.Tristan)
Tristan.linearModel
summary(Tristan.linearModel)

#Gough
penguinPlot.Gough <- ggplot(penguin.Gough, aes(Year, Volume, color = AorB)) + 
  geom_point(aes(fill = AorB), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Gough
#Regression
Gough.linearModel <- lm(Volume~AorB*Year, data = penguin.Gough)
Gough.linearModel
summary(Gough.linearModel)

#Inaccessible     inaccessible island only has U eggs
penguinPlot.Inaccessible <- ggplot(penguin.Inaccessible, aes(Year, Volume, color = AorB)) + 
  geom_point(aes(fill = AorB), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Inaccessible
#Regression
Inaccessible.linearModel <- lm(Volume~AorB*Year, data = penguin.Inaccessible)
Inaccessible.linearModel
summary(Inaccessible.linearModel)
#inaccessible island only has U eggs


#nightingale   nightingale island only differentiated between a and b eggs in 2014
penguinPlot.Nightingale <- ggplot(penguin.Nightingale, aes(Year, Volume, color = AorB)) + 
  geom_point(aes(fill = AorB), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Nightingale
#Regression
Nightingale.linearModel <- lm(Volume~AorB*Year, data = penguin.Inaccessible)
Nightingale.linearModel
summary(Nightingale.linearModel)
#nightingale island only differentiated between a and b eggs in 2014



# Discriminant Function from Bond et al. 2016
# D = 0.73 * Length + 0.5 * Breadth - 72.39
# Pr(A) = 1 / (1+e^(-D))
penguin.data$discriminant <- 0.73 * penguin.data$Length+ 0.5 * penguin.data$Breadth - 72.39
penguin.data$ProbabilityA <- 1 / (1 + exp(-penguin.data$discriminant))
penguin.data$Decision <- ifelse(penguin.data$ProbabilityA >= 0.66, "A", ifelse(penguin.data$ProbabilityA <= 0.33, "B", "U"))



#Remake plots with Discriminant function data
#Matts Experimental plots
penguinPlot2 <- ggplot(penguin.new, aes(Year, Volume, color = Decision)) + 
  geom_point(aes(fill = Decision), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()


#Call Plot
penguinPlot2



#Regression, Find the Slope
linearModel2 <- lm(Volume~Decision*Year, data = penguin.data)

linearModel2

summary(linearModel2)

#Same plot as above, but for each hisland separately

#tristan
penguinPlot.Tristan2 <- ggplot(penguin.Tristan, aes(Year, Volume, color = Decision)) + 
  geom_point(aes(fill = Decision), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Tristan2
#Regression
Tristan.linearModel2 <- lm(Volume~Decision*Year, data = penguin.Tristan)
Tristan.linearModel2
summary(Tristan.linearModel2)

#Gough
penguinPlot.Gough2 <- ggplot(penguin.Gough, aes(Year, Volume, color = Decision)) + 
  geom_point(aes(fill = Decision), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Gough2
#Regression
Gough.linearModel2 <- lm(Volume~Decision*Year, data = penguin.Gough)
Gough.linearModel2
summary(Gough.linearModel2)

#Inaccessible     inaccessible island only has U eggs
penguinPlot.Inaccessible2 <- ggplot(penguin.Inaccessible, aes(Year, Volume, color = Decision)) + 
  geom_point(aes(fill = Decision), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Inaccessible2
#Regression
Inaccessible.linearModel2 <- lm(Volume~Decision*Year, data = penguin.Inaccessible)
Inaccessible.linearModel2
summary(Inaccessible.linearModel2)
#inaccessible island only has U eggs


#nightingale   nightingale island only differentiated between a and b eggs in 2014
penguinPlot.Nightingale2 <- ggplot(penguin.Nightingale, aes(Year, Volume, color = Decision)) + 
  geom_point(aes(fill = Decision), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()
#Call
penguinPlot.Nightingale2
#Regression
Nightingale.linearModel2 <- lm(Volume~Decision*Year, data = penguin.Inaccessible)
Nightingale.linearModel2
summary(Nightingale.linearModel2)
#nightingale island only differentiated between a and b eggs in 2014


#by climate zone
penguinPlotnorth <- ggplot(penguin.north, aes(Year, Volume, color = Decision)) + 
  geom_point(aes(fill = Decision), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()


#Call Plot
penguinPlotnorth



#Regression, Find the Slope
linearModelnorth <- lm(Volume~Decision*Year, data = penguin.north)

linearModelnorth

summary(linearModelnorth)


write.csv(penguin.data, "penguin.data.csv")
