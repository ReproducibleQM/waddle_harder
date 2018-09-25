#rockhopper penguin data from Alex Bond

#created new variables: Location2 =gough and alex islands are merged. changed the name alex to gough
#                     : Zone =gough island separated from all other islands.


#needed packages
library("nlme")
library("ggplot2")

#loading the data
penguin.data<-read.csv("data/penguin.csv")
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

#subset by climate zone
penguin.north=penguin.data[penguin.data$Zone=="Other",]
penguin.south=penguin.data[penguin.data$Zone=="Gough",]

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



#Subset by years

#cutting out all of the years with a single specimen
penguin.n1<-read.csv("C:/Users/Anthony Minerovic/Google Drive/Blackwood Lab/2018/penguins/waddle_harder/penguin.n1.csv")

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

penguin.vol.location=lme(fixed=Volume~Location, random=~1|Length..mm., data=penguin.data)
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


# PLot Year and Length
plot(penguin.data$Length..mm.~penguin.data$Year)


qplot(Year,Volume, data=penguin.a)
qplot(Year,Volume, data=penguin.b)
qplot(Year,Volume, data=penguin.u)
qplot(Year,Volume, data=penguin.Tristan)
