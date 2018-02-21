#rockhopper penguin data from Alex Bond

#created new variables: Location2 =gough and alex islands are merged. changed the name alex to gough
#                     : Zone =gough island separated from all other islands.


#needed packages
library("nlme")

#loading the data
penguin.data<-read.csv("C:/Users/Anthony Minerovic/Google Drive/Blackwood Lab/2018/penguins/waddle_harder/penguin.csv")
names(penguin.data)
dim(penguin.data)
View(penguin.data)
summary(penguin.data)


#subset by egg size

penguin.a=penguin.data[penguin.data$A.or.B.egg=="A",]
penguin.b=penguin.data[penguin.data$A.or.B.egg=="B",]
penguin.u=penguin.data[penguin.data$A.or.B.egg=="U",]
penguin.no.u=penguin.data[penguin.data$A.or.B.egg!="U",]

#subset by climate zone
penguin.north=penguin.data[penguin.data$Zone=="Other",]
penguin.south=penguin.data[penguin.data$Zone=="Gough",]

#subset by location

penguin.Nightingale=penguin.data[penguin.data$Location=="Nightingale",]
penguin.Tristan=penguin.data[penguin.data$Location=="Tristan",]
#Gough and Alex
penguin.Gough=penguin.data[penguin.data$Location2=="Gough",]
penguin.Inaccessible=penguin.data[penguin.data$Location=="Inaccessible",]


#Subset by years

#cutting out all of the years with a single specimen
penguin.n1<-read.csv("C:/Users/Anthony Minerovic/Google Drive/Blackwood Lab/2018/penguins/waddle_harder/penguin.n1.csv")

#cutting out all of the years where there's only one island with specimens


#cut out years before 2002
penguin.new=penguin.data[penguin.data$Year>2001,]
View(penguin.new)



#linear models

#giving me trouble with there being only one location for some years. also doesn't like using Day as a random factor

penguin.vol.location=lme(fixed=Volume~Location, random=~1|Length..mm., data=penguin.data)
anova(penguin.vol.location)

penguin.loc.yr=lme(fixed=Volume~Location*Year, random=~1|SpecimenID, data=penguin.n1)


penguin2all=lm(Volume~Location*Year*Zone*Source, data=penguin.data)
anova(penguin2all)

