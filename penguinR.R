#rockhopper penguin data from Alex Bond

#created new variables: Location2 =gough and alex islands are merged. changed the name alex to gough
#                     : Zone =gough island separated from all other islands.

remove('Final_North', 'Final_South')
#needed packages
library(nlme)
library(ggplot2)
library(car)
library(plyr)
library(lme4)
library(doBy)
library(reshape2)
library(AICcmodavg)
library(vegan)


#loading the data
penguin.data<-read.csv("penguin.csv")
names(penguin.data)
dim(penguin.data)
View(penguin.data)
summary(penguin.data)

#to get rid of na
#penguin<-subset(penguin.data,!is.na(penguin.data$))

#subset by location
#Nightingale and Alex grouped together
penguin.Nightingale=penguin.data[penguin.data$Location2=="Nightingale",]
penguin.Tristan=penguin.data[penguin.data$Location=="Tristan",]
penguin.Gough=penguin.data[penguin.data$Location=="Gough",]
penguin.Inaccessible=penguin.data[penguin.data$Location=="Inaccessible",]


# Discriminant Function from Bond et al. 2016
# D = 0.73 * Length + 0.5 * Breadth - 72.39
# Pr(A) = 1 / (1+e^(-D))
penguin.data$discriminant <- 0.73 * penguin.data$Length+ 0.5 * penguin.data$Breadth - 72.39
penguin.data$ProbabilityA <- 1 / (1 + exp(penguin.data$discriminant))
#also cull out very small eggs
penguin.data$Decision <- ifelse(penguin.data$ProbabilityA >= 0.66 & penguin.data$Volume>=50, "A", ifelse(penguin.data$ProbabilityA <= 0.33, "B", "U"))


#subset by climate zone
penguin.north=penguin.data[penguin.data$Zone=="Other",]
penguin.south=penguin.data[penguin.data$Zone=="Gough",]

#subset A or B egg for each climate zone
#subset by egg size
penguin.A=penguin.data[penguin.data$A.or.B.egg=="A",]
penguin.B=penguin.data[penguin.data$A.or.B.egg=="B",]
penguin.u=penguin.data[penguin.data$A.or.B.egg=="U",]

penguin.north.A=penguin.data[penguin.A$Zone=="Other",]
penguin.south.A=penguin.data[penguin.A$Zone=="Gough",]

penguin.north.B=penguin.data[penguin.B$Zone=="Other",]
penguin.south.B=penguin.data[penguin.B$Zone=="Gough",]


#bring in the SST data
SST<-read.csv(file="SST_TG.csv", header=T)

melted_sst<-melt(SST, id=c("Year", "Month"))
# create an object with the SSTs
sst_island<-melted_sst[which(melted_sst$variable=="Tristan"|melted_sst$variable=="Gough"),]
#create an object with the residuals
sst_residual<-melted_sst[which(melted_sst$variable=="Tristan_Residual"| melted_sst$variable =="Gough_Residual"),]

#give meaningful column names
names(sst_island)<-c("Year", "Month", "Zone", "SST")
names(sst_residual)<-c("Year", "Month", "Zone", "SST_Residual")

#harmonize variable names
sst_residual$Zone<-gsub("_Residual", "", sst_residual$Zone)

#Merge the SST data with the penguin data
#all_data<-merge(penguin.data, sst_working, by=c("Year", "Zone")

# write new penguin data table to csv
write.csv(penguin.data, "penguin.data.csv") 


# merge the data
sst_working<-merge(sst_island, sst_residual)

# harmonize naming conventions with the penguin data
sst_working$Zone<-gsub("Tristan", "Other", sst_working$Zone)

# sort data by site, then by year, then by month
SST<-SST[order(SST$Year, SST$Month),]

# may already be sorted this way, but probably good to have a command to explicitly do it in case data gets mixed up
# insert index variable here
SST$index<-1:length(SST$Year)

SST_Gough = sst_working[which(sst_working$Zone=='Gough'),]
SST_North= sst_working[which(sst_working$Zone=='Other'),]
penguin.north$Zone <- gsub('Other', 'North',penguin.north$Zone)

years_in_set<-unique(penguin.north$Year)
lags<-1:17
sst_north_lags = matrix(0,nrow=length(penguin.north$Year),ncol=length(lags)) # Zero matrix for north, will fill rows w/ samples, and cols w/ lags
sst_south_lags = matrix(0,nrow=length(penguin.south$Year),ncol=length(lags)) # Zero matrix for Gough, will fill rows w/ samples, and cols w/ lags
sst_north_lags_residual = matrix(0,nrow=length(penguin.north$Year),ncol=length(lags)) # Zero matrix for north, will fill rows w/ samples, and cols w/ lags
sst_south_lags_residual = matrix(0,nrow=length(penguin.south$Year),ncol=length(lags)) # Zero matrix for Gough, will fill rows w/ samples, and cols w/ lags


## North Lags SST
for (i in 1:length(penguin.north$Year)){
  year=penguin.north$Year[i]
  # create an empty data frame to put your calculations into 
  # should be something like year, lag, average, max, min in columns
  indexline<-SST[(SST$Year==year) & (SST$Month==10),]
  indexno<-as.numeric(indexline$index[1])
  for (j in 1:length(lags)){
    sst_data<-SST[which(SST$index<indexno+1 & SST$index>indexno-j),] # This subsets the SST for lags, really only 16 is important
  }
  # This snippet takes all 16 lags (column) and slaps it into a row representing the lags for each sample...total length 641 for North
  # The values in the 1st column are the sst for 16 months before egg, 2nd column is 15 before, etc....
  sst_north_lags[i,] = sst_data$Tristan 
  sst_north_lags=sst_north_lags[,c(length(lags):1)]
}

## North Lags SST Residual
for (i in 1:length(penguin.north$Year)){
  year=penguin.north$Year[i]
  #### create an empty data frame to put your calculations into 
  #### should be something like year, lag, average, max, min in columns
  indexline<-SST[(SST$Year==year) & (SST$Month==10),]
  indexno<-as.numeric(indexline$index[1])
  for (j in 1:length(lags)){
    
    sst_data<-SST[which(SST$index<indexno+1 & SST$index>indexno-j),] # This subsets the SST for lags, really only 16 is important
    
    
    ####then use this index to subset the data by j
    #### then perform the calculation to get the average, max and min for j
    ####then add these values to the data frame, with a label for year
  }
  # This snippet takes all 16 lags (column) and slaps it into a row representing the lags for each sample...total length 641 for North
  # The values in the 1st column are the sst for 16 months before egg, 2nd column is 15 before, etc....
  sst_north_lags_residual[i,] = sst_data$Tristan_Residual
  sst_north_lags_residual=sst_north_lags_residual[,c(length(lags):1)]
}

# Penguin South Lags
for (i in 1:length(penguin.south$Year)){
  year=penguin.south$Year[i]
  #### create an empty data frame to put your calculations into 
  #### should be something like year, lag, average, max, min in columns
  indexline<-SST[(SST$Year==year) & (SST$Month==10),]
  indexno<-as.numeric(indexline$index[1])
  for (j in 1:length(lags)){
    
    sst_data<-SST[which(SST$index<indexno+1 & SST$index>indexno-j),] # This subsets the SST for lags, really only 16 is important
  }
  # This snippet takes all 16 lags (column) and slaps it into a row representing the lags for each sample...total length 641 for North
  # The values in the 1st column are the sst for 16 months before egg, 2nd column is 15 before, etc....
  sst_south_lags[i,] = sst_data$Gough 
  sst_south_lags=sst_south_lags[,c(length(lags):1)]
}

## South Lags SST Residual
for (i in 1:length(penguin.south$Year)){
  year=penguin.south$Year[i]
  #### create an empty data frame to put your calculations into 
  #### should be something like year, lag, average, max, min in columns
  indexline<-SST[(SST$Year==year) & (SST$Month==10),]
  indexno<-as.numeric(indexline$index[1])
  for (j in 1:length(lags)){
    
    sst_data<-SST[which(SST$index<indexno+1 & SST$index>indexno-j),] # This subsets the SST for lags, really only 16 is important
  }
  # This snippet takes all 16 lags (column) and slaps it into a row representing the lags for each sample...total length 641 for North
  # The values in the 1st column are the sst for 16 months before egg, 2nd column is 15 before, etc....
  sst_south_lags_residual[i,] = sst_data$Gough_Residual
  sst_south_lags_residual=sst_south_lags_residual[,c(length(lags):1)]
}

# turn matrix back into data.frame
sst_south_lags2 = data.frame(sst_south_lags)
sst_north_lags2 = data.frame(sst_north_lags)
sst_north_lags_residual2 = data.frame(sst_north_lags_residual)
sst_south_lags_residual2 = data.frame(sst_south_lags_residual)
sst_lags_all = rbind(sst_north_lags2, sst_south_lags2)
Residual_lags_all = rbind(sst_north_lags_residual2, sst_south_lags_residual2)

# merge lags and penguin data
Final_North = cbind(penguin.north, sst_north_lags2)
Final_South = cbind(penguin.south, sst_south_lags2)
Final_North_Residuals = cbind(penguin.north, sst_north_lags_residual2)
Final_South_Residuals = cbind(penguin.south, sst_south_lags_residual2)

Final_All = rbind(Final_North, Final_South)
Final_All_Residual = rbind(Final_North_Residuals, Final_South_Residuals)

# Do calculations on the lags for SST data
Final_All$avg2 = rowMeans(sst_lags_all[1:2])
Final_All$avg3 = rowMeans(sst_lags_all[1:3])
Final_All$avg4 = rowMeans(sst_lags_all[1:4])
Final_All$avg5_1 = rowMeans(sst_lags_all[1:5])
Final_All$avg5_2 = rowMeans(sst_lags_all[6:10])
Final_All$avg5_3 = rowMeans(sst_lags_all[11:15])
Final_All$avg8_1 = rowMeans(sst_lags_all[1:8])
Final_All$avg8_2 = rowMeans(sst_lags_all[9:16])

Final_All$min = apply(sst_lags_all,1,min)
Final_All$max = apply(sst_lags_all,1,max)

# Do calculations on the lags SST Residuals
Final_All_Residual$avg2 = rowMeans(Residual_lags_all[1:2])
Final_All_Residual$avg3 = rowMeans(Residual_lags_all[1:3])
Final_All_Residual$avg4 = rowMeans(Residual_lags_all[1:4])
Final_All_Residual$avg5_1 = rowMeans(sst_lags_all[1:5])
Final_All_Residual$avg5_2 = rowMeans(sst_lags_all[6:10])
Final_All_Residual$avg5_3 = rowMeans(sst_lags_all[11:15])
Final_All_Residual$avg8_1 = rowMeans(sst_lags_all[1:8])
Final_All_Residual$avg8_2 = rowMeans(sst_lags_all[9:16])

Final_All_Residual$min = apply(Residual_lags_all,1,min)
Final_All_Residual$max = apply(Residual_lags_all,1,max)

# Linear Models
LM = list()
LM[[1]] = lm(Final_All_Residual$Volume~Decision+Zone*X1,data=Final_All_Residual)
LM[[2]] = lm(Final_All_Residual$Volume~Decision+Zone*avg2,data=Final_All_Residual)
LM[[3]] = lm(Final_All_Residual$Volume~Decision+Zone*avg3,data=Final_All_Residual)
LM[[4]] = lm(Final_All_Residual$Volume~Decision+Zone*avg4,data=Final_All_Residual)
LM[[5]] = lm(Final_All_Residual$Volume~Decision+Zone*min,data=Final_All_Residual)
LM[[6]] = lm(Final_All_Residual$Volume~Decision+Zone*max,data=Final_All_Residual)
LM[[7]] = lm(Final_All_Residual$Volume~Decision+Zone*avg5_1,data=Final_All_Residual)
LM[[8]] = lm(Final_All_Residual$Volume~Decision+Zone*avg5_2,data=Final_All_Residual)
LM[[9]] = lm(Final_All_Residual$Volume~Decision+Zone*avg5_3,data=Final_All_Residual)
LM[[10]] = lm(Final_All_Residual$Volume~Decision+Zone*avg8_1,data=Final_All_Residual)
LM[[11]] = lm(Final_All_Residual$Volume~Decision+Zone*avg8_2,data=Final_All_Residual)

Modnames <- c("Month_1", "avg2", "avg3", "avg4", "min", "max","avg5_1","avg5_2","avg5_3","avg8_1","avg8_2")

(aict <- aictab(cand.set = LM, modnames=Modnames, sort=TRUE))

summary(LM[[3]])
anova(LM[[3]])

# we have weak eveidence for a differnt trend between the two zones, so let's look at them separately

Final_All_Residual_North<-Final_All_Residual[which(Final_All_Residual$Zone=="North"),] 
Final_All_Residual_South<-Final_All_Residual[which(Final_All_Residual$Zone!="North"),] 

North_SST_model = lm(Volume~Decision+avg3,data=Final_All_Residual_North)
summary(North_SST_model)
anova(North_SST_model)

