
#needed packages
library("nlme")

#loading the data
penguin.data<-read.csv("C:/Users/Anthony Minerovic/Google Drive/Blackwood Lab/2018/penguins/penguin.csv")
names(penguin.data)
dim(penguin.data)

penguin.a=penguin.data[penguin.data$A.or.B.egg=="A",]
penguin.b=penguin.data[penguin.data$A.or.B.egg=="B",]
penguin.u=penguin.data[penguin.data$A.or.B.egg=="U",]
penguin.no.u=penguin.data[penguin.data$A.or.B.egg!="U",]


penguin1all=lme(fixed=Volume~Location*Year*Month*Source, random=~1|Day, data=penguin.data)




#we hooked up!
