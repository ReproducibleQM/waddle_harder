# Plot Script 


#Remake plots with Discriminant function data
#Matts Experimental plots
penguinPlot2 <- ggplot(penguin.data, aes(Year, Volume, color = Decision)) + 
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



#nightingale  
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


#by climate zone

#North
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


#South

penguinPlotsouth <- ggplot(penguin.south, aes(Year, Volume, color = Decision)) + 
  geom_point(aes(fill = Decision), pch = 21) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', size = 2, fullrange = TRUE) + 
  theme_bw()

#Call Plot
penguinPlotsouth

#Regression, Find the Slope
linearModelsouth <- lm(Volume~Decision*Year, data = penguin.south)

linearModelsouth

summary(linearModelsouth)



#Things to do next: some of the linear models from above that'll be affected by the discriminant change


penguin.A=penguin.data[penguin.data$Decision=="A",]
penguin.B=penguin.data[penguin.data$Decision=="B",]


penguin2A = aov(Volume~Location2 , data=penguin.A)
anova(penguin2A)
TukeyHSD(penguin2A, "Location", ordered=FALSE)

penguin2B = aov(Volume~Location2 , data=penguin.B)
anova(penguin2B)
TukeyHSD(penguin2B, "Location", ordered=FALSE)


#linear model of A eggs or B eggs by climate zone
penguin.Zone.A.lm = aov(Volume~Zone, data=penguin.A)
anova(penguin.Zone.A.lm)

penguin.Zone.B.lm = aov(Volume~Zone, data=penguin.B)
anova(penguin.Zone.B.lm)



#plot the A eggs and B eggs north and south separated on the same graph
shape1 <- c(21, 22)

#Matts Experimental plots
penguinPlot <- ggplot(penguin.data, aes(Year, Volume, color = Decision, shape = as.factor(Zone))) + 
  geom_point(aes(fill = Decision, size = 4)) +
  scale_shape_manual(values = shape1) +
  scale_fill_manual(values = c('red', 'green', 'blue')) +
  geom_smooth(method = 'lm', aes(linetype = Zone), size = 2, fullrange = TRUE) + 
  scale_linetype_manual(values = c(1, 2)) +
  theme_bw()


#Call Plot
penguinPlot

# Plot Year and Length
plot(penguin.data$Length~penguin.data$Year)


qplot(Year,Volume, data=penguin.a)
qplot(Year,Volume, data=penguin.b)
qplot(Year,Volume, data=penguin.u)
qplot(Year,Volume, data=penguin.Tristan)

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