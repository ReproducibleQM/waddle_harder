eggs.div<-read.csv(file="justdiv.csv", header=TRUE)
#note I changed the file path, just to make it all operate out of my downloads directory
eggs.div$year <- as.factor(eggs.div$year)

modeldiv<-glm(cbind(end_num, start_num)~year+month+site+crop_sys, offset=(1-control_eaten), family=binomial, data=eggs.div, na.action=na.exclude)
summary(modeldiv)
anova(modeldiv, test="Chisq")
summary(anova(modeldiv, test="Chisq"))

modeldiv.noyear<-glm(cbind(end_num, start_num)~month+site+crop_sys, offset=(1-control_eaten), family=binomial, data=eggs.div, na.action=na.exclude)
residual.vector.noyeardiv<-modeldiv.noyear$resid
modeldiv.nomonth<-glm(cbind(end_num, start_num)~year+site+crop_sys, offset=(1-control_eaten), family=binomial, data=eggs.div, na.action=na.exclude)
residual.vector.nomonthdiv<-modeldiv.nomonth$resid
modeldiv.nosite<-glm(cbind(end_num, start_num)~year+month+crop_sys, offset=(1-control_eaten), family=binomial, data=eggs.div, na.action=na.exclude)
residual.vector.nositediv<-modeldiv.nosite$resid
modeldiv.nocrop<-glm(cbind(end_num, start_num)~year+month+site, offset=(1-control_eaten), family=binomial, data=eggs.div, na.action=na.exclude)
residual.vector.nocropdiv<-modeldiv.nocrop$resid

#I think what was going on here is that the way this was coded, it didn't tell R explicitly where the factors 
#were coming from, and the error you were getting was because another data frame was attachedusing the attach()
#function at come point, so it was calling data that wasn't relevant to the test you were doing. I tweaked the code to make
#it explicit whic data the factors were coming from and it's working from my end now!
pairwise.t.test(residual.vector.noyeardiv, eggs.div$year, p.adj = "holm") #don't need to do a post-hoc here because there's only two levels, so if the model finds year significant, that's all you need
pairwise.t.test(residual.vector.nomonthdiv, eggs.div$month, p.adj = "holm")#month is already a factor so don't need to explicitly say it is
pairwise.t.test(residual.vector.nositediv, eggs.div$site, p.adj = "holm")
pairwise.t.test(residual.vector.nocropdiv, eggs.div$plot_trt, p.adj = "holm")
