#task 1
#importing data
dat<-read.delim("clipboard", header = T, dec = ".")
head(dat)

#'blank' =bad
#'dots'= pretty good
#'stars'=good
#'more stars'= very good.
#' '1=bad, '.'0.1=90%, '*'0.05= 95%, '**'0.01=99%, '***'0.001=99.99%

#fitting linear model for heel-cracks

lmf1<-lm(dat$Heel.Cracks~dat$Power+dat$Time+dat$Pressure+dat$LoopValue)
summary(lmf1) #see word doc

#for this model none of the coefficients appear to be signinficant at 95% conf level
#intercept and power appear to be significant at 99% so that holds true for 95% as well
#as you can see the probabilities are less than 0.05
#as per the introductory text, the coefficients show a direct relationship that 
#increase in power, time, pressure and loop value will lead to increase in occurence of
#heel crack.

#fitting linear model for lift off
lmf2<-lm(dat$Liftoff~dat$Power+dat$Time+dat$Pressure+dat$LoopValue)
summary(lmf2) #see word doc
#intercept and power are significant at 99%and 99.99% respectively.
#but the coefficients of power and time are negative which cannot happen in a welding 
#process and can be acceptable only if the gradient of entire model is high enough to 
#compensate.

#fitting linear model for wire tear
lmf3<-lm(dat$WireTear~dat$Power+dat$Time+dat$Pressure+dat$LoopValue)
summary(lmf3)  #see word doc
#intercept is significant at 95% confidence level.
#pressure and loopvalue come out be be negative which is not possible, pressure can 
#still be compensated if gradient of model is high enough to bring intercept to 
#positive side but loop value still can never be negative.

#now plotting component plus residual plot
#usually a square term is introduced when a plot of residuals vs fitted is observed and 
#we see a center point alarm
#but since this plot is residuals vs components, i am hoping to see some kind of 
#patterns in the graph which bear a similarity to the plots of residulas vs fitted

#plots for lmf1. ALL PLOTS COPIED IN WORD DOC
plot(dat$Power, lmf1$residuals, main = 'power vs residuals in fn 1') #see word doc
#there is no observable pattern
plot(dat$Time, lmf1$residuals, main = 'time vs residuals in fn 1') #see word doc
#there is no observable pattern
plot(dat$Pressure, lmf1$residuals, main = 'pressure vs residuals in fn 1') #see word doc
#there is no observable pattern
plot(dat$LoopValue, lmf1$residuals, main = 'loop value vs residuals in fn 1') #see word doc
#there is no observable pattern

#plots for lmf2  ALL PLOTS COPIED IN WORD DOC
plot(dat$Power, lmf2$residuals, main = 'power vs residuals in fn 2') #see word doc
#there is no observable pattern
plot(dat$Time, lmf2$residuals, main = 'time vs residuals in fn 2') #see word doc
#******i see a certain ppoint in the top but i dont know it is enough to say we 
#need square term
plot(dat$Pressure, lmf2$residuals, main = 'pressure vs residuals in fn 2') #see word doc
#******i see a certain ppoint in the top but i dont know it is enough to say we 
#square term
plot(dat$LoopValue, lmf2$residuals, main = 'loop value vs residuals in fn 2') #see word doc

#there is no observable pattern

#plots for lmf3  ALL PLOTS COPIED IN WORD DOC
plot(dat$Power, lmf3$residuals, main = 'power vs residuals in fn 3') #see word doc
#i see a little center point alarm, square term needed
plot(dat$Time, lmf3$residuals, main = 'time vs residuals in fn 3') #see word doc
#i see a little center point alarm, square term needed
plot(dat$Pressure, lmf3$residuals, main = 'pressure vs residuals in fn 3') #see word doc
#i see a little center point alarm, square term needed
plot(dat$LoopValue, lmf3$residuals, main = 'loop value vs residuals in fn 3') #see word doc
#i see a little center point alarm, square term needed

#checking if intrducing square terms improves the model
s1<-lm(dat$WireTear~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Power^2))
plot(dat$Power, s1$residuals, main = 'power vs residuals in fn 3 with sq term introduced')
#better dispersion of residuals observed
#see word doc for comparison

s2<-lm(dat$WireTear~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Time^2))
plot(dat$Power, s2$residuals, main = ' time vs residuals in fn 3 with sq term introduced')
#better dispersion of residuals observed
#see word doc for comparison

s3<-lm(dat$WireTear~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Pressure^2))
plot(dat$Power, s3$residuals, main = 'pressure vs residuals in fn 3 with sq term introduced')
#better dispersion of residuals observed
#see word doc for comparison

s4<-lm(dat$WireTear~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$LoopValue^2))
plot(dat$Power, s4$residuals, main = 'pressure vs residuals in fn 3 with sq term introduced')
#better dispersion of residuals observed
#see word doc for comparison

#---------------------------------------------------------------------

#TASK 2
sqfm1<-lm(dat$Heel.Cracks~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Power^2)+I(dat$Time^2)+I(dat$Pressure^2)+I(dat$LoopValue^2))
summary(sqfm1) #see word doc
#none of the coefficients are significant

sqfm2<-lm(dat$Liftoff ~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Power^2)+I(dat$Time^2)+I(dat$Pressure^2)+I(dat$LoopValue^2))
summary(sqfm2) #see word doc
#both linear and square term for power are significant
#but the linear power term is negative but square power term is positive so it might be ok.

sqfm3<-lm(dat$WireTear ~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Power^2)+I(dat$Time^2)+I(dat$Pressure^2)+I(dat$LoopValue^2))
summary(sqfm3) #see word doc
#both linear and square term for power are significant

#plots of residuals vs fitted values for sqfm1
plot(sqfm1$fitted.values, sqfm1$residuals, main = 'residuals vs fitted for sqfm1')
#trumpet shape observed, transformation required

#plots of residuals vs fitted values for sqfm2
plot(sqfm2$fitted.values, sqfm2$residuals, main = 'residuals vs fitted for sqfm2')
#trumpet shape observed, transformation required

#plots of residuals vs fitted values for sqfm3
plot(sqfm3$fitted.values, sqfm3$residuals, main = 'residuals vs fitted for sqfm3')
#no distinct shape observed. no transformation needed

#applying transformation on sqfm1
tsqfm1<-lm(log(dat$Heel.Cracks)~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Power^2)+I(dat$Time^2)+I(dat$Pressure^2)+I(dat$LoopValue^2))
#error obtained - Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#NA/NaN/Inf in 'y'
#log(0) in data set so we get error since the y variable has 0 in data.

#applying transformation on sqfm2
tsqfm2<-lm(log(dat$Liftoff)~dat$Power+dat$Time+dat$Pressure+dat$LoopValue+I(dat$Power^2)+I(dat$Time^2)+I(dat$Pressure^2)+I(dat$LoopValue^2), na.action = omit())
#error obtained - Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#NA/NaN/Inf in 'y'
#log(0) in data set so we get error since the y variable has 0 in data.

#-------------------------------------------------------------
#TASK 4
#we assume null hypothesis= power does not affect occurence of heel crack
#cutting point s=0.2. to view contigency tables see word doc
tab1<-matrix(c(12,0,3,6), ncol=2, byrow = T, dimnames = list(c("low", "high"), c("low", "high")))
fisher.test(tab, alternative = "greater")
# p value is less. reject null hypothesis
#power affects heel cracks
#sensitivity
sn<-(12/(12+3))
sn
#specificity
sp<-(6/(6+0))
sp
#install.packages("bdpv")
library("bdpv")
BDtest(tab1, pr=0.71, conf.level = 0.95)
#sensitivity and specificity are obtained as manually calculated. 

#changing cutting point to s=0.1
tab2<-matrix(c(11,1,0,9), ncol=2, byrow = T, dimnames = list(c("low", "high"), c("low", "high")))
BDtest(tab2, pr=0.52, conf.level = 0.95)

#changing cutting point to s=0.3
tab3<-matrix(c(12,0,4,5), ncol=2, byrow = T, dimnames = list(c("low", "high"), c("low", "high")))
BDtest(tab3, pr=0.76, conf.level = 0.95)

#changing cutting point to s=0.4
tab4<-matrix(c(12,0,7,2), ncol=2, byrow = T, dimnames = list(c("low", "high"), c("low", "high")))
BDtest(tab4, pr=0.90, conf.level = 0.95)

#for table of cutting points with sensitivity and specificity see word doc
y<-c(1,0.8,0.75,0.63)
x<-c(0.1,0,0,0)
plot(x,y,main = 'roc curve', xlab = '1-specificity', ylab = 'sensitivity', type = "b")
abline(0.630,3.7)
#the roc curve shows a significant deviation from the 45 degree line. 
#which implies that there is a good tradeoff between sensitivity and specificity
#any increase in sensitivity will be accompanied by a decrease in specificity
