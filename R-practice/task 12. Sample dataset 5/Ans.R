#import<-read.csv("C:\\Users\\sharm\\Google Drive\\M Sc\\sem 2\\data mining\\ws16\\catalyst_temps.csv",header = TRUE, sep = ";", dec = ",", fill = T )
#import

catalyst_data<-read.csv("D:\\M Sc\\sem 2\\data mining\\exams done\\sneha ss16\\data.csv",header = TRUE, sep = ";", dec = ",", fill = T )
head(catalyst_data)

import<-catalyst_data
a1<-catalyst_data
#a1<-import[-c(1:12),]
head(a1)
plot(a1$InletTemp,a1$OutletTemp)
# 3 outliers spotted. 1 in lower section between 200 ansd 300 and 2 spotted 
#to the right between 500 to 600
#plot(a1$NH3conc,a1$OutletTemp)
#removing outliers ------------------------------
text(a1$OutletTemp~a1$InletTemp,labels=1:nrow(import),pos=2)
a1<-a1[-c(105,190,249),]
plot(a1$InletTemp,a1$OutletTemp)
text(a1$OutletTemp~a1$InletTemp,labels=1:nrow(import),pos=2)

lm01<-lm(a1$OutletTemp~a1$InletTemp+a1$NH3conc)
summary(lm01)
#inlettremp is very significant ~99.9% rest other coefficients do not make much sense.
#if we increase inlettemp by 1 then outlettemp by 1.22
#the term nh3conc does not affect the model anyhow
plot(lm01$residuals~lm01$fitted.values)
abline(0,0)
#a transformation of the y var is not needed. no distinguishable trumpet shaped obs
plot(lm01$residuals+lm01$fitted.values, lm01$fitted.values)
abline(0,1)
#the points are scattered around 45' line. 
#some outliers are observed

qqnorm(lm01$residuals)
qqline(lm01$residuals)
#data is normally distributed except for some outlying points which are observed in the upper right corner

#---------------------------------------------
plot(a1$InletTemp,a1$OutletTemp)
boxplot(a1$InletTemp, main="BoxPlot", xlab="InletTemp")
plot(a1$OutletTemp~a1$InletTemp+a1$NH3conc)
highd<-a1[a1$InletTemp>400,]
plot(highd$InletTemp,highd$OutletTemp)
#----------------------------------------------
lm02<-lm(highd$OutletTemp~highd$InletTemp+I(highd$InletTemp^2))
summary(lm02)
#no significant coefficients.
#rse is high while r sq is low so we do scaling
#both linear and sq terms have negligle effect on y var
#lm02 <- lm( highd$OutletTemp ~ scale(highd$InletTemp) + I(scale(highd$InletTemp)^2), data=highd)
cen<-mean(highd$InletTemp)
cen
lm02<-lm(highd$OutletTemp~I(highd$InletTemp-cen)+I((highd$InletTemp-cen)^2), data = highd)
summary(lm02)
#coeffs are highly significant ~99.9%
#rse high
#sq term does not have significant impact on y var
#-----------------------------------------------------
plot(a1$OutletTemp~a1$InletTemp, col="orange")
lines(lm02$fitted.values~highd$InletTemp, col="blue")
lines(lm01$fitted.values~a1$InletTemp, col="red")
#-----------------------------------------------------
#cubic spilnes
#one knot and no boundary conditions: 5 columns in the X-matrix,
#1, x, x^2, x^3, I(350,inf)*(x-350)^3
h1<-function(x) {x}
h2<-function(x) {x^2}
h3<-function(x) {x^3}
h4x<-function(x) {if(x>350) (x-350)^3 else 0}
h4<-function(v) {sapply(v,h4x)}
lm03<-lm(a1$OutletTemp~h1(a1$InletTemp)+h2(a1$InletTemp)+h3(a1$InletTemp)+h4(a1$InletTemp))
summary(lm03)
lines(seq(100,700,by=1),lm03$coef[1]+lm03$coef[2]*seq(100,700,by=1)+lm03$coef[3]*h2(seq(100,700,by=1))+lm03$coef[4]*h3(seq(100,700,by=1))+lm03$coef[5]*h4(seq(100,700,by=1)),col="green")
