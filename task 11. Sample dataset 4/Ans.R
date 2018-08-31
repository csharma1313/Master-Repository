photo<- read.delim('clipboard', header=T, dec = ".")
head(photo)
u<-photo$Voltage
r<-photo$Resistence
ur<-u/r
#plot of i vr u/r
p<-plot(photo$Current,ur, xlab = "current", ylab = "voltage/resistance", main="i vs u/i")
abline(0,1)
text(photo$Current~ur, labels = 1:nrow(photo), pos=1)
# removing 3 outliers top right
photo<-photo[-(1:3),]
#graph after outlier removal
u<-photo$Voltage
r<-photo$Resistence
ur<-u/r
p<-plot(photo$Current,ur, xlab = "current", ylab = "voltage/resistance", main="i vs u/i")
abline(0,1)

#task 2
#scatter plot of power vs resistence
plot(photo$Power,photo$Resistence, xlab = "power", ylab = "resistence", main = "power vs resistence")

#task 3
fm1<-lm(photo$Power~photo$Resistence+I(photo$Resistence*photo$Resistence), data = photo)
summary(fm1)
lines(photo$Resistence,fm1$coef[1]+fm1$coef[2]*photo$Resistence+fm1$coef[3]*photo$Resistence*photo$Resistence)
#coef1 is intercept, coef to is resistence, coef 3 is resist*resistence

#task 4
x<-seq(0,30,0.5)
x
plot(x^2 ~ x)
plot((x-15)^2*(x>15) ~ x)

#(R-15)^2*(R>15)? 
#fm1<-lm(photo$Power~photo$Resistence+I((photo$Resistence-15)^2*(photo$Resistence>15)), data = photo)
#plot(photo$Power,((photo$Resistence-15)^2*(photo$Resistence>15)), xlab = "power", ylab = "resistence", main = "power vs resistence")

#task 5
#P  ~ R + I(R*R) + I((R-15)^2*(R>15))
fm2<-lm(photo$Power~photo$Resistence+I(photo$Resistence*photo$Resistence)+I((photo$Resistence-15)^2*(photo$Resistence>15)), data = photo)
plot(photo$Power,photo$Resistence, xlab = "power", ylab = "resistence", main = "power vs resistence")
#plot(photo$Power,((photo$Resistence*photo$Resistence)+(photo$Resistence-15)^2*(photo$Resistence>15)), xlab = "power", ylab = "resistence", main = "power vs resistence")
lines(photo$Resistence,fm2$coef[1]+fm2$coef[2]*photo$Resistence+fm2$coef[3]*photo$Resistence*photo$Resistence+fm2$coef[4]*(photo$Resistence-15)^2*(photo$Resistence>15))

#task 6
#P  ~ R + I(R^2) + I(R^3) + I((R-15)^3*(R>15))		
fm3<-lm(photo$Power~photo$Resistence+I(photo$Resistence^2)+I(photo$Resistence^3)+I((photo$Resistence-15)^3*(photo$Resistence>15)))
summary(fm3)
plot(photo$Power,photo$Resistence, xlab = "power", ylab = "resistence", main = "power vs resistence")
lines(photo$Resistence,fm3$coef[1]+fm3$coef[2]*photo$Resistence+fm3$coef[3]*photo$Resistence*photo$Resistence+fm3$coef[4]*photo$Resistence*photo$Resistence*photo$Resistence+fm3$coef[5]*(photo$Resistence-15)^2*(photo$Resistence>15))

#task 7
fm4<-lm(exp(photo$Power)~log(photo$Resistence)+I(log(photo$Resistence)^2)+I(log(photo$Resistence)^3)+I((log(photo$Resistence)-15)^3*(log(photo$Resistence)>15)))
summary(fm4)
plot(exp(photo$Power),log(photo$Resistence), xlab = "power", ylab = "resistence", main = "power vs resistence")

#task 8
#residual analysis for model in task 5
fm5<-lm(photo$Power~photo$Resistence+I(photo$Resistence*photo$Resistence)+I((photo$Resistence-15)^2*(photo$Resistence>15)), data = photo)
summary(fm5)
plot(fitted(fm5), residuals(fm5), main = "fitted vs residuals")
qqnorm(residuals(fm5), main = "qq plot of residuals")
