shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
bspl<-data.frame(shoesize,bodysize)
sqfm<-lm(bspl$bodysize~bspl$shoesize+ I(bspl$shoesize^2), data = bspl)
summary(sqfm)
fm<- formula(bodysize~shoesize)

plot(shoesize,fitted(sqfm))
points(shoesize, fitted(sqfm), col='blue')
points(shoesize,bodysize)

lines(formula=fitted(sqfm)~shoesize, col='red')
abline(coef(fm),col='green')

#-----------------------------
#improving by scaling
fmscenter<-lm(bodysize~I(shoesize-40)+ I((shoesize-40)^2), data = bspl)
summary(fmscenter)
plot(shoesize,fitted(fmscenter))
lines(formula=fitted(fmscenter)~shoesize, col='green')
lines(formula=fitted(sqfm)~shoesize, col='red')
#----------------------------
#further improving by centering and scaling
fmcands<-lm(bodysize~I((shoesize-40)/(42-40))+ I((shoesize-40)^2/4), data = bspl)
summary(fmcands)
plot(shoesize,fitted(fmcands))
lines(formula=fitted(fmcands)~shoesize, col='green')
lines(formula=fitted(sqfm)~shoesize, col='red')
