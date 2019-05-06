shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
bspl<-data.frame(shoesize,bodysize)

formula1<-formula(bspl$bodysize~bspl$shoesize)
fm<-lm(formula1, data = bspl)
summary(fm)
fm$residuals
fm$fitted.values
#fitting log of bodyise
logformula<- formula(log(bspl$bodysize) ~ bspl$shoesize)
lfm<-lm(logformula,data=bspl)
summary(lfm)

lfm$fitted.values #not interpretable. use exp to interpret log value.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
exp(lfm$fitted.values)

lfm$residuals
exp(lfm$residuals)



plot(bspl$shoesize, bspl$bodysize)
points(bspl$shoesize,bspl$bodysize)
#plotting points of log formula model
abline(coef(fm),col="blue")
#drawing regrssion line for normal formula
points(bspl$shoesize,exp(fitted(lfm)), col="red")
#plotting points of log formula model
lines(formula= exp(fitted(lfm)) ~ shoesize,col="red")
#drawing regression line for logformula

#if in the formula we make formula [y~log(x)] then horizontL line passing below the second point set.
#if in the formula we make [lo(y)~log(x)] then no visible line.

#square formula model
sqfm<-lm(bspl$bodysize~bspl$shoesize+ I(bspl$shoesize^2), data = bspl)
summary(sqfm)

plot(shoesize,fitted(sqfm))
points(shoesize,bodysize)
points(shoesize, fitted(sqfm), col='blue')
lines(formula=fitted(sqfm)~shoesize, col='red')
abline(coef(fm),col='green')

