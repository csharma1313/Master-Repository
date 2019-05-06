shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
bspl<-data.frame(shoesize,bodysize)
fmsq<-lm(bspl$bodysize~bspl$shoesize+ I(bspl$shoesize^2), data = bspl)

#------------------------
#aic of sqmodel
step(fmsq)
