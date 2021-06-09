shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
bspl<-data.frame(shoesize,bodysize)
fm<- formula(bodysize~shoesize)
plot(shoesize,bodysize)
points(shoesize, fitted(fm), col='green')
abline(coef(fm), col='red')


#---------------------------------------------------------------
#centering

fm_s=lm(bodysize~I(shoesize-40), data=bspl)
fm_s

#-----------------------------------------------------------------
#min max scaling
fm_sc=lm(bodysize~I((shoesize-40)/(42-40)),data = bspl)
fm_sc

#----------------------------------------------------------------
#unit variance scaling which is not possible in this case coz we know data is structured. 
m=mean(shoesize)
s=sd(shoesize)
fm_uv=lm(bodysize~I((shoesize-m)/s),data = bspl)
fm_uv
