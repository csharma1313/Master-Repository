shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
bspl<-data.frame(shoesize,bodysize)
head(bspl)

#plotting scatter plot
#to open new window
dev.new()
#to divide new window opened in 3 sections
par(mfrow=c(3,1))
#to plot in first section
plot(bspl$shoesize, bspl$bodysize)

##line plot in second section
plot(bspl$shoesize, bspl$bodysize, type = "l")

#making formula
linearformula<- formula(bspl$bodysize ~ bspl$shoesize)
linearformula

#calculating coeff
basicmodel<-lm(linearformula,data=bspl)
#drwing regression line
abline(basicmodel, col ="purple", lty =2 ,lwd =2)
basicmodel
#when we increase shoesize by 1, bodysize increases by 7.65
#for the shoesize=0, bodysize comes out to be -132 cm.

#summary
summary(basicmodel)

#coef
coef(basicmodel)

##manual calculation of coefficients
ss<-matrix(c(1,1,1,1,1,1,1,1,1,1,38,38,39,39,40,40,41,41,42,42), nrow =10 ,ncol =2)
bs<-matrix(c(153,161,167,169,173,176,182,181,188,189), nrow = 10, ncol = 1 )
b <- solve( t(ss) %*% ss ) %*% t(ss) %*% bs
b

#fitted/predicted
plot(bspl$shoesize,bspl$bodysize)
ypred<-ss %*% b
lines(ss[,2],ypred, type = "o", col="red")

res<- bs-ypred
res

