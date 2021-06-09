l1<-c(1,1,1,1)
x1<-c(-1,1,-1,1)
x2<-c(-1,-1,1,1)
x<-matrix(c(l1,x1,x2), ncol = 3)
y<- matrix(c(3,5,7,11), ncol = 1)
dat<-data.frame((x1+x2),y)
#manual calculation of coefficients
b<-solve((t(x)%*%x)) %*% t(x) %*% y
b

#making model
fm<- lm(y~(x1+x2), data = dat)
#finding coefficients
coef(fm)
# predicting y values
ypred<-fitted.values(fm)
ypred
#calculcating residuals
res<-y-ypred
res
#-----------------
#for interaction model
dat<-data.frame(x,y)
ifm=lm(y~x1+x2+I(x1*x2),data=dat)
coef(ifm)

#-----------------
x1<-c(20,40,20,40)
x2<-c(20,20,30,30)
y<- matrix(c(3,5,7,11), ncol = 1)
dat<-data.frame((x1+x2),y)
#making model
fm<- lm(y~(x1+x2), data = dat)
#finding coefficients
coef(fm)
# predicting y values
ypred<-fitted.values(fm)
ypred
#calculcating residuals
res2<-y-ypred
res2
#coefficients change but ypred and residuals remain the same. why so...

