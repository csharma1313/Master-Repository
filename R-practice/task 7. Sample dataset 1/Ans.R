a<-read.delim("clipboard", dec=".",header = T)
a
a<-a[-94,]
a
m1=lm(a$age~a$Leukos+a$Lymphos+a$CD14p+a$CD3p+a$CD4p+a$CD56p+a$CD8p+a$CD19p,data=a)
summary(m1)
plot(residuals(m1))
plot(fitted(m1),residuals(m1), main = "residuals vs fitted",xlab= "Fitted",ylab ="Residuals")
plot(lm(a$age~a$Leukos+a$Lymphos+a$CD14p+a$CD3p+a$CD4p+a$CD56p+a$CD8p+a$CD19p,data=a))
# a few outliers can be spotted on the top and 1or 2 in the bottom
hist(resid(m1))
#yes we can say that the residuals are normally distributed though a little left skew 
#in the graph can be obs
pdata<-read.delim("clipboard", dec=",") #reading prediction data table values
pdata
b1<-as.matrix(pdata)
xr<-nrow(b1)
xr
xc<-ncol(b1)
xc
b1<-t(b1)
b0<-as.matrix(coefficients(m1)[1])
b0
coefs<-as.matrix(coefficients(m1)[-1])
coefs
yr<-nrow(coefs)
yr
yc<-ncol(coefs)
yc
coefs<-t(coefs)
ypred<-(coefs%*%b1)
ypred
#predicted ages

#--------------------------------------------------------------------------
#exercise 2
m2<-lm(a$age~log(a$Leukos)+log(a$Lymphos)+log(a$CD14p)+log(a$CD3p)+log(a$CD4p)+log(a$CD56p)+log(a$CD8p)+log(a$CD19p),data=a)
summary(m2)
plot(fitted(m2), residuals(m2))
abline(0,0)
hist(residuals(m2))
#not exactly normally distributed. left skew still observed
co<-as.matrix(coefficients(m2)[-1])
co
matdata<-as.matrix(a)
yp<-matdata%*%co
