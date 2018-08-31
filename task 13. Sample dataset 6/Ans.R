#odat<-read.delim('clipboard', dec = ';')
odat<-read.csv("D:\\M Sc\\sem 2\\data mining\\navneet supplymentarz 16\\oilData.csv", header=T, sep=';', dec=',', fill=T)
head(odat)
catalyst_data<-odat[-101,]
head(catalyst_data)
#head function just displays the start of data set to see the ending data rows use tail
tail(catalyst_data)
#it shows that the values of row 130 are now appearing at row number 129 which means the 
#row was deleted.
qqnorm(odat$OilCons)
qqline(odat$OilCons)

text(qqnorm(odat$OilCons), labels = 1:nrow(odat),pos = 2)



odat<- odat[-29,]
qqnorm(odat$OilCons)
text(qqnorm(odat$OilCons),labels=1:nrow(odat),pos=2) #cant properly see with pos=2
text(qqnorm(odat$OilCons),labels=1:nrow(odat),pos=4) #we see a slightly out of line 29
#we cannot observe 29 at the top as an outlier just by looking at the qqplot. 
#the data is not normally distributed. 

#------------------------------------------------------------
X<-odat[,-1]
head(X)
Y<-as.matrix(odat[,1,drop=FALSE])
#maccel<-mean(X[,1])
#maccel
X<-scale(Y)
head(X)
#maccel<-mean(X[,1])
#maccel
apply(X,2,mean) #
X<-cbind(1,X) 
head(X)
#-----------------------------------------------------------
xt<-t(X)
#x<-as.matrix(x)
xtx<-xt %*%X
xinv<-solve(xtx)
xinfo<-diag(xinv)
xinfo #these determine the size of confidence intervals for coefficients
### so one can read off, which coeffients are the most reliable.
#-------------------------------------------------------------
hat<- X %*% xinv %*% xt
hat
dhat<-diag(hat)
dhat
plot(dhat)
text(diag(hat),labels=1:nrow(hat),pos=2)
#we can see that 29 now has high leverage
#-------------------------------------------------------------
omodel<-lm(odat$OilCons~X)
summary(omodel)
#rse is less which is good. 
#r sq should be near 100. adj r should be nearly less than r sq
#negative coeffs are there which is illogical
plot(omodel$res, omodel$f)
abline(0,0)
#no observed trumpet shape cannot determine to transform y variable
plot(omodel$res+omodel$f,omodel$f)
abline(0,1)
#data somewhat behaves but there are some outliers
qqnorm(omodel$residuals)
qqline(omodel$residuals)
#residuals are somewhat normally distributed but some outliers are spotted towards the lower left side
#--------------------------------------------------------------
odat<-odat[-c(29,30,111),]
head(odat)
plot(omodel$res, omodel$f)
abline(0,0) ##graph same as before

plot(omodel$residuals+omodel$fitted.values, omodel$fitted.values)
abline(0,1)#graph same as before

qqnorm(omodel$residuals)
qqline(omodel$residuals)
#no changes.
#--------------------------------------------------------------
head(X)
X2<-X[seq(2,nrow(X),2),-1]
head(X2)
head(Y)
Y2<-Y[seq(2,nrow(X),2)]
head(Y2)



