shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
cnt<-length(shoesize)
one=NULL
for(i in 1:cnt)
  one[i]=1
x<-cbind(one,shoesize)
x
xt<-t(x)
xt
xtx<-xt %*% x
xtx
xinv<-solve(xtx)
xinv
xty<-xt %*% bodysize
xty
b=xinv %*% xty
b
ypred<- x%*% xinv %*% xty
ypred
ssres<- sum((ypred-bodysize)^2)
ms_res<-ssres / 8
rse<-sqrt(ms_res)
rse
stde<-sqrt(diag(xinv))*rse
stde
#calc t values
t1<-b[1]/stde[1]
t1
t2<-b[2]/stde[2]
t2
#calc p values
#lower tail= true liya cz humari t1 value negative aayi thi which is on the left side 
#of the graph
p1.1<- pt(t1,8, lower.tail = TRUE)
p1<-2*p1.1
p1
#lower.tail=false liya coz humari value positive right side pe h
p2<- pt(t2,8, lower.tail = FALSE)
p2<-p2*2
p2

