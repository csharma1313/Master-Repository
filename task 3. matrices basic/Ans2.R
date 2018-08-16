i<-matrix(c(1,0,0,1), nrow = 2)
vec<- matrix(c(2,5), ncol = 1)
ivec<- i %*% vec
ivec
# resultant remains the same as vector

mat<- matrix(c(7,3,9,11,2,6,8,10), nrow = 2)
imat <- i %*% mat
imat
#resultant matrix remains the same.

b1<- matrix(c(1,0), ncol = 1)
bi1 <- i %*% b1
bi1
#resultant remains same as base vector

b2<- matrix(c(0,1), ncol = 1)
bi2 <- i %*% b2
bi2
