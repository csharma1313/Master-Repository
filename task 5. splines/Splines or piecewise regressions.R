shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
indicatorData= data.frame(bodysize, shoesize)
indicatorData
I1= function(x) { if(x>37.5 && x<=39.5) 1 else 0}
I2= function(x) { if(x>39.5 && x<=41.5) 1 else 0}
I3= function(x) { if(x>41.5 && x<=43.0) 1 else 0}
i1=apply(as.matrix(indicatorData$shoesize),1,I1)
i2=apply(as.matrix(indicatorData$shoesize),1,I2)
i3=apply(as.matrix(indicatorData$shoesize),1,I3)

#newX=matrix(c(i1, i2, i3), nrow = 10, ncol = 3, byrow = FALSE, dimnames = list(c(), c("x1", "x2", "x3")))
#newX

newdataframe= data.frame(bodysize, i1, i2, i3)
newdataframe


piecewisemodel=lm(formula = bodysize ~ 0+i1+i2+i3, data = newdataframe)
summary(piecewisemodel)
fitted(piecewisemodel)
coefficients(piecewisemodel)

plot(shoesize, bodysize, type = "p" ,col="red", main="constant function")
lines(shoesize, fitted(piecewisemodel), col="blue")


#     *****piecewise linear function ******
shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
indicatorData= data.frame(bodysize, shoesize)
indicatorData

h1= function(x) {if(x>37.5 && x<=39.5) 1 else 0 }
h2= function(x) {if(x>37.5 && x<= 39.5) x*1 else x*0}
h3= function(x) { if(x>39.5 && x<=41.5) 1 else 0}
h4= function(x) { if(x>39.5 && x<=41.5) x*1 else x*0}
h5= function(x) { if(x>41.5 && x<=43.5) 1 else 0}
h6= function(x) { if(x>41.5 && x<=43.5) x*1 else x*0}

x1= apply(as.matrix(indicatorData$shoesize), 1, h1)
x2= apply(as.matrix(indicatorData$shoesize), 1, h2)
x3= apply(as.matrix(indicatorData$shoesize), 1, h3)
x4= apply(as.matrix(indicatorData$shoesize), 1, h4)
x5= apply(as.matrix(indicatorData$shoesize), 1, h5)
x6= apply(as.matrix(indicatorData$shoesize), 1, h6)
x1;x2;x3;x4;x5;x6

newdataframe2= data.frame(bodysize, x1,x2,x3,x4,x5,x6)
newdataframe2

piecewiselinearmodel= lm(formula = bodysize ~ 0+x1+x2+x3+x4+x5+x6, data = newdataframe2)
summary(piecewiselinearmodel)
fitted(piecewiselinearmodel) 

plot(shoesize, bodysize, main="linear model")
lines(shoesize, fitted(piecewiselinearmodel), col="blue")



### ******* piecewise liner continous functions *******
shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
indicatorData= data.frame(bodysize, shoesize)
indicatorData

h0= function(x) {1}
h1= function(x) {x}
h2= function(x) { if(x>39.5) (x-39.5) else 0}
h3= function(x) { if(x>41.5) (x-41.5) else 0}

x0= apply(as.matrix(indicatorData$shoesize), 1, h0)
x1= apply(as.matrix(indicatorData$shoesize), 1, h1)
x2= apply(as.matrix(indicatorData$shoesize), 1, h2)
x3= apply(as.matrix(indicatorData$shoesize), 1, h3)

newdataframe3= data.frame(bodysize, x0,x1,x2,x3)
newdataframe3

continueslinearmodel= lm( formula = bodysize ~ x0+x1+x2+x3, data = newdataframe3)
continueslinearmodel
summary(continueslinearmodel)

plot(shoesize, bodysize, main="continues linear model")
lines(shoesize, fitted(continueslinearmodel), col="blue")


### **** piecewiese squat function ******
shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
indicatorData= data.frame(bodysize, shoesize)
indicatorData

h1= function(x) {if(x>37.5 && x<=39.5) 1 else 0 }
h2= function(x) {if(x>37.5 && x<= 39.5) x*1 else x*0}
h3= function(x) {if(x>37.5 && x<= 39.5) x^2 else 0}
h4= function(x) {if(x>39.5 && x<=41.5) 1 else 0}
h5= function(x) {if(x>39.5 && x<=41.5) x*1 else x*0}
h6= function(x) {if(x>39.5 && x<=41.5) x^2 else 0}
h7= function(x) { if(x>41.5 && x<=43.5) 1 else 0}
h8= function(x) { if(x>41.5 && x<=43.5) x*1 else x*0}
h9= function(x) { if(x>41.5 && x<= 43.5) x^2 else 0}

x1= apply(as.matrix(indicatorData$shoesize), 1, h1)
x2= apply(as.matrix(indicatorData$shoesize), 1, h2)
x3= apply(as.matrix(indicatorData$shoesize), 1, h3)
x4= apply(as.matrix(indicatorData$shoesize), 1, h4)
x5= apply(as.matrix(indicatorData$shoesize), 1, h5)
x6= apply(as.matrix(indicatorData$shoesize), 1, h6)
x7= apply(as.matrix(indicatorData$shoesize), 1, h7)
x8= apply(as.matrix(indicatorData$shoesize), 1, h8)
x9= apply(as.matrix(indicatorData$shoesize), 1, h9)

newdataframe4= data.frame(bodysize, x1,x2,x3,x4,x5,x6,x7,x8,x9)
newdataframe4

squarmodel= lm(formula = bodysize ~0+x1+x2+x3+x4+x5+x6+x7+x8+x9, data = newdataframe4)
squarmodel
summary(squarmodel)
fitted(squarmodel)
plot(shoesize, bodysize, main=" square model")
lines(shoesize, fitted(squarmodel), col="blue")


### ******* piecewise liner continous functions *******
shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
indicatorData= data.frame(bodysize, shoesize)
indicatorData

h0= function(x) {1}
h1= function(x) {x}
h2= function(x) { x^2}
h3= function(x) { if(x>39.5) (x-39.5)^2 else 0}
h4= function(x) { if(x>41.5) (x-41.5)^2 else 0}

x0= apply(as.matrix(indicatorData$shoesize), 1, h0)
x1= apply(as.matrix(indicatorData$shoesize), 1, h1)
x2= apply(as.matrix(indicatorData$shoesize), 1, h2)
x3= apply(as.matrix(indicatorData$shoesize), 1, h3)
x4= apply(as.matrix(indicatorData$shoesize), 1, h4)

newdataframe5= data.frame(bodysize, x0,x1,x2,x3,x4)
newdataframe5

continuessquarmodel= lm(formula = bodysize ~ 0+x0+x1+x2+x3+x4, data = newdataframe5)
continuessquarmodel
summary(continuessquarmodel)
fitted(continuessquarmodel)

plot(shoesize, bodysize, main="continues square model")
lines(shoesize, fitted(continuessquarmodel), col="blue")
      