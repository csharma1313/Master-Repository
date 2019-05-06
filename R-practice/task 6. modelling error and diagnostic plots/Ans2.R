shoesize <- c(38,38,39,39,40,40,41,41,42,42)
bodysize <- c(153,161,167,169,173,176,182,181,188,189)
y <- bodysize
bspl <- data.frame(shoesize,bodysize)
m <- lm(bodysize ~ shoesize , data = bspl)
summary(m)
#we got p1= 0.000421 for first coeff
#we got p2= 9.415997e-07  for second coeff
#in summary function also we get Pr(>|t|) for first coeff as 0.00042 and for second as 9.42e-07
#p values coincide

#diagnosis plot
plot(shoesize,residuals(m),main = "Shoesize vs residuals",xlab= "shoesize",ylab ="Residuals")
#we can observe that a linear regression line can fit in this graph

plot(fitted(m),residuals(m),main = "Fitted vs residuals",xlab= "Fitted",ylab ="Residuals")
#we see outlier in the bottom left

plot(fitted(m),y,main = "Fitted vs observed",xlab= "Fitted",ylab ="Observed")
#we obs values close to diagonal line 

hist(residuals(m))

qqnorm(resid(m), main ="Residual rankit plot")
abline(0,1)
#outlier appart clearly displayed

