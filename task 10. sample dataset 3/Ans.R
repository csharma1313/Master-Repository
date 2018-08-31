a<-read.delim('clipboard', header = T, dec = '.')
head(a)
#1-------------------------------------------------
attach(a)
plot(Lymphos~alter, main='lymphos vs alter', xlab='alter', ylab ='lymphos' )
#we can observe that with the increase in age the lymphos count decrease as a general 
#pattern.
plot(Lymphos~sex, main='lymphos vs sex', xlab='sex', ylab ='lymphos' )
# the lymphos count in men is higher than women during the entire lifecycle. 
#we can observe one reading at top right for women which can be ruled out as an outlier.
plot(Lymphos~blutgr, main='lymphos vs blutgr', xlab='blutgr', ylab ='lymphos' )
#data seems a little abnormal as not all tuples have been assigned with a blood group
#subjects with blood group B showed higher lymphos levelthan that of A and AB.
#AB shows the lowest lymphos count.
detach(a)
#2-------------------------------------------
attach(a)
#y = f(t) = c + b0 . (1 - exp(-t)) 
B<-lm(Lymphos~I(1-exp(-(alter-250)/250)))
summary(B)      
#we get intercept and linear terms highly significant at 99.99%
#we get the linear term as negative which is good in this case.
#if we increase age by 1, lymphos count decrease by 15 which can be considered ok since
#in the graph we observed that with age lymphos are decreasing.
# r square value comes out to be 0.47 which is not so good. the model correctly explains 
#only ~40% of data.
#since the data we have is a litlle large the r square value is expaining the fit a 
#little bit precisely as small data set does not does not provide precise estimate
#of strength of relationship between response and predictors.
#RSE is less which is good. 
# we also have a high f statistic value which tells if there is relationship between 
#predictor and response variable. 
#adj r square value comes close to r sq value. 
# overall the model is good.
plot(Lymphos~alter, main='measured y vs t', xlab='alter', ylab ='lymphos' )
lines(alter, B$coef[1]+B$coef[2]*(1-exp(-(alter-250)/250)), col='green')
#like i said before, model is good and fits data
#for better observation look at the graph below
plot(fitted(B)~alter, main='measured y vs t', xlab='alter', ylab ='lymphos' )
lines(alter, B$coef[1]+B$coef[2]*(1-exp(-(alter-250)/250)), col='green')

boxplot(B$residuals, main='residuals in boxplot')
#i see two readings at the top which are outliers
#residuals range between -25 to 25 and the median value is 0.
plot(B$residuals~B$fitted.values, main='residuals vs fitted', xlab='predictions', ylab = 'residuals')
# a little trumpet shape is observed, a transformation of y varisable might make model
#fit better
hist(B$residuals)
#residuals appaear to be normally distributed around mean with 0. It is a good indication.
qqnorm(B$residuals)
qqline(B$residuals)
detach(a)
#3-----------------------------------------------
attach(a)
O<-lm(log(Lymphos)~I((sqrt(alter)-mean(sqrt(alter)))/sd(sqrt(alter))), data = a)
summary(O)
plot(Lymphos~alter)
lines(alter, exp(O$coef[1]+O$coef[2]*(sqrt(alter)-mean(sqrt(alter)))/sd(sqrt(alter))), col='red')
boxplot(O$residuals)
#outliers spotted at the bottom. range between -0.5 to 0.6 with mean around 0.1
plot(O$residuals~O$fitted.values)
#randomly distributed. no trumpet shape obs
hist(O$residuals)
#not normally distributed. right skewed
qqnorm(O$residuals)
qqline(O$residuals)
#little skewness obs here as well.  
#-----------------------------------------------
#would use the second model for further analysis coy coef values are better and 
#also higher r sq value as compared to B model. F statistic is also good.
#but the most important factor is RSE which is very low.
#5------------------------------------------------
X<-lm(log(Lymphos)~I((sqrt(alter)-mean(sqrt(alter)))/sd(sqrt(alter)))+sex+blutgr, data = a)
summary(X)
step(X)
#when formula contains both blutgr and sex values absolute aic value comes smaller
#and in that too when sqrt alter term is excluded we get the best aic value that is 218
#which means that sex facotr is significant to the model



###havnt done 14,15,16