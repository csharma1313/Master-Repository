#null hypothesis= equality of means
#wash<-read.delim("clipboard", dec = " , ")
#print(wash)
#t.test(wash[[Probe.1]], wash[[Probe.2]????????])
#t.test(x, y, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,)

# we must first perform Fischer's F test to check verify the homogeniety of variances.
#f test to check whether group means are equal or not.
x<-c(201,138,132,117,177,168,178,104)
y<-c(127,60,79,63,105,82,57,72)
var.test(x,y)
#we obtain p value greater than 0.05 so 2 variances are homogeneous.
#also under ideal conditions the f value equals one and our is near 1 so we can also state 
#that means are approximately similar.
#we accept null hypothesis that equality of means is established.
#comparing value of F obtained with tabulated value of F for alpha=0.05, df=7, 
#df of denominator=7
qf(0.95,7,7)
# the value of F we computed(1.96) is less than tabulated (3.78) so accept null hypothesis of 
#homogeneity of variances.

#we use f test one tail when the variances of samples have an unequality relationship. if the
#relationship is strictly in terms of equal or not equal to theen we use 2 tail test. 
# F distribution has 1 tail but t distri has 2 tails, so conf level of 95% => p=0.975 
#95% confidence interval has 2 critical values: 0.392, 9.804
#rejection region if (F<crit1) + (F> crit2): 1.9 is not less than 0.39 and 1.9 is not greater than 9.80.
#so we accept null hypo
#F dist we have taken one tail coz var of one sample is greater than the other. 
#on 95% conf interval, 2.5% is on one side and 2.5% is on other side. but computing both to one side we take 
#95+2.5=97.5%.

#t test is done when var are not known. right now we did f test to check variances
#now we are supporting our observation by assuming that we dont know the variances and doing t test
#doing t test to check the difference between the samples when the variances of two normal distributions 
#are not known.

t.test(x,y,var.equal = TRUE, paired = FALSE)
# we obtain p value less than 0.05, therefore avgs of two groups are significantly similar
#The t score is a ratio between the difference between two groups and the difference within the groups. 
#The larger the t score, the more difference there is between groups. 
#The smaller the t score, the more similarity there is between groups. 
#http://www.statisticshowto.com/t-test/ 
qt(0.975,14)
#observe that p value is very less. so there is very less chance that the data has occur by error.
#so we can say that two groups are similar
#this supports are obs from f test done above.
#http://www.itl.nist.gov/div898/handbook/eda/section3/eda359.htm 

