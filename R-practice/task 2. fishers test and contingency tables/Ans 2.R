#null hypo= treatments do not affect outcomes. treatment and outcome are independent.
## using fishers test deviation from null hypothesis can be calculated exactly.
#usually chisq statistic is applied to each cell in such cases but should be avoided when we 
#have small expectation value.should be greater than 5.

#In an exact method you calculate the probability of each table and then sum the probability
#of the given table and every other "unusual" table. (The "unusual" tables are those that 
#have probabilities less than or equal to the given table.) If the total probability of such
#unusual tables is "small" --that is if it is rare to have observed such unlikely tables-- 
#we can reject the null hypothesis that the outcome is independent of the treatment.

install.packages("bdpv")
library("bdpv")
tab<- matrix(
  c(3,1,3,1),
  nrow = 2,
  dimnames = list( c("Med", "Plac"), c("ok", "ill"))
)
fisher.test(tab, alternative = "greater")
#p=0.78 is greater than common alpha levels so data is consistent with null hypo
#so there is no evidence that med/placebo affects in the resultant of person being 
#healthy or ill.
#lets say if the p value was equal to 0.001 then we could conclude that the treatment 
#was affecting patients.
prevalence1<-(6/8)
prevalence1
BDtest(tab, pr=0.1, conf.level = 0.95)
#sesitivity is 0.75. It is able to detect 75% of people with disease. it misses 25% of 
#people with disease.
#specificity=0.25. 25% people with negative results are truly negative. while 75% people 
#test positive for a disease they do not have. 
#PPV=0.1. among ppl who test positive 10% actually have disease
#npv=0.9. for those who test negative, 90% do not have disease. 

#lets say if  we use the computed prevalence value which is 0.75 instead of 0.1
BDtest(tab, pr=0.75, conf.level = 0.95)
# ppv values are increased. npv values are decreased.
BDtest(tab, pr=0.9, conf.level = 0.95)
## for higher number 20
tab2<- matrix(
  c(60,20,60,20),
  nrow = 2,
  dimnames = list( c("Med", "Plac"), c("ok", "ill"))
)
prevalence2<-(120/160)
prevalence2
#even if we increase values, prevalence remains same. if ratio is changed then 
#prevalence is changed.
fisher.test(tab2, alternative = "greater")
BDtest(tab2, pr=0.1, conf.level = 0.95)
#values remains same coz it is dependent on prevalence. 
#but if you use the prevalence we calculated
BDtest(tab2, pr=prevalence2, conf.level = 0.95)
#npv decreases and ppv increases. therefore better test. 
#odds rATIO IS HEALTHY:ill which is 3:1 for medicament. and same is for 3:1 for placebo. 
#ratio of these 2 ratios is 1 which means there is no difference which is significantly not correct. 
#if the 95% conf interval value is greater than one than we reject the null hypo.
#H0= 1 (accept)
#h0!=1 (reject)
#if odds ratio is far away from 1 then there will be a lot of difference between treatment and placebo and then the test will be significant.

tab2<- matrix(
  c(65,15,55,25),
  nrow = 2,
  dimnames = list( c("Med", "Plac"), c("ok", "ill"))
)
fisher.test(tab2, alternative = "greater")
BDtest(tab2, pr=0.3, conf.level = 0.95)

#when you increase the values on the diagonals difference in patients are cured by medicaments 
#and placebo.bdtest becomes significant.and we start to accept alternative hypo. 
#Specificity is a measure of your false positive rate 
#A high sensitivity test is reliable when its result is negative, 
#since it rarely misdiagnoses those who have the disease. A test with 100% sensitivity will 
#recognize all patients with the disease by testing positive. 