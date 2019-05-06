##using fishers test deviation from null hypothesis can be calculated exactly.
#usually chisq statistic is applied to each cell in such cases but should be avoided when we 
#have small expectation value.
#assumptions: 1.sampling methd is simple random sampling. 2.variables under study 
# are categorical. 3.If sample data are displayed in a contingency table, the expected
#frequency count for each cell of the table is at least 5.
#H0=effects of medicine and placebo are independent.
tab2<- matrix(
  c(60,20,60,20),
  nrow = 2,
  dimnames = list( c("Med", "Plac"), c("ok", "ill"))
)
chisq.test(tab2)
#critical chise test statistic value for p=0.05 is 3.84 for df=1.
#chisq value=0 means that your observed value is equal to your expected value which is 
#highly unlikely in real life.  
#since our p value=1. 1 is not less than or equal to 0.05, our test is not significant.

tabp<- matrix(
  c(20,15,17,20),
  nrow = 2,
  dimnames = list( c("Med", "Plac"), c("ok", "ill"))
)
chisq.test(tabp)
##value obt is equal to the manually calculated value 0.51. 
##as the p value is greater than 0.05 significance level we 
#accept null hypothesis.



