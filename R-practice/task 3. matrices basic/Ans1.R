shopping_list<-matrix(c(6,4,5,5,3,7,7,8,3,5,3,6),nrow = 4,
                      dimnames = list(names = c("Fred", "Jinny", "Lizzy", "Joe"),
                                      fruits = c("Apples", "Pears", "Bananas")))

pri<- matrix(c(0.5,0.4,0.35,0.6,0.8,0.65,0.3,0.2,0.25), nrow=3,
             dimnames = list(fruits=c("Apples", "Pears", "Bananas"), 
                             shops= c("shop1","shop2", "shop3")))

shopping_list %*% pri
