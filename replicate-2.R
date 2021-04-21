rm(list=ls())
n <- 100
alpha <- 0.05
x <- factor(c(rep("1",n),
              rep("2",n),
              rep("3",n),
              rep("4",n)))
aov_sim <- function(x,n,alpha){
  y <- c(rchisq(n,4000),
         rchisq(n,4000),
         rchisq(n,4000),
         rchisq(n,4000))
  mod <- leveneTest(y~x,center=mean)
  mod1 <- bartlett.test(y~x)
  return(c(mod$p.value<alpha,mod1$p.value<alpha))
}
set.seed(1)
res <-replicate(10000,aov_sim(x,n,alpha))
rowMeans(res)