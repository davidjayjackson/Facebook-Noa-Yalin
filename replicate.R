rm(list=ls())
### Use leveneTest  function from car package
install.packages("car")
library(car)

## Moved Functiom to the top
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
n <- 100
alpha <- 0.05
x <- factor(c(rep("1",n),
              rep("2",n),
              rep("3",n),
              rep("4",n)))


## The replicate takes a couple of minutes to run.
res <-replicate(10000,aov_sim(x,n,alpha))
## Create TRUE/FALSE Summary Table
barplot(table(res))

summary(res)

# rowMeans(res)