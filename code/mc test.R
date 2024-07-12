# H0 = the distrubtion S is uniquely determined
#survival funciton G[x]=p[s(X(n)>=x|H0]
#step:
#1 compute statistics S0
#2 generate N i.i.d. replications S1,..., SN of the statistics S under H0
#3 Using S0 and replications compute p-value where it is the randomized sample rank of S0
#4check if p<= alpha

#Demonstration of how to use MaxMC::mc
#generate x ~poisson(10).y ~Poisson(10)
x <- rpois (8, lambda = 10)
y <- rpois (8, lambda = 10)
data <- list (x=x, y=y)
# because z, y are random generated -> we use ks statistics
#apply the test statistics
ks.test(data$x, data$y)
#set statistics and DGP function
#statistic is the test statistc we use in this case
#suppressWarnings only use to ignore warning message
statistic <- function(data){
  out <- suppressWarnings(ks.test(data$x, data$y))
  return(out$statistic)
}
# dgp is the simulation of y, default is sample(y, replace = TRUE)
dgp <- function(data){
  perm <- sample (c(data$x, data$y))
  x<- perm[1:length(data$x)]
  y<- perm[-(1:length(data$x))]
  return(list(x=x, y=y))
}
#install.packages("MaxMC")
library("MaxMC")
mc(y=data, statistic=statistic, dgp=dgp, N=999, type = "absolute")


