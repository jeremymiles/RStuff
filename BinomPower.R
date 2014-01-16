

#Code to simulate power for a binomial test, comparing 
#sample proportion to a fixed population proportion


null <- 0.02  #null hypothesis
hyp <- 0.01   #Hypothesized proportion in population
n <- 1400    #sample size
nReps <- 10000  #number of simulations to run
alpha <- 0.05  #alpha

EHPowerCalc <- function (null, hyp, n) {
  nDeaths <-   sum(sample(c(rep(1, hyp*n ), rep(0, (1-hyp)*n)), 
                          size=n, replace=TRUE))
  nDeaths
  return ( binom.test(nDeaths, n, p=null)$p.value )
  
}
mean(replicate(nReps, EHPowerCalc(null=null, hyp=hyp, n=n)) < alpha)
