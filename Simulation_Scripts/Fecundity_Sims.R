
### Simulation for latent fecundity model

library(tidyverse)
library(rstan)

N = 500 # sample size
lam = 5 # poisson rate for true whelks
p = .75 # detection probability
f = 125 # mean fecundity per whelk

true_whelks<-rpois(N,lam) # real whelk density

# let's make sure that the value isn't < 1
inds <- true_whelks < 1 
while(sum(inds) > 0){
  true_whelks[inds] <- rpois(sum(inds), lam)
  inds <- true_whelks < 1
}

whelks = rbinom(N, true_whelks, p)
cases = rpois(N, true_whelks*f)

stan_dat <- list(N = N, cases = cases, whelks = whelks)

mod <- stan(file = "Stan_Scripts/simple_fecundity.stan", data = stan_dat, 
             chains = 2, cores = 2)

