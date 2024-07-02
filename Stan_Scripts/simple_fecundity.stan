
data {
  int<lower=0> N; // number of egg masses
  int cases[N]; // number of egg cases in a mass
  int whelks[N]; // number of whelks in a photo
}

parameters {
  real<lower=0> lam; // whelk occurrence rate
  real<lower=0,upper=1> p; // detection probability
  real<lower=0> f; // mean fecundity
}

model {
  // prior
  lam ~ normal(1, 5);
  p ~ beta(2, 1.5);
  f ~ normal(200, 100);
  
  // likelihood
  real lp[N,50];
  for(i in 1:N){
    if(whelks[i] == 0){
      for(j in (whelks[i]+1):50){
        lp[i, j] = poisson_lpmf(j | lam) + 
        binomial_lpmf(whelks[i] | j, p) + 
        neg_binomial_2_lpmf(cases[i] | j * f, 1);
        }
        target += log_sum_exp(lp[i,(whelks[i]+1):50]);
    }
    else{
      for(j in whelks[i]:50){
        lp[i, j] = poisson_lpmf(j | lam) + 
        binomial_lpmf(whelks[i] | j, p) + 
        neg_binomial_2_lpmf(cases[i] | j * f, 1);
        }
        target += log_sum_exp(lp[i,whelks[i]:50]);
        }
  }
}
