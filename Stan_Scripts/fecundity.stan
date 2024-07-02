
data {
  int<lower=0> N; // number of egg masses
  int<lower=0> S; // number of sites
  int site[N]; // site associated with case
  int cases[N]; // number of egg cases in a mass
  int whelks[N]; // number of whelks in a photo
  int max_whelks[S]; // absolute max number of whelks at each site
}

parameters {
  real<lower=0> lam[S]; // whelk occurrence rate
  real<lower=0,upper=1> p; // detection probability
  real<lower=0> f; // mean fecundity
}

model {
  // prior
  lam ~ normal(1, 5);
  p ~ beta(2, 1.5);
  f ~ normal(200, 100);
  
  // likelihood
  real lp[N,100];
  for(i in 1:N){
    if(whelks[i] == 0){
      for(j in (whelks[i]+1):(max_whelks[site[i]] + 20)){
        lp[i, j] = poisson_lpmf(j | lam[site[i]]) + 
        binomial_lpmf(whelks[i] | j, p) + 
        neg_binomial_2_lpmf(cases[i] | j * f, 1);
        }
        target += log_sum_exp(lp[i,(whelks[i]+1):(max_whelks[site[i]] + 20)]);
    }
    else{
      for(j in whelks[i]:(max_whelks[site[i]] + 20)){
        lp[i, j] = poisson_lpmf(j | lam[site[i]]) + 
        binomial_lpmf(whelks[i] | j, p) + 
        neg_binomial_2_lpmf(cases[i] | j * f, 1);
        }
        target += log_sum_exp(lp[i,whelks[i]:(max_whelks[site[i]] + 20)]);
        }
  }
}
