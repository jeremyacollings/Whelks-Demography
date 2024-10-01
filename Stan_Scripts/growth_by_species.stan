// Growth Model

data {
  int<lower=0> N; // number of data points
  // int<lower=0> I; // number of individuals
  int<lower=0> S; // number of sites
  vector[N] growth; // per-day growth
  // int ind[N]; // individual id
  vector[N] prev_length; // previous length of individual
  vector[N] temp; // mean temperature
  vector[N] food; // total cover of food
  int site[N]; // site
}

parameters {
  real fixed[3]; // fixed effects
  real site_raw[S]; // site deviations
  
  // real ind_mu; // mean intercept across individuals
  // real ind_sd; // sd of individual differences

  real site_mu; // mean site effect
  real<lower=0> site_sd; // sd of site effects
  
  real scale; // scale for gamma
}


transformed parameters{
  real site_fx[S]; // site specific intercept
  real mu[N]; // expected growth rate

  
  for(i in 1:S){
    site_fx[i] = site_mu + site_raw[i]*site_sd;
  }
  
    for(i in 1:N){
    mu[i] = site_fx[site[i]] + 
      fixed[1]*prev_length[i] + 
      fixed[2]*temp[i] + 
      fixed[3]*food[i];
    
  }
}
model {
  // Priors
  site_raw ~ normal(0, 1);
  site_mu ~ normal(0, 1); 
  fixed ~ normal(0, 1);

  site_sd ~ exponential(1);
  scale ~ exponential(1); 
  
  // Likelihood
  for(i in 1:N){
      growth ~ gamma(mu[i]/scale, 1/scale);
  }
}

