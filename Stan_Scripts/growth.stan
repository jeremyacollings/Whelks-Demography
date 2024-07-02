// Growth Model

data {
  int<lower=0> N; // number of data points
  int<lower=0> I; // number of individuals
  int<lower=0> S; // number of sites
  vector[N] growth; // per-day growth
  int ind[N]; // individual id
  vector[N] species; // species of individual
  vector[N] prev_length; // previous length of individual
  vector[N] temp; // mean temperature
  vector[N] food; // total cover of food
  int site[N]; // site
}

parameters {
  real fixed[4]; // fixed effects
  // real ind_fx[I]; // individual effects
  real site_fx[S]; // site effects
  
  // real ind_mu; // mean intercept across individuals
  // real ind_sd; // sd of individual differences

  real site_mu; // mean site effect
  real site_sd; // sd of site effects
  
  real<lower=0> sigma;
}


model {
  real mu[N]; // expected growth rate
  
  // ind_fx ~ normal(rep_row_vector(ind_mu, I), rep_row_vector(ind_sd, I));
  
  site_fx ~ normal(rep_row_vector(site_mu, S), rep_row_vector(site_sd, S));
  
  for(i in 1:N){
    mu[i] = site_fx[site[i]] + fixed[1]*species[i] + 
      fixed[2]*prev_length[i] + fixed[3]*temp[i] + fixed[4]*food[i];
    
    growth[i] ~ normal(mu[i], sigma);
  }

}

