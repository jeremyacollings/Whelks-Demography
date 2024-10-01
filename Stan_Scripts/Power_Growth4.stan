data {
  int N;
  int S;
  // int In;
  real sizet[N];
  real size_t0[N];
  real days[N];
  int site[N];
  // int ind[N];
  real size_max;
}

parameters {
  // real ind_raw[In];
  real site_raw[S];
  real site_mean;
  // real ind_sd;
  real site_sd;
  real growth_sd;
}

transformed parameters {
  real beta[S];

  for(i in 1:S){
    beta[i] = site_mean + site_sd * site_raw[i];
    print(beta[i]);
  }
}

model {
  // Priors
  // ind_raw ~ normal(.5,.25);
  // site_raw ~ normal(.5,.25);
  // site_mean ~ normal(.5,.25);
  // ind_sd ~ exponential(10);
  // site_sd ~ exponential(10); 
  // growth_sd ~ exponential(10);
  
  // Likelihood
  for(i in 1:N){
    target += normal_lpdf(sizet[i] | size_t0[i] + pow((size_t0[i] / size_max), (beta[site[i]]))*days[i], growth_sd);
  }
}