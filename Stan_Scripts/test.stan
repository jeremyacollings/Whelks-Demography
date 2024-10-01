data {
  int<lower=0> N; // Number of observations
  int<lower=0> S; // Number of sites
  int<lower=0> In; // Number of individuals
  real size_t[N]; // Observed size at time t
  real size_t0[N]; // Initial size
  real days[N]; // Days passed
  int<lower=1, upper=S> site[N]; // Site index for each observation
  int<lower=1, upper=In> ind[N]; // Individual index for each observation
  real size_max; // Maximum possible size
}

parameters {
  real ind_raw[In]; // Raw individual effects
  real site_raw[S]; // Raw site effects
  real site_mean; // Mean effect across sites
  real<lower=0> ind_sd; // Standard deviation of individual effects
  real<lower=0> site_sd; // Standard deviation of site effects
  real<lower=0> growth_sd; // Standard deviation of the growth model
}

transformed parameters {
  real beta[In]; // Growth rates for individuals
  real ind_mean[S]; // Mean growth rate for each site
  
  for (s in 1:S) {
    ind_mean[s] = site_mean + site_sd * site_raw[s];
  }
  for (i in 1:In) {
    beta[i] = ind_mean[site[i]] + ind_sd * ind_raw[i];
  }
}

model {
  // Priors
  ind_raw ~ normal(0, 1);
  site_raw ~ normal(0, 1);
  site_mean ~ normal(0, 1);
  ind_sd ~ cauchy(0, 1);
  site_sd ~ cauchy(0, 1);
  growth_sd ~ cauchy(0, 1);

  // Likelihood
  for (i in 1:N) {
    real growth_rate = pow(size_t0[i] / size_max, beta[ind[i]] * days[i]);
    size_t[i] ~ normal(growth_rate, growth_sd);
  }
}