data {
  int<lower=1> S; // number of size classes
  int<lower=1> Ti; // number of timepoints
  matrix[S, Ti] obs_count; // frequency matrix
  real<lower=0> v_min; // minimum size
  real<lower=0> delta_v; // size intervals
}

transformed data {
  real<lower=0> v[S+1]; // vector of (minimum) sizes for each size class
  row_vector[S] v_mid; // vector of sizes for each size class
  
  for (i in 1:(S+1)) {
    v[i] = v_min + ((i-1) * delta_v);
  }
  for (i in 1:S) {
    // using geometric mean
    v_mid[i] = sqrt(v[i] * v[i+1]);
  }
}

parameters {
  real<lower=0> phi; // dispersion of counts
  real<lower=0> G_beta_0; // intercept of growth function
  real<lower=0> G_var; // variation about the expected growth
  real S_beta_0; // intercept of survival function
  real<lower=1> F_beta_0; // intercept of fecundity function
  real R_beta_0; // intercept of recruitment function
}

transformed parameters {
  matrix[S, S] les_mat; // Leslie matrix
  real exp_growth; // expected growth rate at size j
  
  // fill in first row with fecundity * recruitment rate
  les_mat[1, ] = rep_row_vector(F_beta_0 * inv_logit(R_beta_0), S);
  
  for (i in 2:S) {
    for (j in 1:S) {
      if (i >= j) {
        // growth & survival
        exp_growth = (v_mid[j] / v[S+1]) ^ G_beta_0;
        les_mat[i, j] = exp(normal_lpdf(v_mid[i] | v_mid[j] + exp_growth, G_var)) * 
        inv_logit(S_beta_0);
      } else {
        les_mat[i, j] = 0;
      }
    }
  }
}

model {
  // Priors
  phi ~ exponential(1);
  G_beta_0 ~ normal(0, .5);
  G_var ~ exponential(1);
  S_beta_0 ~ normal(0, .1);
  F_beta_0 ~ normal(20, 5);
  R_beta_0 ~ normal(-1, 1);
  
  // Likelihood
  matrix[S, Ti] mu; // matrix of expected abundances
  
  for (j in 2:Ti) {
    mu[, j] = les_mat * obs_count[, j-1]; // Ensure correct matrix multiplication
    for (i in 1:S) {
      obs_count[i, j] ~ normal(mu[i, j], phi);
    }
  }
}