
data {
  // dims
  int<lower=0> Ng; // N for cage growth data
  int<lower=0> Nsu; // N for cage survival data
  int<lower=0> Nf; // N for egg mass data
  int<lower=0> Nsi; // N for surveyed size distribution data
  int<lower=0> SI; // number of sites
  int<lower=0> SP; // number of species
  int<lower=1> S; // number of size classes
  int<lower=1> Ti; // number of timepoints
  real<lower=0> v_min; // minimum size
  real<lower=0> delta_v; // size intervals
  
  // whelk measurements
  real growth; // per day growth
  real surv; // per dat survival
  int<lower=0> Ne; // number of egg capsules in case
  int<lower=0> Nw; // number of whelks around case
  matrix[S, Ti] obs_count; // frequency matrix
  real emb_per_cap[SP]<lower=0>; // number of ebryos per egg capsule
  // could replace this with raw data and propogate uncertainty further...
  
  // covariates
  int<lower=0> SIg; // site for cage growth data
  int<lower=0> SIsu; // site for cage survival data
  int<lower=0> SIf; // site for egg mass data
  int<lower=0> SIsi; // site for surveyed distribution data
  
  int<lower=0> SPg; // species for cage growth data
  int<lower=0> SPsu; // species for cage survival data
  int<lower=0> SPf; // species for egg mass data
  int<lower=0> SPsi; // species for surveyed distribution data
  
  real<lower=0> size; // size for cage data
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
  real gamma[SP]; // annual growth rate
  real psi[SP]; // annual survival rate
  real phi[SP]; // annual fecundity rate (in terms of embryos)
  real rho[SP]; // annual recruitment rate
  
  real lambda[SP; SI]; // rate parameter for localized density of mature whelks
  real p; // whelk detection probability
  
  // latent variables
  int<lower=0> Nw[Nf]; // number of parent whelks per egg case
}

transformed parameters {
  real gamma2[SP]; // daily growth rate
  real psi2[SP]; // daily survival rate
  real phi2[SP]; // annual fecundity rate (in terms of egg capsules)
  
  array[S, S] les_mat; // Leslie matrix
  
  gamma2 = gamma/365; // get daily growth rate from annual growth rate
  psi2 = psi^(1/365); // get daily survival rate from annual survival rate
  phi2 = phi*emb_per_cap; // get egg capsule fecundity from embryo fecundity
}

model {
  // priors
  lambda ~ normal(1, 5);
  p ~ beta(2, 1.5);
  phi ~ normal(200, 100);
  
  // cage growth likelihood
  
  
  // cage surival likelihood
  
  // egg mass likelihood
  
  real lp[Fn,100];
  
  for(i in 1:Fn){
    if(Nw[i] == 0){
      for(j in (Nw[i]+1):(max_whelks[ SPf[i],SIf[i]] + 20)){
        lp[i, j] = poisson_lpmf(j | lambda[SPf[i],SIf[i]) + 
        binomial_lpmf(Nw[i] | j, p) + 
        neg_binomial_2_lpmf(Ne[i] | j * phi[SPf[i],SIf[i]], 1);
        }
        target += log_sum_exp(lp[i,(Nw[i]+1):(max_whelks[SPf[i],SIf[i]] + 20)]);
    }
    else{
      for(j in Nw[i]:(max_whelks[SPf[i],SIf[i]] + 20)){
        lp[i, j] = poisson_lpmf(j | lambda[SPf[i],SIf[i]]) + 
        binomial_lpmf(Nw[i] | j, p) + 
        neg_binomial_2_lpmf(Ne[i] | j * f, 1);
        }
        target += log_sum_exp(lp[i,Nw[i]:(max_whelks[SPf[i],SIf[i]] + 20)]);
        }
  }
  
  // surveyed size distribution likelihood
  
}

