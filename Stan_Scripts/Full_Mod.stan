// WHELK IIPM
data {
  // dims -----
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
  
  // whelk measurements -----
  real growth; // per day growth
  real surv; // per dat survival
  int<lower=0> Ne; // number of egg capsules in case
  int<lower=0> Nw; // number of whelks around case
  matrix[SP, S, Ti] obs_count; // frequency matrix
  real emb_per_cap[SP]<lower=0>; // number of ebryos per egg capsule
  // could replace this with raw data and propogate uncertainty further...
  
  // covariates -----
  int<lower=0> SIg; // site for cage growth data
  int<lower=0> SIsu; // site for cage survival data
  int<lower=0> SIf; // site for egg mass data
  int<lower=0> SIsi; // site for surveyed distribution data
  
  int<lower=0> SPg; // species for cage growth data
  int<lower=0> SPsu; // species for cage survival data
  int<lower=0> SPf; // species for egg mass data
  int<lower=0> SPsi; // species for surveyed distribution data
  
  real<lower=0> SIZg; // size for growth data
  real<lower=0> SIZsu; // size for survival data
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
  // growth rate parameters -----
  real gamma0[SP]; // annual growth rate at size 0
  real gamma1[SP]; // size dependency of growth rate
  real<lower=0> growth_sd[SP]; // sd of per day growth rates
  
  // survival parameters -----
  real psi0[SP]; // annual survival rate at size 0
  real psi0[SP]; // size dependency of mortality
  
  // fecundity parameters -----
  real phi[SP]; // annual fecundity rate (in terms of embryos)
  real lambda[SP; SI]; // rate parameter for localized density of mature whelks
  real p; // whelk detection probability
  int<lower=0> Nw[Nf]; // latent number of parent whelks per egg case
  
  // recruitment parameters -----
  real rho[SP]; // annual recruitment rate
  
  // observation error -----
  real<lower=0> disp[SP]; // dispersion for observed counts
}

transformed parameters {
  real gamma_exp[Ng]; // expected daily growth rate
  real psi_exp[Nsu]; // expected daily survival rate
  real phi2[SP]; // annual fecundity rate (in terms of egg capsules)
  
  array[S, S] lef_mat; // lefkovich matrix
  
  // calculate expected daily growth rate
  for(i in 1:Ng){
    gamma_exp[i] = (gamma0[SPg[i]] + gamma1[SPg[i]]*SIZg[i])/365;
  }
  
  // calculate expected daily mortality rate
  for(i in 1:Nsu){
    psi_exp[i] = (logit(psi0[SPsu[i]] + psi1[SPsu[i]]*SIZsu[i]))/365;
  }
  
  // get egg capsule fecundity from embryo fecundity
  phi2 = phi*emb_per_cap;
  
  // fill in lefkovich matrix
  
  // fill in first row with fecundity * recruitment rate
  for(sp in 1:SP){
    lef_mat[sp, 1, ] = rep_row_vector(phi2[sp] * inv_logit(rho[sp]), S);
    
    for (i in 2:S) {
    for (j in 1:S) {
      if (i >= j) {
        // growth & survival
        exp_growth = gamma0[sp] + gamma1[sp]*v_mid[j];
        lef_mat[i, j] = exp(normal_lpdf(v_mid[i] | v_mid[j] + exp_growth, growth_sd[sp])) * 
        inv_logit(psi0[sp] + psi1[sp]*v_mid[j]);
      } else {
        lef_mat[i, j] = 0;
      }
    }
  }
  }
}

model {
  // priors
  lambda ~ normal(1, 5);
  p ~ beta(2, 1.5);
  phi ~ normal(200, 100);
  
  // cage growth likelihood
  
  for(i in 1:Ng){
    growth[i] ~ normal(gamma_exp[i], growth_sd[SPg[i]]);
  }
  
  // cage surival likelihood
  
  for(i in 1:Nsu){
    surv[i] ~ bernoulli(psi_exp[i]);
  }
  
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
  
  matrix[SP, S, Ti] mu; // matrix of expected abundances
  
  for(sp in 1:SP){
      for (j in 2:Ti) {
    mu[sp, , j] = lef_mat * obs_count[sp, , j-1]; // Ensure correct matrix multiplication
    for (i in 1:S) {
      obs_count[sp, i, j] ~ neg_binomial_2(mu[sp, i, j], disp[sp]);
    }
  }
  }
}

