
# messing around with full IPM

inv_logit <- function(x) exp(x)/(1 + exp(x))

# annual growth ~ last years size + covariates
g_mod <- function(size_y, size_x, covs, coes){
  per_day_growth_rate = coes[1] + size_x*coes[2] + sum(covs*coes[3:(length(coes)-1)])
  mu = size_x + per_day_growth_rate*365
  dnorm(size_y, mu, coes[length(coes)])
}

g_mod2 <- function(size_y, size_x, covariates, coefs){
  per_day_growth_rate = (size_x/coefs["size_max"])^coefs["beta"]
  mu = size_x + per_day_growth_rate*365
  dnorm(size_y, mu, coefs["sd"])
}

# survival probability ~ last years size + covariates
s_mod <- function(size_y, size_x, covariates, coefs){
  p = inv_logit(coefs[1] + size_x*coefs[2] + sum(covariates*coefs[3:(3+length(covariates)-1)]))
  dbinom(rep(1, length(size_y)), 1, 1 - p)
}

# transition rate = g_mod*s_mod
t_mod <- function(size_y, size_x, covariates_g, covariates_s, 
                  coefs_g, coefs_s){
  g_mod(size_y, size_x, covariates_g, coefs_g)*
    s_mod(size_y, size_x, covariates_s, coefs_s)
}

# fecundity ~ last years size + covariates
f_mod <- function(covariates, coefs){
  coefs[1] + sum(covariates*coefs[2:(2+length(covariates)-1)])
}

# establishment probability ~ covariates
p_mod <- function(covariates, coefs){
  p = inv_logit(coefs[1] + sum(covariates*coefs[2:(2+length(covariates)-1)]))
  dbinom(1, 1, p)
}

# modified from Eldred & Miller 2016

bigmatrix<-function(covs, coefs, lower, upper, matsize, 
                    G_mod, S_mod, P_mod, F_mod){  
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  
  n<-matsize
  L<-lower; U<-upper
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins 
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints

  # Fertility Matrix
  Fmat<-matrix(0,(n),(n), byrow = FALSE)
  
  # Growth/Survival Transition Matrix
  Tmat<-matrix(0,(n),(n))
  
  # Fecundity & Recruitment
  Fmat[1,1:(n)] <- F_mod(covs$fert, coefs$fert) * P_mod(covs$rec, coefs$rec)

  t_mod <- function(size_y, size_x, covariates_g, covariates_s, 
                    coefs_g, coefs_s){
    G_mod(size_y, size_x, covariates_g, coefs_g)*
      S_mod(size_y, size_x, covariates_s, coefs_s)
  }
  
  # Growth & Survival
  Tmat[1:(n), 1:(n)] <- t(outer(y,y,t_mod,covariates_g = covs$growth,
                                    covariates_s = covs$surv, coefs_g = coefs$growth,
                                    coefs_s = coefs$surv))*h


  # Put it all together
  IPMmat<-Fmat+Tmat     
  
  IPMmat[which(upper.tri(IPMmat) & row(IPMmat) != 1)] <- 0
  
  return(list(IPMmat=IPMmat,Fmat=Fmat,Tmat=Tmat,meshpts=y))
}

