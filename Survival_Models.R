
# Survival Models

library(readr)
library(readxl)
library(tidyverse)
library(lme4)
library(brms)
library(rstan)
library(MCMCvis)

source("~/Documents/GitHub/Whelks-Demography/Compile_Cage_Data.R")

# Descriptives ------------------------------------------------------------

sum(cage_dat$mort)

tapply(cage_dat$mort, cage_dat$species, sum)

ggplot(data = cage_dat, aes(x = prev_length, y = mort)) + 
  geom_point() + facet_wrap(~ species)

ggplot(data = cage_dat, aes(x = date_diff, y = mort)) + 
  geom_point() + facet_wrap(~ species)
# real low mortality... might be tough

# Fitting Models ----------------------------------------------------------

inv_logit <- function(x) exp(x)/(1 + exp(x))

# prob of mortality in a year
yr_mort <- function(x){
  yr_surv <- (1 - x)^365
  1 - yr_surv
}

fit <- brm(mort ~ date_diff, 
           data = cage_dat[which(cage_dat$species == "M"),], 
           family = "bernoulli")

per_day_pred <- posterior_linpred(fit, cbind.data.frame(date_diff = 1))

yr_mort(per_day_pred)

# this model predicts that mortality is less likely the longer the 
# time interval between observations...
# also, this model predicts that the probability of dying in a year
# is >90%
# I think this has to do with the weird dat_diff coef bringing the 
# intercept of the model closer to 0 and thus the 1-day predictions
# are pretty close to 0

# reasonably... I'm not sure if we can estimate differences < 2 weeks
# in mortality probability from this dataset; we aren't super confident
# in the diff_date effect anyway... why don't we try ignoring it? 

hist(cage_dat$date_diff)
median(cage_dat$date_diff, na.rm = TRUE)

yr_mort2 <- function(x){
  yr_surv <- (1 - x)^52
  1 - yr_surv
}

fit2M <- brm(mort ~ mean_temp, 
           data = cage_dat[which(cage_dat$species == "M" &
                                   !is.na(cage_dat$prev_length)),], 
           family = "bernoulli", 
           iter = 4000, cores = 4)

fit2A <- brm(mort ~ mean_temp, 
             data = cage_dat[which(cage_dat$species == "As" &
                                     !is.na(cage_dat$prev_length)),], 
             family = "bernoulli", 
             iter = 4000, cores = 4)

temp_df <- cbind.data.frame(meds = c(fixef(fit2M)[2,1], 
                                     fixef(fit2A)[2,1]),
                            lows = c(fixef(fit2M)[2,3],
                                     fixef(fit2A)[2,3]),
                            ups = c(fixef(fit2M)[2,4], 
                                     fixef(fit2A)[2,4]), 
                            sp = c("Mex", "Acan"))

ggplot(data = temp_df, aes(x = sp, y = meds, ymin = lows, ymax = ups)) + 
  geom_pointrange() + geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic() + ylab("Estimated Temperature Effect") + xlab("Species")

tapply(cage_dat$mort, list(cage_dat$species, cage_dat$site), 
       sum)

fit3 <- brm(mort ~ mean_temp + (1|site), 
            data = cage_dat[which(cage_dat$species == "M" &
                                    !is.na(cage_dat$prev_length)),], 
            family = "bernoulli", 
            iter = 4000, cores = 4)

fit4 <- brm(mort ~ (1|site), 
            data = cage_dat[which(cage_dat$species == "M" &
                                    !is.na(cage_dat$prev_length)),], 
            family = "bernoulli", 
            iter = 4000, cores = 4)

per_fortnight_pred <- posterior_epred(fit2, 
                                      cbind.data.frame(date_diff = 1))

median(yr_mort2(per_fortnight_pred))

# okay, now with size?

fit3 <- brm(mort ~ prev_length, 
            data = cage_dat[which(cage_dat$species == "M" &
                                    !is.na(cage_dat$prev_length)),], 
            family = "bernoulli")

loo_compare(loo(fit2), loo(fit3))

# looks like fit3 performs worse...

per_fortnight_pred <- posterior_linpred(fit3)

View(yr_mort2(per_dat_pred))
# these are less reasonable

# size independent mortality? maybe?

# mod attempt Sep 1st

fit <- brm(mort ~ prev_length + species  + range, 
           data = cage_dat[which(!is.na(cage_dat$prev_length)),], 
           family = "bernoulli")

