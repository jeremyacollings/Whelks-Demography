
### Estimating Fecundity Rates

library(readxl)
library(tidyverse)
library(rstan)

setwd("~/Documents/GitHub/Whelks-Demography")

egg_dat <- read_excel("~/Documents/Data/Whelks/Range-Shift Community Survey Data - FINAL.xlsx", 
                      sheet = "EggCount")

# Prep Data for Model -----------------------------------------------------

egg_dat$Num_Whelks_By_Eggs[is.na(egg_dat$Num_Whelks_By_Eggs)]
egg_dat$Num_Egg_Capsules[is.na(egg_dat$Num_Egg_Capsules)]
egg_dat$Site[is.na(egg_dat$Site)]
egg_dat$Num_Whelks_By_Eggs[which(egg_dat$Num_Whelks_By_Eggs == "40+")] <- 40

egg_dat2 <- egg_dat[which(!is.na(egg_dat$Num_Egg_Capsules)),]
egg_dat2$site2 <- as.numeric(as.factor(egg_dat2$Site))
stan_dat <- list(N = nrow(egg_dat2), 
                 S = n_distinct(egg_dat2$site2), 
                 site = egg_dat2$site2, 
                 cases = egg_dat2$Num_Egg_Capsules, 
                 whelks = as.numeric(egg_dat2$Num_Whelks_By_Eggs), 
                 max_whelks = unname(tapply(as.numeric(egg_dat2$Num_Whelks_By_Eggs), 
                                            egg_dat2$site2, max, na.rm = TRUE)))

# Fit Model ---------------------------------------------------------------

mod <- stan(file = "Stan_Scripts/fecundity.stan", data = stan_dat, 
             chains = 4, cores = 4)
