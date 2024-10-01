
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
egg_datM <- egg_dat2[which(egg_dat2$Whelk_Sp == "M"),]
egg_datA <- egg_dat2[which(egg_dat2$Whelk_Sp == "As"),]

egg_datM$site2 <- as.numeric(as.factor(egg_datM$Site))
egg_datA$site2 <- as.numeric(as.factor(egg_datA$Site))

stan_datM <- list(N = nrow(egg_datM), 
                 S = n_distinct(egg_datM$site2), 
                 site = egg_datM$site2, 
                 cases = egg_datM$Num_Egg_Capsules, 
                 whelks = as.numeric(egg_datM$Num_Whelks_By_Eggs), 
                 max_whelks = unname(tapply(as.numeric(egg_datM$Num_Whelks_By_Eggs), 
                                            egg_datM$site2, max, na.rm = TRUE)))

stan_datA <- list(N = nrow(egg_datA), 
                  S = n_distinct(egg_datA$site2), 
                  site = egg_datA$site2, 
                  cases = egg_datA$Num_Egg_Capsules, 
                  whelks = as.numeric(egg_datA$Num_Whelks_By_Eggs), 
                  max_whelks = unname(tapply(as.numeric(egg_datA$Num_Whelks_By_Eggs), 
                                             egg_datA$site2, max, na.rm = TRUE)))


# Fit Model ---------------------------------------------------------------

modM <- stan(file = "Stan_Scripts/fecundity.stan", data = stan_datM, 
             chains = 4, cores = 4)
modA <- stan(file = "Stan_Scripts/fecundity.stan", data = stan_datA, 
             chains = 4, cores = 4)

modM2 <- stan(file = "Stan_Scripts/fecundity2.stan", data = stan_datM, 
             chains = 4, cores = 4)
