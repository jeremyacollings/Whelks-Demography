
##### WHELK GROWTH & SURVIVAL MODELS #####

library(readr)
library(readxl)
library(tidyverse)
library(lme4)
library(brms)
library(rstan)
library(MCMCvis)
library(mice)

source("~/Documents/GitHub/Whelks-Demography/Compile_Cage_Data.R")

# Descriptives ------------------------------------------------------------

ggplot(data = length_dat, aes(x = as.numeric(Total_Length), fill = Species)) + 
  geom_histogram(position = "identity", alpha = .6)

table(length_dat$Species, length_dat$Region)
table(egg_dat$Whelk_Sp, egg_dat$Region)
table(cage_dat$Species, cage_dat$Region)

ggplot(data = length_dat[which(length_dat$Species %in% c("M", "As")),], 
       aes(x = as.numeric(Total_Length), fill = Species)) + 
  geom_histogram(position = "identity", alpha = .6) + 
  facet_wrap( ~ Region) 

ggplot(data = cage_dat, aes(x = survey_date, y = shell_length, 
                            group = id)) + 
  geom_line() + facet_wrap(species ~ region) + 
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = cage_dat, aes(x = survey_date, y = shell_width, 
                            group = id)) + 
  geom_line() + facet_wrap(species ~ region, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ylim(0, 25)

ids <- vals <- c()
for(i in unique(cage_dat$id)){
  ids <- c(ids, i)
  temp <- cage_dat[which(cage_dat$id == i), ]
  vals <- c(vals, temp$shell_length[which(temp$survey_date == 
                                            min(temp$survey_date))] <
              temp$shell_length[which(temp$survey_date == 
                                        max(temp$survey_date))])
}

tapply(cage_dat$shell_length, list(cage_dat$site, week(cage_dat$survey_date)),
       mean, na.rm = TRUE)

summary(lmer(shell_length ~ survey_date + (1|id), data = cage_dat))
# per day growth rate: 0.002076
0.002076*365 # = 0.75774
# how long would it take for the smallest recorded whelk to grow to the 
# size of the largest recorded whelk
max(cage_dat$shell_length, na.rm = TRUE) - min(cage_dat$shell_length, na.rm = TRUE)/
  0.75774
# 30.8 years...

# just thought... maybe size dependent growth rate really matters here
# it does kind of look like smallest ones (Mexicanthina in Baja)
# might be growing faster... check this out? 

summary(lmer(shell_length ~ survey_date*region + (1|id), data = cage_dat))
# it does look like Baja has the fastest growth rate... at about 0.006552
0.006552*365 # = 2.39

# Fitting Models ----------------------------------------------------------

# some helpful distributions only have support for positive real numbers...
# to account for 0's, lets create a new variable

cage_dat$per_day_growth2 <- ifelse(cage_dat$per_day_growth == 0, 
                                   cage_dat$per_day_growth + 1*10^-100, 
                                   cage_dat$per_day_growth)

# now let's run some models

M_global <- brm(per_day_growth2 ~ prev_length + mean_temp + total + Mprop + 
                  (1|site), 
                data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                        cage_dat$id != "Yellow19", 
                                      cage_dat$species == "M"),],
                chains = 2, cores = 2, 
                prior = c(set_prior("exponential(1)", class = "sd"), 
                          set_prior("exponential(1)", class = "sd", group = "site"), 
                          set_prior("normal(0,.25)", class = "Intercept"),
                          set_prior("normal(0,.25)", class = "b", coef = "prev_length"), 
                          set_prior("normal(0,.25)", class = "b", coef = "mean_temp"), 
                          set_prior("normal(0,.25)", class = "b", coef = "total"), 
                          set_prior("normal(0,.25)", class = "b", coef = "Mprop")),
                control = list(adapt_delta = .95))

coef_dat <- as.data.frame(fixef(M_global)*365.25)
coef_dat$coef <- rownames(coef_dat)
names(coef_dat) <- c("est", "err", "low", "up", "coef")

# Visualizing Output ------------------------------------------------------

ggplot(data = coef_dat, aes(x = coef, y = est, ymin = low, ymax = up)) + 
  geom_pointrange() + theme_classic(base_size = 15) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Parameter") + ylab("Estimate") + 
  scale_x_discrete(labels = c("Intercept", "Temperature", "Mussel Prop.", 
                             "Prev. Length", "Total Food"))

ggplot(data = coef_dat[which(coef_dat$coef %in% 
                               c("prev_length", "mean_temp", "total")),], 
       aes(x = coef, y = est, ymin = low, ymax = up)) + 
  geom_pointrange() + theme_classic(base_size = 15) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Parameter") + ylab("Estimate") + 
  scale_x_discrete(labels = c("Temperature", "Prev. Length", "Total Food"))

