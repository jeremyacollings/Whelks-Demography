
##### WHELK GROWTH & SURVIVAL MODELS #####

library(readr)
library(readxl)
library(tidyverse)
library(lme4)
library(brms)
library(rstan)
library(MCMCvis)
library(mice)
library(tidybayes)
library(modelr)

source("~/Documents/GitHub/Whelks-Demography/Compile_Cage_Data.R")

# Descriptives ------------------------------------------------------------

ggplot(data = cage_dat, aes(x = as.numeric(shell_length), fill = species)) + 
  geom_histogram(position = "identity", alpha = .6)

table(cage_dat$species, cage_dat$region)

ggplot(data = cage_dat[which(cage_dat$species %in% c("M", "As")),], 
       aes(x = as.numeric(shell_length), fill = species)) + 
  geom_histogram(position = "identity", alpha = .6) + 
  facet_wrap( ~ region) 

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

test <- brm(per_day_growth2 ~ species + s(prev_length), 
            data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                    cage_dat$id != "Yellow19"),], 
            chains = 4, cores = 4)

M_null <- brm(per_day_growth2 ~ prev_length + (1|site), 
              data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                      cage_dat$id != "Yellow19", 
                                    cage_dat$species == "M"),],
              chains = 4, cores = 4, 
              prior = c(set_prior("exponential(1)", class = "sd"), 
                        set_prior("exponential(1)", class = "sd", group = "site"), 
                        set_prior("normal(0,.25)", class = "Intercept"),
                        set_prior("normal(0,.25)", class = "b", ub = 0)),
              control = list(adapt_delta = .95))

M_global <- brm(per_day_growth2 ~ prev_length + mean_temp + total +
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
                          set_prior("normal(0,.25)", class = "b", coef = "total")),
                control = list(adapt_delta = .95))

M_global <- brm(per_day_growth2 ~ prev_length + mean_temp + total +
                  (1|site), 
                data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                        cage_dat$id != "Yellow19" & 
                                      cage_dat$species == "M"),],
                chains = 2, cores = 2, 
                prior = c(set_prior("exponential(1)", class = "sd"), 
                          set_prior("exponential(1)", class = "sd", group = "site"), 
                          set_prior("normal(0,.25)", class = "Intercept"),
                          set_prior("normal(0,.25)", class = "b", coef = "prev_length"), 
                          set_prior("normal(0,.25)", class = "b", coef = "mean_temp"), 
                          set_prior("normal(0,.25)", class = "b", coef = "total")),
                control = list(adapt_delta = .95))

A_global <- brm(per_day_growth2 ~ prev_length + mean_temp + total +
                  (1|site), 
                data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                        cage_dat$id != "Yellow19" & 
                                      cage_dat$species == "As"),],
                chains = 2, cores = 2, 
                prior = c(set_prior("exponential(1)", class = "sd"), 
                          set_prior("exponential(1)", class = "sd", group = "site"), 
                          set_prior("normal(0,.25)", class = "Intercept"),
                          set_prior("normal(0,.25)", class = "b", coef = "prev_length"), 
                          set_prior("normal(0,.25)", class = "b", coef = "mean_temp"), 
                          set_prior("normal(0,.25)", class = "b", coef = "total")),
                control = list(adapt_delta = .95))

fixed.df <- cbind.data.frame(med = c(fixef(M_global)[2:4, 1], 
                                     fixef(A_global)[2:4, 1]), 
                             low = c(fixef(M_global)[2:4, 3], 
                                     fixef(A_global)[2:4, 3]), 
                             up = c(fixef(M_global)[2:4, 4], 
                                    fixef(A_global)[2:4, 4]),
                             var = rep(c("size", "temp", "food"), 2), 
                             sp = rep(c("Mex", "Acan"), each = 3))
ggplot(data = fixed.df, 
       aes(x = var, y = med, ymin = low, ymax = up, color = sp)) + 
         geom_pointrange(position = position_dodge(width = .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 12) + 
  xlab("Site") + ylab("Estimate")

rando.df <- cbind.data.frame(med = c(ranef(M_global)[[1]][,1,],
                                     ranef(A_global)[[1]][,1,]), 
                             low = c(ranef(M_global)[[1]][,3,],
                                     ranef(A_global)[[1]][,3,]), 
                             up = c(ranef(M_global)[[1]][,4,],
                                    ranef(A_global)[[1]][,4,]), 
                             site = names(c(ranef(M_global)[[1]][,1,],
                                      ranef(A_global)[[1]][,1,])), 
                             sp = rep(c("Mex", "Acan"), each = 4))

ggplot(data = rando.df, aes(x = site, y = med, 
                            ymin = low, ymax = up, 
                            color = sp)) + 
  geom_pointrange(position = position_dodge(width = .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 12) + 
  xlab("Site") + ylab("Estimate") + 
  theme(axis.text.x = element_text(angle = 320, hjust = 0, vjust = .9))
       
ggplot(data = cage_dat[which(cage_dat$per_day_growth != Inf &
                               cage_dat$id != "Yellow19", 
                             cage_dat$species == "M"),], 
       aes(x = prev_length, y = per_day_growth2, color = site)) + 
  geom_point() + 
  stat_function()

ggplot(epred_draws(M_global, newdata = cage_dat[which(cage_dat$per_day_growth != Inf &
                                                 cage_dat$id != "Yellow19" & 
                                                 cage_dat$species == "M"),]), 
       aes(x = .row, y = .epred, color = site)) + 
  stat_summary(
    fun.min = function(z) { quantile(z,0.25) },
    fun.max = function(z) { quantile(z,0.75) },
    fun = median) + geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 12) + ylab("Predicted Growth") + 
  ylab("Data Point")


cage_dat[which(cage_dat$per_day_growth != Inf &
                 cage_dat$id != "Yellow19", 
               cage_dat$species == "M"),] %>%
  group_by(site) %>%
  data_grid(prev_length = seq_range(prev_length, n = 51)) %>%
  mutate(mean_temp = mean(cage_dat$mean_temp, na.rm = TRUE)) %>%
  mutate(total = mean(cage_dat$total, na.rm = TRUE)) %>%
  add_epred_draws(M_global) %>%
  ggplot(aes(x = prev_length, y = per_day_growth, color = ordered(site))) +
  stat_lineribbon(aes(y = .epred), geom = "lineribbon", 
                  .width = c(.5, .5, .5), 
                  alpha = 1) +
  geom_point(data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                     cage_dat$id != "Yellow19", 
                                   cage_dat$species == "M"),], 
             alpha = .25) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") + 
  ylim(-.02, .2) + 
  theme_classic(base_size = 15) + 
  theme(legend.position = "none")

# nonlinear growth model

mod_dat <- cage_dat[which(!is.na(cage_dat$prev_length) & 
                            !is.na(cage_dat$shell_length) & 
                            cage_dat$species == "M"),]
mod_dat$ind <- as.numeric(factor(mod_dat$id))
mod_dat$site2 <- as.numeric(factor(mod_dat$site))

stan_dat2 <- list(N = nrow(mod_dat), S = n_distinct(mod_dat$site2), 
                  In = n_distinct(mod_dat$ind), 
                  sizet = mod_dat$shell_length, 
                  size_t0 = mod_dat$prev_length, 
                  days = mod_dat$date_diff, 
                  site = mod_dat$site2, 
                  ind = mod_dat$ind, 
                  size_max = max(mod_dat$shell_length) + 5)

test <- stan("Stan_Scripts/Power_Growth6.stan", data = stan_dat2, 
     cores = 2, chains = 2, iter = 500)

M_null <- brm(per_day_growth2 ~ prev_length , 
              data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                      cage_dat$id != "Yellow19", 
                                    cage_dat$species == "M"),],
              chains = 4, cores = 4, 
              prior = c(set_prior("exponential(1)", class = "sd"), 
                        set_prior("exponential(1)", class = "sd", group = "site"), 
                        set_prior("normal(0,.25)", class = "Intercept"),
                        set_prior("normal(0,.25)", class = "b", ub = 0)),
              control = list(adapt_delta = .95))

coef_dat <- as.data.frame(fixef(M_global)*365.25)
coef_dat$coef <- rownames(coef_dat)
names(coef_dat) <- c("est", "err", "low", "up", "coef")

cage_dat2 <- cage_dat[
  which(cage_dat$per_day_growth != Inf & 
          cage_dat$id != "Yellow19"),]

stan_dat <- list(N = nrow(cage_dat2), 
  S = n_distinct(cage_dat2$site), 
  growth = cage_dat2$per_day_growth, 
  species = as.numeric(factor(cage_dat2$species)),
  prev_length = cage_dat2$prev_length,
  temp = cage_dat2$mean_temp, 
  food = cage_dat2$total, 
  site = as.numeric(factor(cage_dat2$site)))

fit <- stan("Stan_Scripts/growth.stan", data = stan_dat, 
     cores = 2, chains = 2, iter = 2000, 
     pars = c("fixed", "site_fx"))

stan_datM <- list(N = nrow(cage_dat2[which(cage_dat2$species == "M"),]), 
                  S = n_distinct(cage_dat2$site[which(cage_dat2$species == "M")]), 
                  growth = cage_dat2$per_day_growth2[which(cage_dat2$species == "M")], 
                  prev_length = cage_dat2$prev_length[which(cage_dat2$species == "M")],
                  temp = cage_dat2$mean_temp[which(cage_dat2$species == "M")], 
                  food = cage_dat2$total[which(cage_dat2$species == "M")], 
                  site = as.numeric(factor(cage_dat2$site[which(cage_dat2$species == "M")])))

fitM <- stan("Stan_Scripts/growth_by_species.stan", 
             data = stan_datM, 
            cores = 2, chains = 2, iter = 2000, 
            pars = c("fixed", "site_fx"))

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

