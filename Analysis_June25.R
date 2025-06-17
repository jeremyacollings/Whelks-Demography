
library(brms)
library(tidybayes)
library(tidyverse)
library(popbio)


# Data Prep ---------------------------------------------------------------

sites = c("Mendocino North", "Mendocino South", 
          "Mussel Rock", "Sea Ranch", 
          "Dillon Beach", "Coal Oil", 
          "Little Corona", "Shaw's Cove", 
          "Goff Island", "Heisler Park", 
          "Dana Point", "Victoria Beach", 
          "Swami's", "Saldamando", "Scripps",
          "San Miguel", "Punta Morro",
          "Campo Kennedy", "La Chorera Norte", 
          "La Chorera")

# bring in cage data for growth and survival estimation
source("1_Compile.Cage.Data.R")
exp_df <- read.csv(file.path("Data", "TT.csv"))

sp.df <- cbind.data.frame(sp = c("As", "M", "Unk"), code = 1:3)
# keep only entries with previous length measurement
cage_dat2 <- cage_dat[which(!is.na(cage_dat$prev_length) &
                              cage_dat$per_day_growth != Inf &
                              cage_dat$per_day_growth < 2),]

cage_dat2$species2 <- sp.df$code[match(cage_dat2$species, sp.df$sp)]

cage_dat2$per_day_growth2 <- ifelse(cage_dat2$per_day_growth == 0, 
                                    cage_dat2$per_day_growth + 1*10^-8, 
                                    cage_dat2$per_day_growth)

# get site names aligned

cage_dat3 <- cage_dat2 %>%
  mutate(site = as.factor(site)) %>%
  mutate(site = fct_recode(site, 
                           "Mendocino North" = "Cape Mendocino", 
                           "Mendocino South" = "Cape Mendocino South"))

# bring in egg mass data for fecundity estimation
egg_dat <- read_excel(file.path("Data", "Range-Shift Community Survey Data - FINAL.xlsx"), 
                      sheet = "EggCount")

# set 40+ to 40
egg_dat$Num_Whelks_By_Eggs[which(egg_dat$Num_Whelks_By_Eggs == "40+")] <- 40
# keep only complete cases
egg_dat2 <- egg_dat[which(!is.na(egg_dat$Num_Egg_Capsules) &
                            egg_dat$Whelk_Sp %in% c("As", "M")),]

egg_dat2$Num_Whelks_By_Eggs <- as.numeric(egg_dat2$Num_Whelks_By_Eggs)

egg_dat2$species2 <- sp.df$code[match(egg_dat2$Whelk_Sp, sp.df$sp)]


# Fitting Models ----------------------------------------------------------

# Fit Growth Model

growth_fit <- cage_dat3 %>%
  filter(species %in% c("As", "M")) %>%
  brm(formula(per_day_growth2 ~ prev_length*species + 
                mean_temp*species + (1|id) + 
                (1|site:species)), 
      data = ., cores = 4, chains = 4, iter = 1000, 
      family = "gamma")


# Fit Baseline Survival Model

surv_fit <- cage_dat3 %>%
  filter(species %in% c("As", "M")) %>%
  brm(formula(mort ~ species*prev_length), 
      data = ., cores = 4, chains = 4, iter = 1000, 
      family = "bernoulli")

# Fit Temperature Survival Model

surv_fit_exp <- exp_df %>%
  filter(Species %in% c("Acanthinucella", "Mexacanthina")) %>%
  brm(formula = Survival ~ Treatment*Species, 
      family = "Bernoulli", cores = 4, 
      chains = 4, iter = 1000)

# Fit Fecundity Model

egg_fit <- egg_dat %>% 
  filter(Whelk_Sp %in% c("As", "M")) %>%
  mutate(Num_Whelks_By_Eggs = as.numeric(Num_Whelks_By_Eggs)) %>% 
  brm(formula(Num_Egg_Capsules ~ Num_Whelks_By_Eggs*Whelk_Sp + 
                (1|Site:Whelk_Sp)), 
      data = ., family = "Poisson", 
      cores = 4, chains = 4, iter = 1000)


# Demographic Parameter Figures -------------------------------------------


## Growth -----------------------------------------------------------------


new_data <- expand_grid(
  species = c("As", "M"),
  site = unique(cage_dat3$site), # EDIT: make this site specific temperatures
  mean_temp = c(min(cage_dat3$mean_temp, na.rm = TRUE),
                median(cage_dat3$mean_temp, na.rm = TRUE),
                max(cage_dat3$mean_temp, na.rm = TRUE)),
  high_temp = c(min(cage_dat3$high_temp, na.rm = TRUE),
                median(cage_dat3$high_temp, na.rm = TRUE),
                max(cage_dat3$high_temp, na.rm = TRUE))
) %>% 
  mutate(prev_length = 24
  )

growth_pred_dat <- growth_fit %>% 
  epred_draws(newdata = new_data, 
              allow_new_levels = TRUE,
              # Sample from random effect distribution:
              sample_new_levels = "gaussian") %>% 
  mutate(.epred = .epred*365) %>%
  group_by(species, mean_temp, site) %>%
  summarise(
    median = median(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975),
    .groups = "drop"
  )

growth_pred_dat$site <- factor(growth_pred_dat$site, 
                               levels = c("Mendocino North", "Mendocino South",
                                          "Dana Point", "Scripps",
                                          "Punta Morro", "Campo Kennedy"))
# Plot
ggplot(growth_pred_dat, aes(x = site, y = median, color = as.factor(mean_temp))) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  facet_wrap(~ species, labeller = labeller(species = c("As" = "Acanthinucella", 
                                                        "M" = "Mexicanthina"))) + 
  labs(y = "Predicted Annual Growth (mm)", x = "Site") +
  scale_color_manual(name = "Temperature",
                     values = c("#087E8B", "#4B4237", "#FF8811"),
                     labels = c("Low", "Med", "High")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5), 
        legend.position = "top")

ggsave("Growth_Predictions_Med.pdf", units = "in", width = 6, height = 5)

ggplot(growth_pred_dat, aes(x = site, y = median, ymin = lower, ymax = upper,
                            color = as.factor(mean_temp))) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) + 
  facet_wrap(~ species, labeller = labeller(species = c("As" = "Acanthinucella", 
                                                        "M" = "Mexicanthina"))) + 
  labs(y = "Predicted Annual Growth (mm)", x = "Site") +
  scale_color_manual(name = "Temperature",
                     values = c("#087E8B", "#4B4237", "#FF8811"),
                     labels = c("Low", "Med", "High")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5), 
        legend.position = "top")

ggsave("Growth_Predictions_Full.pdf", units = "in", width = 6, height = 5)


## Survival ---------------------------------------------------------------

surv_pred_dat <- surv_fit %>% 
  epred_draws(newdata = new_data, 
              allow_new_levels = TRUE,
              sample_new_levels = "gaussian") %>% 
  # Convert mortality probability to survival probability
  mutate(.epred = 1 - .epred) %>% 
  mutate(.epred = .epred^365) %>%
  group_by(species, mean_temp, site) %>% 
  summarise(
    median = median(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975),
    .groups = "drop"
  )

surv_pred_dat %>% 
  group_by(species) %>% 
  summarise(median = median(median)) %>%
  ggplot(aes(x = species, y = median)) + 
  geom_bar(stat = "identity") + 
  theme_classic() + ylab("Survival Probability") + xlab("Species")

ggsave("Survival_Pred.pdf", units = "in", width = 6, height = 5)

## Fecundity --------------------------------------------------------------

egg_preds <- egg_fit %>% 
  epred_draws(newdata = new_data_eggs,
              allow_new_levels = TRUE,
              sample_new_levels = "gaussian") %>% 
  mutate(.epred = case_when(
    Whelk_Sp == "As" ~ .epred * 30, 
    Whelk_Sp == "M" ~ .epred * 25.5
  )) %>%
  group_by(Whelk_Sp, Site) %>% 
  summarise(
    median = median(log(.epred)),
    lower = quantile(log(.epred), 0.025),
    upper = quantile(log(.epred), 0.975),
    .groups = "drop"
  )

egg_preds$Site = factor(egg_preds$Site, 
                        levels = c("Mendocino North", "Mendocino South", 
                                   "Mussel Rock", "Sea Ranch", 
                                   "Dillon Beach", "Coal Oil", 
                                   "Little Corona","Shaw's Cove", 
                                   "Heisler Park", "Goff Island", 
                                   "Dana Point", "Victoria Beach", 
                                   "Swami's", "Saldamando", 
                                   "San Miguel", "Punta Morro",
                                   "Campo Kennedy", "La Chorera Norte", 
                                   "La Chorera"))
# Create plot
ggplot(egg_preds, 
       aes(x = Site, y = median, 
           color = Whelk_Sp)) +
  geom_point(position = position_dodge(width = 0.5), 
             size = 3) +
  labs(x = "Site",
       y = "Predicted log(Fecundity)") +
  scale_color_manual(name = "Species", 
                     labels = c("As" = "Acanthinucella", 
                                "M" = "Mexicanthina"),
                     values = c("As" = "#E69F00", "M" = "#56B4E9")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5), 
        legend.position = "top")

ggsave("Fecundity_Predictions_Med.pdf", units = "in", width = 6, height = 5)

ggplot(egg_preds, 
       aes(x = Site, y = median, ymin = lower, ymax = upper,
           color = Whelk_Sp)) +
  geom_point(position = position_dodge(width = 0.5), 
             size = 3) +
  geom_errorbar(width = 0, position = position_dodge(width = .5)) + 
  labs(x = "Site",
       y = "Predicted log(Fecundity)") +
  scale_color_manual(name = "Species", 
                     labels = c("As" = "Acanthinucella", 
                                "M" = "Mexicanthina"),
                     values = c("As" = "#E69F00", "M" = "#56B4E9")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5), 
        legend.position = "top")

ggsave("Fecundity_Predictions_Full.pdf", units = "in", width = 6, height = 5)



# Constructing IPM --------------------------------------------------------

# Pre-generate all parameter combinations
param_grid <- expand.grid(sp = c("As", "M"), si = sites)

param_grid <- cbind.data.frame(rbind(param_grid, param_grid), 
                               temp.treat = rep(c("current", "warmed"), each = nrow(param_grid)))

param_grid <- param_grid %>%
  mutate(mean.temp = case_when(
    temp.treat == "current" ~ mean(mean_temp_range, na.rm = TRUE), 
    temp.treat == "warmed" ~ mean(mean_temp_range, na.rm = TRUE) + 2
  ), 
  high.temp = case_when(
    temp.treat == "current" ~ mean(high_temp_range, na.rm = TRUE), 
    temp.treat == "warmed" ~ mean(high_temp_range, na.rm = TRUE) + 2
  ))

# EDIT: add code that adds temps to param_grid

# Use lapply or parallel processing
IPM_list <- lapply(1:nrow(param_grid), function(i) {
  sp <- param_grid$sp[i]
  si <- param_grid$si[i]
  te <- param_grid$mean.temp[i]
  
  larvae_per_egg <- ifelse(sp == "As", 30, ifelse(sp == "M", 25.5, NA))
  
  fake_dat <- data.frame(species = sp, site = si, mean_temp = te, prev_length = 0)
  fake_dat2 <- data.frame(Whelk_Sp = sp, Num_Whelks_By_Eggs = 1,
                          Site = si, mean_temp = te, prev_length = 0)
  
  IPM_fun_optimized(min(cage_dat3$prev_length), max(cage_dat3$shell_length), 100,
                    growth_fit, surv_fit, egg_fit, 
                    fake_dat, fake_dat2, rec = .02, larvae = larvae_per_egg)
})

param_grid$lambda <- as.numeric(lapply(IPM_list, function(x) eigen(x$IPMmat)$values[1]))

