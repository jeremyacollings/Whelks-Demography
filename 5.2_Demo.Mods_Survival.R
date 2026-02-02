
########## DEMOGRAPHIC MODELS - SURVIVAL ##########

library(tidyverse)
library(brms)
library(tidybayes)

# Data Prep ---------------------------------------------------------------

sites = c("Mendocino North", "Mendocino South", 
          "Mussel Rock", "Moat Creek", "Sea Ranch", 
          "Dillon Beach", "Coal Oil", 
          "Little Corona", "Crystal Cove", "Shaw's Cove", 
          "Heisler Park", "Victoria Beach", 
          "Goff Island", "Dana Point", 
          "Swami's", "Cardiff", "Scripps", 
          "Cabrillo", "Saldamando", 
          "San Miguel", "Punta Morro",
          "Campo Kennedy", "La Chorera Norte", 
          "La Chorera")

exp_df <- read.csv(file.path("Data", "TT.csv"))

sp.df <- cbind.data.frame(sp = c("As", "M", "Unk"), code = 1:3)

# Fitting candidate models ------------------------------------------------

# keeping in species

# Fit survival model
mean_field_temp = mean(cage_dat$mean_temp, na.rm = TRUE)

mod1 <- exp_df %>%
  filter(Species %in% c("Acanthinucella", "Mexacanthina")) %>%
  mutate(mort = case_when(
    Survival == 1 ~ 0,
    Survival == 0 ~ 1
  ), 
  temp = Treatment) %>%
  brm(formula = mort ~ Species + temp + temp*Species, 
      family = "bernoulli", cores = 4, 
      chains = 4, iter = 2000)

mod2 <- exp_df %>%
  filter(Species %in% c("Acanthinucella", "Mexacanthina")) %>%
  mutate(mort = case_when(
    Survival == 1 ~ 0,
    Survival == 0 ~ 1
  ), 
  temp = Treatment) %>%
  brm(formula = mort ~ Species + temp, 
      family = "bernoulli", cores = 4, 
      chains = 4, iter = 2000)

mod3 <- exp_df %>%
  filter(Species %in% c("Acanthinucella", "Mexacanthina")) %>%
  mutate(mort = case_when(
    Survival == 1 ~ 0,
    Survival == 0 ~ 1
  ), 
  temp = Treatment) %>%
  brm(formula = mort ~ Species, 
      family = "bernoulli", cores = 4, 
      chains = 4, iter = 2000)

loo_compare(loo(mod1), loo(mod2), loo(mod3))

# 2, - 1, -- 3

saveRDS(mod2, file = file.path("Outputs", "surv.mod.RDS"))

# Figures -----------------------------------------------------------------

# this will take more thought ...

mod2 %>%
  linpred_draws(newdata = expand.grid(temp = seq(0, 30, 1), 
                                      Species = c("Acanthinucella", "Mexacanthina")), allow_new_levels = TRUE) %>%
  mutate(survival_rate = 1 - plogis(.linpred))  %>%
  mutate(Species = case_when(
    Species == "Acanthinucella" ~ "As", 
    Species == "Mexacanthina" ~ "M"
  )) %>%
  ggplot(aes(x = temp, y = survival_rate, color = Species)) + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina")) + 
  scale_fill_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina")) + 
  stat_lineribbon(aes(fill = Species, alpha = after_stat(level))) + 
  scale_alpha_manual(name = "Confidence", values = c(.05, .3, .55)) + 
  theme_classic(base_size = 12) + 
  facet_wrap(~ Species) + 
  geom_vline(xintercept = 12, linetype = "dashed") + 
  geom_vline(xintercept = 21, linetype = "dashed") + 
  xlab(paste("Temperature (", "\u00B0", "C)", sep = "")) + 
  ylab("Survival Rate")

ggsave(file.path("Demographic_Model_Figures", "Surv_Curve_Lab.pdf"), 
       width = 10, height = 6, units = "in")
