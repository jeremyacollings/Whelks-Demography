
########## DEMOGRAPHIC MODELS - GROWTH ##########

library(tidyverse)
library(brms)
library(tidybayes)

set.seed(6)

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

cage_dat <- read.csv(file.path("Outputs", "clean.cage.dat.csv"))

sp.df <- cbind.data.frame(sp = c("As", "M", "Unk"), code = 1:3)

# keep only entries with previous length measurement
cage_dat2 <- cage_dat[which(!is.na(cage_dat$prev_length) &
                              cage_dat$per_day_growth != Inf &
                              cage_dat$per_day_growth < 2),]

cage_dat2$species2 <- sp.df$code[match(cage_dat2$species, sp.df$sp)]

cage_dat2$per_day_growth2 <- ifelse(cage_dat2$per_day_growth == 0, 
                                    cage_dat2$per_day_growth + 1*10^-8, 
                                    cage_dat2$per_day_growth)

# Fitting candidate models ------------------------------------------------

# keeping in species + (1|id) + (1|site) + (1|site:species)

mod1 <- cage_dat2 %>%
  filter(species %in% c("As", "M") &
           !is.na(prev_length) & !is.na(mean_temp) &
           !is.na(id) & !is.na(site)) %>%
  brm(formula(per_day_growth2 ~ prev_length + species + mean_temp + 
                prev_length:species + mean_temp:species + 
                (1|id) + (1|site) + (1|site:species)), 
      data = ., cores = 4, chains = 4, iter = 2000, 
      family = "gamma")

mod2 <- cage_dat2 %>%
  filter(species %in% c("As", "M") &
           !is.na(prev_length) & !is.na(mean_temp) &
           !is.na(id) & !is.na(site)) %>%
  brm(formula(per_day_growth2 ~ prev_length + species + mean_temp + 
                prev_length:species + 
                (1|id) + (1|site) + (1|site:species)), 
      data = ., cores = 4, chains = 4, iter = 2000, 
      family = "gamma")

mod3 <- cage_dat2 %>%
  filter(species %in% c("As", "M") &
           !is.na(prev_length) & !is.na(mean_temp) &
           !is.na(id) & !is.na(site)) %>%
  brm(formula(per_day_growth2 ~ prev_length + species + mean_temp + 
                mean_temp:species + 
                (1|id) + (1|site) + (1|site:species)), 
      data = ., cores = 4, chains = 4, iter = 2000, 
      family = "gamma")

mod4 <- cage_dat2 %>%
  filter(species %in% c("As", "M") &
           !is.na(prev_length) & !is.na(mean_temp) &
           !is.na(id) & !is.na(site)) %>%
  brm(formula(per_day_growth2 ~ prev_length + species + 
                (1|id) + (1|site) + (1|site:species)), 
      data = ., cores = 4, chains = 4, iter = 2000, 
      family = "gamma")

mod5 <- cage_dat2 %>%
  filter(species %in% c("As", "M") &
           !is.na(prev_length) & !is.na(mean_temp) &
           !is.na(id) & !is.na(site)) %>%
  brm(formula(per_day_growth2 ~ species + mean_temp + 
                (1|id) + (1|site) + (1|site:species)), 
      data = ., cores = 4, chains = 4, iter = 2000, 
      family = "gamma")

mod6 <- cage_dat2 %>%
  filter(species %in% c("As", "M") &
           !is.na(prev_length) & !is.na(mean_temp) &
           !is.na(id) & !is.na(site)) %>%
  brm(formula(per_day_growth2 ~ species + 
                (1|id) + (1|site) + (1|site:species)), 
      data = ., cores = 4, chains = 4, iter = 2000, 
      family = "gamma")

# Model comparison --------------------------------------------------------

loo_compare(loo(mod1), loo(mod2),
            loo(mod3), loo(mod4), 
            loo(mod5), loo(mod6))

# 4, - 3, 2, 1, -- 6, 5

saveRDS(mod4, file = file.path("Outputs", "growth.mod.RDS"))

# Figures -----------------------------------------------------------------

mod4 %>%
  epred_draws(., newdata = expand.grid(species = c("As", "M"), 
                                       prev_length = c(mean(cage_dat2$shell_length)), 
                                       mean_temp = c(min(cage_dat2$mean_temp, na.rm = TRUE), 
                                                     max(cage_dat2$mean_temp, na.rm = TRUE)), 
                                       site = sites), 
              re_formula = ~ (1|site:species), 
              allow_new_levels = TRUE) %>%
  mutate(mean_temp = case_when(
    mean_temp == min(cage_dat2$mean_temp, na.rm = TRUE) ~ "low", 
    mean_temp == max(cage_dat2$mean_temp, na.rm = TRUE) ~ "high"
  )) %>%
  ggplot(aes(x = site, y = .epred, color = species)) + 
  stat_pointinterval() + 
  theme_classic(base_size = 15) + xlab("Site") + ylab("Expected Growth Rate (mm/day)") + 
  # ylim(0, .04) + 
  scale_color_manual(name = "Species", values = c("#087E8B", "#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina")) + 
  theme(axis.text.x = element_text(size = 10, angle = 300, hjust = 0, vjust = .5))

ggsave(file.path("Demographic_Model_Figures", "Growth_Plot1.pdf"), width = 10, height = 6, units = "in")

