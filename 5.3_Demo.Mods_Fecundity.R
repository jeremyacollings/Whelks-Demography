
########## DEMOGRAPHIC MODELS - FECUNDITY ##########

library(tidyverse)
library(brms)
library(tidybayes)

egg_dat <- read.csv(file.path("Outputs", "clean.egg.dat.csv"))

sp.df <- cbind.data.frame(sp = c("As", "M", "Unk"), code = 1:3)

# Fitting candidate models ------------------------------------------------

# keeping in species + (1|site) + (1|site:species)

mod1 <- egg_dat %>% 
  filter(Whelk_Sp %in% c("As", "M") & 
           Num_Whelks_By_Eggs != "NA" &
           Num_Egg_Capsules != "NA" &
           !is.na(mean_temp)) %>%
  mutate(Num_Whelks_By_Eggs = as.numeric(Num_Whelks_By_Eggs), 
         Num_Egg_Capsules = as.numeric(Num_Egg_Capsules), 
         Num_Whelks_By_Eggs = ifelse(Num_Whelks_By_Eggs == 0, 1, Num_Whelks_By_Eggs)) %>% 
  brm(formula(Num_Egg_Capsules/Num_Whelks_By_Eggs ~ Whelk_Sp + mean_temp + Whelk_Sp*mean_temp + 
                (1|Site) + (1|Site:Whelk_Sp)), 
      data = ., family = "gamma", 
      cores = 4, chains = 4, iter = 2000)

mod2 <- egg_dat %>% 
  filter(Whelk_Sp %in% c("As", "M") & 
           Num_Whelks_By_Eggs != "NA" &
           Num_Egg_Capsules != "NA" &
           !is.na(mean_temp)) %>%
  mutate(Num_Whelks_By_Eggs = as.numeric(Num_Whelks_By_Eggs), 
         Num_Egg_Capsules = as.numeric(Num_Egg_Capsules), 
         Num_Whelks_By_Eggs = ifelse(Num_Whelks_By_Eggs == 0, 1, Num_Whelks_By_Eggs)) %>% 
  brm(formula(Num_Egg_Capsules/Num_Whelks_By_Eggs ~ Whelk_Sp + mean_temp + 
                (1|Site) + (1|Site:Whelk_Sp)), 
      data = ., family = "gamma", 
      cores = 4, chains = 4, iter = 2000)

mod3 <- egg_dat %>% 
  filter(Whelk_Sp %in% c("As", "M") & 
           Num_Whelks_By_Eggs != "NA" &
           Num_Egg_Capsules != "NA" &
           !is.na(mean_temp)) %>%
  mutate(Num_Whelks_By_Eggs = as.numeric(Num_Whelks_By_Eggs), 
         Num_Egg_Capsules = as.numeric(Num_Egg_Capsules), 
         Num_Whelks_By_Eggs = ifelse(Num_Whelks_By_Eggs == 0, 1, Num_Whelks_By_Eggs)) %>% 
  brm(formula(Num_Egg_Capsules/Num_Whelks_By_Eggs ~ Whelk_Sp + 
                (1|Site) + (1|Site:Whelk_Sp)), 
      data = ., family = "gamma", 
      cores = 4, chains = 4, iter = 2000)

loo_compare(loo(mod1), loo(mod2), loo(mod3))

# 2, 3, - 1

# because temperature doesn't matter... what if we fit without excluding temp data

mod4 <- egg_dat %>% 
  filter(Whelk_Sp %in% c("As", "M") & 
           Num_Whelks_By_Eggs != "NA" &
           Num_Egg_Capsules != "NA") %>%
  mutate(Num_Whelks_By_Eggs = as.numeric(Num_Whelks_By_Eggs), 
         Num_Egg_Capsules = as.numeric(Num_Egg_Capsules), 
         Num_Whelks_By_Eggs = ifelse(Num_Whelks_By_Eggs == 0, 1, Num_Whelks_By_Eggs)) %>% 
  brm(formula(Num_Egg_Capsules/Num_Whelks_By_Eggs ~ Whelk_Sp + 
                (1|Site) + (1|Site:Whelk_Sp)), 
      data = ., family = "gamma", 
      cores = 4, chains = 4, iter = 2000)

saveRDS(mod4, file = file.path("Outputs", "fec.mod.RDS"))

# Figures -----------------------------------------------------------------

mod4 %>%
  epred_draws(newdata = expand.grid(Site = sites, 
                                    Num_Whelks_By_Eggs = 1, 
                                    Whelk_Sp = c("As", "M")), 
              allow_new_levels = TRUE, 
              re_formula = ~ (1|Site:Whelk_Sp)) %>%
  mutate(fec = case_when(
    Whelk_Sp == "As" ~ .epred * 30, 
    Whelk_Sp == "M" ~ .epred * 25.5
  )) %>%
  mutate(data = case_when(
    Whelk_Sp == "As" ~ Site %in% egg_dat$Site[which(egg_dat$Whelk_Sp == "As")], 
    Whelk_Sp == "M" ~ Site %in% egg_dat$Site[which(egg_dat$Whelk_Sp == "M")], 
  )) %>%
  ggplot(aes(x = Site, y = log(fec), color = Whelk_Sp, alpha = data)) + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"),
                     labels = c("Acanthinucella", "Mexacanthina")) +
  stat_pointinterval(position = position_dodge(width = .5)) + theme_classic(base_size = 15) + 
  xlab("Site") + ylab("log Fecundity (embryos/individual)") + 
  theme(axis.text.x = element_text(size = 10, angle = 300, hjust = 0, vjust = .5))

ggsave(file.path("Demographic_Model_Figures", "fec1.pdf"), 
       units = "in", width = 8, height = 6)

mod4 %>%
  epred_draws(newdata = expand.grid(Site = sites, 
                                    Num_Whelks_By_Eggs = 1, 
                                    Whelk_Sp = c("As", "M")), 
              allow_new_levels = TRUE, 
              re_formula = ~ (1|Site:Whelk_Sp)) %>%
  mutate(fec = case_when(
    Whelk_Sp == "As" ~ .epred * 30, 
    Whelk_Sp == "M" ~ .epred * 25.5
  )) %>%
  mutate(data = case_when(
    Whelk_Sp == "As" ~ Site %in% egg_dat$Site[which(egg_dat$Whelk_Sp == "As")], 
    Whelk_Sp == "M" ~ Site %in% egg_dat$Site[which(egg_dat$Whelk_Sp == "M")], 
  )) %>%
  ggplot(aes(x = Site, y = log(fec), color = Whelk_Sp, alpha = data)) + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"),
                     labels = c("Acanthinucella", "Mexacanthina")) +
  stat_pointinterval(position = position_dodge(width = .5)) + theme_classic(base_size = 15) + 
  xlab("Site") + ylab("Fecundity (embryos/individual)") + 
  theme(axis.text.x = element_text(size = 10, angle = 300, hjust = 0, vjust = .5))

ggsave(file.path("Demographic_Model_Figures", "fec2.pdf"), 
       units = "in", width = 8, height = 6)


