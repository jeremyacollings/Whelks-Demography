
########## SENSITIVITY ANALYSIS ##########

########## CALCULATE GROWTH RATES ##########

source("Functions.R")

library(tidyverse)

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

cage_dat <- read_csv(file.path("Outputs", "clean.cage.dat.csv"))
egg_dat <- read_csv(file.path("Outputs", "clean.egg.dat.csv"))
temp_dat <- read_csv(file.path("Outputs", "clean.temp.csv"))

growth_mod <- readRDS(file.path("Outputs", "growth.mod.RDS"))
surv_mod <- readRDS(file.path("Outputs", "surv.mod.RDS"))
fec_mod <- readRDS(file.path("Outputs", "fec.mod.RDS"))


# Prepping data for IPM ---------------------------------------------------

# fecundity rate predictions (eggs_per_whelk)
f.df <- fec_mod %>% 
  epred_draws(newdata = expand.grid(Site = sites, 
                                    Whelk_Sp = c("As", "M")), 
              allow_new_levels = TRUE, 
              re_formula = ~ (1|Site:Whelk_Sp))

# growth when size = 0 predictions on log scale (intercept for linear predictor; growth0)
growth0.df <- growth_mod %>%
  linpred_draws(newdata = expand.grid(site = sites, 
                                      species = c("As", "M"), 
                                      prev_length = 0, 
                                      id = "new"), 
                allow_new_levels = TRUE, 
                re_formula = ~ (1|site:species))

# growth sensitivity to size predictions (growth1)
growth1.df <- growth_mod %>% 
  spread_draws(`b_prev_length`) %>%
  rename("beta" = "b_prev_length") %>%
  select(.draw, beta)

# survival rates (survival)
surv.df1 <- surv_mod %>% 
  spread_draws(`Intercept`, `b_SpeciesMexacanthina`) %>%
  mutate(As = Intercept, 
         M = Intercept + b_SpeciesMexacanthina) %>%
  pivot_longer(cols = c("As", "M"), values_to = "int", names_to = "sp") %>%
  select(.draw, sp, int)

surv.df2 <- surv_mod %>% 
  spread_draws(`b_temp`) %>%
  rename("beta" = "b_temp") %>%
  select(.draw, beta)

surv_df <- merge(surv.df1, surv.df2, by = c(".draw")) %>%
  cbind(temp = rep(c(min(temp_dat$lower, na.rm = TRUE), 
                     mean(temp_dat$mean, na.rm = TRUE), 
                     max(temp_dat$upper, na.rm = TRUE)), each = nrow(.))) %>%
  mutate(survival_rate = plogis(int + beta*temp))


surv_df <- surv_mod %>%
  linpred_draws(newdata = expand.grid(temp = c(min(temp_dat$lower, na.rm = TRUE), 
                                               mean(temp_dat$mean, na.rm = TRUE), 
                                               max(temp_dat$upper, na.rm = TRUE)),
                                      Species = c("Acanthinucella", "Mexacanthina")), 
                allow_new_levels = TRUE) %>%
  mutate(survival_rate = 1 - plogis(.linpred))  %>%
  mutate(Species = case_when(
    Species == "Acanthinucella" ~ "As", 
    Species == "Mexacanthina" ~ "M"
  ))

growth_shape1 <- growth_mod %>%
  spread_draws(shape) %>%
  select(shape) %>% unlist()

# Making IPM

# Acanthinucella

f_temp <- f.df %>%
  filter(Whelk_Sp == "As") %>%
  pull(.epred) %>%
  median()

lpe <- 30

recruitment <- 1/1000

growth0_temp <- growth0.df %>%
  ungroup() %>%
  filter(species == "As") %>%
  pull(.linpred) %>% median()

growth1_temp <- growth1.df %>%
  pull(beta) %>% median() 

surv_temp <- surv_df %>%
  ungroup() %>%
  filter(Species == "As") %>%
  pull(survival_rate) %>% median()

growth_shape1 <- growth_mod %>%
  spread_draws(shape) %>%
  pull(shape) %>% median()

IPM_As <- make_IPM(n_bin = 100, min_size = min(cage_dat$shell_width, na.rm = TRUE), 
                      max_size = max(cage_dat$shell_width, na.rm = TRUE), 
                      eggs_per_whelk = f_temp, 
                      larvae_per_egg = lpe, 
                      recruitment = recruitment, 
                      growth0 = growth0_temp, 
                      growth1 = growth1_temp, 
                      growth_shape = growth_shape1,
                      survival = surv_temp)

IPM_As %>%
  as.data.frame() %>%
  mutate(to = 1:n()) %>%
  pivot_longer(cols = 1:(ncol(.)-1), names_to = "from", values_to = "p_transition") %>%
  mutate(from = as.numeric(sub("V", "", from))) %>%
  ggplot(aes(x = from, y = to, fill = pseudo_log(p_transition))) + 
  geom_tile() + theme_classic(base_size = 15) + 
  ylab("Size (t+1)") + xlab("Size (t)") + 
  scale_y_reverse()

popbio::elasticity(IPM_As) %>%
  as.data.frame() %>%
  mutate(from = 1:n()) %>%
  pivot_longer(cols = 1:(ncol(.)-1), names_to = "to", values_to = "elasticity") %>%
  mutate(to = as.numeric(sub("V", "", to))) %>%
  mutate(transition = case_when(
    to == 1 ~ "reproduction", 
    .default = "growth/survival"
  )) %>%
  group_by(transition) %>%
  summarise(proportion = sum(elasticity))

emat <- elasticity(IPM_As)
sum(emat)

# Mexacanthina 

f_temp <- f.df %>%
  filter(Whelk_Sp == "M") %>%
  pull(.epred) %>%
  median()

lpe <- 25.5

recruitment <- 1/1000

growth0_temp <- growth0.df %>%
  ungroup() %>%
  filter(species == "M") %>%
  pull(.linpred) %>% median()

growth1_temp <- growth1.df %>%
  pull(beta) %>% median() 

surv_temp <- surv_df %>%
  ungroup() %>%
  filter(Species == "M") %>%
  pull(survival_rate) %>% median()

growth_shape1 <- growth_mod %>%
  spread_draws(shape) %>%
  pull(shape) %>% median()

IPM_M <- make_IPM(n_bin = 100, min_size = min(cage_dat$shell_width, na.rm = TRUE), 
                   max_size = max(cage_dat$shell_width, na.rm = TRUE), 
                   eggs_per_whelk = f_temp, 
                   larvae_per_egg = lpe, 
                   recruitment = recruitment, 
                   growth0 = growth0_temp, 
                   growth1 = growth1_temp, 
                   growth_shape = growth_shape1,
                   survival = surv_temp)

popbio::elasticity(IPM_As) %>%
  as.data.frame() %>%
  mutate(from = 1:n()) %>%
  pivot_longer(cols = 1:(ncol(.)-1), names_to = "to", values_to = "elasticity") %>%
  mutate(to = as.numeric(sub("V", "", to))) %>%
  mutate(transition = case_when(
    to == 1 ~ "reproduction", 
    .default = "growth/survival"
  )) %>%
  group_by(transition) %>%
  summarise(proportion = sum(elasticity)) %>%
  bind_rows(popbio::elasticity(IPM_M) %>%
              as.data.frame() %>%
              mutate(from = 1:n()) %>%
              pivot_longer(cols = 1:(ncol(.)-1), names_to = "to", values_to = "elasticity") %>%
              mutate(to = as.numeric(sub("V", "", to))) %>%
              mutate(transition = case_when(
                to == 1 ~ "reproduction", 
                .default = "growth/survival"
              )) %>%
              group_by(transition) %>%
              summarise(proportion = sum(elasticity)), 
            .id = "species") %>%
  mutate(species = case_when(
    species == 1 ~ "Acanthinucella", 
    species == 2 ~ "Mexacanthina"
  )) %>%
  ggplot(aes(x = "", y = proportion, fill = transition)) +
  scale_fill_manual(name = "Process", 
                   values = c("#FF8811","#087E8B"), 
                   labels = c("Growth/Survival", "Reproduction")) + 
  geom_col() +
  coord_polar(theta = "y") + 
  theme_classic(base_size = 15) + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.line = element_blank()) + 
  facet_wrap(~ species)

ggsave(file.path("Population_Model_Figures", "elasticity_pies.pdf"), 
       units = "in", width = 8, height = 6)
