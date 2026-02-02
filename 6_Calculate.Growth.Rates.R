
########## CALCULATE GROWTH RATES ##########

source("Functions.R")

library(tidyverse)
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

cage_dat <- read_csv(file.path("Outputs", "clean.cage.dat.csv"))
egg_dat <- read_csv(file.path("Outputs", "clean.egg.dat.csv"))
temp_dat <- read_csv(file.path("Outputs", "clean.temp.csv"))

growth_mod <- readRDS(file.path("Outputs", "growth.mod.RDS"))
surv_mod <- readRDS(file.path("Outputs", "surv.mod.RDS"))
fec_mod <- readRDS(file.path("Outputs", "fec.mod.RDS"))

survey_counts <- read_xlsx(file.path("Data", "Range-Shift Community Survey Data - FINAL.xlsx"), 
                           sheet = "WhelkLength")

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

# Calculating growth rates ------------------------------------------------

output_df <- data.frame()
for(si in sites){
  for(s in c("As", "M")){
    for(te in 1:3){
      te_val = case_when(
        te == 1 ~ min(temp_dat$lower, na.rm = TRUE),
        te == 2 ~ mean(temp_dat$mean, na.rm = TRUE), 
        te == 3 ~ max(temp_dat$upper, na.rm = TRUE)
      )
      
      f_temp = f.df %>% 
        ungroup() %>%
        filter(Whelk_Sp == s & 
                 Site == si) %>%
        select(.epred) %>% unlist()
      
      growth0_temp <- growth0.df %>%
        ungroup() %>%
        filter(species == s & site == si) %>%
        filter(species == s & site == si) %>%
        select(.linpred) %>% unlist()
      
      growth1_temp <- growth1.df %>%
        select(beta) %>% unlist()
      
      surv_temp <- surv_df %>%
        ungroup() %>%
        filter(Species == s & temp == te_val) %>%
        filter(Species == s) %>%
        select(survival_rate) %>% unlist()
      
      IPM <- make_IPM_array(n_bin = 100, min_size = min(cage_dat$shell_width, na.rm = TRUE), 
                      max_size = max(cage_dat$shell_width, na.rm = TRUE), 
                      eggs_per_whelk = f_temp, 
                      larvae_per_egg = ifelse(s == "As", 30, 25.5), 
                      recruitment = rexp(2000, 1000), 
                      growth0 = growth0_temp, 
                      growth1 = growth1_temp, 
                      growth_shape = growth_shape1,
                      survival = surv_temp)
      
      row_temp <- apply(IPM, 3, get_lambda) %>%
        Re() %>%
        median_qi() %>%
        mutate(sp = s, site = si, temp = te)
      
      output_df <- output_df %>% rbind(row_temp)
    }
  }
}

write.csv(output_df, file.path("Outputs","lambdas.csv"))

output_df %>%
  ggplot(aes(y = log(y), ymin = log(ymin), ymax = log(ymax), x = site, 
             alpha = as.factor(temp), color = sp)) + 
  geom_pointrange(position = position_dodge(width = 1)) + 
  theme_classic() + ylab("Log Population Growth Rate") + xlab("Site") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5)) + 
  scale_alpha_manual(name = "Temperature",
                     values = c(.1, .4, 1),
                     labels = c("Low", "Med", "High")) + 
  scale_color_manual(name = "Species", 
                     values = c("#087E8B", "#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))

ggsave(file.path("Population_Model_Figures", "growth_rates.pdf"), 
       units = "in", width = 8, height = 6)

# Compare with empirical growth rates -------------------------------------

# Estimated v.s. Empirical Growth Rates (with error bars)

survey_counts %>%
  mutate(year = year(Survey_Date)) %>%
  group_by(Site, Species, year) %>%
  count() %>%
  filter(Species %in% c("As", "M")) %>%
  arrange(Site, Species, year) %>%
  group_by(Site, Species) %>%
  mutate(last_year_n = lag(n)) %>%
  filter(!is.na(last_year_n)) %>%
  mutate(lambda = log(n/last_year_n)) %>%
  left_join(output_df %>%
          rename("Site" = "site", "Species" = "sp") %>%
            filter(temp == 2)) %>%
  ggplot(aes(x = log(y), xmin = log(ymin), xmax = log(ymax), y = lambda, color = Species)) + 
  geom_point(size = 2) + geom_errorbarh(linewidth = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  xlab("Mean Estimated Growth Rate") + ylab("Empirical Growth Rate") + 
  theme_classic(base_size = 15) + 
  scale_color_manual(name = "Species", 
                     values = c("#087E8B", "#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))

ggsave(file.path("Population_Model_Figures", "predicted_vs_empirical1.pdf"), 
       units = "in", width = 8, height = 6)

# Estimated v.s. Empirical Growth Rates (with lines connecting species per site)

survey_counts %>%
  mutate(year = year(Survey_Date)) %>%
  group_by(Site, Species, year) %>%
  count() %>%
  filter(Species %in% c("As", "M")) %>%
  arrange(Site, Species, year) %>%
  group_by(Site, Species) %>%
  mutate(last_year_n = lag(n)) %>%
  filter(!is.na(last_year_n)) %>%
  mutate(lambda = log(n/last_year_n)) %>%
  left_join(output_df %>%
              rename("Site" = "site", "Species" = "sp") %>%
              filter(temp == 3)) %>%
  ggplot(aes(x = log(y), y = lambda)) + 
  geom_point(aes(color = Species), size = 2) + geom_line(aes(goup = Site), linewidth = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  xlab("Mean Estimated Growth Rate") + ylab("Empirical Growth Rate") + 
  theme_classic(base_size = 15) + 
  scale_color_manual(name = "Species", 
                     values = c("#087E8B", "#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))

ggsave(file.path("Population_Model_Figures", "predicted_vs_empirical2.pdf"), 
       units = "in", width = 8, height = 6)

# Estimated Growth Rates v.s. Mean Counts (with error bars)

survey_counts %>%
  mutate(year = year(Survey_Date)) %>%
  group_by(Site, Species, year) %>%
  count() %>%
  filter(Species %in% c("As", "M")) %>%
  arrange(Site, Species, year) %>%
  group_by(Site, Species) %>%
  summarise(mean_abund = mean(n)) %>%
  left_join(output_df %>%
              rename("Site" = "site", "Species" = "sp") %>%
              filter(temp == 2)) %>%
  ggplot(aes(x = log(y), xmin = log(ymin), xmax = log(ymax), y = mean_abund, color = Species)) + 
  geom_point(size = 2) + geom_errorbarh(linewidth = 1) + 
  xlab("Mean Estimated Growth Rate") + ylab("Mean Count") + 
  theme_classic(base_size = 15) + 
  scale_color_manual(name = "Species", 
                     values = c("#087E8B", "#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))

ggsave(file.path("Population_Model_Figures", "predicted_vs_abundance1.pdf"), 
       units = "in", width = 8, height = 6)

# Estimated Growth Rates v.s. Mean Counts (with lines connecting species per site)

survey_counts %>%
  mutate(year = year(Survey_Date)) %>%
  group_by(Site, Species, year) %>%
  count() %>%
  filter(Species %in% c("As", "M")) %>%
  arrange(Site, Species, year) %>%
  group_by(Site, Species) %>%
  summarise(mean_abund = mean(n)) %>%
  left_join(output_df %>%
              rename("Site" = "site", "Species" = "sp") %>%
              filter(temp == 2)) %>%
  ggplot(aes(x = log(y), y = mean_abund)) + 
  geom_point(aes(color = Species), size = 2) + geom_line(aes(group = Site), linewidth = 1) + 
  xlab("Mean Estimated Growth Rate") + ylab("Mean Count") + 
  theme_classic(base_size = 15) + 
  scale_color_manual(name = "Species", 
                     values = c("#087E8B", "#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))

ggsave(file.path("Population_Model_Figures", "predicted_vs_abundance2.pdf"), 
       units = "in", width = 8, height = 6)
