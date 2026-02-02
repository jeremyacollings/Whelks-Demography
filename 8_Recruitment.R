
########## RECRUITMENT ANALYSIS ##########

source("Functions.R")

library(tidyverse)
library(tidybayes)

set.seed(6)


cage_dat <- read_csv(file.path("Outputs", "clean.cage.dat.csv"))
egg_dat <- read_csv(file.path("Outputs", "clean.egg.dat.csv"))
temp_dat <- read_csv(file.path("Outputs", "clean.temp.csv"))

growth_mod <- readRDS(file.path("Outputs", "growth.mod.RDS"))
surv_mod <- readRDS(file.path("Outputs", "surv.mod.RDS"))
fec_mod <- readRDS(file.path("Outputs", "fec.mod.RDS"))

survey_counts <- read_xlsx(file.path("Data", "Range-Shift Community Survey Data - FINAL.xlsx"), 
                           sheet = "WhelkLength")

# Prepping data for IPM ---------------------------------------------------

# just homing in on species differences... 
# marginalizing across site and using mean temperature from cage data

# fecundity rate predictions (eggs_per_whelk)
f.df <- fec_mod %>% 
  epred_draws(newdata = expand.grid(Site = "fake_site", 
                                    Whelk_Sp = c("As", "M")), 
              allow_new_levels = TRUE, 
              re_formula = ~ (1|Site:Whelk_Sp))

# growth when size = 0 predictions on log scale (intercept for linear predictor; growth0)
growth0.df <- growth_mod %>%
  linpred_draws(newdata = expand.grid(site = "fake_site", 
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
  cbind(temp = mean(temp_dat$mean, na.rm = TRUE)) %>%
  mutate(survival_rate = plogis(int + beta*temp))


surv_df <- surv_mod %>%
  linpred_draws(newdata = expand.grid(temp = mean(temp_dat$mean, na.rm = TRUE),
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


# Calculating growth rates along recruitment gradient ---------------------

output_df <- data.frame()
for(s in c("As", "M")){
  for(r in seq(0.0001, 1, 0.0001)){
    f_temp = f.df %>% 
      ungroup() %>%
      filter(Whelk_Sp == s) %>%
      select(.epred) %>% unlist()
    
    growth0_temp <- growth0.df %>%
      ungroup() %>%
      filter(species == s) %>%
      filter(species == s) %>%
      select(.linpred) %>% unlist()
    
    growth1_temp <- growth1.df %>%
      select(beta) %>% unlist()
    
    surv_temp <- surv_df %>%
      ungroup() %>%
      filter(Species == s) %>%
      filter(Species == s) %>%
      select(survival_rate) %>% unlist()
    
    IPM <- make_IPM_array(n_bin = 100, min_size = min(cage_dat$shell_width, na.rm = TRUE), 
                    max_size = max(cage_dat$shell_width, na.rm = TRUE),
                    eggs_per_whelk = mean(f_temp), 
                    larvae_per_egg = ifelse(s == "As", 30, 25.5), 
                    recruitment = r, 
                    growth0 = growth0_temp, 
                    growth1 = growth1_temp, 
                    growth_shape = growth_shape1,
                    survival = surv_temp)
    
    row_temp <- apply(IPM, 3, get_lambda) %>%
      Re() %>%
      median_qi() %>%
      mutate(sp = s, recruit = r)
    
    output_df <- output_df %>% rbind(row_temp)
  }
}
