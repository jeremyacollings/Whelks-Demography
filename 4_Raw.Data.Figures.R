
########## RAW DATA FIGURES ##########

library(tidyverse)

cage_dat <- read_csv(file.path("Outputs", "clean.cage.dat.csv"))
egg_dat <- read_csv(file.path("Outputs", "clean.egg.dat.csv"))
exp_df <- read.csv(file.path("Data", "TT.csv"))
temp_dat <- read_csv(file.path("Outputs", "clean.temp.csv"))


survey_counts <- read_xlsx(file.path("Data", "Range-Shift Community Survey Data - FINAL.xlsx"), 
                           sheet = "WhelkLength")

canonical_sites <- c("Mendocino North", "Mendocino South", 
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

# Individual growth figures -----------------------------------------------

# growth by species

cage_dat %>%
  filter(is.finite(per_day_growth)) %>%
  filter(per_day_growth < 4) %>% # one odd observation with high per day growth
  ggplot(aes(x = species, y = per_day_growth)) + 
  geom_jitter() + geom_violin(fill = NA) + 
  theme_classic(base_size = 15) + 
  scale_x_discrete(labels = c("Acanthinucella", "Mexacanthina")) + 
  xlab("Species") + ylab("Per Day Growth (mm/day)")

ggsave(file.path("Raw_Data_Figures", "growth_by_species.pdf"), 
       width = 10, height = 6, units = "in")

# growth by site

cage_dat %>%
  filter(is.finite(per_day_growth)) %>%
  filter(per_day_growth < 4) %>% # one odd observation with high per day growth
  mutate(site = factor(site, levels = canonical_sites)) %>%
  ggplot(aes(x = site, color = species, y = per_day_growth)) + 
  geom_jitter(position = position_jitterdodge(jitter.width = .5, 
                                              dodge.width = 1), 
              ) + geom_violin(fill = NA) + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina")) + 
  theme_classic(base_size = 15) + 
  xlab("Site") + ylab("Per Day Growth (mm/day)") + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5))

ggsave(file.path("Raw_Data_Figures", "growth_by_site.pdf"), 
       width = 10, height = 6, units = "in")

# growth by historic v.s. expanded

cage_dat %>%
  filter(is.finite(per_day_growth)) %>%
  filter(per_day_growth < 4) %>%
  ggplot(aes(x = range, y = per_day_growth, 
             color = species)) + 
  geom_jitter(position = position_jitterdodge(jitter.width = .5, 
                                              dodge.width = 1), 
  ) + geom_violin(fill = NA) + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina")) + 
  theme_classic(base_size = 15) + 
  xlab("Range Status") + ylab("Per Day Growth (mm/day)") + 
  scale_x_discrete(labels = c("Expanded", "Historic")) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5))

ggsave(file.path("Raw_Data_Figures", "growth_by_range.pdf"), 
       width = 10, height = 6, units = "in")

# growth by temperature

cage_dat %>%
  filter(is.finite(per_day_growth)) %>%
  filter(per_day_growth < 4) %>%
  ggplot(aes(x = mean_temp, y = per_day_growth, 
             color = species)) + 
  geom_point(alpha = .25) + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))  + 
  geom_smooth(method = "lm") + 
  theme_classic(base_size = 15)

ggsave(file.path("Raw_Data_Figures", "growth_by_temp.pdf"), 
       width = 10, height = 6, units = "in")

# by previous size

cage_dat %>%
  filter(is.finite(per_day_growth)) %>%
  filter(per_day_growth < 4) %>%
  ggplot(aes(x = prev_length, y = per_day_growth, 
             color = species)) + 
  geom_point(alpha = .25) + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))  + 
  geom_smooth(method = "lm") + 
  theme_classic(base_size = 15)

ggsave(file.path("Raw_Data_Figures", "growth_by_size.pdf"), 
       width = 10, height = 6, units = "in")

# Survival figures --------------------------------------------------------

# cage data: mortality by species

cage_dat %>%
  ggplot(aes(x = species, y = mort)) + 
  geom_jitter() + 
  theme_classic(base_size = 15) + 
  xlab("Species") + ylab("Mortality") + 
  scale_x_discrete(labels = c("Acanthinucella", "Mexacanthina"))

ggsave(file.path("Raw_Data_Figures", "cage_survival_by_species.pdf"), 
       width = 10, height = 6, units = "in")

# not a lot there...

# lab data: survival by species

exp_df %>%
  filter(Species %in% c("Acanthinucella", "Mexacanthina")) %>%
  ggplot(aes(x = Species, y = Survival)) + 
  geom_jitter() + 
  theme_classic(base_size = 15) + 
  xlab("Species") + ylab("Survival")

ggsave(file.path("Raw_Data_Figures", "lab_survival_by_species.pdf"), 
       width = 10, height = 6, units = "in")

# lab data: survival by temp

exp_df %>%
  filter(Species %in% c("Acanthinucella", "Mexacanthina")) %>%
  ggplot(aes(x = Treatment, y = Survival, color = Species)) + 
  geom_jitter() + 
  theme_classic(base_size = 15) + 
  xlab("Temperature") + ylab("Survival") + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))  + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ggsave(file.path("Raw_Data_Figures", "lab_survival_by_temp.pdf"), 
       width = 10, height = 6, units = "in")

# Fecundity figures -------------------------------------------------------

# egg capsules/adult whelk by species

egg_dat %>%
  filter(Num_Whelks_By_Eggs > 0) %>%
  ggplot(aes(x = Whelk_Sp, y = Num_Egg_Capsules/Num_Whelks_By_Eggs)) + 
  geom_jitter() + 
  theme_classic(base_size = 15) + 
  xlab("Species") + ylab("Number of Egg Capsules per Adult Whelk") + 
  scale_x_discrete(labels = c("Acanthinucella", "Mexacanthina"))

ggsave(file.path("Raw_Data_Figures", "fec_by_species.pdf"), 
       width = 10, height = 6, units = "in")

# egg capsules/adult whelk by site

egg_dat %>%
  mutate(Site = factor(Site, levels = canonical_sites)) %>%
  filter(Num_Whelks_By_Eggs > 0) %>%
  ggplot(aes(x = Site, y = Num_Egg_Capsules/Num_Whelks_By_Eggs)) + 
  geom_jitter() + 
  theme_classic(base_size = 15) + 
  xlab("Site") + ylab("Number of Egg Capsules per Adult Whelk") + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5))

ggsave(file.path("Raw_Data_Figures", "fec_by_site.pdf"), 
       width = 10, height = 6, units = "in")

# egg capsules/adult whelk v.s. temperature

egg_dat %>%
  filter(Num_Whelks_By_Eggs > 0) %>%
  ggplot(aes(x = mean_temp, y = Num_Egg_Capsules/Num_Whelks_By_Eggs, 
             color = Whelk_Sp)) + 
  geom_jitter() + 
  theme_classic(base_size = 15) + 
  xlab("Mean Temperature") + ylab("Number of Egg Capsules per Adult Whelk") + 
  scale_color_manual(name = "Species", values = c("#087E8B","#FF8811"), 
                     labels = c("Acanthinucella", "Mexacanthina"))  + 
  geom_smooth(method = "lm") + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5))

ggsave(file.path("Raw_Data_Figures", "fec_by_temp.pdf"), 
       width = 10, height = 6, units = "in")

# Survey count figures ----------------------------------------------------


