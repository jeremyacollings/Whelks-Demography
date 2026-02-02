
########## COMPILE EGG DATA ##########

library(tidyverse)
library(readxl)

# Bring in data -----------------------------------------------------------

egg_dat22 <- read_excel(file.path("Data", "Range-Shift Community Survey Data - FINAL.xlsx"), 
                        sheet = "EggCount")

egg_dat23 <- read_excel(file.path("Data", "Egg Capsule Data, Spring 2023 - FINAL.xlsx"))

egg_dat <- egg_dat22 %>%
  mutate(Num_Egg_Capsules = as.character(Num_Egg_Capsules)) %>%
  bind_rows(egg_dat23 %>% rename("Whelk_Sp" = "Species") %>%
              mutate(Whelk_Sp = ifelse(Whelk_Sp == "A", "As", Whelk_Sp)))


temp_dat <- read_csv("clean.temp.csv")

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

sp.df <- cbind.data.frame(sp = c("As", "M", "Unk"), code = 1:3)

# Add temperature data ----------------------------------------------------

# taking the mean temperature from the six months preceding the reproductive period

season_start_date <- as.Date("12/1/2022", format = "%m/%d/%Y")
season_end_date <- as.Date("6/1/2023", format = "%m/%d/%Y")

temp_dat %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = site, y = date, color = as.factor(height))) +
  geom_point(position = position_dodge(width = .5)) +  
  xlab("Site") + ylab("Date") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5)) 

ggsave(filename = file.path("Raw_Data_Figures", "temp_logger_timelines_full.pdf"), 
       units = "in", width = 12, height = 6.75)

temp_dat %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = site, y = date, color = as.factor(height))) +
  geom_point(position = position_dodge(width = .5)) +  
  xlab("Site") + ylab("Date") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5)) +
  ylim(season_start_date, season_end_date)

ggsave(filename = file.path("Raw_Data_Figures", "temp_logger_timelines_reduced.pdf"), 
       units = "in", width = 12, height = 6.75)

temp_dat %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(height == 1) %>%
  ggplot(aes(x = date, y = mean, color = site)) + 
  geom_line() + 
  xlab("Date") + ylab("Mean Daily Temp") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = .5)) +
  xlim(season_start_date, season_end_date)

temp_means <- temp_dat %>% 
  filter(date %in% seq.Date(from = season_start_date,
                            to = season_end_date, 
                            by = 1) &
           height == 1) %>%
  group_by(site) %>%
  summarise(mean = mean(mean, na.rm = TRUE), 
            lower = mean(lower, na.rm = TRUE),
            upper = mean(upper, na.rm = TRUE))

egg_dat$mean_temp <- temp_means$mean[match(egg_dat$Site, temp_means$site)]
egg_dat$lower_temp <- temp_means$lower[match(egg_dat$Site, temp_means$site)]
egg_dat$upper_temp <- temp_means$upper[match(egg_dat$Site, temp_means$site)]

# Miscellaneous changes ---------------------------------------------------

# set 40+ to 40
egg_dat$Num_Whelks_By_Eggs[which(egg_dat$Num_Whelks_By_Eggs == "40+")] <- 40
# keep only complete cases
egg_dat2 <- egg_dat[which(!is.na(egg_dat$Num_Egg_Capsules) &
                            egg_dat$Whelk_Sp %in% c("As", "M")),]

egg_dat2$Num_Whelks_By_Eggs <- as.numeric(egg_dat2$Num_Whelks_By_Eggs)

egg_dat2$species2 <- sp.df$code[match(egg_dat2$Whelk_Sp, sp.df$sp)]

egg_dat2 %>% 
  filter(!is.na(mean_temp) & species2 %in% 1:2) %>%
  count()

egg_dat2 %>% 
  filter(species2 %in% 1:2) %>%
  count()

# fix site discrepencancies
unique(egg_dat2$Site) %>% setdiff(canonical_sites)

egg_dat2 <- egg_dat2 %>%
  mutate(Site = case_when(
    Site == "Cape Mendocino" ~ "Mendocino North", 
    Site == "Cape Mendocino South" ~ "Mendocino South", 
    .default = Site
  ))
  
write_csv(egg_dat2, file.path("Outputs", "clean.egg.dat.csv"))
