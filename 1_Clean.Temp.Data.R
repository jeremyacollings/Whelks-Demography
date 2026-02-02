
########## CLEAN TEMPERATURE DATA ##########

library(tidyverse)

# Bring in temp data ------------------------------------------------------

# temperature files are stores as a list of .csv files from the loggers
# one per site per tidal height

temp_files <- list.files(file.path("Data", "temp_dat"))
temp_dat <- lapply(file.path("Data", "temp_dat", temp_files), read.csv)
names(temp_dat) <- sub("\\.csv$", "", temp_files)

# vector of standardized site names

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

# Make big dataframe ------------------------------------------------------

# get all the .csv files into one big dataframe
temp_df <- lapply(temp_dat, function(x){
  if(ncol(x) == 3){ # some have date-time column
    temp_df <- x[,-1]
    names(temp_df) <- c("date_time", "temp")
    temp_df <- separate(temp_df, date_time, c("date", "time"), sep = " ")
    temp_df
  }
  else{
    temp_df <- x[,-1]
    names(temp_df) <- c("date", "time", "temp")
    temp_df
  }
}) %>%
  bind_rows(.id = "label")

temp_df <- temp_df %>%
  mutate(site = sub("_.*", "", label), # extract site from file name
         height = sub(".*_", "", label), # extract tidal height from file name
         height = sub("m", "", height)) %>%
  group_by(site, height, date) %>%
  summarise(mean = mean(temp, na.rm = TRUE), # calculate daily summary stats
            lower = quantile(temp, 0.025, na.rm = TRUE), 
            upper = quantile(temp, 0.975, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(date_fixed = case_when( # Saldamando has weird year typos
    grepl("/202$", date) ~ sub("/202$", "/2022", date),
    TRUE ~ date
  )) %>%
  mutate(date = case_when( # convert to date, fixing weird La Chorera formatting
    site == "LaChorerra" & height %in% c("0.5", "1.5") ~ as.Date(date_fixed, format = "%m/%d/%y"),
    .default = as.Date(date_fixed, format = "%m/%d/%Y")
  )) %>%
  select(-date_fixed)  # Remove temporary column

temp_df <- temp_df %>%
  ungroup() %>%
  mutate(site = case_when( # fix any site name discrepencies 
    site == "CampoKennedy" ~ "Campo Kennedy", 
    site == "CapeMendocino" ~ "Mendocino North", 
    site == "CapeMendocinoSouth" ~ "Mendocino South", 
    site == "Crystal" ~ "Crystal Cove", 
    site == "Dana" ~ "Dana Point", 
    site == "Goff"~ "Goff Island", 
    site == "Heisler" ~ "Heisler Park", 
    site == "LaChorerra" ~ "La Chorera", 
    site == "Little" ~ "Little Corona", 
    site == "MoatCreek" ~ "Moat Creek", 
    site == "PuntaMorro" ~ "Punta Morro", 
    site == "SanMiguel" ~ "San Miguel", 
    site == "Shaws" ~ "Shaw's Cove", 
    site == "Swamis" ~ "Swami's", 
    site == "Victoria" ~ "Victoria Beach", 
    .default = site
  ))

write.csv(temp_df, file.path("Outputs","clean.temp.csv"))
