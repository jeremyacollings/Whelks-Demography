
########## COMPILE CAGE DATA ##########

library(tidyverse)

# Bring in data -----------------------------------------------------------

cage_dat <- read_csv(file.path("Data", "Cage Data  Whelk Sizes_Growth.csv"))
com_dat <- read_excel(file.path("Data", "Range-Shift Community Survey Data - FINAL.xlsx"), 
                      sheet = "CommunityData")

names(cage_dat) <- tolower(names(cage_dat))
names(cage_dat) <- gsub(" ", "_", names(cage_dat))

# make unique individual IDs
cage_dat$id <- paste(cage_dat$bee_tag_color, cage_dat$bee_tag_number, sep = "")
cage_dat$survey_date <- as.Date(cage_dat$survey_date, format = "%m/%d/%Y")

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


# Fix site names ----------------------------------------------------------

cage_dat <- cage_dat %>%
  ungroup() %>%
  mutate(site = case_when(
    site == "Cape Mendocino" ~ "Mendocino North", 
    site == "Cape Mendocino South" ~ "Mendocino South", 
    .default = site
  ))


# Get lagged values -------------------------------------------------------

cage_dat <- cage_dat[
  with(cage_dat, order(id, survey_date)),
]

cage_dat <- cage_dat %>%
  group_by(id) %>%
  mutate(prev_length = lag(shell_length), 
         prev_date = lag(survey_date))

cage_dat <- cage_dat %>%
  mutate(date_diff = as.numeric(survey_date - prev_date),
         size_diff = shell_length - prev_length) %>%
  mutate(per_day_growth = size_diff/date_diff)

cage_dat$per_day_growth <- ifelse(cage_dat$per_day_growth < 0, 0, 
                                  cage_dat$per_day_growth)

# get mortality status based on whether death was recorded in notes
cage_dat$mort <- grepl("dea", cage_dat$notes)

# Incorporate prey data ---------------------------------------------------

com_dat$year <- year(com_dat$Survey_Date)
com_dat$year_season <- paste(com_dat$Season, com_dat$year, sep = "-")
com_dat$Acorn_Barnacle_Balanus_Chthamalus[is.na(com_dat$Acorn_Barnacle_Balanus_Chthamalus)] <- 0
com_dat$California_Mussel_M._californianus[is.na(com_dat$California_Mussel_M._californianus)] <- 0
com_dat$tot_food <- com_dat$Acorn_Barnacle_Balanus_Chthamalus + com_dat$California_Mussel_M._californianus
com_dat$mussel_prop <- com_dat$California_Mussel_M._californianus/com_dat$tot_food

month(cage_dat$prev_date)
apply(com_dat[,which(grepl("Barn", names(com_dat)))], 2, 
      function(x) sum(x > 0, na.rm = TRUE))

apply(com_dat[,which(grepl("Muss", names(com_dat)))], 2, 
      function(x) sum(x > 0, na.rm = TRUE))

com_dat$Site[which(com_dat$Site == "Campo kennedy")] <- "Campo Kennedy"
com_dat$Site[which(com_dat$Site == "Mendocino North")] <- "Cape Mendocino"
com_dat$Site[which(com_dat$Site == "Mendocino Sount")] <- "Cape Mendocino South"

# subsetting community data to tide heights relevant to each species
com_datA <- com_dat[which(com_dat$Quad_TH_m > 0.25 &
                            com_dat$Quad_TH_m < 0.75),]
com_datM <- com_dat[which(com_dat$Quad_TH_m > 0.75 &
                            com_dat$Quad_TH_m < 1.25),]

prey_datA <- cbind.data.frame(mussel_abund = as.numeric(tapply(com_datA$California_Mussel_M._californianus, 
                                                               list(com_datA$Site, com_datA$year), 
                                                               mean, na.rm = TRUE)), 
                              barnacle_abund = as.numeric(tapply(com_datA$Acorn_Barnacle_Balanus_Chthamalus, 
                                                                 list(com_datA$Site, com_datA$year), 
                                                                 mean, na.rm = TRUE)), 
                              sites = rep(sort(unique(com_dat$Site)), 2), 
                              years = rep(c(2022, 2023), each = length(sort(unique(com_dat$Site)))))

prey_datM <- cbind.data.frame(mussel_abund = as.numeric(tapply(com_datM$California_Mussel_M._californianus, 
                                                               list(com_datM$Site, com_datM$year), 
                                                               mean, na.rm = TRUE)), 
                              barnacle_abund = as.numeric(tapply(com_datM$Acorn_Barnacle_Balanus_Chthamalus, 
                                                                 list(com_datM$Site, com_datM$year), 
                                                                 mean, na.rm = TRUE)), 
                              sites = rep(sort(unique(com_dat$Site)), 2), 
                              years = rep(c(2022, 2023), each = length(sort(unique(com_dat$Site)))))

prey_datA$total <- as.numeric(tapply(com_datA$tot_food, 
                                     list(com_datA$Site, com_datA$year), 
                                     mean, na.rm = TRUE))

prey_datM$total <- as.numeric(tapply(com_datM$tot_food, 
                                     list(com_datM$Site, com_datM$year), 
                                     mean, na.rm = TRUE))

prey_datA$Mprop <- as.numeric(tapply(com_datA$mussel_prop, 
                                     list(com_datA$Site, com_datA$year), 
                                     mean, na.rm = TRUE))

prey_datM$Mprop <- as.numeric(tapply(com_datM$mussel_prop, 
                                     list(com_datM$Site, com_datM$year), 
                                     mean, na.rm = TRUE))

prey_datA$sites[which(prey_datA$sites == "Mendocino South")] <- "Cape Mendocino South"
prey_datA$site_year <- paste(prey_datA$sites, prey_datA$years, sep = "-")
prey_datM$sites[which(prey_datM$sites == "Mendocino South")] <- "Cape Mendocino South"
prey_datM$site_year <- paste(prey_datM$sites, prey_datM$years, sep = "-")

cage_dat$year <- year(cage_dat$prev_date)
cage_dat$site_year <- paste(cage_dat$site, cage_dat$year, sep = "-")
cage_dat$mussel[which(cage_dat$species == "As")] <- 
  prey_datA$mussel_abund[match(cage_dat$site_year[which(cage_dat$species == "As")], 
                               prey_datA$site_year)]
cage_dat$barnacle[which(cage_dat$species == "As")] <- 
  prey_datA$barnacle_abund[match(cage_dat$site_year[which(cage_dat$species == "As")], 
                                 prey_datA$site_year)]

cage_dat$mussel[which(cage_dat$species == "M")] <- 
  prey_datM$mussel_abund[match(cage_dat$site_year[which(cage_dat$species == "M")], 
                               prey_datM$site_year)]
cage_dat$barnacle[which(cage_dat$species == "M")] <- 
  prey_datM$barnacle_abund[match(cage_dat$site_year[which(cage_dat$species == "M")], 
                                 prey_datM$site_year)]

cage_dat$total[which(cage_dat$species == "As")] <- 
  prey_datA$total[match(cage_dat$site_year[which(cage_dat$species == "As")], 
                        prey_datA$site_year)]
cage_dat$total[which(cage_dat$species == "M")] <- 
  prey_datM$total[match(cage_dat$site_year[which(cage_dat$species == "M")], 
                        prey_datM$site_year)]
cage_dat$Mprop[which(cage_dat$species == "As")] <- 
  prey_datA$Mprop[match(cage_dat$site_year[which(cage_dat$species == "As")], 
                        prey_datA$site_year)]
cage_dat$Mprop[which(cage_dat$species == "M")] <- 
  prey_datM$Mprop[match(cage_dat$site_year[which(cage_dat$species == "M")], 
                        prey_datM$site_year)]

# Incorporating temperature data ------------------------------------------

cage_dat$mean_temp <- cage_dat$low_temp <- cage_dat$high_temp <- NA
for(i in 1:nrow(cage_dat)){
  start_date <- cage_dat$prev_date[i]
  end_date <- cage_dat$survey_date[i]
  sp <- cage_dat$species[i]
  si <- cage_dat$site[i]
  
  if(is.na(start_date)){
    cage_dat$mean_temp[i] <- cage_dat$low_temp[i] <- cage_dat$high_temp[i] <- NA
  }
  else{
    if(sp == "As"){
      cage_dat$mean_temp[i] <- temp_dat %>%
        filter(
          site == si,
          date %in% seq.Date(from = start_date, to = end_date, by = 1),
          height == if(si %in% c("Scripps", "Dana Point")) 0.5 else 1.0
        ) %>%
        pull(mean) %>%
        mean()
      
      cage_dat$low_temp[i] <- temp_dat %>%
        filter(
          site == si,
          date %in% seq.Date(from = start_date, to = end_date, by = 1),
          height == if(si %in% c("Scripps", "Dana Point")) 0.5 else 1.0
        ) %>%
        pull(lower) %>%
        mean()
      
      cage_dat$high_temp[i] <- temp_dat %>%
        filter(
          site == si,
          date %in% seq.Date(from = start_date, to = end_date, by = 1),
          height == if(si %in% c("Scripps", "Dana Point")) 0.5 else 1.0
        ) %>%
        pull(upper) %>%
        mean()
    }
    if(sp == "M"){
      cage_dat$mean_temp[i] <-temp_dat %>%
        filter(
          site == si,
          date %in% seq.Date(from = start_date, to = end_date, by = 1),
          height == 1.0
        ) %>%
        pull(mean) %>%
        mean()
      
      cage_dat$low_temp[i] <- temp_dat %>%
        filter(
          site == si,
          date %in% seq.Date(from = start_date, to = end_date, by = 1),
          height == 1.0
        ) %>%
        pull(lower) %>%
        mean()
      
      cage_dat$high_temp[i] <- temp_dat %>%
        filter(
          site == si,
          date %in% seq.Date(from = start_date, to = end_date, by = 1),
          height == 1.0
        ) %>%
        pull(upper) %>%
        mean()
    }
  }
}

# Categorizing historic v.s. expanded range -------------------------------

range_dat <- cbind.data.frame(site = c("Mendocino North", "Mendocino South", 
                                       "Dana Point", "Scripps", 
                                       "Punta Morro", "Campo Kennedy"), 
                              mex = c(NA, NA, "E", "E", "H", "H"), 
                              acan = c("E", "E", "H", "H", NA, NA))

cage_dat$range[which(cage_dat$species == "As")] <- 
  range_dat$acan[match(cage_dat$site[which(cage_dat$species == "As")], 
                       range_dat$site)]

cage_dat$range[which(cage_dat$species == "M")] <- 
  range_dat$mex[match(cage_dat$site[which(cage_dat$species == "M")], 
                      range_dat$site)]

write_csv(cage_dat, file.path("Outputs", "clean.cage.dat.csv"))



