
### Compile Cage Experiment Data

library(readxl)
library(tidyverse)

# Bring in data -----------------------------------------------------------

cage_dat <- read_csv("~/Documents/Data/Whelks/Cage Data  Whelk Sizes_Growth.csv")
com_dat <- read_excel("~/Documents/Data/Whelks/Range-Shift Community Survey Data - FINAL.xlsx", 
                      sheet = "CommunityData")

temp_files <- list.files("~/Documents/Data/Whelks/temp_dat")
temp_dat <- lapply(paste("~/Documents/Data/Whelks/temp_dat", temp_files, sep = "/"), read.csv)

names(cage_dat) <- tolower(names(cage_dat))
names(cage_dat) <- gsub(" ", "_", names(cage_dat))

cage_dat$id <- paste(cage_dat$bee_tag_color, cage_dat$bee_tag_number, sep = "")
cage_dat$survey_date <- as.Date(cage_dat$survey_date, format = "%m/%d/%Y")

# Get lagged values -------------------------------------------------------

j = sample(1:nrow(cage_dat), 1)
cage_dat$id[j]
temp_dates <- sort(cage_dat$survey_date[which(cage_dat$id == cage_dat$id[j])])
temp_dates[which(temp_dates == cage_dat$survey_date[j])-1]

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

prey_dat <- cbind.data.frame(mussel_abund = as.numeric(tapply(com_dat$California_Mussel_M._californianus, 
                                                              list(com_dat$Site, com_dat$year), 
                                                              mean, na.rm = TRUE)), 
                             barnacle_abund = as.numeric(tapply(com_dat$Acorn_Barnacle_Balanus_Chthamalus, 
                                                                list(com_dat$Site, com_dat$year), 
                                                                mean, na.rm = TRUE)),
                             sites = rep(sort(unique(com_dat$Site)), 2), 
                             years = rep(c(2022, 2023), each = length(sort(unique(com_dat$Site)))))
prey_dat$sites[which(prey_dat$sites == "Mendocino South")] <- "Cape Mendocino South"
prey_dat$site_year <- paste(prey_dat$sites, prey_dat$years, sep = "-")
cage_dat$year <- year(cage_dat$prev_date)
cage_dat$site_year <- paste(cage_dat$site, cage_dat$year, sep = "-")
cage_dat$mussel <- prey_dat$mussel_abund[match(cage_dat$site_year, prey_dat$site_year)]
cage_dat$barnacle <- prey_dat$barnacle_abund[match(cage_dat$site_year, prey_dat$site_year)]

prey_dat$total <- as.numeric(tapply(com_dat$tot_food, 
                                    list(com_dat$Site, com_dat$year), 
                                    mean, na.rm = TRUE))
prey_dat$Mprop <- as.numeric(tapply(com_dat$mussel_prop, 
                                    list(com_dat$Site, com_dat$year), 
                                    mean, na.rm = TRUE))

cage_dat$total <- prey_dat$total[match(cage_dat$site_year, prey_dat$site_year)]
cage_dat$Mprop <- prey_dat$Mprop[match(cage_dat$site_year, prey_dat$site_year)]

# Incorporate temperature data --------------------------------------------


temp_dat2 <- lapply(temp_dat, function(df) {
  if (ncol(df) == 3) {
    # Convert the second column to Date
    df[, 2] <- as.Date(df[, 2], format = "%m/%d/%Y")
    # Rename columns
    names(df) <- c("x", "date", "temp")
  }
  else {
    names(df) <- c("x", "date", "time", "temp")
  }
  return(df)
})

mean_temps <- lapply(temp_dat2, function(x) tapply(x$temp, x$date, 
                                                   mean, na.rm = TRUE))

low_temps <- lapply(temp_dat2, function(x) tapply(x$temp, x$date, 
                                                  min, na.rm = TRUE))

high_temps <- lapply(temp_dat2, function(x) tapply(x$temp, x$date, 
                                                   max, na.rm = TRUE))

names(mean_temps) <- names(low_temps) <- names(high_temps) <- sub("\\.csv$", "", temp_files)

# looks like some sites have logs at different tide heights... 
# but all sites have readings at 1.0... so let's just use those

mean_temps2 <- mean_temps[which(grepl("1\\.0", names(mean_temps)))]
low_temps2 <- low_temps[which(grepl("1\\.0", names(low_temps)))]
high_temps2 <- high_temps[which(grepl("1\\.0", names(high_temps)))]

names(mean_temps2) <- c("Cabrillo", "Campo Kennedy", "Cape Mendocino",
                        "Cape Mendocino South", "Cardiff", "Crystal Cove", 
                        "Dana Point", "Goff Island", "Heisler", 
                        "La Chorerra", "Little Corona", "Moat Creek", 
                        "Punta Morro", "Saldamando", "San Miguel", 
                        "Scripps", "Shaws", "Swamis", "Victoria Beach")

# not sure why Scripps is formatted incorrectly...
new_dates <- strptime(names(mean_temps2$Scripps), format = "%Y-%m-%d")
names(mean_temps2$Scripps) <- format(new_dates, format = "%m/%d/%Y")

mean_temp_range <- c()
for(i in 1:nrow(cage_dat)){
  site = cage_dat$site[i]
  start = as.Date(cage_dat$prev_date[i])
  stop = as.Date(cage_dat$survey_date[i])
  if(is.na(start)){
    mean_temp_range <- c(mean_temp_range, NA)
  }
  else{
    dates <- as.Date(names(mean_temps2[[site]]), format = "%m/%d/%Y")
    mean_temp <- mean(mean_temps2[[site]][which(dates %in% 
                                                  seq(from = start, 
                                                      to = stop, 
                                                      by = 1))])
    mean_temp_range <- c(mean_temp_range, mean_temp)
  }
}

cage_dat$mean_temp <- mean_temp_range

