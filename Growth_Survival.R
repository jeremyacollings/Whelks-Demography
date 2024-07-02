
##### WHELK GROWTH & SURVIVAL MODELS #####

library(readr)
library(readxl)
library(tidyverse)
library(lme4)
library(brms)
library(rstan)
library(MCMCvis)
library(mice)

# Reading in Data ---------------------------------------------------------

com_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                      sheet = "CommunityData")
length_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                         sheet = "WhelkLength")
egg_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                      sheet = "EggCount")
tran_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                       sheet = "Transect")
cage_dat <- read_csv("Cage Data  Whelk Sizes_Growth.csv")

temp_files <- list.files("temp_dat")
temp_dat <- lapply(paste("temp_dat", temp_files, sep = "/"), read.csv)

names(cage_dat) <- tolower(names(cage_dat))
names(cage_dat) <- gsub(" ", "_", names(cage_dat))

cage_dat$id <- paste(cage_dat$bee_tag_color, cage_dat$bee_tag_number, sep = "")
cage_dat$survey_date <- as.Date(cage_dat$survey_date, format = "%m/%d/%Y")

# Descriptives ------------------------------------------------------------

ggplot(data = length_dat, aes(x = as.numeric(Total_Length), fill = Species)) + 
  geom_histogram(position = "identity", alpha = .6)

table(length_dat$Species, length_dat$Region)
table(egg_dat$Whelk_Sp, egg_dat$Region)
table(cage_dat$Species, cage_dat$Region)

ggplot(data = length_dat[which(length_dat$Species %in% c("M", "As")),], 
       aes(x = as.numeric(Total_Length), fill = Species)) + 
  geom_histogram(position = "identity", alpha = .6) + 
  facet_wrap( ~ Region) 

ggplot(data = cage_dat, aes(x = survey_date, y = shell_length, 
                            group = id)) + 
  geom_line() + facet_wrap(species ~ region) + 
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = cage_dat, aes(x = survey_date, y = shell_width, 
                            group = id)) + 
  geom_line() + facet_wrap(species ~ region, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ylim(0, 25)

ids <- vals <- c()
for(i in unique(cage_dat$id)){
  ids <- c(ids, i)
  temp <- cage_dat[which(cage_dat$id == i), ]
  vals <- c(vals, temp$shell_length[which(temp$survey_date == 
                                            min(temp$survey_date))] <
              temp$shell_length[which(temp$survey_date == 
                                        max(temp$survey_date))])
}

tapply(cage_dat$shell_length, list(cage_dat$site, week(cage_dat$survey_date)),
       mean, na.rm = TRUE)

summary(lmer(shell_length ~ survey_date + (1|id), data = cage_dat))
# per day growth rate: 0.002076
0.002076*365 # = 0.75774
# how long would it take for the smallest recorded whelk to grow to the 
# size of the largest recorded whelk
max(cage_dat$shell_length, na.rm = TRUE) - min(cage_dat$shell_length, na.rm = TRUE)/
  0.75774
# 30.8 years...

# just thought... maybe size dependent growth rate really matters here
# it does kind of look like smallest ones (Mexicanthina in Baja)
# might be growing faster... check this out? 

summary(lmer(shell_length ~ survey_date*region + (1|id), data = cage_dat))
# it does look like Baja has the fastest growth rate... at about 0.006552
0.006552*365 # = 2.39


# Cleaning Data -----------------------------------------------------------

cage_dat$survey_date[which(cage_dat$id == i)]

j = sample(1:nrow(cage_dat), 1)
cage_dat$id[j]
temp_dates <- sort(cage_dat$survey_date[which(cage_dat$id == cage_dat$id[j])])
temp_dates[which(temp_dates == cage_dat$survey_date[j])-1]

cage_dat$prev_dat <- NA
for(i in 1:nrow(cage_dat)){
  temp_dates <- sort(cage_dat$survey_date[which(cage_dat$id == cage_dat$id[i])])
  cage_dat$prev_dat[i] <- ifelse(length(temp_dates[which(temp_dates == cage_dat$survey_date[j])-1]) == 1, 
                                 as.Date(temp_dates[which(temp_dates == cage_dat$survey_date[j])-1]), NA)
}

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

# temperature data ...

# so all of the csv's are read into a list
# the csvs aren't all in the same format
# some have 3 columns, with one being date-time
# some have 4 columns, with date and time contained in different columns
# also, names of columns aren't all the same

# first, let's get dates and times all formatted the same
# next, we can summarize data into daily means, mins, and maxs
# then we can aggregate daily data however we need to for other data

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


# Fitting Models ----------------------------------------------------------

# some helpful distributions only have support for positive real numbers...
# to account for 0's, lets create a new variable

cage_dat$per_day_growth2 <- ifelse(cage_dat$per_day_growth == 0, 
                                   cage_dat$per_day_growth + 1*10^-100, 
                                   cage_dat$per_day_growth)

# now let's run some models

M_global <- brm(per_day_growth2 ~ prev_length + mean_temp + total + Mprop + 
                  (1|site), 
                data = cage_dat[which(cage_dat$per_day_growth != Inf &
                                        cage_dat$id != "Yellow19", 
                                      cage_dat$species == "M"),],
                chains = 2, cores = 2, 
                prior = c(set_prior("exponential(1)", class = "sd"), 
                          set_prior("exponential(1)", class = "sd", group = "site"), 
                          set_prior("normal(0,.25)", class = "Intercept"),
                          set_prior("normal(0,.25)", class = "b", coef = "prev_length"), 
                          set_prior("normal(0,.25)", class = "b", coef = "mean_temp"), 
                          set_prior("normal(0,.25)", class = "b", coef = "total"), 
                          set_prior("normal(0,.25)", class = "b", coef = "Mprop")),
                control = list(adapt_delta = .95))

coef_dat <- as.data.frame(fixef(M_global)*365.25)
coef_dat$coef <- rownames(coef_dat)
names(coef_dat) <- c("est", "err", "low", "up", "coef")

# Visualizing Output ------------------------------------------------------

ggplot(data = coef_dat, aes(x = coef, y = est, ymin = low, ymax = up)) + 
  geom_pointrange() + theme_classic(base_size = 15) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Parameter") + ylab("Estimate") + 
  scale_x_discrete(labels = c("Intercept", "Temperature", "Mussel Prop.", 
                             "Prev. Length", "Total Food"))

ggplot(data = coef_dat[which(coef_dat$coef %in% 
                               c("prev_length", "mean_temp", "total")),], 
       aes(x = coef, y = est, ymin = low, ymax = up)) + 
  geom_pointrange() + theme_classic(base_size = 15) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Parameter") + ylab("Estimate") + 
  scale_x_discrete(labels = c("Temperature", "Prev. Length", "Total Food"))

