
# First Exploration of Whelks Data

library(readr)
library(readxl)
library(tidyverse)
library(lme4)
library(brms)
library(MCMCvis)

com_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                      sheet = "CommunityData")
length_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                         sheet = "WhelkLength")
egg_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                      sheet = "EggCount")
tran_dat <- read_excel("Range-Shift Community Survey Data - FINAL.xlsx", 
                       sheet = "Transect")
cage_dat <- read_csv("Cage Data  Whelk Sizes_Growth.csv")
names(cage_dat) <- tolower(names(cage_dat))
names(cage_dat) <- gsub(" ", "_", names(cage_dat))

ggplot(data = length_dat, aes(x = as.numeric(Total_Length), fill = Species)) + 
  geom_histogram(position = "identity", alpha = .6)

table(length_dat$Species, length_dat$Region)
table(egg_dat$Whelk_Sp, egg_dat$Region)
table(cage_dat$Species, cage_dat$Region)

# Are the size distributions different across the sites?

ggplot(data = length_dat[which(length_dat$Species %in% c("M", "As")),], 
       aes(x = as.numeric(Total_Length), fill = Species)) + 
  geom_histogram(position = "identity", alpha = .6) + 
  facet_wrap( ~ Region) 

ggplot(cbind.data.frame(means = as.numeric(tapply(as.numeric(length_dat$Total_Length), 
                                       list(length_dat$Species, 
                                            length_dat$Region), 
                                       mean, na.rm = TRUE)), 
                        lows = as.numeric(tapply(as.numeric(length_dat$Total_Length), 
                                                 list(length_dat$Species, 
                                                      length_dat$Region), 
                                                 mean, na.rm = TRUE)) - 
                          as.numeric(tapply(as.numeric(length_dat$Total_Length), 
                                            list(length_dat$Species, 
                                                 length_dat$Region), 
                                            function(x) sd(x, na.rm = TRUE)/
                                              sqrt(length(x)))), 
                        ups = as.numeric(tapply(as.numeric(length_dat$Total_Length), 
                                                list(length_dat$Species, 
                                                     length_dat$Region), 
                                                mean, na.rm = TRUE)) + 
                          as.numeric(tapply(as.numeric(length_dat$Total_Length), 
                                            list(length_dat$Species, 
                                                 length_dat$Region), 
                                            function(x) sd(x, na.rm = TRUE)/
                                              sqrt(length(x)))), 
                        region = rep(c("Baja", "NorCal", "SoCal"), each = 8), 
                        sp = rep(c("Ap", "As", "M", "Nc", "Ne", "Nl", "Ol", "Rp"), 3)), 
       aes(x = sp, y = means, ymin = lows, ymax = ups, color = region)) + 
  geom_point(position = position_dodge(width = .2)) + 
  geom_errorbar(position = position_dodge(width = .2), width = 0) + 
  theme_classic(base_size = 15)

## Ryan said that we he hasn't looked too much at the cage data.. so let's take a look now

cage_dat$id <- paste(cage_dat$bee_tag_color, cage_dat$bee_tag_number, sep = "")
cage_dat$survey_date <- as.Date(cage_dat$survey_date, format = "%m/%d/%Y")

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

# it really looks like they're barely growing...
# I guess this could be because their growth is so slow (probably past some size)
# Alternatively this could be because their growth happens at a different time of year

# Do the averages increase?? 

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

# I'll need to add a variable for previous shell length

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

# this isn't working... figure out later. 

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

ggplot(cage_dat, aes(x = per_day_growth, fill = species)) + 
  geom_histogram(position = "identity", alpha = .5) + xlim(-.02, 1) + 
  facet_wrap(~ region)

ggplot(cage_dat[which(cage_dat$per_day_growth != Inf),], 
       aes(y = per_day_growth*365, x = prev_length, 
           color = species)) + 
  geom_point(alpha = .2) + ylim(0, 350) + 
  facet_wrap(~ region) + 
  geom_smooth(method = "lm", size = 2) + 
  theme_classic(base_size = 15) + xlab("Previous Length (mm)") + 
  ylab("Growth per Year (mm)")


Mmod1 <- brm(per_day_growth ~ prev_length + region + 
               prev_length*region, 
            data = cage_dat[which(cage_dat$species == "M" & 
                                    cage_dat$per_day_growth != Inf & 
                                    cage_dat$id != "Yellow19"),])

Amod1 <- brm(per_day_growth ~ prev_length + region + 
               prev_length*region, 
             data = cage_dat[which(cage_dat$species == "As" & 
                                     cage_dat$per_day_growth != Inf & 
                                     cage_dat$id != "Yellow19"),])


Mmod1_posts <- posterior_samples(Mmod1)
Amod1_posts <- posterior_samples(Amod1)

hist(Mmod1_posts$b_Intercept - Amod1_posts$b_Intercept) 
# M has higher baseline growthrate than As

ggplot(data = cbind.data.frame(meds = apply(Mmod1_posts[,1:4], 2, median),
                               lows = apply(Mmod1_posts[,1:4], 2, quantile, 0.025),
                               ups = apply(Mmod1_posts[,1:4], 2, quantile, 0.975), 
                               pars = names(apply(Mmod1_posts[,1:4], 2, median))), 
       aes(x = pars, y = meds, ymin = lows, ymax = ups)) + 
  geom_point() + geom_errorbar(width = 0)

# integrating with community data

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
prey_dat$site_year <- paste(prey_dat$sites, prey_dat$years, sep = "-")
cage_dat$year <- year(cage_dat$prev_date)
cage_dat$site_year <- paste(cage_dat$site, cage_dat$year, sep = "-")
cage_dat$mussel <- prey_dat$mussel_abund[match(cage_dat$site_year, prey_dat$site_year)]
cage_dat$barnacle <- prey_dat$barnacle_abund[match(cage_dat$site_year, prey_dat$site_year)]

pairs(cage_dat[which(cage_dat$species == "M"),
               c("per_day_growth", "mussel", "barnacle", "prev_length")])

pairs(cage_dat[which(cage_dat$species == "As" & cage_dat$id != "Yellow19"),
               c("per_day_growth", "mussel", "barnacle", "prev_length")])

Mmod2 <- brm(per_day_growth ~ prev_length + region + mussel, 
             data = cage_dat[which(cage_dat$species == "M" & 
                                     cage_dat$per_day_growth != Inf & 
                                     cage_dat$id != "Yellow19"),])

MCMCtrace(Mmod2)

ggplot(data = cage_dat[which(cage_dat$species == "M" & 
                               cage_dat$per_day_growth != Inf & 
                               cage_dat$id != "Yellow19"),], 
       aes(y = per_day_growth, x = prev_length, color = "region")) + 
  geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)

ggplot(data = cage_dat[which(cage_dat$species == "M" & 
                               cage_dat$per_day_growth != Inf & 
                               cage_dat$id != "Yellow19"),], 
       aes(y = per_day_growth, x = mussel, color = region)) + 
  geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)

ggplot(data = cage_dat[which(cage_dat$species == "M" & 
                               cage_dat$per_day_growth != Inf & 
                               cage_dat$id != "Yellow19"),], 
       aes(y = per_day_growth, x = barnacle, color = region)) + 
  geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)

cov2cor(cov(cage_dat[which(cage_dat$species == "M"),
             c("per_day_growth", "mussel", "barnacle", "prev_length")], 
    use = "na.or.complete"))

# what if I try total food and then the barnacle to mussel ratio

prey_dat$total <- as.numeric(tapply(com_dat$tot_food, 
                                    list(com_dat$Site, com_dat$year), 
                                    mean, na.rm = TRUE))
prey_dat$Mprop <- as.numeric(tapply(com_dat$mussel_prop, 
                                   list(com_dat$Site, com_dat$year), 
                                   mean, na.rm = TRUE))

cage_dat$total <- prey_dat$total[match(cage_dat$site_year, prey_dat$site_year)]
cage_dat$Mprop <- prey_dat$Mprop[match(cage_dat$site_year, prey_dat$site_year)]


cov2cor(cov(cage_dat[which(cage_dat$species == "As"),
                     c("per_day_growth", "total", "Mprop", "prev_length")], 
            use = "na.or.complete"))

Mmod2 <- brm(per_day_growth ~ prev_length + region + total + Mprop, 
             data = cage_dat[which(cage_dat$species == "M" & 
                                     cage_dat$per_day_growth != Inf & 
                                     cage_dat$id != "Yellow19"),])

Amod2 <- brm(per_day_growth ~ prev_length + region + total + Mprop, 
             data = cage_dat[which(cage_dat$species == "As" & 
                                     cage_dat$per_day_growth != Inf & 
                                     cage_dat$id != "Yellow19"),])

# maybe not enough data to fit full Acanthaneucella model

ggplot(data = cage_dat, aes(x = region, y = total)) + 
  geom_point()
