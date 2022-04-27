
# Data pre-processing
# by Alvin Sheng and Yinqiao Wang

#####

library(here)
library(tidyverse)
library(tidyr)
library(data.table)

orders2020_df <- readRDS(here("intermediary_data/orders2020_df.rds"))

# aggregated by day by summing up any count variable
# and taking the earliest Date Filled date

orders2020_agg_received <- orders2020_df %>% 
  group_by(`Date Received`) %>%
  summarise(earliest_filled = min(`Date Filled`, na.rm = T), across(9:33, sum, na.rm = T)) # adjusted the indexing because grouping variable is not included within summarise
  
# checking specific dates
# colSums(orders2020_df[orders2020_df$`Date Received` == as.Date("2020-12-08"), 14:34])

saveRDS(orders2020_agg_received, file = here("intermediary_data/orders2020_agg_received.rds"))



orders2019_agg <- readRDS(here("intermediary_data/orders2019_agg_received.rds"))
orders2020_agg <- readRDS(here("intermediary_data/orders2020_agg_received.rds"))
orders2021_agg <- readRDS(here("intermediary_data/orders2021_agg_received.rds"))

# Combine all datasets for 2019 up to 2021 September (ensuring all orders have been filled)
orders_allyears_agg_received <- rbind(orders2019_agg, orders2020_agg, orders2021_agg[orders2021_agg$`Date Received` < as.Date("2021-10-01"), ])

saveRDS(orders_allyears_agg_received, file = here("intermediary_data/orders_allyears_agg_received.rds"))



#####

# Data pre-processing after 2/25 meeting with Note in the Pocket: 

# checking that each row in fact only corresponds to one person

orders2019_df <- readRDS("intermediary_data/orders2019_df.rds")
orders2020_df <- readRDS("intermediary_data/orders2020_df.rds")
orders2021_df <- readRDS("intermediary_data/orders2021_df.rds")
orders_df = rbind(orders2019_df, orders2020_df, orders2021_df[orders2021_df$`Date Received` < as.Date("2021-10-01"), ])

# Zero padding of all the received dates
# Note: for the above time range, there are 990 unique days
orders_df <- orders_df %>% 
  mutate(`Date Received` = as.Date(`Date Received`)) %>%
  mutate(`Date Filled` = as.Date(`Date Filled`)) %>%
  complete(`Date Received` = seq.Date(min(`Date Received`), max(`Date Received`), by = "day"))

# instead of tracking the clothing count, track the number of girls/boys/men/women served

girl_clothes <- c("gt", "gb", "gu", "gs", "gc")
boy_clothes <- c("bt", "bb", "bu", "bs", "bc")
woman_clothes <- c("wt", "wb", "wu", "ws", "wc")
man_clothes <- c("mt", "mb", "mu", "ms", "mc")

num_persons <- (rowSums(orders_df[girl_clothes], na.rm = T) != 0) + (rowSums(orders_df[boy_clothes], na.rm = T) != 0) + (rowSums(orders_df[woman_clothes], na.rm = T) != 0) + (rowSums(orders_df[man_clothes], na.rm = T) != 0)
table(num_persons)

# For zero persons served: if Date Filled is NA, it's probably listed as "withdrawn" in the original excel file.

# View(orders_df[num_persons != 1,])
# View(orders_df[num_persons == 0,])



# add four indicator columns: female_small (girl), male_small (boy), female_large (woman), male_large (man)
orders_allyears_df <- orders_df %>% mutate(female_small = (rowSums(orders_df[girl_clothes], na.rm = T) != 0), 
                                           male_small = (rowSums(orders_df[boy_clothes], na.rm = T) != 0), 
                                           female_large = (rowSums(orders_df[woman_clothes], na.rm = T) != 0), 
                                           male_large = (rowSums(orders_df[man_clothes], na.rm = T) != 0))

saveRDS(orders_allyears_df, file = "intermediary_data/orders_allyears_df.rds")



# do the aggregation, this time summing up the four new columns, 
# IGNORING the Emergency Clothing Events (and Clothing Exchanges) and the adults

orders_allyears_df <- readRDS(here("intermediary_data/orders_allyears_df.rds"))

orders_allyears_df <- orders_allyears_df[!(orders_allyears_df$POC %in% c("Clothing Exchange", "Emergency Clothing Events")), ]
orders_allyears_df <- orders_allyears_df[orders_allyears_df$adult != 1 | is.na(orders_allyears_df$adult), ] # in 2019 data, NA counts as not adult

# re-doing the zero-padding, in case the the above code removed some days
orders_allyears_df <- orders_allyears_df %>% 
  complete(`Date Received` = seq.Date(min(`Date Received`), max(`Date Received`), by = "day"))

# re-creating the four indicator columns, for the zero-padded rows
orders_allyears_df <- orders_allyears_df %>% mutate(female_small = (rowSums(orders_allyears_df[girl_clothes], na.rm = T) != 0), 
                                           male_small = (rowSums(orders_allyears_df[boy_clothes], na.rm = T) != 0), 
                                           female_large = (rowSums(orders_allyears_df[woman_clothes], na.rm = T) != 0), 
                                           male_large = (rowSums(orders_allyears_df[man_clothes], na.rm = T) != 0))

# # add count of individuals served
# # some rows may have more than one individual (or none). Use the four indicator variables to find true number of individuals in a row
# orders_allyears_df <- orders_allyears_df %>% 
#   group_by(`Date Received`) %>%
#   mutate(`Daily Requests` = n())

# aggregated by day by summing up any count variable
# and taking the earliest Date Filled date
orders_allyears_agg_received <- orders_allyears_df %>% 
  group_by(`Date Received`) %>%
  summarise(earliest_filled = min(`Date Filled`, na.rm = T), 
            latest_filled = max(`Date Filled`, na.rm = T),
            mean_filled = mean(`Date Filled`, na.rm = T),
            mean_lag = mean(`Date Filled` - `Date Received`, na.rm = T),
            num_requests = sum(female_small + male_small + female_large + male_large),
            across(9:37, sum, na.rm = T)) # adjusted the indexing because grouping variable is not included within summarise

# checking specific dates
# colSums(orders_allyears_df[orders_allyears_df$`Date Received` == as.Date("2020-12-08"), 14:38])

saveRDS(orders_allyears_agg_received, file = here("intermediary_data/orders_allyears_agg_received.rds"))



##### 

# Updating the data: aggregate weekly, include Yinqiao's feature engineering

# Re-run the code in the last section, right before the aggregation

girl_clothes <- c("gt", "gb", "gu", "gs", "gc")
boy_clothes <- c("bt", "bb", "bu", "bs", "bc")
woman_clothes <- c("wt", "wb", "wu", "ws", "wc")
man_clothes <- c("mt", "mb", "mu", "ms", "mc")

orders_allyears_df <- readRDS(here("intermediary_data/orders_allyears_df.rds"))

orders_allyears_df <- orders_allyears_df[!(orders_allyears_df$POC %in% c("Clothing Exchange", "Emergency Clothing Events")), ]
orders_allyears_df <- orders_allyears_df[orders_allyears_df$adult != 1 | is.na(orders_allyears_df$adult), ] # in 2019 data, NA counts as not adult

# re-doing the zero-padding, in case the the above code removed some days
orders_allyears_df <- orders_allyears_df %>% 
  complete(`Date Received` = seq.Date(min(`Date Received`), max(`Date Received`), by = "day"))

# re-creating the four indicator columns, for the zero-padded rows
orders_allyears_df <- orders_allyears_df %>% mutate(female_small = (rowSums(orders_allyears_df[girl_clothes], na.rm = T) != 0), 
                                                    male_small = (rowSums(orders_allyears_df[boy_clothes], na.rm = T) != 0), 
                                                    female_large = (rowSums(orders_allyears_df[woman_clothes], na.rm = T) != 0), 
                                                    male_large = (rowSums(orders_allyears_df[man_clothes], na.rm = T) != 0))



# starting it on a Sunday, so a week interval will be Sunday to Saturday (to be consistent with Zoho data) 
# so I omit data prior to "2019/01/20"

orders_allyears_df <- orders_allyears_df[orders_allyears_df$`Date Received` >= as.Date("2019/01/20", "%Y/%m/%d"), ]

weekno <- as.numeric(orders_allyears_df$`Date Received` - as.Date("2019/01/20", "%Y/%m/%d")) %/% 7
orders_allyears_df$start_of_week <- as.Date("2019/01/20", "%Y/%m/%d") + 7 * weekno



# aggregated by start_of_week by summing up any count variable
# and taking the earliest Date Filled date
orders_weekly_agg_received <- orders_allyears_df %>% 
  group_by(start_of_week) %>%
  summarise(earliest_filled = min(`Date Filled`, na.rm = T), 
            latest_filled = max(`Date Filled`, na.rm = T),
            mean_filled = mean(`Date Filled`, na.rm = T),
            mean_lag = mean(`Date Filled` - `Date Received`, na.rm = T),
            num_requests = sum(female_small + male_small + female_large + male_large),
            across(10:38, sum, na.rm = T))

# checking specific dates
# colSums(orders_allyears_df[orders_allyears_df$start_of_week == as.Date("2020-12-06"), 10:38], na.rm = T)



# Now, include some useful features based on Yinqiao's ft engineering 
# (from Feature engineering_new indicators.r)

# instead of being based on `Date Received`, the features are based on start_of_week

orders_weekly_agg_received$Month <- format(as.Date(orders_weekly_agg_received$start_of_week,"%Y/%m/%d"),"%m")

getSeason <- function(Dates){
  Month <- as.numeric(format(as.Date(Dates, "%Y/%m/%d"),"%m"))
  ifelse(Month >= 12 | Month < 03,
         "Winter",
         ifelse(Month >= 03 & Month < 06,
                "Spring",
                ifelse(Month >= 06 & Month < 09,
                       "Summer",
                       "Fall")))
}

orders_weekly_agg_received$Season <- getSeason(as.Date(orders_weekly_agg_received$start_of_week,"%Y/%m/%d"))

# Add indicator for the pre/post Covid status
getCovidStatus <- function(Dates){
  date <- as.Date(Dates,"%Y/%m/%d")
  Covid_date <- as.Date("2020/03/12", "%Y/%m/%d")
  ifelse(date < Covid_date, FALSE, TRUE)
}
orders_weekly_agg_received$Post_covid <- getCovidStatus(orders_weekly_agg_received$start_of_week)

# fixed Yinqiao's code a bit
getSchoolYear <- function(Dates){
  date <- as.Date(Dates,"%Y/%m/%d")
  schoolyear2018_e <- as.Date("2019/06/11", "%Y/%m/%d")
  schoolyear2019_b <- as.Date("2019/08/26", "%Y/%m/%d")
  schoolyear2019_e <- as.Date("2020/06/12", "%Y/%m/%d")
  schoolyear2020_b <- as.Date("2020/08/17", "%Y/%m/%d")
  schoolyear2020_e <- as.Date("2021/06/10", "%Y/%m/%d")
  schoolyear2021_b <- as.Date("2021/08/23", "%Y/%m/%d")
  ifelse(date %between% c(schoolyear2018_e,schoolyear2019_b), FALSE,
         ifelse(date %between% c(schoolyear2019_e,schoolyear2020_b), FALSE,
                ifelse(date %between% c(schoolyear2020_e,schoolyear2021_b), FALSE, TRUE)))
}
orders_weekly_agg_received$School_year <- getSchoolYear(orders_weekly_agg_received$start_of_week)

getFirstWeek <- function(Dates){
  date <- as.Date(Dates,"%Y/%m/%d")
  ifelse(date %between% c("2019/08/25", "2019/08/31"), TRUE,
         ifelse(date %between% c("2020/01/05", "2020/01/11"), TRUE,
                ifelse(date %between% c("2020/08/16", "2020/08/22"), TRUE,
                       ifelse(date %between% c("2021/01/03", "2021/01/09"), TRUE,
                              ifelse(date %between% c("2021/08/22", "2021/08/28"), TRUE, FALSE)))))
}
orders_weekly_agg_received$Firstweek <- getFirstWeek(orders_weekly_agg_received$start_of_week)

# Check the data set
table(orders_weekly_agg_received$Month)
table(orders_weekly_agg_received$Season)
table(orders_weekly_agg_received$Post_covid)
table(orders_weekly_agg_received$School_year)
table(orders_weekly_agg_received$Firstweek)



saveRDS(orders_weekly_agg_received, file = here("intermediary_data/orders_weekly_agg_received.rds"))


