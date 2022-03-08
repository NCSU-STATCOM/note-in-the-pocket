
# Data pre-processing
# by Alvin Sheng

#####

library(here)
library(tidyverse)

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
# instead of tracking the clothing count, track the number of girls/boys/men/women served
# Also, omit the Emergency Clothing Events

# checking that each row in fact only corresponds to one person

orders2019_df <- readRDS("intermediary_data/orders2019_df.rds")
orders2020_df <- readRDS("intermediary_data/orders2020_df.rds")
orders2021_df <- readRDS("intermediary_data/orders2021_df.rds")
orders_df = rbind(orders2019_df, orders2020_df, orders2021_df[orders2021_df$`Date Received` < as.Date("2021-10-01"), ])

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
# to orders2020_df.rds (non-aggregated versions)

orders_allyears_df <- orders_df %>% mutate(female_small = (rowSums(orders_df[girl_clothes], na.rm = T) != 0), 
                                           male_small = (rowSums(orders_df[boy_clothes], na.rm = T) != 0), 
                                           female_large = (rowSums(orders_df[woman_clothes], na.rm = T) != 0), 
                                           male_large = (rowSums(orders_df[man_clothes], na.rm = T) != 0))

saveRDS(orders_allyears_df, file = "intermediary_data/orders_allyears_df.rds")



# re-do the aggregation, this time summing up the four new columns, 
# IGNORING the Emergency Clothing Events (and Clothing Exchanges) and the adults

orders_allyears_df <- readRDS(here("intermediary_data/orders_allyears_df.rds"))

orders_allyears_df <- orders_allyears_df[!(orders_allyears_df$POC %in% c("Clothing Exchange", "Emergency Clothing Events")), ]
orders_allyears_df <- orders_allyears_df[orders_allyears_df$adult != 1 | is.na(orders_allyears_df$adult), ] # in 2019 data, NA counts as not adult


# aggregated by day by summing up any count variable
# and taking the earliest Date Filled date

orders_allyears_agg_received <- orders_allyears_df %>% 
  group_by(`Date Received`) %>%
  summarise(earliest_filled = min(`Date Filled`, na.rm = T), across(9:37, sum, na.rm = T)) # adjusted the indexing because grouping variable is not included within summarise

# checking specific dates
# colSums(orders_allyears_df[orders_allyears_df$`Date Received` == as.Date("2020-12-08"), 14:38])

saveRDS(orders_allyears_agg_received, file = here("intermediary_data/orders_allyears_agg_received.rds"))


