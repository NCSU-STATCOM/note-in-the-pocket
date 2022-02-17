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


