library(here)
library(tidyverse)

orders2020_df <- readRDS(here("intermediary_data/orders2020_df.rds"))



# aggregated by day by summing up any count variable
# and taking the earliest Date Filled date

orders2020_agg_received <- orders2020_df %>% 
  group_by(`Date Received`) %>%
  summarise(earliest_filled = min(`Date Filled`), across(9:33, sum)) # adjusted the indexing because grouping variable is not included within summarise
  
# checking specific dates
# colSums(orders2020_df[orders2020_df$`Date Received` == as.Date("2020-12-08"), 14:34])

saveRDS(orders2020_agg_received, file = here("intermediary_data/orders2020_agg_received.rds"))
