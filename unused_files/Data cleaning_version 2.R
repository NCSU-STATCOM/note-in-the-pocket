###########################################################################################################
library(dplyr)
library(here)
library(tidyr)

# Read in data sets
orders2019_df <- readRDS(here("intermediary_data/orders2019_df.rds"))
orders2020_df <- readRDS(here("intermediary_data/orders2020_df.rds"))
orders2021_df <- readRDS(here("intermediary_data/orders2021_df.rds"))

# Combine all data sets for 2019 up to 2021 September (ensuring all orders have been filled)
orders_allyears_df <- rbind(orders2019_df, orders2020_df, orders2021_df[orders2021_df$`Date Received` < as.Date("2021-10-01"), ])

# Filled all the missing received date
# Note: put this with aggregation code in data_preprocessing. Also turn Date Filled into a Date
orders_allyears_df <- orders_allyears_df %>% 
  mutate(`Date Received` = as.Date(`Date Received`)) %>%
  complete(`Date Received` = seq.Date(min(`Date Received`), max(`Date Received`), by = "day"))

# add a summary variable 
# Note: put this with aggregation code in data_preprocessing.
# Note: some rows may have more than one individual. Use the four indicator variables to find true number of individuals in a row
orders_allyears_df <- orders_allyears_df %>% 
  group_by(`Date Received`) %>%
  mutate(`Daily Requests` = n())

# Subset the non-emergency request
orders_allyears_NE <- orders_allyears_df %>% 
  filter(POC != "Emergency Clothing Events")
saveRDS(orders_allyears_NE, file = here("intermediary_data/orders_allyears_NE.rds"))
orders_allyears_Emergency <- orders_allyears_df %>% 
  filter(POC == "Emergency Clothing Events")
saveRDS(orders_allyears_Emergency, file = here("intermediary_data/orders_allyears_Emergency.rds"))

# Following aggregation using the 'non-emergency' data set
## aggregated by earliest filled date
orders_agg_earlist_filled <- orders_allyears_NE %>% 
  group_by(`Date Received`) %>%
  summarise(earliest_filled = min(`Date Filled`), across(9:33, sum),`Daily Requests` = n())
saveRDS(orders_agg_earlist_filled, file = here("intermediary_data/orders_agg_earlist_filled.rds"))

## aggregated by latest filled date
# Note: put this with aggregation code in data_preprocessing
orders_agg_lastest_filled <- orders_allyears_NE %>% 
  group_by(`Date Received`) %>%
  summarise(latest_filled = max(`Date Filled`), across(9:33, sum),`Daily Requests` = n()) 
saveRDS(orders_agg_lastest_filled, file = here("intermediary_data/orders_agg_lastest_filled.rds"))

## mean_order_filled_date
# Note: fix this and put this with aggregation code in data_preprocessing
orders_mean_filled <- orders_allyears_NE %>%
  filter(!is.na(`Date Filled`)) %>%
  arrange(`Date Filled`) %>%
  group_by(`Date Filled`) %>%
  summarise(across(9:33, sum),`Daily Requests` = n())
orders_mean_filled <- orders_mean_filled[c(1,length(orders_mean_filled$`Date Filled`)),] %>%
  mutate(`Date Filled` = as.Date(`Date Filled`, format = "%Y/%m/%d")) %>%
  summarise(across(1:27, mean))

## aggregate by mean order lag
orders_allyears_NE$date_diff <- as.Date(orders_allyears_NE$`Date Filled`, format = "%Y/%m/%d") -
  as.Date(orders_allyears_NE$`Date Received`, format = "%Y/%m/%d")
orders_agg_datediff <- orders_allyears_NE %>%
  mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>% 
  group_by(`date_diff`) %>%
  summarise(across(10:35, mean)) %>%
  mutate(across(2:27, round))
saveRDS(orders_agg_datediff, file = here("intermediary_data/orders_agg_datediff.rds"))



##### Categorical Data Exploration

# Note: some rows may have more than one individual. Use the four indicator variables to find true number of individuals in a row
# use the updated orders_allyears_df.rds, summarise by sum(female_small + male_small + female_large + male_large)
# Also, remove Emergency, Clothing Exchange, and adults.

# Following aggregation using the full data set

## aggregated by POC
orders_agg_POC <- orders_allyears_df %>% 
  group_by(`POC`) %>%
  summarise(`Served individuals` = n())

## aggregated by Organization. prop_requests is proportion of rows in dataset corresponding to that organization
orders_agg_Organization <- orders_allyears_df %>%
  mutate(Organization = ifelse(Organization == "Boys & Girls Club, & Other organizations where we host Clothing Exchanges",
                             "Boys & Girls Club & Others_Clothing Exchanges", Organization)) %>%
  mutate(Organization = ifelse(Organization == "Boys & Girls Club, & Other organizations where we host Emergency Clothing Events",
                           "Boys & Girls Club & Others_Emergency", Organization)) %>%
  group_by(Organization) %>%
  filter(!is.na(Organization)) %>% 
  summarise(`Served individuals` = n()) %>%
  mutate(prop_requests = `Served individuals` / sum(`Served individuals`)) %>%
  mutate_if(is.numeric, round, 3)
saveRDS(orders_agg_Organization, file = here("intermediary_data/orders_agg_Organization.rds"))

## aggregated by School
orders_agg_School <- orders_allyears_df %>%
  group_by(School) %>%
  filter(!is.na(School)) %>% 
  summarise(`Served individuals` = n()) %>%
  mutate(prop_requests = `Served individuals` / sum(`Served individuals`)) %>%
  mutate_if(is.numeric, round, 3)
saveRDS(orders_agg_School, file = here("intermediary_data/orders_agg_School.rds"))

## aggregated by City and make a pie chart
orders_agg_City <- orders_allyears_df %>%
  group_by(City) %>%
  mutate(City = replace(City, City == "#N/A", NA)) %>%
  filter(!is.na(City)) %>% 
  summarise(`Served individuals` = n()) %>%
  mutate(prop_requests = `Served individuals` / sum(`Served individuals`)) %>%
  mutate_if(is.numeric, round, 3)
saveRDS(orders_agg_City, file = here("intermediary_data/orders_agg_City.rds"))

## aggregated by Zip code and calculate the proportion of the request orders
## Read in zip code data set
data(zip.regions)
north_carolina_zip <- zip.regions$region[zip.regions$state.name == "north carolina"]
orders_agg_Zip <- orders_allyears_df %>%
  filter(!is.na(`Zip`) & `Zip` %in% north_carolina_zip) %>%
  group_by(`Zip`) %>%
  summarise(num_requests = n()) %>%
  mutate(prop_requests = num_requests / sum(num_requests)) %>%
  mutate_if(is.numeric, round, 3)
saveRDS(orders_agg_Zip, file = here("intermediary_data/orders_agg_Zip.rds"))