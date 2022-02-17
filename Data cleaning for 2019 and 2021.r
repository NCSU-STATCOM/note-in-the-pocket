library(readxl)
library(dplyr)
library(here)

# Reading in 2019 data
orders2019 <- vector("list", length = 12)

col_types2019 <- c("numeric", "date", "date", rep("text", 4), 
                   "numeric", "text", rep("numeric", 3), 
                   "text", "numeric", rep("numeric", 20), "skip")

for (i in 1:12) {
  # going through each month
  if (i == 1) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-2,-27,-164,-215:-348), ] # skipping the empty rows
  } else if (i == 2) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-170:-373), ]
  } else if (i == 3) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-294:-374), ]
  } else if (i == 4) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-182:-184,-244,-245,-338:-700), ]
  } else if (i == 5) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-333:-417), ]
  } else if (i == 6) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-186:-370), ]
  } else if (i == 7) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-263:-400), ]
  } else if (i == 8) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-590:-705), ]
  } else if (i == 9) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[-1, ]
  } else if (i == 10) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-721:-729), ]
  } else if (i == 11) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-580:-587), ]
  } else {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2019 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2019)
    read_in <- read_in[c(-1,-267:-275), ]
  }
  
  names(read_in)[1] <- "Id"
  
  orders2019[[i]] <- read_in
  
}

# stacking the datasets together, naming mismatch for non-student
for (i in 1:12) {
  
  names(orders2019[[i]])[12] <- "NonStudent" 
  
}

orders2019_df <- do.call("rbind", orders2019)

# changing McKinney Vento into binary variable 1 = yes, 0 = no
orders2019_df$`McKinney Vento` <- ifelse(orders2019_df$`McKinney Vento` == "yes", 1, 0)

saveRDS(orders2019_df, file = here("intermediary_data/orders2019_df.rds"))

orders2019_df <- readRDS(here("intermediary_data/orders2019_df.rds"))

# aggregated by date
orders2019_agg_received <- orders2019_df %>% 
  group_by(`Date Received`) %>%
  summarise(earliest_filled = min(`Date Filled`, na.rm = T), across(9:33, sum, na.rm = T)) 

saveRDS(orders2019_agg_received, file = here("intermediary_data/orders2019_agg_received.rds"))

##############################################################################################################

# Reading in 2021 data

orders2021 <- vector("list", length = 12)

col_types2021 <- c("numeric", "date", "date", rep("text", 4), 
                   "numeric", "text", rep("numeric", 3), 
                   "text", "text", "numeric", rep("numeric", 20), "skip")

for (i in 1:12) {
  
  read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2021 Orders Filled.xlsx"), 
                        sheet = i, col_types = col_types2021)
  
  read_in <- rename(read_in, Id = `...1`)
  
  read_in <- read_in[-1, ] # skipping first row that lists the totals
  
  # skipping the 4 empty lines at the bottom, that just contain totals  

  read_in <- read_in[-c((nrow(read_in) - 3):nrow(read_in)), -14]
  
  orders2021[[i]] <- read_in
  
}

# stacking the datasets together, naming mismatch for non-student
for (i in 1:12) {
  
  names(orders2021[[i]])[12] <- "NonStudent" 
  
}

orders2021_df <- do.call("rbind", orders2021)

# changing McKinney Vento into binary variable 1 = yes, 0 = no
orders2021_df$`McKinney Vento` <- ifelse(orders2021_df$`McKinney Vento` == "yes", 1, 0)

saveRDS(orders2021_df, file = here("intermediary_data/orders2021_df.rds"))

orders2021_df <- readRDS(here("intermediary_data/orders2021_df.rds"))

# aggregated by date
orders2021_agg_received <- orders2021_df %>% 
  group_by(`Date Received`) %>%
  summarise(earliest_filled = min(`Date Filled`, na.rm = T), across(9:33, sum, na.rm = T)) 
# warnings arise from all orders being withdrawn on that day

saveRDS(orders2021_agg_received, file = here("intermediary_data/orders2021_agg_received.rds"))


