
library(readxl)
library(dplyr)
library(here)



order_file_names <- list.files("OrdersFilled")



# Adjust the below code for the 2020 data to read in the 2019 and other data.

# Reading in 2020 data

orders2020 <- vector("list", length = 12)

col_types2020 <- c("numeric", "date", "date", rep("text", 4), 
                   "numeric", "text", rep("numeric", 3), 
                   "text", "numeric", rep("numeric", 20), "skip")

col_types2021 <- c("numeric", "date", "date", rep("text", 4), 
                   "numeric", "text", rep("numeric", 3), 
                   "text", "text", "numeric", rep("numeric", 20), "skip")

for (i in 1:12) {
  
  # going through special cases
  if (i %in% c(1:9)) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2020 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2020)
    read_in <- read_in[-1, ] # skipping first row that lists the totals
  } else if (i == 10) {
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2020 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2020)
  } else { # months 11, 12 start including "Senior Head of Household" 
    read_in <- read_excel(paste0("OrdersFilled/", "Note in the Pocket 2020 Orders Filled.xlsx"), 
                          sheet = i, col_types = col_types2021)
    read_in <- read_in[-1, ] # skipping first row that lists the totals
  }
  
  names(read_in)[1] <- "Id"
  
  orders2020[[i]] <- read_in
  
}

# stacking the datasets together

# correcting names for October
names(orders2020[[10]]) <- names(orders2020[[1]])

# removing `Senior Head of Household` for months 11 and 12 to be consistent with other months
orders2020[[11]] <- orders2020[[11]][, names(orders2020[[11]]) != "Senior Head of Household"]

orders2020[[12]] <- orders2020[[12]][, names(orders2020[[12]]) != "Senior Head of Household"]

# naming mismatch for non-student

for (i in 1:12) {
  
  names(orders2020[[i]])[12] <- "NonStudent" 
  
}

# checking the names
# sapply(orders2020, function(df) names(df))

orders2020_df <- do.call("rbind", orders2020)

# changing McKinney Vento into binary variable 1 = yes, 0 = no
orders2020_df$`McKinney Vento` <- ifelse(orders2020_df$`McKinney Vento` == "yes", 1, 0)

saveRDS(orders2020_df, file = here("intermediary_data/orders2020_df.rds"))





##########

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
  # NOTE: do this only for 2021. For other years, check whether there are the empty
  # lines at the bottom. I didn't see any for 2019 or 2020
  read_in <- read_in[-c((nrow(read_in) - 3):nrow(read_in)), ]
  
  orders2021[[i]] <- read_in
  
}





# 2019 variables
#         Date Received	Date Filled	POC	Organization	School	City	Zip	Recipient	adult	student	NonStudent	McKinney Vento	Acc	gt	gb	gu	gs	gc	bt	bb	bu	bs	bc	wt	wb	wu	ws	wc	mt	mb	mu	ms	mc	Total Distributed from Inventory	

# 2020 variables
#         Date Received	Date Filled	POC	Organization	School	City	Zip	Recipient	adult	student	NonStudent	McKinney Vento	Acc	gt	gb	gu	gs	gc	bt	bb	bu	bs	bc	wt	wb	wu	ws	wc	mt	mb	mu	ms	mc	Total Distributed from Inventory

# 2021 variables
#         Date Received	Date Filled	POC	Organization	School	City	Zip	Recipient	adult	student	NonStudent	McKinney Vento	Senior Head of Household	Acc	gt	gb	gu	gs	gc	bt	bb	bu	bs	bc	wt	wb	wu	ws	wc	mt	mb	mu	ms	mc	Total Distributed from Inventory
# 12635.1	1/4/21	1/6/21	Erica Stuckey	Wake County Public School Social Workers	Poe ES	Raleigh	27610		0	1	0	no	no	2	1	6	10	2	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	

# 2019, 2020, and 2021 variables same except that 2021 also has Senior Head of Household


