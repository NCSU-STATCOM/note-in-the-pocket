
library(readxl)
library(dplyr)



order_file_names <- list.files("OrdersFilled")



# Reading in 2021 data

orders2021 <- vector("list", length = 12)

col_types2021 <- c("numeric", "date", "date", rep("text", 4), 
                   "numeric", "text", rep("numeric", 3), 
                   "text", "text", "numeric", rep("numeric", 20), "skip")

for (i in 1:12) {
  
  read_in <- read_excel(paste0("OrdersFilled/", order_file_names[length(order_file_names)]), 
                        sheet = i, col_types = col_types2021)
  
  read_in <- rename(read_in, Id = `...1`)
  
  orders2021[[i]] <- read_in[-1, ] # skipping first row that lists the totals
  
}



# 2019 variables
#         Date Received	Date Filled	POC	Organization	School	City	Zip	Recipient	adult	student	NonStudent	McKinney Vento	Acc	gt	gb	gu	gs	gc	bt	bb	bu	bs	bc	wt	wb	wu	ws	wc	mt	mb	mu	ms	mc	Total Distributed from Inventory	

# 2021 variables
#         Date Received	Date Filled	POC	Organization	School	City	Zip	Recipient	adult	student	NonStudent	McKinney Vento	Senior Head of Household	Acc	gt	gb	gu	gs	gc	bt	bb	bu	bs	bc	wt	wb	wu	ws	wc	mt	mb	mu	ms	mc	Total Distributed from Inventory
# 12635.1	1/4/21	1/6/21	Erica Stuckey	Wake County Public School Social Workers	Poe ES	Raleigh	27610		0	1	0	no	no	2	1	6	10	2	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	

# 2019 and 2021 variables same except that 2021 also has Senior Head of Household


