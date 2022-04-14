
library(stringr)
library(abind)
library(here)

# Data Wrangling of inventory stock details

inventory_file_names <- list.files(here("inventory/"))

# The first week of January is an outlier, as the current inventory was 
# listed as quantity_in as Zoho was being set up
inventory_file_names <- inventory_file_names[inventory_file_names != "inventory_stock_details 2022 01 01-08.csv"]



weekly_inventory_list <- vector("list", length = length(inventory_file_names))

for (i in 1:length(inventory_file_names)) {
  
  weekly_inventory_list[[i]] <- read.csv(here(paste0("inventory/", 
                                                inventory_file_names[i])))
  
}

# naming the datasets accordingly
names(weekly_inventory_list) <- str_sub(inventory_file_names, start = 25)

# putting everything into an array
# all the dimensions and clothing names match, conveniently
# but it turns everything to character. Need to do as.numeric to get numbers again
weekly_inventory_array <- abind(weekly_inventory_list, along = 3)

clothing_names <- weekly_inventory_list[[1]]$sku

# subsetting the array to just the numeric variables
# clothing_names will indicate which of the 918 rows corresponds to what clothing
weekly_count_list <- lapply(weekly_inventory_list, function(df) df[, 4:7])
weekly_count_array <- abind(weekly_count_list, along = 3)



# summary statistics

# mean count of each clothing given out (distributed) across the 12 time points
mean_distr <- apply(weekly_count_array[, 3, ], 1, mean)

summary(mean_distr)
quantile(mean_distr, c(.75, .90, .95, .99))

# which clothing items were at or above 99th percentile of donation counts (on average)
clothing_names[which(mean_distr >= 40.91333)]
mean_distr[clothing_names == "BOY-TOP-10"]



# choosing 1 clothing item to test with: 
# Boys Top Size 10 (Medium). Had high number of donations on average.

boy_tops10_donated <- weekly_count_array[clothing_names == "BOY-TOP-10", 2, ]
boy_tops10_distr <- weekly_count_array[clothing_names == "BOY-TOP-10", 3, ]

dat <- cbind(boy_tops10_donated, boy_tops10_distr)



##########

# summary statistics for donation side

# mean count of each clothing donated across the 12 time points
mean_donate <- apply(weekly_count_array[, 2, ], 1, mean)

summary(mean_donate)
quantile(mean_donate, c(.75, .90, .95, .99))

# which clothing items were at or above 99th percentile of donation counts (on average)
clothing_names[which(mean_donate >= 39.30500)]
mean_donate[clothing_names == "BOY-TOP-10"]



# after running the WarpDLM in 
# "warpDLM-reproducible-code-main/Code/application_analysis_local_job.R":

load(here("zoho_forecasting/boys_tops10_np.RData"))

post_median_counts <- apply(boys_tops10_np$post_pred, c(2, 3), median)

matplot(dat, type = "l", col = 1)

matplot(post_median_counts, type = "l", col = 2, add = T)

legend("topleft", legend = c("Observed Donated", "Observed Distributed", 
                             "warpDLM Donated", "warpDLM Distributed"), 
       col = c(1, 1, 2, 2), lty = c(1, 2, 1, 2))


