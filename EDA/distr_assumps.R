
library(here)
library(performance)
library(tidyverse)

orders_dat <- readRDS(here("intermediary_data/orders_allyears_agg_received.rds"))



# clothing count variables

count_variables <- c("Acc", "gt", "gb", "gu", "gs", "gc", 
                     "bt", "bb", "bu", "bs",             
                     "bc", "wt", "wb", "wu",             
                     "ws", "wc", "mt", "mb",             
                     "mu", "ms", "mc")

other_variables <- setdiff(names(orders_dat), count_variables)



# Better understand the days in which requests are received

# create new variable that is the 



# Looking at histograms

for(colname in c("Acc", "gb", "bt", "mu", "wc")) {
  hist(orders_dat[[colname]], main=colname)
}



# mean-variance summary statistics for each clothing item

moments_mat <- matrix(NA, nrow = length(count_variables), ncol = 2)

colnames(moments_mat) <- c("mean", "variance")
row.names(moments_mat) <- count_variables

for(j in 1:length(count_variables)) {
  moments_mat[j, 1] <- mean(orders_dat[[count_variables[j]]])
  moments_mat[j, 2] <- var(orders_dat[[count_variables[j]]])
}

plot(moments_mat[, 1], moments_mat[, 2], xlab = "mean", ylab = "variance")
lines(0:70, 0:70)



# Very overdispersed. What if I treated the girls, boys, men, women separately, 
# and removed rows that are all zeros? 

gcount <- orders_dat[, names(orders_dat) %in% c(other_variables, "gt", "gb", "gu", "gs", "gc")]
bcount <- orders_dat[, names(orders_dat) %in% c(other_variables, "bt", "bb", "bu", "bs", "bc")]
wcount <- orders_dat[, names(orders_dat) %in% c(other_variables, "wt", "wb", "wu", "ws", "wc")]
mcount <- orders_dat[, names(orders_dat) %in% c(other_variables, "mt", "mb", "mu", "ms", "mc")]

# proportion of all zeros on a given receive date
mean(rowSums(gcount[, names(gcount) %in% count_variables]) == 0)
mean(rowSums(bcount[, names(gcount) %in% count_variables]) == 0)
mean(rowSums(wcount[, names(gcount) %in% count_variables]) == 0)
mean(rowSums(mcount[, names(gcount) %in% count_variables]) == 0)

# remove all zero rows
gcount_no0 <- gcount[rowSums(gcount[, names(gcount) %in% count_variables]) != 0, ]
bcount_no0 <- bcount[rowSums(bcount[, names(bcount) %in% count_variables]) != 0, ]
wcount_no0 <- wcount[rowSums(wcount[, names(wcount) %in% count_variables]) != 0, ]
mcount_no0 <- mcount[rowSums(mcount[, names(mcount) %in% count_variables]) != 0, ]



gmoments_mat <- matrix(NA, nrow = 5, ncol = 2)

colnames(gmoments_mat) <- c("mean", "variance")

for(j in 1:5) {
  gmoments_mat[j, 1] <- mean(gcount_no0[[j + 6]])
  gmoments_mat[j, 2] <- var(gcount_no0[[j + 6]])
}

gmoments_mat
plot(gmoments_mat[, 1], gmoments_mat[, 2], xlab = "mean", ylab = "variance")



bmoments_mat <- matrix(NA, nrow = 5, ncol = 2)

colnames(bmoments_mat) <- c("mean", "variance")

for(j in 1:5) {
  bmoments_mat[j, 1] <- mean(gcount_no0[[j + 6]])
  bmoments_mat[j, 2] <- var(gcount_no0[[j + 6]])
}

bmoments_mat



wmoments_mat <- matrix(NA, nrow = 5, ncol = 2)

colnames(wmoments_mat) <- c("mean", "variance")

for(j in 1:5) {
  wmoments_mat[j, 1] <- mean(gcount_no0[[j + 6]])
  wmoments_mat[j, 2] <- var(gcount_no0[[j + 6]])
}

wmoments_mat



mmoments_mat <- matrix(NA, nrow = 5, ncol = 2)

colnames(mmoments_mat) <- c("mean", "variance")

for(j in 1:5) {
  mmoments_mat[j, 1] <- mean(gcount_no0[[j + 6]])
  mmoments_mat[j, 2] <- var(gcount_no0[[j + 6]])
}

mmoments_mat







# May want to repeat this overdispersion check after fitting a time series model. 
