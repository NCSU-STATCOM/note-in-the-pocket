# Applying WarpDLM to Boys Tops Size 10 donations/distribution forecasting (only 12 time points)

library(dlm)
# # to install github package rSTAR
# library(devtools)
# install_github("drkowal/rSTAR")
library(rSTAR)
library(tidyverse)
library(mc2d)
library(bayesplot)
library(TruncatedNormal)
library(spatstat)  #ewcdf function
library(mvnfast)
library(beepr)

library(here)
setwd("D:/GitHub/note-in-the-pocket/warpDLM-reproducible-code-main")
source("Code/helper_functions.R")


### STEP 1. orders filled data, daily
# dat <- orders_allyears_agg_received %>%
#   dplyr::select(female_small,
#                 female_large,
#                 male_small,
#                 male_large)

dat <- orders_weekly_agg_received %>%
  dplyr::select(female_small,
                female_large) %>%
  as.matrix()

dat <- orders_weekly_agg_received %>%
  dplyr::select(male_small,
                male_large) %>%
  as.matrix()

#Initial model
init_mod <- dlm(FF = matrix(c(1, 0), nrow = 1) %x% diag(2), V = diag(2),
                GG = matrix(c(1, 0, 1, 1), 2, 2) %x% diag(2),
                W = bdiag(diag(2), diag(2)),
                m0 = c(dat[1, 1], dat[1, 2], 0, 0),
                C0 = diag(x = 3, nrow = 4))

#This can take several hours to run
orders_male_np <- SUTSE_mcmc_dlm(dat, init_mod, update_mod = updatemod_invWish_dlm, transformation = "np",
                                 nsave=10000, nburn=5000, nskip=1, nfc=1, particle = T)
beep(1)

save(orders_male_np, file = here("D:/GitHub/note-in-the-pocket/orders_filled_forecasting/orders_male_np.RData"))

###################################################################

load("D:/GitHub/note-in-the-pocket/orders_filled_forecasting/orders_np.RData")
str(orders_np$post_pred)

######### Try running a separate model for orders_weekly_agg_received using covariates? 



