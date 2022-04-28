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

library(here)
setwd("D:/GitHub/note-in-the-pocket/warpDLM-reproducible-code-main")
source("Code/helper_functions.R")


### STEP 1. orders filled data, daily
dat <- orders_allyears_agg_received %>%
  dyplr::select(female_small,
                female_large,
                male_small,
                male_large)

#Initial model
init_mod <- dlm(FF = matrix(c(1, 0), nrow = 1) %x% diag(2), V = diag(2),
                GG = matrix(c(1, 0, 1, 1), 2, 2) %x% diag(2),
                W = bdiag(diag(2), diag(2)),
                m0 = c(dat[1, 1], dat[1, 2], 0, 0),
                C0 = diag(x = 3, nrow = 4))

#This can take several hours to run
orders_np <- SUTSE_mcmc_dlm(dat, init_mod, update_mod = updatemod_invWish_dlm, transformation = "np",
                                 nsave=10000, nburn=5000, nskip=1, nfc=1, particle = T)


save(orders_np, file = here("D:/GitHub/note-in-the-pocket/orders_filled_forecasting/orders_np.RData"))

###################################################################

