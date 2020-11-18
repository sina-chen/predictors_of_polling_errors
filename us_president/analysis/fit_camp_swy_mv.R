########################################################################################
# Poll accuracy & campaigns with modified variance
# Author: Sina Chen
# Notes: 2000 is excluded from this analysis
#
############################################################################################

#### Libraries ####

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)
library(stringr)
library(dplyr)
library(readxl)
library(arm)
library(foreach)
library(doParallel)
library(data.table)
library(purrr)
library(bayesplot)
library(readr)
library(zoo)


#### Load data ####

# Poll-level data
data <- readRDS('data.RDS')
data <- data %>% 
  mutate(swy = paste0(election_year,'_', swing),
         swy_int = as.integer(as.factor(swy)))

# Election-level (state-year) data
vote_sy <- data %>% 
  group_by(state_year, state_year_int, state, election_year, rep_result2,
           swing, ev_sc, swy, swy_int) %>% 
  summarise(n_poll = n())


#### Set up stan model ####

# Stan data - State only model
stan_dat <- list(
  N = nrow(data),                               # number of polls
  SY = length(unique(data$state_year)),         # number of elections
  SWY = length(unique(data$swy_int)),           # number of swing/safe state year group
  
  poll = data$rep_poll2,                        # two-party poll share
  vote = vote_sy$rep_result2,                   # two-party vote share
  
  sample_size = as.numeric(data$n) * (data$rep_poll + data$dem_poll), 
                                                # sample size adjusted for Rep. & Dem. poll share
  
  c_int = data$exp_week_sc,                     # scaled campaign intensity
  c_bal = data$diff_exp_sc,                     # scaled campaign balanced
  ev = vote_sy$ev_sc,                           # scaled electoral votes
  after_con = data$after_con,                   # dummy after/before convention
  dte = data$dte_sc,                            # scaled days to election
  
  sy_id = data$state_year_int,                  # state-year identifier
  swy_id = data$swy_int                         # swing state-year identifier
)

# check of lengths
sapply(stan_dat, length)
sapply(stan_dat, range)


#### Fit stan model ####

resStan <- stan(file = "stan_ml/ml_camp_swy_mv.stan", data = stan_dat,
                chains = 4, iter = 15000,
                control = list(adapt_delta = 0.95)
)


#### Model fit for sensetivity analysis ####

resStan25 <- stan(file = "stan_ml/ml_camp_swy_mv25.stan", data = stan_dat,
                chains = 4, iter = 15000,
                control = list(adapt_delta = 0.99)
)

resStan01 <- stan(file = "stan_ml/ml_camp_swy_mv01.stan", data = stan_dat,
                  chains = 4, iter = 15000,
                  control = list(adapt_delta = 0.99)
)


#### Save stan fit objects ####

saveRDS(list(stan_fit = resStan), file = "camp_swy_mv.RDS")
saveRDS(list(stan_fit = resStan01), file = "camp_swy_mv01.RDS")
saveRDS(list(stan_fit = resStan25), file = "camp_swy_mv25.RDS")
