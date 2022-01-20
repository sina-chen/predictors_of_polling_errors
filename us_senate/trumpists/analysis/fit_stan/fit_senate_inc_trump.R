#-------------------------------------------------------------------------------
# Trumpists did not fare worse in the polls: fit incumbency model
# 
# Author: Sina Chen
#
#-------------------------------------------------------------------------------

#### Libraries ####

library(readr)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)


#### Data ####

# polls and exp
polls <- readRDS("~/data/senate_polls1998_2020_score.RDS")

#-------------------------------------------------------------------------------

#### Pre-processing ####

# Prepare data: 
  # remove observations with missing information on sample size
  # subset polls for 2018 and 2020
  # generate state-year groups
  # compute endorsement dummy
polls <- polls  %>%
  mutate(n = na_if(n, '-'))  %>%
  subset(is.na(n) == F & election_year >= 2018)  %>% 
  mutate(state_year = paste0(state, election_year),
         state_year_int = as.integer(as.factor(state_year)),
         inc_rep = if_else(rep_candidate == senator, 1, 0))

# Election-level data 
vote_sy <- polls %>%
  group_by(state_year, election_year, state, rep_result2, inc_rep) %>%
  summarise(n_poll = n()) 

# Stan data 
stan_dat <- list(
  
  N = nrow(polls),                             # number of polls
  SY = length(unique(polls$state_year)),       # number of elections (state x election year)

  poll = polls$rep_poll2,                      # two-party poll share
  vote = vote_sy$rep_result2,                  # two-party vote share
  
  rep_main = vote_sy$inc_rep,               # dummy Rep. candidate endorsed by Trump
  t = polls$t/max(polls$t),                    # between 0 and 1 scaled days to election
  
  sample_size = as.numeric(polls$n) * 
    (polls$rep_result + polls$dem_result),     # sample size adjusted for Rep. & Dem. poll support
  
  sy_id = polls$state_year_int                 # state-year identifier
  
)

sapply(stan_dat, length)
sapply(stan_dat, range)


#### Fit stan model ####

resStan <- stan(file = "~/fit_stan/stan_ml/ml_senate_main_trump.stan", 
                data = stan_dat,
                chains = 4, iter = 10000,
                control = list(adapt_delta = 0.99, max_treedepth = 12)
) 

#launch_shinystan(resStan)
saveRDS(resStan, '~/fit_stan/resStan_senate_inc_trump.RDS') # 0 divergencies
