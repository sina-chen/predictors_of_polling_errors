#-------------------------------------------------------------------------------
# Trumpists did not fare worse in the polls: fit endorsement-incumbency  model
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
  # compute endorsement and incumbency dummies  
polls <- polls  %>%
  mutate(n = na_if(n, '-'))  %>%
  subset(is.na(n) == F & election_year >= 2018)  %>% 
  mutate(state_year = paste0(state, election_year),
         state_year_int = as.integer(as.factor(state_year)),
         end_trump = if_else(state == 'WV' & election_year == 2020 |
                               state == 'LA' & election_year == 2020|
                               state == 'TX' & election_year == 2020|
                               state == 'MT' & election_year == 2020|
                               state == 'IA' & election_year == 2020|
                               state == 'CO' & election_year == 2020|
                               state == 'SC' & election_year == 2020|
                               state == 'TN' & election_year == 2020|
                               state == 'MS' & election_year == 2020|
                               state == 'OK' & election_year == 2020|
                               state == 'MN' & election_year == 2020|
                               state == 'WY' & election_year == 2020|
                               state == 'KS' & election_year == 2020|
                               state == 'KY' & election_year == 2020|
                               state == 'NH' & election_year == 2020|
                               state == 'GA' & election_year == 2020|
                               state == 'ID' & election_year == 2020|
                               state == 'SD' & election_year == 2020|
                               state == 'NE' & election_year == 2020|
                               state == 'AK' & election_year == 2020|
                               state == 'NC' & election_year == 2020|
                               state == 'AL' & election_year == 2020|
                               state == 'AZ' & election_year == 2018|
                               state == 'IN' & election_year == 2018|
                               state == 'FL' & election_year == 2018|
                               state == 'MI' & election_year == 2018|
                               state == 'MN' & election_year == 2018|
                               state == 'MS' & election_year == 2018|
                               state == 'MO' & election_year == 2018|
                               state == 'MT' & election_year == 2018|
                               state == 'NE' & election_year == 2018|
                               state == 'NV' & election_year == 2018|
                               state == 'NJ' & election_year == 2018|
                               state == 'ND' & election_year == 2018|
                               state == 'OH' & election_year == 2018|
                               state == 'PA' & election_year == 2018|
                               state == 'TN' & election_year == 2018|
                               state == 'TX' & election_year == 2018|
                               state == 'UT' & election_year == 2018|
                               state == 'WV' & election_year == 2018|
                               state == 'WI' & election_year == 2018|
                               state == 'WY' & election_year == 2018, 1, 0),
         inc_rep = if_else(rep_candidate == senator, 1, 0))

# Election-level data 
vote_sy <- polls %>%
  group_by(state_year, election_year, state, rep_result2, end_trump, inc_rep) %>%
  summarise(n_poll = n()) 

# Stan data 
stan_dat <- list(
  
  N = nrow(polls),                             # number of polls
  SY = length(unique(polls$state_year)),       # number of elections (state x election year)

  poll = polls$rep_poll2,                      # two-party poll share
  vote = vote_sy$rep_result2,                  # two-party vote share
  
  end_trump = vote_sy$end_trump,               # dummy Rep. candidate endorsed by trump
  inc_rep = vote_sy$inc_rep,                   # dummy Rep. candidate incumbent
  t = polls$t/max(polls$t),                    # between 0 and 1 scaled days to election
  
  sample_size = as.numeric(polls$n) * 
    (polls$rep_result + polls$dem_result),     # sample size adjusted for Rep. & Dem. poll support
  
  sy_id = polls$state_year_int                 # state-year identifier
  
)

sapply(stan_dat, length) # check stan data
sapply(stan_dat, range) # check stan data


#### Fit stan model ####

resStan <- stan(file = "~/fit_stan/stan_ml/ml_senate_end_trump_inc.stan", 
                data = stan_dat,
                chains = 4, iter = 10000,
                control = list(adapt_delta = 0.99, max_treedepth = 12)
) 

# check results
#launch_shinystan(resStan)

# save results
saveRDS(resStan, '~/fit_stan/resStan_senate_end_trump_inc.RDS') # 0 divergencies
