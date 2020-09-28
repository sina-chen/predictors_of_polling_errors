########################################################################################
# Senate poll accuracy, race and incumbency
# Author: Sina Chen
#
############################################################################################

#### Libraries ####

library(readr)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)

#### Data ####

data_senate <- read_csv("~/Documents/Uni/PollingError/senate/data/senate_polls_merged.csv")

# senate results
senate_results <- readRDS("~/Documents/Uni/PollingError/senate/data/senate_results.RDS")

# merge data
data_senate <- merge(data_senate, senate_results, by = c('election_year', 'state'), all.x = T )

# compute days until election, gender and race dummy
data_senate <- data_senate %>%
  mutate(gender_rep_dummy = if_else(clarifai_gender_rep == 'feminine', 1, 0),
         gender_dem_dummy = if_else(clarifai_gender_dem == 'feminine', 1, 0),
         gender_dummy = if_else(clarifai_gender_rep == 'feminine' | 
                                clarifai_gender_dem == 'feminine', 1, 0),
         race_rep_dummy = if_else(clarifai_race_rep != 'white', 1, 0),
         race_dem_dummy = if_else(clarifai_race_dem != 'white', 1, 0),
         race_dummy = if_else(clarifai_race_rep != 'white' | 
                                clarifai_race_dem != 'white', 1, 0),
         inc_dummy = if_else(incumbency == 'TRUE', 1, 0),
         t = case_when(
           election_year == '1998' ~ difftime(as.Date('11/03/1998','%m/%d/%Y'), date),
           election_year == '2000' ~ difftime(as.Date('11/07/2000','%m/%d/%Y'), date),
           election_year == '2002' ~ difftime(as.Date('11/05/2002','%m/%d/%Y'), date),
           election_year == '2004' ~ difftime(as.Date('11/02/2004','%m/%d/%Y'), date),
           election_year == '2006' ~ difftime(as.Date('11/07/2006','%m/%d/%Y'), date),
           election_year == '2008' ~ difftime(as.Date('11/04/2008','%m/%d/%Y'), date),
           election_year == '2010' ~ difftime(as.Date('11/02/2010','%m/%d/%Y'), date),
           election_year == '2012' ~ difftime(as.Date('11/06/2012','%m/%d/%Y'), date),
           election_year == '2014' ~ difftime(as.Date('11/04/2014','%m/%d/%Y'), date),
           election_year == '2016' ~ difftime(as.Date('11/08/2016','%m/%d/%Y'), date),
           election_year == '2018' ~ difftime(as.Date('11/06/2018','%m/%d/%Y'), date)),
         t_sc = as.numeric(t)/max(as.numeric(t)),
         state_year = paste0(election_year, '_', state),
         rep_poll2 = rep_vote/(rep_vote + dem_vote)
  )



# subset complete cases
data_complete <- data_senate %>% 
  subset(!is.na(gender_dummy) & 
           !is.na(race_dummy) &
           !is.na(rep_poll2) &
           !is.na(rep_result2) &
           !is.na(t) &
           t >= 0 &
           !is.na(n))

# data_complete <- data_complete %>%
#   group_by(state_year) %>%
#   filter(n() > 3)


# election level information

vote_sy <- data_complete %>%
  group_by(state_year, election_year, state, rep_result2, race_rep_dummy,race_dem_dummy, inc_dummy) %>%
  summarise()


#### Set up stan model ####

# Stan data 

stan_dat <- list(
  N = nrow(data_complete),                               # number of polls
  SY = length(unique(data_complete$state_year)),         # number of elections (state x election year)
  S = length(unique(data_complete$state)),          # number of states 

  poll = data_complete$rep_poll2,                        # two-party poll share
  vote = vote_sy$rep_result2,                      # two-party vote share
  
  sample_size = as.numeric(data_complete$n) * (data_complete$rep_result + data_complete$dem_result), 
                                                         # sample size adjusted for Rep. & Dem. poll share
  
  race_rep = vote_sy$race_rep_dummy,               # gender dummy Rep.: 1 = female Rep. candidate
  race_dem = vote_sy$race_dem_dummy,              # gender dummy Dem.: 1 = female Dem. candidate
  inc_dummy = vote_sy$inc_dummy,                   # incumbency dummy: 1 = incumbency participates
  t = data_complete$t_sc,                                # scaled days to election
  
  sy_id = as.integer(as.factor(data_complete$state_year)),# election(state-year) identifier
  state_id = as.integer(as.factor(data_complete$state)),  # state identifier
)


sapply(stan_dat, length)
sapply(stan_dat, range)


#### Fit stan model ####

resStan_race <- stan(file = "stan_ml/ml_senate_race_inc.stan", data = stan_dat,
                chains = 4, iter = 5000,
                control = list(adapt_delta = 0.95)
)    



launch_shinystan(res_stan_race$stand_fit)

saveRDS(list(stand_fit = resStan_race), 'res_stan_race.RDS')
