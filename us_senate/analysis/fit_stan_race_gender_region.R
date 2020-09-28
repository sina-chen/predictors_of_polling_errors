########################################################################################
# Senate poll accuracy, race and incumbency by conservative-liberiterian group 
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

data_senate <-read_csv("~/Documents/Uni/PollingError/senate/data/senate_polls_merged.csv")

# senate results
senate_results <- readRDS("~/Documents/Uni/PollingError/senate/data/senate_results.RDS")

# merge data
data_senate <- merge(data_senate, senate_results, by = c('election_year', 'state'), all.x = T )

# compute days until election, gender and race dummy, incumbency dummy, two-party vote share
data_senate <- data_senate %>%
  mutate(gender_rep_dummy = if_else(clarifai_gender_rep == 'feminine', 1, 0),
         gender_dem_dummy = if_else(clarifai_gender_dem == 'feminine', 1, 0),
         race_rep_dummy = if_else(clarifai_race_rep != 'white', 1, 0),
         race_dem_dummy = if_else(clarifai_race_dem != 'white', 1, 0),
         state_year = paste0(election_year, '_', state),
         rep_poll2 = rep_vote/(rep_vote + dem_vote)
  )


# Subset complete cases
data_senate <- data_senate %>% 
  subset(!is.na(gender_rep_dummy) &
           !is.na(gender_dem_dummy) &
           !is.na(race_rep_dummy) &
           !is.na(race_dem_dummy) &
           !is.na(rep_poll2) &
           !is.na(rep_result2) &
           !is.na(n))

# Add groups (source: https://en.wikipedia.org/wiki/United_States_Census_Bureau)
data_senate <- data_senate %>%
  mutate(region_group4 = case_when(state == 'CT'|
                                     state == 'ME'|
                                     state == 'MA'|
                                     state == 'NH'|
                                     state == 'NJ'|
                                     state == 'NY'|
                                     state == 'PA'|
                                     state == 'RI'|
                                     state == 'VT' ~ 'Northeast',
                                   state == 'IL'|
                                     state == 'IN'|
                                     state == 'IA'|
                                     state == 'KS'|
                                     state == 'MI'|
                                     state == 'MN'|
                                     state == 'MO'|
                                     state == 'NE'|
                                     state == 'ND'|
                                     state == 'OH'|
                                     state == 'SD'|
                                     state == 'WI' ~ 'Midwest',
                                   state == 'AL'|
                                     state == 'AR'|
                                     state == 'DE'|
                                     state == 'FL'|
                                     state == 'GA'|
                                     state == 'KY'|
                                     state == 'LA'|
                                     state == 'MD'|
                                     state == 'MS'|
                                     state == 'NC'|
                                     state == 'OK'|
                                     state == 'SC'|
                                     state == 'TN'|
                                     state == 'TX'|
                                     state == 'VA'|
                                     state == 'WV' ~ 'South',
                                   state == 'AZ'|
                                     state == 'AK'|
                                     state == 'CA'|
                                     state == 'CO'|
                                     state == 'HI'|
                                     state == 'ID'|
                                     state == 'MT'|
                                     state == 'NV'|
                                     state == 'NM'|
                                     state == 'UT'|
                                     state == 'OR'|
                                     state == 'WA'|
                                     state == 'WY' ~ 'West'))




#### Set up stan model ####

# Election-level data 
vote_sy <- data_senate %>%
  group_by(state_year, election_year, state, rep_result2, 
           race_rep_dummy, race_dem_dummy, gender_rep_dummy,gender_dem_dummy) %>%
  summarise()

# Stan data 

stan_dat <- list(
  N = nrow(data_senate),                            # number of polls
  SY = length(unique(data_senate$state_year)),      # number of elections (state x election year)
  RG = length(unique(data_senate$region_group4)),   # number of regions
  
  poll = data_senate$rep_poll2,                     # two-party poll share
  vote = vote_sy$rep_result2,                       # two-party vote share
  
  sample_size = as.numeric(data_senate$n) * (data_senate$rep_result + data_senate$dem_result), 
  # sample size adjusted for Rep. & Dem. poll share
  
  race_rep = vote_sy$race_rep_dummy,                # race dummy Rep.: 1 = non-white Rep. candidate
  race_dem = vote_sy$race_dem_dummy,                # race dummy Dem.: 1 = non-white Dem. candidate
  gender_rep = vote_sy$gender_rep_dummy,            # gender dummy Rep.: 1 = female Rep. candidate
  gender_dem = vote_sy$gender_dem_dummy,            # gender dummy Dem.: 1 = female Dem. candidate
  
  sy_id = as.integer(as.factor(data_senate$state_year)),   # election(state-year) identifier
  rg_id = as.integer(as.factor(data_senate$region_group4)) # region identifier
)


sapply(stan_dat, length)
sapply(stan_dat, range)

#### Fit stan model ####

resStan_race <- stan(file = "stan_ml/ml_senate_race_region.stan", data = stan_dat,
                         chains = 4, iter = 5000,
                         control = list(adapt_delta = 0.95)
) 

resStan_gender <- stan(file = "stan_ml/ml_senate_gender_region.stan", data = stan_dat,
                     chains = 4, iter = 5000,
                     control = list(adapt_delta = 0.95)
) 






