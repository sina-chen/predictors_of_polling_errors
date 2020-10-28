################################################################################
# Cleaning: Senat Polls  from 1998 to 2018 
# Author: Sina Chen
#
# note: running this script removes special and runoff elections
################################################################################

#### Libraries ####

library(dplyr)
library(openintro)
library(stringr)

#### Directory ####

setwd('your_wd')

#### Load data ####

data_raw <- readRDS('polls_senate1998_2018.RDS')

#### Helper functions ####

source('helper_func_senate.R')


#### Clean ####

# State abbreviations
data_raw$state <- state2abbr(data_raw$state_long)

# Group respondents into LV(= likely voters), RV (= registered voters) and statewide
data_raw$resp_formated <- sapply(data_raw$respondents, resp)

# relabel vote to poll and compute two-party poll vote share
data_raw <- data_raw %>%
  rename(rep_poll = rep_vote,
         dem_poll = dem_vote) %>%
  mutate(rep_poll = as.numeric(rep_poll),
         dem_poll = as.numeric(dem_poll),
         rep_poll2 = rep_poll/(rep_poll + dem_poll),
         dem_poll2 = dem_poll/(rep_poll + dem_poll))


# Add election vote share and two-party vote share
senate_results <- read.csv("~/Documents/Uni/PollingError/predictors_of_polling_errors/data/senate/senate_election_results.csv")

data_results <- merge (data_raw, senate_results, 
                       by = c('state', 'election_year')) # election results for speciale elections are not included

# Compute days until election
data_results <- data_results %>%
  mutate(t = case_when(
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
    election_year == '2018' ~ difftime(as.Date('11/06/2018','%m/%d/%Y'), date)))

# Remove runoff elections (t < 0; 15 obs.)
data_results <- data_results %>%
  subset(t >= 0)

# Remove white spaces in full state names ('state_long')
data_results$state_long <- str_remove_all(data_results$state_long,' ')

# Clean senator names
data_results <- data_results %>%
  mutate_at(vars(ends_with('candidate')), list(~ sub("- ", "\\1", .)))

# Save polls
saveRDS(data_results, "polls_senate1998_2018_clean.RDS")


