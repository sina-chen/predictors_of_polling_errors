#-------------------------------------------------------------------------------
#
# US Senate predictable: prepare swing state proxies
#
# Author: Sina Chen
# 
#-------------------------------------------------------------------------------

setwd("~/Documents/Uni/PollingError/us/senate/data/")


# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyverse)


# Data --------------------------------------------------------------------

load("senate_results1976_2020_MIT.RData")


# Function ----------------------------------------------------------------

# get margin form previous election
get_prev_margin <- function(state_data) {
  election_years <- state_data$year
  margin2_rep_prev <- sapply(2:nrow(state_data), 
                             function(x) state_data[which(state_data$year == election_years[x-1]), "margin2_rep"]) 
  margin2_rep_prev <- lapply(margin2_rep_prev, function(x) if(length(x) > 1) mean(x) else x) %>% 
    unlist()
  
  state_data$margin2_rep_prev <- c(0, margin2_rep_prev)
  return(state_data)
}

# get previous 3 election year results
  # >3 if special elections took place (all elections form previous 3 election YEARS are considered)
get_prev_res3 <- function(state_data) {
  
  # order data by year
  state_data <- state_data[order(state_data$year),] 
  
  # unique election years
  election_years <- unique(state_data$year)
  winner3_rep_prev <- sapply(4:nrow(state_data), function(x) 
  {
    # position in unique year vector
    pos <- which(election_years == state_data$year[x]);
    
    # if rep > 0.5 in all elections conducted in previous 3 years
    if(all(pull(state_data[which(state_data$year == election_years[pos-1]), "res2_rep"]) > 0.5, 
           pull(state_data[which(state_data$year == election_years[pos-2]), "res2_rep"]) > 0.5,
           pull(state_data[which(state_data$year == election_years[pos-3]), "res2_rep"]) > 0.5) %>% 
       isTRUE()) "solid_rep" 
    # if dem > 0.5 in all elections conducted in previous 3 years
    else if (all(pull(state_data[which(state_data$year == election_years[pos-1]), "res2_dem"]) > 0.5,
                 pull(state_data[which(state_data$year == election_years[pos-2]), "res2_dem"]) > 0.5,
                 pull(state_data[which(state_data$year == election_years[pos-3]), "res2_dem"]) > 0.5) %>% 
             isTRUE()) "solid_dem"
    else "swing_state"
  })
  
  state_data$winner3_rep_prev <- c(0, 0, 0, winner3_rep_prev)
  return(state_data)
}


# Preparation -------------------------------------------------------------

# subset relevant parties and select relevant variables
results <- x %>% 
  subset(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  select(year, state, special, party_simplified, candidatevotes, totalvotes,
         candidate) %>% 
  mutate(res = candidatevotes/totalvotes)  %>% 
  group_by(year, state, special, party_simplified) %>% 
  mutate(n_res = n(),
         party = if_else(party_simplified == "DEMOCRAT", "dem", "rep"))%>% 
  ungroup() 

# select candidate who recieved most votes if several candidates from one party participated
max_vote <- results %>%  subset(n_res > 1) %>% 
  group_by(state, year, special, party) %>% 
  summarise(max_votes = max(candidatevotes)) %>% 
  ungroup()

results <- merge(results, max_vote, by = c("state", "year", "special", 
                                           "party"), all.x = T) 
results <- results %>% 
  subset(n_res == 1 | n_res > 1 & candidatevotes == max_votes)

# reshape to wide and add 2- party vote share and margin
  # if no dem candidate was running the rep. 2-party vote share is the vote share
  # if no rep candidate was running there is no rep. 2-party vote share
results_wide <- results %>% 
  select(-c(max_votes, party_simplified)) %>% 
  pivot_wider(names_from = c("party"), 
              values_from = c("candidatevotes", "candidate", "n_res", "res")) %>% 
  mutate(res2_rep = if_else(!is.na(candidatevotes_dem),
                            candidatevotes_rep/(candidatevotes_rep+candidatevotes_dem),
                            candidatevotes_rep/totalvotes),
         res2_dem = if_else(!is.na(candidatevotes_rep),
                            candidatevotes_dem/(candidatevotes_rep+candidatevotes_dem),
                            candidatevotes_dem/totalvotes),
         margin2_rep = res2_rep-(1-res2_rep))

# check elections with missing 
check <- results_wide[which(is.na(results_wide$candidate_dem)|is.na(results_wide$candidate_rep)), ]



# Swing state proxies -----------------------------------------------------


# add margin from previous election
results_prev_margin <- lapply(unique(results_wide$state), 
                              function(x) subset(results_wide, state == x) %>% 
                                get_prev_margin()) %>% bind_rows()

# add results of previous 3 elections
reults_prev_res <- lapply(unique(results_prev_margin$state), 
                          function(x) subset(results_prev_margin, state == x) %>% 
                              get_prev_res3()) %>% bind_rows()




saveRDS(reults_prev_res, "us_senate_swing_proxi.RDS")
