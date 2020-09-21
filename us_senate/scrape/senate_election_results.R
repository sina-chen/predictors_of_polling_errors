########################################################################################
# Senate election results
# Author: Sina Chen
# Notes: 
#
########################################################################################

#### Libraries ####

library(readr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(stringr)

#### Data ####

load("~/Documents/Uni/PollingError/senate/data/1976-2018-senate.RData")

# Subset relevant information
senate_results <- x[,c('year', 'state_po', 'party', 'candidatevotes', 'totalvotes')] %>% 
  subset(str_detect(party,'republican') == T & year >= 1998 |
           str_detect(party,'democrat') == T & year >= 1998) 
senate_results$party <-  unlist(str_extract_all(senate_results$party,
                                                'republican|democrat'), 
                                recursive = F)

# Subset only polls with rep and dem candidates
senate_results_n <- senate_results %>%
  group_by(state_po, year,party) %>%
  summarise(n = n()) %>%
  rename(election_year = 'year',
         state = 'state_po',
         n_camdidate = 'n')

senate_results <- merge(senate_results, senate_results_n, by = c('year', 'state_po', 'party'))

# Elections with more than one Rep. or Dem. candidate

senate_multi_candidate <- senate_results[,c('year', 'state_po', 'n', 'party')] %>%
  subset(n > 1) %>%
  unique

# Rename variables and compute vote shares
senate_results <- senate_results %>%
  subset(n == 1) %>%
  mutate(res_share = candidatevotes/totalvotes) %>% 
  select(-c('candidatevotes', 'totalvotes','n')) %>%
  dcast(year + state_po ~ party, value.var = 'res_share') %>%
  rename(rep_result  = 'republican',
         dem_result = 'democrat',
         election_year  = 'year',
         state = 'state_po') %>%
  mutate(rep_result2 = rep_result/(rep_result + dem_result),
         dem_result2 = dem_result/(rep_result + dem_result))


saveRDS(senate_results, 'senate_results.RDS')
saveRDS(senate_multi_candidate, 'senate_multi_candidate.RDS')
