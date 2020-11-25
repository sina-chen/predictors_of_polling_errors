#------------------------------------------------------------------------------#
# Merge senate polls to bonica RF scores
# Author: Sina Chen
# Notes: 
# Source: 
#   - rf_score: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/7EWDRF
#   - dime: https://data.stanford.edu/dime#download-data
#------------------------------------------------------------------------------#

#### Libraries ####

library(readr)
library(dplyr)
library(stringr)

#### Data ####

dime <- read_csv("dime_recipients_all_1979_2014.csv") 
rf_scores <- read_csv("rf_predicted_scores.csv")
polls <- readRDS("senate_polls_fec_id.RDS")

#------------------------------------------------------------------------------#

#### Preprocessing ####

# select relevant information from dime for merging
dime_id <- dime %>% 
  mutate(switcher = if_else(is.na(before.switch.ICPSR) == T, 0, 1)) %>% 
  select(Cand.ID, bonica.rid, fecyear, switcher) %>% 
  subset(str_detect(Cand.ID, '^[S]') == T) 

# select relevant information from rf scores 
rf_id <- rf_scores %>% 
  subset(party == 100 & state != '00'| 
           party == 200 & state != '00') %>% 
  select(dwdime, dwnom1, rid, party, num_unq_donors) %>% 
  mutate(party = recode(party, '100' = 'Dem', '200' = 'Rep'))

#------------------------------------------------------------------------------#

#### Merge ####

# merge FEC candidate and committee id to rf scores based on bonica's rid
rf_dime <- merge(rf_id, dime_id, by.x = 'rid', by.y = 'bonica.rid') %>% 
  distinct() %>% 
  mutate(party = recode(party, '100' = 'Dem', '200' = 'Rep'))
rm(dime, dime_id, rf_scores, rf_id)

# merge scores to Rep. candidates (not switching party) based on FEC id
polls_score <- merge(polls,
                     subset(rf_dime, switcher == 0, 
                            select = c('Cand.ID','dwdime', 'dwnom1', 
                                       'num_unq_donors')),
                     by.x = c('fec_rep_cand_id'), by.y = c('Cand.ID'), 
                     all.x = T) %>% 
  distinct() %>% 
  rename(rf_score_rep = dwdime,
         dwnom_score_rep = dwnom1,
         n_donors_rep = num_unq_donors)

# merge scores to Dem. candidates (not switching party) based on FEC id
polls_score <- merge(polls_score, 
                     subset(rf_dime, switcher == 0,
                            select = c('Cand.ID','dwdime', 'dwnom1', 
                                       'num_unq_donors')),
                     by.x = c('fec_dem_cand_id'), by.y = c('Cand.ID'), 
                     all.x = T) %>% 
  distinct() %>% 
  rename(rf_score_dem = dwdime,
         dwnom_score_dem = dwnom1,
         n_donors_dem = num_unq_donors)

# merge scores to Rep. candidates (switching party) based on FEC id and election year
polls_score <- merge(polls_score, 
                     subset(rf_dime, switcher == 1, 
                            select = c('Cand.ID', 'fecyear', 'dwdime', 'dwnom1', 
                                       'num_unq_donors')),
                     by.x = c('fec_rep_cand_id', 'election_year'), 
                     by.y = c('Cand.ID', 'fecyear'), all.x = T) %>% 
  distinct() %>% 
  mutate(rf_score_rep = if_else(is.na(rf_score_rep) ==T, dwdime, rf_score_rep),
         dwnom_score_rep = if_else(is.na(dwnom_score_rep)== T, dwnom1, 
                                   dwnom_score_rep),
         n_donors_rep = if_else(is.na(n_donors_rep) == T, num_unq_donors, 
                                n_donors_rep)) %>% 
  select(-c(dwdime, dwnom1, num_unq_donors))

# merge scores to Dem. candidates (switching party) based on FEC id and election year
polls_score <- merge(polls_score, 
                     subset(rf_dime, switcher == 1, 
                            select = c('Cand.ID', 'fecyear', 'dwdime', 'dwnom1', 
                                       'num_unq_donors')),
                     by.x = c('fec_dem_cand_id', 'election_year'), 
                     by.y = c('Cand.ID', 'fecyear'), all.x = T) %>% 
  distinct() %>% 
  mutate(rf_score_dem = if_else(is.na(rf_score_dem) == T, dwdime, rf_score_dem),
         dwnom_score_dem = if_else(is.na(dwnom_score_dem) == T, dwnom1, 
                                   dwnom_score_dem),
         n_donors_dem = if_else(is.na(n_donors_dem) == T, num_unq_donors, 
                                n_donors_dem)) %>% 
  select(-c(dwdime, dwnom1, num_unq_donors))

#saveRDS(polls_score, 'senate_polls1998_2018_score.RDS')

