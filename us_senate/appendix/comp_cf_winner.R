#-------------------------------------------------------------------------------
# Compare Bonica's CF Score between winner and loosers
# Author: Sina Chen
# Notes: 
#-------------------------------------------------------------------------------

#### Librarries ####

library(dplyr)
library(ggplot2)
library(reshape2)


#### Data ####

polls <- readRDS("~senate_polls1998_2018_score_new.RDS")


#-------------------------------------------------------------------------------

#### Pre-processing ####

# election-level data
election <- polls %>% 
  group_by(election_year, state, rep_result2, dem_result2, rep_candidate, dem_candidate, 
           rf_score_rep, rf_score_dem, cf_score_rep, cf_score_dem, 
           clarifai_gender_rep,  clarifai_gender_dem, clarifai_race_rep, clarifai_race_dem) %>% 
  summarise(n_poll = n(),
            bias_rep = mean(rep_poll2 - rep_result2),
            var_rep = var(rep_poll2 - rep_result2)) %>% 
  mutate(winner = if_else(rep_result2 > dem_result2, 'REP', 'DEM'),
         cf_dist = abs(cf_score_rep - cf_score_dem),
         rf_dist = abs(rf_score_rep - rf_score_dem),
         race_rep_dummy = if_else(clarifai_race_rep != 'white', 'non_white', 'white'),
         race_dem_dummy = if_else(clarifai_race_dem != 'white', 'non_white', 'white'))

# winner/loser data Rep. candidates
rep_winner <- election %>% 
  subset(select = c(rep_candidate, winner, rf_score_rep, cf_score_rep,
                    election_year, state)) %>% 
  mutate(winner = if_else(winner == 'REP', 1, 0),
         party = 'REP')  %>% 
  rename(cand = rep_candidate,
         rf_score = rf_score_rep,
         cf_score = cf_score_rep)

# winner/looser data for Dem. candidates
dem_winner <- election %>% 
  subset(select = c(dem_candidate, winner, rf_score_dem, cf_score_dem,
                    election_year, state)) %>% 
  mutate(winner = if_else(winner == 'DEM', 1, 0),
         party = 'DEM')  %>% 
  rename(cand = dem_candidate,
         rf_score = rf_score_dem,
         cf_score = cf_score_dem)

# winner data
winner <- rbind(rep_winner, dem_winner)
rm(rep_winner, dem_winner)


#-------------------------------------------------------------------------------

#### Plots ####

# CF score by winner/loser separate for REp and Dem
ggplot(winner, aes(x = cf_score, color = as.factor(winner), fill = as.factor(winner))) +
  geom_density(alpha = 0.5) +
  labs(x = 'CF score') +
  scale_color_discrete(name = '', labels = c('Loser', 'Winner')) +
  scale_fill_discrete(name = '', labels = c('Loser', 'Winner')) +
  facet_wrap(~party)
