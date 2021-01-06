#-------------------------------------------------------------------------------
# Compare Bonic's CF and RF score
# Author: Sina Chen
# Notes: cf score rescaled to -1:1
#-------------------------------------------------------------------------------

#### Librraies ####

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(wiqid)


#### Data ####

polls <- readRDS("senate_polls1998_2018_score.RDS")


#------------------------------------------------------------------------------#

#### Pre-processing ####

# subset election level data

election <- polls %>% group_by(election_year, state,
                               rf_score_rep, rf_score_dem, 
                               cf_score_rep, cf_score_dem) %>% 
  summarise()

# reshape scores to long format
score_long_rf <- election %>% 
  subset(select = c(election_year, state, rf_score_rep, rf_score_dem)) %>% 
  as.data.frame() %>% 
  mutate(score = 'rf') %>% 
  melt(id.vars = c('election_year', 'state','score'), 
       variable.name = 'score_type', value.name = 'score_value')

score_long_cf <- election %>% 
  subset(select = c(election_year, state, cf_score_rep, cf_score_dem)) %>% 
  as.data.frame() %>% 
  mutate(score = 'cf')  %>% 
  melt(id.vars = c('election_year', 'state', 'score'), 
       variable.name = 'score_type', value.name = 'score_value')

score_long <- rbind(score_long_rf, score_long_cf)


# standardize and reshape scores to long format
score_long_rf_st <- election %>% 
  subset(select = c(election_year, state, rf_score_rep, rf_score_dem)) %>% 
  as.data.frame() %>% 
  mutate(rf_score_rep = standardize(rf_score_rep),
         rf_score_dem = standardize(rf_score_dem)) %>% 
  melt(id.vars = c('election_year', 'state'), 
       variable.name = 'score_type', value.name = 'score_value')

score_long_cf_st <- election %>% 
  subset(select = c(election_year, state, cf_score_rep, cf_score_dem)) %>% 
  as.data.frame() %>% 
  mutate(cf_score_rep = standardize(cf_score_rep),
         cf_score_dem = standardize(cf_score_dem)) %>% 
  melt(id.vars = c('election_year', 'state'), 
       variable.name = 'score_type', value.name = 'score_value')

score_long_st <- rbind(score_long_rf_st, score_long_cf_st)

# Year mean cf-score by party
year_mean_cf <- election %>%
  group_by(election_year) %>%
  summarise_at(vars(cf_score_rep, cf_score_dem), mean, na.rm = T) %>% 
  melt(id.vars = c('election_year'), variable.name = 'party', value.name = 'cf_score')

year_mean_rf <- election %>%
  group_by(election_year) %>%
  summarise_at(vars(rf_score_rep, rf_score_dem), mean, na.rm = T) %>% 
  melt(id.vars = c('election_year'), variable.name = 'party', 
       value.name = 'rf_score')



#------------------------------------------------------------------------------#

#### Plots ####

# plot standardized scores  

ggplot(score_long_st, aes(x = score_value, color = score_type)) +
  geom_density() 

# plot unstandardized scores  

ggplot(score_long, aes(x = score_value, color = score_type)) +
  geom_density() +
  facet_wrap(~score, scales = 'free') +
  scale_color_manual(name = 'Score type and party', 
                     labels = c('RF scoe Rep.', 'RF scoe Dem.', 
                                'CF scoe Rep.', 'CF scoe Dem.'), 
                     values = c('darkred', 'darkblue', 'red', 'blue')) +
  labs(x = 'Score')

# Year mean cf score by party 
ggplot(year_mean_cf, aes(x = election_year, y = cf_score, color = party)) +
geom_line() +
  geom_point() +
  scale_color_discrete(name = 'Party', labels = c('Rep.', 'Dem.')) +
  labs(x = 'Year', y = 'Year mean CF Score') 

# Year mean rf score by party 
ggplot(year_mean_rf, aes(x = election_year, y = rf_score, color = party)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name = 'Party', labels = c('Rep.', 'Dem.')) +
  labs(x = 'Year', y = 'Year mean RF Score') 

# Scatterplot score comparison
ggplot(election, aes(x = election_year)) +
  geom_point(aes(y = cf_score_rep, color = 'CF score')) +
  geom_point(aes(y = rf_score_rep, color = 'RF score')) +
  facet_wrap(~state, ncol = 10) +
  labs(x = 'Year', y = 'Score Rep. candidate') +
  scale_color_manual(values = c('CF score' = 'orange', 'RF score' = 'green4'),
                     name = '') +
  ggtitle('CF vs. RF score Rep. candidate') + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks=seq(1998, 2018, 4))

ggplot(election, aes(x = election_year)) +
  geom_point(aes(y = cf_score_dem, color = 'CF score')) +
  geom_point(aes(y = rf_score_dem, color = 'RF score')) +
  facet_wrap(~state, ncol = 10) +
  labs(x = 'Year', y = 'Score Dem. candidate') +
  scale_color_manual(values = c('CF score' = 'orange', 'RF score' = 'green4'),
                     name = '') +
  ggtitle('CF vs. RF score Dem. candidate') + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks=seq(1998, 2018, 4))
