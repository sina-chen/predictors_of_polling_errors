#-------------------------------------------------------------------------------
# Add information on national conventions, swing states and electoral votes
# Author: Sina Chen
# Notes: 
#
#-------------------------------------------------------------------------------

#### Libraries ####

library(lubridate)
library(dplyr)
library(usdata)

source('helper_func_pres.R')

#### Directory ####

setwd("your working directory")

#### Data ####

# Polls

data_poll <- readRDS('pres_polls2000_2020.RDS') 


#-------------------------------------------------------------------------------

#### Add variables ####

# Add state abbreviation
data_poll$state <- state2abbr(data_poll$states_long)

# Compute days to election 
data_poll <- data_poll %>% mutate(
  dte = case_when(
    election_year == '2000' ~ difftime(as.Date('11/07/2000', '%m/%d/%Y'), 
                                       as.Date(date), units = 'days'),
    election_year == '2004' ~ difftime(as.Date('11/02/2004', '%m/%d/%Y'), 
                                       as.Date(date), units = 'days'),
    election_year == '2008' ~ difftime(as.Date('11/04/2008', '%m/%d/%Y'), 
                                       as.Date(date), units = 'days'),
    election_year == '2012' ~ difftime(as.Date('11/06/2012', '%m/%d/%Y'), 
                                       as.Date(date), units = 'days'),
    election_year == '2016' ~ difftime(as.Date('11/08/2016', '%m/%d/%Y'), 
                                       as.Date(date), units = 'days'),
    election_year == '2020' ~ difftime(as.Date('11/03/2020', '%m/%d/%Y'), 
                                       as.Date(date), units = 'days')
  ),
  state_year = paste0(state, election_year)
)

# Compute between 0 and 1 scaled days to election
data_poll <- data_poll %>%
  group_by(election_year) %>%
  mutate(dte_sc = as.numeric(dte)/max(as.numeric(dte)))

# Add election results (source: Wikipedia)
election_result <- readRDS('election_result.RDS')
election_result2020 <- readRDS('election_result2020.RDS') %>% 
  mutate(election_year = 2020)
election_result <- rbind(election_result, election_result2020)

data_poll <- merge(data_poll, election_result, 
                             by = c('election_year', 'state'), all.x = F)


# Scale poll to percentage & compute two-party vote share
data_poll <- data_poll %>%
  mutate(rep_poll = as.numeric(rep_poll)/100,
         dem_poll = as.numeric(dem_poll)/100,
         refused = as.numeric(refused)/100,
         undecided = as.numeric(undecided)/100,
         third_party = as.numeric(third_party)/100,
         other = as.numeric(other)/100,
         rep_result2 = rep_result/(rep_result + dem_result),
         dem_result2 = dem_result/(rep_result + dem_result),
         rep_poll2 = rep_poll/(rep_poll + dem_poll),
         dem_poll2 = dem_poll/(rep_poll + dem_poll))

# Format respondents
data_poll$resp_formated <- sapply(data_poll$respondents, resp)

# Add turnout (source: Wikipedia)
turnout <- readRDS('turnout.RDS')
data_poll <- merge(data_poll, turnout, by = c('state', 'election_year'), 
                   all.x = T)

# Electoral votes
data_ev <- readRDS('ev2000_2016.RDS')

# Merge
data_poll <- merge(data_poll, data_ev, by = c('election_year', 'state'),
                   all.x = T)

# National convention dummy
data_poll <- data_poll %>%
  mutate(after_rep = case_when(election_year == 2000 & date < as.Date('08/03/2000','%m/%d/%Y') ~ 1,
                               election_year == 2004 & date < as.Date('09/02/2004','%m/%d/%Y') ~ 1,
                               election_year == 2008 & date < as.Date('09/04/2008','%m/%d/%Y') ~ 1,
                               election_year == 2012 & date < as.Date('08/30/2012','%m/%d/%Y') ~ 1,
                               election_year == 2016 & date < as.Date('07/21/2016','%m/%d/%Y') ~ 1,
                               election_year == 2020 & date < as.Date('08/27/2020','%m/%d/%Y') ~ 1),
         after_dem = case_when(election_year == 2000 & date < as.Date('08/17/2000','%m/%d/%Y') ~ 1,
                               election_year == 2004 & date < as.Date('07/29/2004','%m/%d/%Y') ~ 1,
                               election_year == 2008 & date < as.Date('08/28/2008','%m/%d/%Y') ~ 1,
                               election_year == 2012 & date < as.Date('09/06/2012','%m/%d/%Y') ~ 1,
                               election_year == 2016 & date < as.Date('07/28/2016','%m/%d/%Y') ~ 1,
                               election_year == 2020 & date < as.Date('08/20/2020','%m/%d/%Y') ~ 1),
         state_year = paste0(election_year,'_',state)) %>%
  mutate_at(vars(after_rep, after_dem), ~replace(., is.na(.), 0))

# Swing states, source:
  # 2000: https://www.theglobeandmail.com/news/world/maps-swing-states-and-2000-election/article1139884/
  # 2004: https://en.wikipedia.org/wiki/2004_United_States_presidential_election,
  # 2008: https://www.washingtonpost.com/wp-srv/politics/interactives/campaign08/battleground_cheat_sheet.html
  # 2012: https://www.washingtonpost.com/blogs/the-fix/post/the-9-swing-states-of-2012/2012/04/16/gIQABuXaLT_blog.html
  # 2016: https://www.politico.com/blogs/swing-states-2016-election/2016/06/what-are-the-swing-states-in-2016-list-224327)
  # 2020: https://www.washingtonpost.com/graphics/2020/politics/united-states-political-geography/

data_poll <- data_poll %>%
  mutate(swing = case_when(state == 'CO' & election_year == 2000 ~ 1,
                           state == 'FL' & election_year == 2000 ~ 1,
                           state == 'IA' & election_year == 2000 ~ 1,
                           state == 'ME' & election_year == 2000 ~ 1,
                           state == 'MN' & election_year == 2000 ~ 1,
                           state == 'NV' & election_year == 2000 ~ 1,
                           state == 'NH' & election_year == 2000 ~ 1,
                           state == 'NM' & election_year == 2000 ~ 1,
                           state == 'OH' & election_year == 2000 ~ 1,
                           state == 'OR' & election_year == 2000 ~ 1,
                           state == 'PA' & election_year == 2000 ~ 1,
                           state == 'WV' & election_year == 2000 ~ 1,
                           state == 'WI' & election_year == 2000 ~ 1,
                           state == 'CO' & election_year == 2004 ~ 1,
                           state == 'FL' & election_year == 2004 ~ 1,
                           state == 'IA' & election_year == 2004 ~ 1,
                           state == 'MI' & election_year == 2004 ~ 1,
                           state == 'MN' & election_year == 2004 ~ 1,
                           state == 'NH' & election_year == 2004 ~ 1,
                           state == 'NM' & election_year == 2004 ~ 1,
                           state == 'NV' & election_year == 2004 ~ 1,
                           state == 'OH' & election_year == 2004 ~ 1,
                           state == 'OR' & election_year == 2004 ~ 1,
                           state == 'PA' & election_year == 2004 ~ 1,
                           state == 'WI' & election_year == 2004 ~ 1,
                           state == 'CO' & election_year == 2008 ~ 1,
                           state == 'FL' & election_year == 2008 ~ 1,
                           state == 'IN' & election_year == 2008 ~ 1,
                           state == 'MO' & election_year == 2008 ~ 1,
                           state == 'NV' & election_year == 2008 ~ 1,
                           state == 'NH' & election_year == 2008 ~ 1,
                           state == 'NC' & election_year == 2008 ~ 1,
                           state == 'OH' & election_year == 2008 ~ 1,
                           state == 'PA' & election_year == 2008 ~ 1,
                           state == 'VA' & election_year == 2008 ~ 1,
                           state == 'CO' & election_year == 2012 ~ 1,
                           state == 'FL' & election_year == 2012 ~ 1,
                           state == 'IA' & election_year == 2012 ~ 1,
                           state == 'NV' & election_year == 2012 ~ 1,
                           state == 'NH' & election_year == 2012 ~ 1,
                           state == 'NC' & election_year == 2012 ~ 1,
                           state == 'OH' & election_year == 2012 ~ 1,
                           state == 'VA' & election_year == 2012 ~ 1,
                           state == 'WI' & election_year == 2012 ~ 1,
                           state == 'CO' & election_year == 2016 ~ 1,
                           state == 'FL' & election_year == 2016 ~ 1,
                           state == 'IQ' & election_year == 2016 ~ 1,
                           state == 'MI' & election_year == 2016 ~ 1,
                           state == 'NV' & election_year == 2016 ~ 1,
                           state == 'NH' & election_year == 2016 ~ 1,
                           state == 'NC' & election_year == 2016 ~ 1,
                           state == 'OH' & election_year == 2016 ~ 1,
                           state == 'PA' & election_year == 2016 ~ 1,
                           state == 'VA' & election_year == 2016 ~ 1,
                           state == 'WI' & election_year == 2016 ~ 1,
                           state == 'MI' & election_year == 2020 ~ 1,
                           state == 'WI' & election_year == 2020 ~ 1,
                           state == 'NC' & election_year == 2020 ~ 1,
                           state == 'FL' & election_year == 2020 ~ 1,
                           state == 'PA' & election_year == 2020 ~ 1,
                           state == 'MN' & election_year == 2020 ~ 1,
                           state == 'AZ' & election_year == 2020 ~ 1,
                           state == 'GA' & election_year == 2020 ~ 1,
                           state == 'TX' & election_year == 2020 ~ 1,
                           state == 'OH' & election_year == 2020 ~ 1)) %>%
  mutate_at(vars(swing), ~replace(., is.na(.), 0))

rm(election_result, election_result2020, data_ev, turnout)

#### Save ####

#saveRDS(data_poll, 'pres_polls2000_2020_covariates.RDS')

