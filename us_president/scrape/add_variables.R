########################################################################################
# Add information on national conventions, swing states and electoral votes
# Author: Sina Chen
# Notes: 
#
########################################################################################

#### Libraries ####

library(lubridate)
library(dplyr)

#### Directory ####

setwd("your working directory")

#### Data ####

# Polls

data_poll <- readRDS('pres_polls2000_2016.RDS') # 5735 obs
data_poll <- subset(data_poll, rep_poll2 != 1)
data_poll <- subset(data_poll, dem_poll2 != 1)

# Electoral votes

data_ev <- readRDS('ev2000_2016.RDS')

# Merge

data <- merge(data_poll, data_ev, by = c('election_year', 'state'))


#### Add variables ####

# National convention dummy

data <- data %>%
  mutate(after_rep = case_when(election_year == 2000 & date < as.Date('08/03/2000','%m/%d/%Y') ~ 1,
                               election_year == 2004 & date < as.Date('09/02/2004','%m/%d/%Y') ~ 1,
                               election_year == 2008 & date < as.Date('09/04/2008','%m/%d/%Y') ~ 1,
                               election_year == 2012 & date < as.Date('08/30/2012','%m/%d/%Y') ~ 1,
                               election_year == 2016 & date < as.Date('07/21/2016','%m/%d/%Y') ~ 1),
         after_dem = case_when(election_year == 2000 & date < as.Date('08/17/2000','%m/%d/%Y') ~ 1,
                               election_year == 2004 & date < as.Date('07/29/2004','%m/%d/%Y') ~ 1,
                               election_year == 2008 & date < as.Date('08/28/2008','%m/%d/%Y') ~ 1,
                               election_year == 2012 & date < as.Date('09/06/2012','%m/%d/%Y') ~ 1,
                               election_year == 2016 & date < as.Date('07/28/2016','%m/%d/%Y') ~ 1),
         state_year = paste0(election_year,'_',state)) %>%
  mutate_at(vars(after_rep,after_dem), ~replace(., is.na(.), 0))

# Swing states, source:
  # 2000: https://www.theglobeandmail.com/news/world/maps-swing-states-and-2000-election/article1139884/
  # 2004: https://en.wikipedia.org/wiki/2004_United_States_presidential_election,
  # 2008: https://www.washingtonpost.com/wp-srv/politics/interactives/campaign08/battleground_cheat_sheet.html
  # 2012: https://www.washingtonpost.com/blogs/the-fix/post/the-9-swing-states-of-2012/2012/04/16/gIQABuXaLT_blog.html
  # 2016: https://www.politico.com/blogs/swing-states-2016-election/2016/06/what-are-the-swing-states-in-2016-list-224327)

data <- data %>%
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
                           state == 'WI' & election_year == 2016 ~ 1)) %>%
  mutate_at(vars(swing), ~replace(., is.na(.), 0))


# Days to election scaled

data <- data %>%
  group_by(election_year) %>%
  mutate(dte_sc = as.numeric(dte)/max(as.numeric(dte)))

# Save

saveRDS(data, 'data_poll2000_2016.RDS')

