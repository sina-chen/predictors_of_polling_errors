#-------------------------------------------------------------------------------
# Merge BTW polls from civey to polls from wahlrecht.de 
#
# Source: https://dawum.de/Bundestag/Civey/
#
# Author: Sina Chen
#-------------------------------------------------------------------------------

#### Libraries ####

library(stringr)
library(dplyr)
library(reshape2)
library(tidyverse)
library(zoo)
library(data.table)


#### Data ####

polls_raw <- readRDS('data/german_bundestag/polls1998_2021_raw.RDS')
civey_raw <- readRDS('data/civey_raw.RDS')


#-------------------------------------------------------------------------------
#### Cleaning ####

## civey

# clean data, add election & sample size
civey_processed <- civey_raw %>% 
  mutate(date = as.Date(date_raw, "%d.%m.%Y"),
         election = case_when(date <= as.Date("2017-09-24", "%Y-%m-%d") ~ 2017,
                              date > as.Date("2017-09-24", "%Y-%m-%d") &
                                date <= as.Date("2021-09-26", "%Y-%m-%d")~ 2021),
         sample_size = 5000) %>% 
  group_by(election, party) %>% 
  mutate(poll_id = paste0(election, institute, seq(1:n()))) %>% 
  ungroup() 

# add results
results <- polls_raw %>% 
  group_by(election, party, result) %>% 
  summarise() %>% 
  subset(!is.na(result) & election %in% c(2017, 2021) & party != "Sonstige")

civey_result <- merge(civey_processed, results, by = c("election", "party"), 
                      all.x = T) %>% 
  select(election, date, institute, sample_size, period, party, sonst_parties,
         forecast, result, poll_id)



## polls 

# merge wahlrecht polls and civey
polls_merged <- rbind(polls_raw, civey_result)


# save result
# saveRDS(polls_merged, 'data/german_bundestag/btw_polls_1998_2021_civey.RDS')
