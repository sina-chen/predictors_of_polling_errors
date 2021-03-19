#-------------------------------------------------------------------------------
# Scraping the 1994 to 2021 German Landtag Election Polls
#
# Source: https://www.wahlrecht.de/umfragen/landtage/
# Author: Sina Chen
#
#-------------------------------------------------------------------------------

#### Libraries ####

library(RCurl)
library(XML)
library(htmltab)
library(rvest)
library(dplyr)
library(stringr)
library(reshape2)
library(data.table)
library(forcats)

#### functions ####

source('code/scrape_wahlrecht_landtag_helper.R')


#### data ####

# bundesland names 
state_names <- c('baden-wuerttemberg', 'bayern', 'berlin', 'brandenburg', 
                 'bremen', 'hamburg', 'hessen', 'mecklenburg-vorpommern', 
                 'niedersachsen', 'nrw', 'rheinland-pfalz', 'saarland', 
                 'sachsen', 'sachsen-anhalt', 'schleswig-holstein', 'thueringen')

#-------------------------------------------------------------------------------

#### Scrape polls ####

# scrape
lt_raw <- lapply(state_names, function(x) get_raw_lt(x))
lt_brandenburg_missing <- html_table(read_html('https://www.wahlrecht.de/umfragen/landtage/brandenburg.htm'), 
  header = T, fill = T) [[2]][c(3:25,38),-5]  
  
# set names
names(lt_raw) <- state_names
lt_raw$brandenburg <- append(list(lt_brandenburg_missing), lt_raw$brandenburg)


#### Clean polls ####

# extract polls 
polls_lt_raw <- lapply(lt_raw, function(x) lapply(x, function(y) subset_polls(y)))

# add others vote share
polls_lt_oth <- lapply(polls_lt_raw, 
                       function(x) lapply(x, function(y) add_oth(y)))

# add state identifier
polls_lt_state <- add_state(polls_lt_oth)

# clean sample size
polls_lt_sample_size <- lapply(polls_lt_state, 
                               function(x) lapply(x, function(y) clean_sample_size(y)))

# add election year 
polls_lt_election <- add_election(polls_lt_sample_size)

# clean institute
polls_lt_inst <- clean_institute(polls_lt_election)
  

#### Clean results ####

# extract results
res_lt_raw <- lapply(lt_raw, function(x) lapply(x, function(y) subset_res(y)))

# compute others vote share
res_lt_oth <- lapply(res_lt_raw, function(x) lapply(x, 
                                                    function(y) add_oth_res(y)))

# add election date and year
res_lt_info <- lapply(res_lt_oth, 
                      function(x) lapply(x, function(y) add_election_info(y)))

# add state identifier
res_lt_state <- add_state(res_lt_info)

# adjust party names
res_lt_name <- party_names(res_lt_state)

# save results
saveRDS(polls_lt_election, 'data/landtag_polls_wide.RDS')
saveRDS(res_lt_name, 'data/landtag_voteshare_wide.RDS')
