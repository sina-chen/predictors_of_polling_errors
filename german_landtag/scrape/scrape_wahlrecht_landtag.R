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
polls_lt_election <- polls_election_long(polls_lt_sample_size)

# clean institute
polls_lt_election <- polls_lt_election %>% 
  mutate(institute_clean = str_remove_all(institute, '\\s[*]|[*]') %>% 
           tolower() %>% 
           str_replace_all('\\s', '_'),
         institute_clean = if_else(institute_clean == 'forschâ€™gr.wahlen' |
                                  institute_clean == 'forschungs-gruppe_wahlen'|
                                  institute_clean == 'forschungs-gruppewahlen'|
                                  institute_clean == 'forschungsgruppewahlen' |
                                  institute_clean == 'fgwtelefonfeld', 'fgw', institute_clean),
         institute_clean = if_else(institute_clean == 'infratest_burke' |
                                institute_clean == 'infratestburke'|
                                institute_clean == 'infratestdimap'|
                                institute_clean == 'infratestpolitik-forschung' |
                                institute_clean == 'infratestpolitikforschung' |
                                institute_clean == 'infratestsozialforschung'|
                                institute_clean == 'tns_forschung'|
                                institute_clean == 'tns_infratest'|
                                institute_clean == 'tnsinfratest'|
                                institute_clean == 'nfo_infratest', 'infratest',institute_clean),
         institute_clean = if_else(institute_clean == 'gessphone_&_field', 'gess', institute_clean),
         institute_clean = if_else(institute_clean == 'ifmleipzig', 'ifm_leipzig', institute_clean),
         institute_clean = if_else(institute_clean == 'universitã¤tkiel', 'uni_kiel', institute_clean),
         institute_clean = if_else(institute_clean == 'universitã¤thamburg', 'uni_hamburg', institute_clean),
         institute_clean = if_else(institute_clean == 'polis', 'polis_sinus', institute_clean),
         institute_clean = if_else(institute_clean == 'polis+sinus', 'polis_sinus', institute_clean),
         institute_clean = if_else(institute_clean == 'mifmmã¼nchen', 'mifm_muenchen', institute_clean),
         institute_clean = if_else(institute_clean == 'inra', 'ipsos', institute_clean)
                                
                                  )



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

# reshape to long and adjust party names
res_lt_long <- party_names_long(res_lt_state)


# save results
# saveRDS(polls_lt_election, 'data/landtag_polls.RDS')
# saveRDS(res_lt_long, 'data/landtag_voteshare.RDS')
