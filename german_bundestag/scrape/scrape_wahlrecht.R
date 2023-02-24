#-------------------------------------------------------------------------------
# Scraping the 1998 to 2023 German Bundestag Election Polls
#
# Source: https://github.com/simonmunzert/gerpol-forecasting-2013-election-polls 
# Author: Peter Selb and Simon Munzert
#
# Modified: Sina Chen
#-------------------------------------------------------------------------------

#### Libraries ####

library(RCurl)
library(XML)
library(stringr)
library(foreign)
library(reshape2)
library(rvest)
library(tidyverse)
library(zoo)
library(data.table)

source('scrape/scrape_wahlrecht_helper.R')


#-------------------------------------------------------------------------------

# Scraping ----------------------------------------------------------------

# get institutes
institutes <- get_institutes("https://www.wahlrecht.de/umfragen/index.htm")

# get raw polls including election results
polls_raw <- lapply(institutes, get_institute_polls) %>%  unlist(recursive = F) 
polls_raw <- do.call(rbind, polls_raw)


#-------------------------------------------------------------------------------

# Processing --------------------------------------------------------------

# get results
results <- get_btw_results(polls_raw)

# clean polls 
clean_polls <- clean_btw_polls(polls_raw)

# add results
df_result <- add_results(clean_polls, results) %>% 
  relocate(election, date, institute, sample_size, period, party, sonst_parties,  
           forecast, result)

# save result
# saveRDS(df_result, 'polls1998_2023_raw.RDS')
