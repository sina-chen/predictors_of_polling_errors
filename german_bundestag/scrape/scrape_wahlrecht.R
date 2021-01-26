#-------------------------------------------------------------------------------
# Scraping the 1998 to 2020 German Bundestag Election Polls
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
library(dplyr)
library(reshape2)
library(rvest)
library(tidyverse)
library(htmltab)
library(zoo)
library(data.table)

source('scrape/scrape_wahlrecht_helper.R')


#-------------------------------------------------------------------------------
#### Scraping ####

# get institutes
institutes <- get_institutes("https://www.wahlrecht.de/umfragen/index.htm")

# get raw polls including election results
polls_raw <- lapply(institutes, get_institute_polls) %>%  unlist(recursive = F) 
polls_raw <- do.call(rbind, polls_raw)

# save raw polls
#saveRDS(polls_raw, 'bundestag_polls_raw.RDS')

