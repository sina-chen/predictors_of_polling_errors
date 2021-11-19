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


#-------------------------------------------------------------------------------
#### Data cleaning ####

##  results
results <- polls_raw[which(polls_raw$date_raw == 'Wahl 1998' | 
                             polls_raw$sample_size == 'Bundestagswahl' |
                             polls_raw$date == '27.09.1998'),] %>% 
  mutate(election = str_extract(date_raw, '\\d{4}'),
         result = str_remove_all(forecast, '%') %>% 
           str_replace_all(',', '.') %>% 
           str_replace_all('-|â€“|–', '0'),
         result = if_else(str_detect(result, "FW") == T & election == 2021, "8.7", result)) %>% 
  subset(str_detect(forecast, 'PIR') == F)  %>% 
  select(-c(sample_size, period, date_raw, year, forecast, sonst_parties, 
            poll_id)) %>% 
  unique()  %>% 
  mutate(result = as.numeric(result))


## polls 

# remove election results
df_clean <- polls_raw[-which(polls_raw$date_raw == 'Wahl 1998' | 
                                polls_raw$sample_size == 'Bundestagswahl' |
                                polls_raw$date_raw == '27.09.1998'),]

# sample_size, clean period, forecast and year
df_clean <- df_clean %>% 
  mutate(sample_size = if_else(str_detect(sample_size, '[?]') == T, 
                               NA_character_, 
                               sample_size) %>%
           str_remove_all('[^0-9]') %>% 
           as.numeric(),
         year = if_else(is.na(str_detect(date_raw, '\\d{4}')) == F & year != str_extract(date_raw, '\\d{4}'),
                        str_extract(date_raw, '\\d{4}'), year) %>% 
           unlist(),
         forecast = str_replace_all(forecast, '-|^[?]|–', NA_character_))

# remove characters from forecast
df_clean[which(str_detect(df_clean$forecast, '[[:alpha:]]')),] <- df_clean[which(str_detect(df_clean$forecast, '[[:alpha:]]')),] %>% 
  mutate(forecast = str_remove_all(forecast, '[^0-9]') %>% 
           str_remove('^ ') %>% 
           strsplit(split = "") %>%  
           lapply(function(x) sum(as.numeric(x))) %>% 
           unlist())

# set empty cells to NA
df_clean[df_clean == ""] <- NA

# convert date 
df_clean <- df_clean %>% 
  mutate(date = as.Date(str_replace_all(date_raw, '[?][?]', '01'), '%d.%m.%Y'))

# fill missing dates with field period information
df_clean <- df_clean %>% 
  mutate(date = if_else(is.na(date) == T & is.na(period) == F, 
                        as.Date(paste0(period,year), '%d.%m.–%d.%m.%Y'),
                        date),
         date_raw = date_raw %>% 
           str_replace_all('Mrz', 'Mar') %>% 
           str_replace_all('Okt', 'Oct') %>% 
           str_replace_all('Dez', 'Dec') %>% 
           str_replace_all('Mai', 'May.'))

# insert date without information on day
df_clean <- df_clean %>%  
  mutate(date = if_else(str_detect(date_raw, '\\w+[.] \\d+') == T & 
                          is.na(date) == T,
                        as.Date(as.yearmon(date_raw, '%b. %Y'), frac = 1), 
                        date ))

# add election
df_clean <- df_clean %>% 
  mutate(election = case_when(date <= as.Date('27.09.1998', '%d.%m.%Y') |
                                date_raw == 'Wahl 1998' ~ 1998,
                              date > as.Date('27.09.1998', '%d.%m.%Y') &
                                date <= as.Date('22.09.2002', '%d.%m.%Y') ~ 2002,
                              date > as.Date('22.09.2002', '%d.%m.%Y') &
                                date <= as.Date('18.09.2005', '%d.%m.%Y') |
                                year == 2004 ~ 2005,
                              date > as.Date('18.09.2005', '%d.%m.%Y') &
                                date <= as.Date('27.09.2009', '%d.%m.%Y') |
                                year == 2005 ~ 2009, # polls with missing information on date from emnid in 2005 were conducted after the election 2005 (source: wahlrecht)
                              date > as.Date('27.09.2009', '%d.%m.%Y') &
                                date <= as.Date('22.09.2013', '%d.%m.%Y') ~ 2013,
                              date > as.Date('22.09.2013', '%d.%m.%Y') &
                                date <= as.Date('24.09.2017', '%d.%m.%Y') ~ 2017,
                              date > as.Date('24.09.2017', '%d.%m.%Y') &
                                date <= as.Date('26.09.2021', '%d.%m.%Y') ~ 2021,
                              date > as.Date('26.09.2021', '%d.%m.%Y') ~ 2025),
         poll_id = if_else(str_detect(poll_id, "NA") == T, 
                           str_replace(poll_id, "NA", as.character(election)), 
                           poll_id))


# remove NA forecast
df_clean <- df_clean %>% 
  subset(is.na(forecast) == F | forecast != '0') %>% 
  mutate(forecast = as.numeric(forecast)) %>% 
  unique() %>% 
  select(-c(date_raw, year)) 


# add results
df_result <- add_results(df_clean, results) %>% 
  relocate(election, date, institute, sample_size, period, party, sonst_parties,  
           forecast, result)

# save result
#saveRDS(df_result, 'bundestag_polls_1998_2021.RDS')
