#-------------------------------------------------------------------------------
# Clean the 1998 to 2020 German Bundestag Election Polls
#
# Source: https://github.com/simonmunzert/gerpol-forecasting-2013-election-polls 
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

source('scrape/scrape_wahlrecht_helper.R')

#### Data ####

polls_raw <- readRDS('bundestag_polls_raw.RDS')
civey_raw <- readRDS('civey_raw.RDS')


#-------------------------------------------------------------------------------
#### Cleaning ####

## get results

# extract results from wahlrecht polls
results <- polls_raw[which(polls_raw$date_raw == 'Wahl 1998' | 
                             polls_raw$sample_size == 'Bundestagswahl' |
                             polls_raw$date == '27.09.1998'),] %>% 
  mutate(election = str_extract(date_raw, '\\d{4}'),
         result = str_remove_all(forecast, '%') %>% 
           str_replace_all(',', '.') %>% 
           str_replace_all('-|â€“|–', '0')) %>% 
  subset(str_detect(forecast, 'PIR') == F)  %>% 
  select(-c(sample_size, period, date_raw, year, forecast, sonst_parties)) %>% 
  unique()  %>% 
  mutate(result = as.numeric(result))

# add results for civey
civey_result <- results[which(results$institute == 'allensbach' &
                                results$election == 2017),] %>%  
  mutate(institute = 'civey')

# merge results
results <- rbind(results, civey_result)


## polls 

# merge wahlrecht polls and civey
polls_merged <- rbind(polls_raw, civey_raw)

# remove election results
df_clean <- polls_merged[-which(polls_merged$date_raw == 'Wahl 1998' | 
                                  polls_merged$sample_size == 'Bundestagswahl' |
                                  polls_merged$date == '27.09.1998'),]

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

# convert date and add exact date dummy
df_clean <- df_clean %>% 
  mutate(date = as.Date(str_replace_all(date_raw, '[?][?]', '01'), '%d.%m.%Y'),
         exact_date = if_else(is.na(date) == F & 
                                str_detect(date_raw, '[?]') == F, 1, 0))

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

# insert date without infromation on day
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
                              date > as.Date('24.09.2017', '%d.%m.%Y') ~ 2021))


# remove NA forecast
df_clean <- df_clean %>% 
  subset(is.na(forecast) == F | forecast != '0') %>% 
  mutate(forecast = as.numeric(forecast)) %>% 
  unique() %>% 
  select(-c(date_raw, year)) 

# add results
df_result <- add_results(df_clean, results) %>% 
  relocate(election, date, exact_date, institute, sample_size, period, party, sonst_parties,  forecast, result)

# add client

df_client <- df_result %>% 
  mutate(client = case_when(institute == 'allensbach' ~ 'faz',
                            institute == 'emnid' & 
                              date <= as.Date('19.08.2004', '%d.%m.%Y') &  
                              !(date %in% c(as.Date('2002-06-25'),
                                            as.Date('2002-07-01'),
                                            as.Date('2002-07-30'),
                                            as.Date('2002-08-06'),
                                            as.Date('2002-08-21'),
                                            as.Date('2002-08-27'),
                                            as.Date('2002-06-10'),
                                            as.Date('2002-09-14'))) ~ 'ntv',
                            institute == 'emnid' & 
                              date %in% c(as.Date('2002-06-25'),
                                          as.Date('2002-07-01'),
                                          as.Date('2002-07-30'),
                                          as.Date('2002-08-06'),
                                          as.Date('2002-08-21'),
                                          as.Date('2002-08-27')) ~ 'die_welt',
                            institute == 'emnid' & 
                              date %in% c(as.Date('2002-06-10'),
                                          as.Date('2002-09-14'),
                                          as.Date('2005-09-10')) |
                              institute == 'insa' &
                              date %in% c(as.Date('2013-04-21'),
                                          as.Date('2013-05-17'),
                                          as.Date('2013-06-01'),
                                          as.Date('2013-09-15'),
                                          as.Date('2017-11-23'),
                                          as.Date('2018-09-21')) ~ 'focus',
                            institute == 'emnid' & 
                              date > as.Date('19.08.2004', '%d.%m.%Y') & 
                              date <= as.Date('09.03.2005', '%d.%m.%Y') |
                              institute == 'emnid' & 
                              date >= as.Date('27.06.2006', '%d.%m.%Y') & 
                              date <= as.Date('20.12.2006', '%d.%m.%Y') |
                              institute == 'emnid' & 
                              date >= as.Date('04.07.2007', '%d.%m.%Y') & 
                              date <= as.Date('15.08.2007', '%d.%m.%Y') |
                              institute == 'emnid' & 
                              date >= as.Date('06.08.2008', '%d.%m.%Y') & 
                              date <= as.Date('27.08.2008', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date %in% c(as.Date('2005-05-18'), as.Date('2006-06-14'), 
                                          as.Date('2007-04-11'), as.Date('2009-04-08')) ~ 'emnid',
                            institute == 'emnid' & 
                              date >= as.Date('16.03.2005', '%d.%m.%Y') & 
                              date <= as.Date('31.05.2005', '%d.%m.%Y') &
                              date != as.Date('18.05.2005', '%d.%m.%Y') |
                              date == as.Date('05.07.2005', '%d.%m.%Y') ~ 'ddp',
                            institute == 'emnid' & 
                              date %in% c(as.Date('2005-06-04'), as.Date('2005-06-11'), 
                                          as.Date('2005-06-18'), as.Date('2005-06-25'),
                                          as.Date('2005-07-02'), as.Date('2005-07-09'),
                                          as.Date('2005-07-16'), as.Date('2005-08-03'),
                                          as.Date('2005-08-10'), as.Date('2005-08-16'),
                                          as.Date('2005-08-23'), as.Date('2005-08-30'),
                                          as.Date('2005-09-07')) ~ 'ber_morgenpost',
                            institute == 'emnid' & 
                              date == as.Date('15.07.2005', '%d.%m.%Y') ~ 'leipziger_volkszeitung',
                            institute == 'emnid' & 
                              date %in% c(as.Date('2005-07-22'), as.Date('2005-07-29'), 
                                          as.Date('2005-08-04'), as.Date('2005-11-08'),
                                          as.Date('2005-08-18'), as.Date('2005-08-25'),
                                          as.Date('2005-09-01'), as.Date('2005-09-08'),
                                          as.Date('2006-06-20'), as.Date('2006-1-28'))  |
                              institute == 'emnid' & 
                              date >= as.Date('13.09.2005', '%d.%m.%Y') & 
                              date <= as.Date('07.06.2006', '%d.%m.%Y') |
                              institute == 'emnid' & 
                              date >= as.Date('30.01.2007', '%d.%m.%Y') & 
                              date <= as.Date('26.06.2007', '%d.%m.%Y') &
                              date != as.Date('20.05.2007', '%d.%m.%Y') |
                              institute == 'emnid' & 
                              date >= as.Date('21.08.2007', '%d.%m.%Y') & 
                              date <= as.Date('18.12.2007', '%d.%m.%Y') &
                              date != as.Date('18.11.2007', '%d.%m.%Y')|
                              institute == 'emnid' & 
                              date >= as.Date('08.01.2008', '%d.%m.%Y') & 
                              date < as.Date('06.08.2008', '%d.%m.%Y') &
                              date != as.Date('02.03.2008', '%d.%m.%Y') |
                              institute == 'emnid' & 
                              date > as.Date('27.08.2008', '%d.%m.%Y') & 
                              date <= as.Date('23.12.2008', '%d.%m.%Y') |
                              institute == 'emnid' & 
                              date >= as.Date('07.01.2009', '%d.%m.%Y') & 
                              date <= as.Date('10.11.2010', '%d.%m.%Y') &
                              date != as.Date('08.04.2009', '%d.%m.%Y')  ~ 'n24',
                            institute == 'emnid' & 
                              date %in% c(as.Date('2007-05-20'), as.Date('2007-11-18'), 
                                          as.Date('2008-03-02')) |
                              institute == 'emnid' & 
                              date >= as.Date('14.11.2010', '%d.%m.%Y') & 
                              date <= as.Date('22.09.2013', '%d.%m.%Y') ~ 'bild_am_so',
                            institute == 'emnid' & 
                              date > as.Date('22.09.2013', '%d.%m.%Y') ~ 'bild_am_so,emnid',
                            institute == 'forsa' & 
                              date <= as.Date('06.03.2002', '%d.%m.%Y') ~ 'die_woche',
                            institute == 'forsa' & 
                              date >= as.Date('13.03.2002', '%d.%m.%Y') &
                              date < as.Date('06.11.2017', '%d.%m.%Y') &
                              date != as.Date('29.06.2017', '%d.%m.%Y') ~ 'stern,rtl',
                            institute == 'forsa' & 
                              date >= as.Date('06.11.2017', '%d.%m.%Y') ~ 'rtl,ntv',
                            institute == 'forsa' & 
                              date == as.Date('29.06.2017', '%d.%m.%Y') ~ 'rtl',
                            institute == 'politbarometer' ~ 'zdf',
                            institute == 'gms' ~ 'gms',
                            institute == 'dimap' & 
                              date <= as.Date('19.12.1998', '%d.%m.%Y') ~ 'bild',
                            institute == 'dimap' & 
                              date > as.Date('19.12.1998', '%d.%m.%Y') &
                              date <= as.Date('24.12.1999', '%d.%m.%Y')~ 'bild, mdr',
                            institute == 'dimap' & 
                              date > as.Date('24.12.1999', '%d.%m.%Y') &
                              date <= as.Date('23.12.2004', '%d.%m.%Y') &
                              !(date  %in% c(as.Date('2002-04-10'), 
                                             as.Date('2002-05-15'),
                                             as.Date('2002-09-04')))~ 'mdr,dimap',
                            institute == 'dimap' & 
                              date  %in% c(as.Date('2002-04-10'), 
                                           as.Date('2002-05-15'),
                                           as.Date('2002-09-04')) ~ 'br',
                            institute == 'dimap' & 
                              date > as.Date('23.12.2004', '%d.%m.%Y') ~ 'ard,dimap',
                            institute == 'insa' & 
                              !(date %in% c(as.Date('2013-04-21'),
                                            as.Date('2013-05-17'),
                                            as.Date('2013-06-01'),
                                            as.Date('2013-09-15'),
                                            as.Date('2017-11-23'),
                                            as.Date('2018-09-21'))) ~ 'bild,insa',
                            institute == 'yougov' & 
                              date >= as.Date('29.06.2017', '%d.%m.%Y') &
                              date <= as.Date('18.12.2019', '%d.%m.%Y') ~ 'rnd',
                            institute == 'yougov' & 
                              date <= as.Date('23.06.2017', '%d.%m.%Y') |
                              institute == 'yougov' & 
                              date >= as.Date('06.03.2020', '%d.%m.%Y') ~ 'yougov',
                            institute == 'civey' ~ 'spiegel'
                            ))


# save result
# saveRDS(df_client, 'data/bundestag_polls_1998_2021.RDS')
