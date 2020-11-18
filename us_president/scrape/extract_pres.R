########################################################################################
# Extract presidential polls from txt fies
# Author: Sina Chen
#
########################################################################################

#### Libraries ####

library(dplyr)
library(stringr)
library(rvest)
library(readr)
library(openintro) # state abbreviations
library(Metrics)


#### Directory ####

pres_wd <- '~/Documents/Uni/PollingError/Scraping/code_github/president/test'
setwd(pres_wd)


#### Helper functions ####

source('helper_func_pres.R')


#### Get list of polls by state form txt files ####

# Directories with txt content for every year

{
  dir_txt2000 <- paste0(pres_wd, '/year2000/txt')
  dir_txt2004 <- paste0(pres_wd, '/year2004/txt')
  dir_txt2008 <- paste0(pres_wd, '/year2008/txt')
  dir_txt2012 <- paste0(pres_wd, '/year2012/txt')
  dir_txt2016 <- paste0(pres_wd, '/year2016/txt')
  dir_txt2020 <- paste0(pres_wd, '/year2020/txt')
}

{
  poll_raw2000 <- read_pres(dir_txt2000)
  poll_raw2004 <- read_pres(dir_txt2004)
  poll_raw2008 <- read_pres(dir_txt2008)
  poll_raw2012 <- read_pres(dir_txt2012)
  poll_raw2016 <- read_pres(dir_txt2016)
  poll_raw2020 <- read_pres(dir_txt2020)
}



#### Extract presidential polls ####

{
  # 2000
  polls2000 <- lapply(poll_raw2000, clean_split) 
  pres_polls_ls2000 <- lapply(polls2000, rm_other)
  pres_polls2000 <- convert_to_dataframe(pres_polls_ls2000, 
                                         repC = 'Bush|[(]R[)]', 
                                         demC = 'AlGore|Al Gore|Gore|[(]D[)]', 
                                         thirdC = 'Nader|[(]L[)]|[(]I[)]|[(]G[)]', 
                                         year = 2000)
  
  # 2004
  polls2004 <- lapply(poll_raw2004, clean_split)
  pres_polls_ls2004 <- lapply(polls2004, rm_other)
  pres_polls2004 <- convert_to_dataframe(pres_polls_ls2004, repC = 'Bush|[(]R[)]',
                                         demC = 'Kerry|Dean|Lieb|Clark|Clinton|Gephardt|Democrat|Edwards|Demo- crat|Daschle|[(]D[)]', 
                                         thirdC = 'Nader|Third-party|[(]L[)]|[(]I[)]|[(]G[)]', 
                                         year = 2004)
 
  # 2008
  polls2008 <- lapply(poll_raw2008, clean_split)
  pres_polls_ls2008 <- lapply(polls2008, rm_other)
  pres_polls2008 <- convert_to_dataframe(pres_polls_ls2008, 
                                         repC = 'McCain|[(]R[)]|Republican|Romney|Giuliani', 
                                         demC = 'Obama|Clinton|[(]D[)]|Democratic|Kerry|Democra|Edwards', 
                                         thirdC = 'Nader|Third-party|[(]L[)]|[(]I[)]|[(]G[)]', 
                                         year = 2008)
  # 2012
  polls2012 <- lapply(poll_raw2012, clean_split)
  pres_polls_ls2012 <- lapply(polls2012, rm_other)
  pres_polls2012 <- convert_to_dataframe(pres_polls_ls2012, 
                                         repC = '[(]R[)]|Republican|Romney', 
                                         demC = 'Obama|[(]D[)]|Democratic', 
                                         thirdC = 'Third-party|Johnson|Stein|[(]L[)]|[(]I[)]|[(]G[)]', 
                                         year = 2012)

  # 2016
  polls2016 <- lapply(poll_raw2016, clean_split)
  pres_polls_ls2016 <- lapply(polls2016, rm_other)
  pres_polls2016 <- convert_to_dataframe(pres_polls_ls2016, 
                                         repC = '[(]R[)]|Republican|Trump', 
                                         demC = 'Clinton|[(]D[)]|Democratic', 
                                         thirdC = 'Third-party|[(]L[)]|[(]I[)]|[(]G[)]', 
                                         year = 2016)
  # 2020
  polls2020 <- lapply(poll_raw2020, clean_split)
  pres_polls_ls2020 <- lapply(polls2020, rm_other)
  pres_polls2020 <- convert_to_dataframe(pres_polls_ls2020, 
                                         repC = '[(]R[)]|Republican|Trump', 
                                         demC = 'Biden|[(]D[)]|Democratic', 
                                         thirdC = 'Third-party|[(]L[)]|[(]I[)]|[(]G[)]', 
                                         year = 2020)
  
}

# Combine election years
pres_polls2000_2020 <- rbind(pres_polls2000, 
                            pres_polls2004, 
                            pres_polls2008, 
                            pres_polls2012, 
                            pres_polls2016, 
                            pres_polls2020)




# Set missing unformation on sample size to NA
pres_polls2000_2020$n <- na_if(pres_polls2000_2020$n, '-')

# Save data
saveRDS(pres_polls2000_2020, 'pres_polls2000_2020.RDS')

