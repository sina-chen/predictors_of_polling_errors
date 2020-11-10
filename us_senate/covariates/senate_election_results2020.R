################################################################################
# Scraping US senate election results 2020
# Author: Sina Chen
# source: https://www.washingtonpost.com/elections/election-results/senate-2020/
################################################################################


#### Libraries ####

library(rvest)
library(dplyr)


#### Scraping ####

senate_results2020_url <- 'https://www.washingtonpost.com/elections/election-results/senate-2020/'

senate_results2020 <- senate_results2020_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="__next"]/main/article/section[1]/div[3]/div') %>%
  html_nodes('table') %>%
  html_table() %>% bind_rows() 

senate_results2020 <- senate_results2020 %>%
  rename(state = State,
         rep_result = Rep.,
         dem_result = Dem.) %>%
  mutate(state = str_remove_all(state,'[*]'),
         rep_result = as.numeric(str_remove_all(rep_result,'[%]'))/100,
         dem_result = as.numeric(str_remove_all(dem_result,'[%]'))/100,
         rep_result2 = rep_result/(rep_result + dem_result),
         dem_result2 = dem_result/(rep_result + dem_result),
         election_year = 2020)

saveRDS(subset(senate_results2020,select = -c(RPT.)),'senate_election_results2020.RDS')
  