#-------------------------------------------------------------------------------
#
# US Senate predictable: scrape state contol data
#
# Source: https://ballotpedia.org/State_government_trifectas#Changes_in_trifecta_status
# Author: Sina Chen
# 
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(rvest)
library(dplyr)
library(tibble)
library(tidyverse)


# Scraping ----------------------------------------------------------------

# url
state_control_url <- read_html("https://ballotpedia.org/State_government_trifectas#Changes_in_trifecta_status")

# get all tables
tbls <- html_elements(state_control_url,  "table") %>% 
  html_table() 

# select relevant tables and convert to data frame
state_control_tbls <- lapply(tbls[14:63], function(x) t(x) %>% 
                               as.data.frame()  %>%
                               `colnames<-`(.[1, ]) %>%
                               .[-1, ])

# get state names 
state_names <-  html_elements(state_control_url, xpath = '//*[@id="mw-content-text"]/div/h3')[8:57] %>% 
  html_text()

# add state name, combine states, compute unity state control
state_control_data <- lapply(seq(1:50), function(x) state_control_tbls[[x]] %>% 
                               mutate(state = state_names[x]) %>% 
                               rownames_to_column(var = "year")) %>% 
  bind_rows() %>% 
  tidyr::unite(House, c("House", "Assembly"), remove = T, na.rm = T) %>% 
  mutate(state_control = case_when(Governor == "R" & Senate == "R" & House == "R" ~ "rep",
                                   Governor == "D" & Senate == "D" & House == "D" ~ "dem",
                                   TRUE ~ "none"),
         year = if_else(year %in% 92:99, paste0(19,year), paste0(20,year))) %>% 
  select(-c(Governor, Senate, House))

# save data
saveRDS(state_control_data, "~/Documents/Uni/PollingError/us/senate/data/us_senate_state_control.RDS")

