#-------------------------------------------------------------------------------
# Scraping German Bundestags Polls from civey
#
# Source: https://dawum.de/Bundestag/Civey/
#
# Author: Sina Chen
#-------------------------------------------------------------------------------

#### Libraries ####

library(RCurl)
library(XML)
library(rvest)
library(dplyr)
library(stringr)
library(reshape2)


#-------------------------------------------------------------------------------
#### Scrape ####

# url
url_civey <- getURL('https://dawum.de/Bundestag/Civey/')

# get table body
civey_body <- url_civey %>%  
  read_html() %>% 
  html_nodes('div.tbody')  %>% 
  html_children() 

# get table head
civey_head <- url_civey %>%  
  read_html() %>% 
  html_nodes('div.thead')  %>% 
  html_children() %>% 
  html_children() %>% 
  html_text() %>% 
  str_remove_all('[^[[:alpha:]][/]]')

# convert to data frame
civey <- lapply(civey_body, function(x) html_text(html_children(x)))
civey  <- data.frame(matrix(unlist(civey), nrow=length(civey), byrow=T))
names(civey) <- civey_head

# rename
civey <- civey %>% 
  rename(LINKE = 'Linke',
         GRÜNE = 'GrÃne',
         date_raw = 'Datum')

# add relevant columns for merging with wahlrecht.de polls
civey <- civey %>% 
  mutate_all(~str_replace_all(.x,',', '.')) %>% 
  mutate(institute = 'civey',
         period = NA,
         sample_size = NA, 
         sonst_parties = NA,
         year = NA)

# remove rows wirth non poll information
civey <- civey[-which(str_detect(civey$date_raw, 'Dat')),]

# reshape to long format
civey_long <- civey %>% 
  reshape2::melt(id.vars = c('date_raw', 'sample_size', 'period', 'institute', 'year', 'sonst_parties'),
                 value.name = 'forecast',
                 variable.name = 'party')

# save raw polls
#saveRDS(civey_long, 'civey_raw.RDS')
