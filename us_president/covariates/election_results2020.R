#-------------------------------------------------------------------------------
# US 2020 presidential eelction results
#
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


#### Libraries ####

library(rvest)
library(tidyverse)


#-------------------------------------------------------------------------------

#### Scrape ####

res2020_raw <- read_html('https://en.wikipedia.org/wiki/2020_United_States_presidential_election')  %>% 
  html_element(xpath = '//*[@id="mw-content-text"]/div[1]/div[42]/table') %>% 
  html_table() %>% 
  select(c(1,3,6))


#### Clean ####

res_2020 <- res2020_raw[-c(1, nrow(res2020_raw) -1, nrow(res2020_raw)),] %>% 
  sapply(function(x) str_remove_all(x, '%')) %>%  
  as.data.frame() %>% 
  rename(state = 'State ordistrict',
         rep_result = 'Trump/PenceRepublican',
         dem_result = 'Biden/HarrisDemocratic') %>% 
  subset(str_detect(state, '-|D[.]C[.]') == F) %>% 
  mutate(rep_result = as.numeric(rep_result)/100,
         dem_result = as.numeric(dem_result)/100,
         state =  recode(state, 
                         `Ala.` = 'AL', 
                         Alaska = 'AK', 
                         Arizona = 'AZ', 
                         `Ark.` = 'AR', 
                         `Calif.` = 'CA', 
                         `Colo.` = 'CO', 
                         `Conn.` = 'CT', 
                         `Del.` = 'DE', 
                         `Florida` = 'FL', 
                         `Georgia` = 'GA',
                         Hawaii = 'HI',
                         Idaho = 'ID',   
                         Illinois = 'IL', 
                         Indiana = 'IN',  
                         Iowa = 'IA',     
                         Kansas = 'KS', 
                         `Ky.` = 'KY',      
                         `La.` = 'LA',     
                         `Maine †` = 'ME',  
                         `Md.` = 'MD',      
                         `Mass.` = 'MA',    
                         `Mich.` = 'MI',  
                         `Minn.` = 'MN',    
                         `Miss.` = 'MS',   
                         `Mo.` = 'MO',
                         `Mont.` = 'MT',    
                         `Neb. †` = 'NE',   
                         `Nev.[o]` = 'NV',  
                         `N.H.` = 'NH',     
                         `N.J.[p]` = 'NJ',
                         `N.M.` = 'NM',     
                         `N.Y.[q]` = 'NY',
                         `N.C.` = 'NC',     
                         `N.D.` = 'ND',     
                         Ohio = 'OH',     
                         `Okla.` = 'OK',   
                         Oregon = 'OR',   
                         `Pa.` = 'PA',    
                         `R.I.` = 'RI',     
                         `S.C.` = 'SC',     
                         `S.D.` = 'SD',
                         `Tenn.` = 'TN',   
                         `Texas[s]` = 'TX', 
                         Utah = 'UT',    
                         `Vt.` = 'VT',
                         `Va.` = 'VA',
                         `Wash.` = 'WA',
                         `W.Va.` = 'WV',
                         `Wis.` = 'WI',
                         `Wyo.` = 'WY'
                         ))


#### Save ####

saveRDS(res_2020, 'election_result2020.RDS')
