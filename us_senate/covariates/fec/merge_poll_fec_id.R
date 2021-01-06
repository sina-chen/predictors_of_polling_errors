#------------------------------------------------------------------------------#
# Merge FEC candidate id to senate poll data
# Author: Sina Chen
# Notes: 
#------------------------------------------------------------------------------#

#### Libraries ####

library(dplyr)
library(stringr)
library(readr)
library(reshape2)

#### Data ####

# FEC candidate 
cn_header <- read.csv('cn_header_file.csv')
cn <- cn_header
for(i in seq(1998, 2020, 2)){
  cn_year <-  read_delim(paste0('cn', i, '.txt'), delim = '|', col_names = F)
  print(i)
  cn <- rbind(cn, cn_year)
}
names(cn) <- names(cn_header)

rm(cn_header, cn_year, i)

# poll data 
senate_polls_merged <- read_csv("senate_polls_merged.csv")

#------------------------------------------------------------------------------#

#### Pre-processing ####

### Candidate data ###

# subset senate elections(CAND_OFFICE = S) from candidate information &
  # Rep. and Dem. candidates(CAND_PTY_AFFILIATION = REP/DEM) &
  # clean names &
  # remove double entries
cn_senate <- cn %>% 
  subset(CAND_OFFICE == 'S' & CAND_PTY_AFFILIATION == 'REP' |
           CAND_OFFICE == 'S' & CAND_PTY_AFFILIATION == 'DEM'|
           CAND_OFFICE == 'S' & CAND_PTY_AFFILIATION == 'DFL'|
           CAND_OFFICE == 'S' & CAND_PTY_AFFILIATION == 'IND' & CAND_ID == 'S0AK00196') %>% # ind. candidate was Dem. nominee
  select(CAND_ID, CAND_NAME, CAND_PTY_AFFILIATION, CAND_ELECTION_YR, 
         CAND_OFFICE_ST, CAND_PCC) %>%
  distinct() %>% 
  subset(str_detect(CAND_ID, '^H') == F)

# split name into first name and last name
cn_senate <- cn_senate$CAND_NAME %>%
  str_split_fixed(',', 2) %>%
  as.data.frame() %>%
  rename(last_name = 'V1',
         first_name = 'V2') %>%
  mutate(last_name = tolower(last_name),
         first_name = str_remove_all(tolower(first_name), 
                                     '^[a-z] | [a-z]$| dr| jr| senator| iii')) %>% 
  mutate(last_name = str_replace_all(last_name, 'ben david', 'ben-david')) %>% 
  cbind(cn_senate) %>%
  rename(state = CAND_OFFICE_ST, 
         election_year = CAND_ELECTION_YR)

### Polls ###

# subset candidates, election_year and state &
  # remove double entries &
  # reshape to long format &
poll_name <- senate_polls_merged %>%
  select(rep_candidate, dem_candidate, state, election_year) %>% 
  distinct() %>%
  reshape2::melt(id.vars = c('state', 'election_year')) %>% 
  rename(party = variable,
         name = value) %>%
  mutate(party = if_else(party == 'rep_candidate', 'REP', 'DEM'))

# split name into first and last name
poll_name <- str_split_fixed(str_remove_all(poll_name$name, ' Jr.| III|,'), 
                             ' (?=[^ ]+$|[V][a][n]|[H][a][y])', 2) %>% 
  as.data.frame() %>%
  rename(last_name = 'V2',
         first_name = 'V1') %>% 
  mutate(last_name = tolower(last_name),
         first_name = str_remove_all(tolower(first_name), ' [a-z][.]$')) %>%
  cbind(poll_name)


#-------------------------------------------------------------------------------
#### Merge ####

### Merge FEC id to poll_name ###

# merge by lastname state and election year
name_fec_id <- merge(poll_name, cn_senate, 
                     by = c('last_name', 'state'), 
                     all.x = F) %>%
  mutate(first_name.x = str_replace_all(str_remove_all(first_name.x, 
                                                       ' $|^ |[.]| [a-z] '), 
                                        '\\s+', ' '),
         first_name.y = str_replace_all(str_remove_all(first_name.y, 
                                                       ' $|^ |[.]| [a-z] '), 
                                        '\\s+', ' '))

# identify matching first names
matching <- c()
for (i in 1:length(name_fec_id$first_name.x)){
  res <- str_detect(name_fec_id$first_name.y[i],name_fec_id$first_name.x[i])
  matching <- c(matching, res)
}

name_fec_id$first_name_match <- matching
rm(res, i, matching)

# recode non matching names
name_fec_id <- name_fec_id %>%
  mutate(first_name_match = ifelse(str_detect(first_name.y, 'thomas') == T & str_detect(first_name.x, 'tom') == T |
                                     str_detect(first_name.y, 'james') == T & str_detect(first_name.x, 'jim') == T |
                                     str_detect(first_name.y, 'richard') == T & str_detect(first_name.x, 'rick|dick') == T|
                                     str_detect(first_name.y, 'robert') == T & str_detect(first_name.x, 'bob') == T|
                                     str_detect(first_name.y, 'rob') == T & str_detect(first_name.x, 'robert') == T|
                                     str_detect(first_name.y, 'christopher') == T & str_detect(first_name.x, 'kit') == T|
                                     str_detect(first_name.y, 'michael') == T & str_detect(first_name.x, 'mike') == T|
                                     str_detect(first_name.y, 'leonard') == T & str_detect(first_name.x, 'len') == T|
                                     str_detect(first_name.y, 'joseph') == T & str_detect(first_name.x, 'joe') == T|
                                     str_detect(first_name.y, 'charles') == T & str_detect(first_name.x, 'chuck') == T|
                                     str_detect(first_name.y, 'edward') == T & str_detect(first_name.x, 'ted|ned') == T|
                                     str_detect(first_name.y, 'john') == T & str_detect(first_name.x, 'johnny') == T|
                                     str_detect(first_name.y, 'margaret') == T & str_detect(first_name.x, 'maggie') == T|
                                     str_detect(first_name.y, 'william') == T & str_detect(first_name.x, 'bill') == T|
                                     str_detect(first_name.y, 'dennis') == T & str_detect(first_name.x, 'denny') == T|
                                     str_detect(first_name.y, 'scott') == T & str_detect(first_name.x, 'scotty') == T|
                                     str_detect(first_name.y, 'anton') == T & str_detect(first_name.x, 'tony') == T|
                                     str_detect(first_name.y, 'ed') == T & str_detect(first_name.x, 'edward') == T|
                                     str_detect(first_name.y, 'theodore') == T & str_detect(first_name.x, 'ted') == T|
                                     str_detect(first_name.y, 'ken') == T & str_detect(first_name.x, 'kenneth') == T|
                                     str_detect(first_name.y, 'rochelle') == T & str_detect(first_name.x, 'chellie') == T|
                                     str_detect(first_name.y, 'duncan') == T & str_detect(first_name.x, 'lauch') == T|
                                     str_detect(first_name.y, 'john william') == T & str_detect(first_name.x, 'jack') == T, 
                                   TRUE, first_name_match))

name_fec_id <- name_fec_id %>%
  subset(first_name_match == T)


### Merge FEC id to all polls ###

# merge dem candidates 
fec_id_dem <- name_fec_id %>% 
  subset(select = c('name', 'CAND_ID', 'state')) %>% 
  rename(dem_candidate = name,
         fec_dem_cand_id = CAND_ID) %>% 
  distinct() 

poll_fec <- merge(senate_polls_merged, fec_id_dem, 
              by = c('dem_candidate', 'state'), all.x = T)

# merge dem candidates & remove wroin candidate ID (source FEC)
fec_id_rep <- name_fec_id %>% 
  subset(select = c('name', 'CAND_ID', 'state')) %>% 
  rename(rep_candidate = name,
         fec_rep_cand_id = CAND_ID) %>% 
  distinct() 

poll_fec <- merge(poll_fec, fec_id_rep, 
              by = c('rep_candidate', 'state'), all.x = T)  %>% 

  relocate(state, election_year, date, rep_candidate, dem_candidate) %>% 
  select(-X1)



#saveRDS(poll_fec, 'senate_polls_fec_id.RDS')

