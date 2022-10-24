#-------------------------------------------------------------------------------
# Merge US senate polls with raising and spending data 1990 -2020
# Author: Sina Chen
# Notes: 
# Source fec
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(fuzzyjoin)


# Data --------------------------------------------------------------------

senate_exp1990_2020 <- readRDS("~/Documents/Uni/PollingError/us/senate/data/covariates/finances/senate_exp1990_2020.RDS")
senate_raising1990_2020 <- readRDS("~/Documents/Uni/PollingError/us/senate/data/covariates/finances/senate_raising1990_2020.RDS")
data_wide_analysis_fte <- readRDS("~/Documents/Uni/PollingError/us/senate/data/data_wide_analysis_fte.RDS")


#-------------------------------------------------------------------------------

# Republican campaign finances --------------------------------------------

#### Prepare poll data ####

# subset cycle, state and name
cand_state_year_rep <- data_wide_analysis_fte %>% 
  select(c("cycle", "state", "candidate_name_rep")) %>% 
  unique() %>% 
  mutate(candidate_name_poll = str_remove_all(tolower(candidate_name_rep), " iii"))

# split first and last name
polls_name <- str_split(cand_state_year_rep$candidate_name_poll, " ", n = 2, 
                  simplify = F) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("first_name" = V1,
         "last_name" = V2)

row.names(polls_name) <- NULL

# add full name and cycle
polls_name <- polls_name %>% 
  mutate(cand_name_poll =  cand_state_year_rep$candidate_name_rep,
         cycle = cand_state_year_rep$cycle)

# clean name
polls_name <- polls_name %>% 
  mutate(last_name = case_when(last_name == "nighthorse campbell" ~ "campbell",
                               last_name == "ross lightfoot" ~ "lightfoot",
                               last_name == "stanley carroll" ~ "carroll",
                               last_name == "bailey hutchison" ~ "hutchison",
                               last_name == "moore capito" ~ "capito",
                               last_name == "lynn land" ~ "land",
                               last_name == "dick mountjoy" ~ "mountjoy",
                               last_name == "rocky raczkowski" ~ "raczkowski",
                               last_name == "chiavacci farley" ~ "farley",
                               TRUE ~ last_name),
         first_name = case_when(last_name == "campbell" & first_name == "ben" ~ "ben nighthorse",
                                last_name == "grassley" & first_name == "chuck" ~ "charles",
                                last_name == "lightfoot" & first_name == "james" ~ "james ross",
                                last_name == "hutchison" & first_name == "kay" ~ "kay bailey",
                                last_name == "capito" & first_name == "shelley" ~ "shelley moore",
                                last_name == "land" & first_name == "terri" ~ "terri lynn",
                                last_name == "raczkowski" & first_name == "andrew" ~ "andrew",
                                last_name == "farley" & first_name == "chele" ~ "chele chiavacci",
                                TRUE ~ first_name),
         cand_name_full = paste0(last_name, " ", first_name)) %>% 
  unique()


#### Campaign finance data ####

# clean candidate name expenditure data
exp_rep <- senate_exp1990_2020 %>% 
  subset(party == "REP") %>% 
  mutate(cand_name_exp = str_remove_all(tolower(candidate_name), " j jr| iii| ii| jr| $|^ |[.]| [a-z] | senator| dr| phd"),
         candidate_name = str_remove(candidate_name, " J JR| JR| [A-Z] | $")) 

# split first and last name
exp_name <- str_split(exp_rep$cand_name_exp, ",", n = 2) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("last_name" = V1,
         "first_name" = V2)
row.names(exp_name) <- NULL

exp_name <- exp_name %>% 
  mutate(cand_name_exp = exp_rep$candidate_name,
         cand_name_full = paste0(last_name, " ", first_name),
         election_year = exp_rep$election_year) %>% 
  unique()

# raising data
raising_rep <- senate_raising1990_2020 %>%
  subset(party == "REP") %>%
  mutate(cand_name_raising = str_remove_all(tolower(candidate_name), " j jr| iii| ii| jr| $|^ |[.]| [a-z] | senator| dr| phd| honorable"),
         candidate_name = str_remove(candidate_name, " J JR| JR| [A-Z] | $")) %>%
  unique()

raising_name <- str_split(raising_rep$cand_name_raising, ",", n = 2) %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  rename("last_name" = V1,
         "first_name" = V2)
row.names(raising_name) <- NULL

raising_name <- raising_name %>%
  mutate(cand_name_raising = raising_rep$candidate_name,
         cand_name_full = paste0(last_name, " ", first_name),
         election_year = raising_rep$election_year) %>%
  unique()

# merge polls and expenditures
exp_name_match_rep <-lapply(seq(1990,2020,2), 
                            function(x) stringdist_join(polls_name %>% 
                                                          filter(cycle == x), 
                                                        exp_name %>% 
                                                          filter(election_year == x), 
                                                        by = c("cand_name_full"),
                                                        method = "jw",
                                                        max_dist = 99,
                                                        distance_col = "dist") %>%
                              group_by(cand_name_poll) %>%
                              slice_min(order_by = dist, n = 1) %>% 
                              unique()) %>% 
  bind_rows()

# check
check <- exp_name_match_rep[with(exp_name_match_rep, which(last_name.x != last_name.y)),]
exp_name_match_rep <- exp_name_match_rep[-with(exp_name_match_rep, which(last_name.x != last_name.y)),]

rm(exp_name, cand_state_year_rep, check)

# 3 miss-match: 
#   - Mark Odom Hatfield (1990): no entry for 1989-1990
#   - Jeff Sessions (1996): no entry
#   - Mark Neumann (1998): no entry

# merge polls and raising
raising_name_match_rep <- lapply(seq(1990,2020,2), 
                                 function(x) stringdist_join(polls_name %>% 
                                                               filter(cycle == x), 
                                                             raising_name %>% 
                                                               filter(election_year == x), 
                                                             by = c("cand_name_full"),
                                                             method = "jw",
                                                             max_dist = 99,
                                                             distance_col = "dist") %>%
                                   group_by(cand_name_poll) %>%
                                   slice_min(order_by = dist, n = 1) %>% 
                                   unique()) %>% 
  bind_rows()
  
# check
check <- raising_name_match_rep[with(raising_name_match_rep, which(last_name.x != last_name.y)),]
raising_name_match_rep <- raising_name_match_rep[-with(raising_name_match_rep, which(last_name.x != last_name.y)),]

rm(raising_name, polls_name, check)


#### Merge ####

# polls with exp rep name
polls_rep <- merge(data_wide_analysis_fte, exp_name_match_rep %>% 
                     select("cand_name_poll", "cand_name_exp", "election_year"),
                   by.x = c("candidate_name_rep", "cycle"), 
                   by.y = c("cand_name_poll", "election_year"),
                   all.x = T)

# polls with exp rep
polls_rep_exp <- merge(polls_rep, exp_rep %>% 
                     select(candidate_name, election_year, exp),
                   by.x = c("cand_name_exp", "cycle"), 
                   by.y = c("candidate_name", "election_year"), all.x = T) %>% 
  rename("exp_rep" = exp) %>% 
  select(-cand_name_exp)

# check
check <- polls_rep_exp[which(is.na(polls_rep_exp$exp_rep)),]

rm(exp_name_match_rep, exp_rep, polls_rep, check)

# polls with raising rep name
polls_rep <- merge(polls_rep_exp, raising_name_match_rep %>% 
                     select("cand_name_poll", "cand_name_raising", "election_year"),
                   by.x = c("candidate_name_rep", "cycle"), 
                   by.y = c("cand_name_poll", "election_year"),
                   all.x = T)

# polls with raising rep
polls_rep_raising <- merge(polls_rep, raising_rep %>% 
                         select(candidate_name, election_year, raising),
                       by.x = c("cand_name_raising", "cycle"), 
                       by.y = c("candidate_name", "election_year"), all.x = T) %>% 
  rename("raising_rep" = raising) %>% 
  select(-cand_name_raising)


# check
check <- polls_rep_raising[which(is.na(polls_rep_raising$raising)),]

rm(raising_name_match_rep, raising_rep, polls_rep, check, polls_rep_exp)


# Democrat campaign finances ----------------------------------------------

#### Prepare poll data ####

# subset cycle, state and name
cand_state_year_dem <- select(data_wide_analysis_fte, 
                              c("cycle", "state", "candidate_name_dem")) %>% 
  unique() %>% 
  mutate(candidate_name_poll = str_remove_all(tolower(candidate_name_dem), ", iii"))

# split first and last name
polls_name <- str_split(cand_state_year_dem$candidate_name_poll, " ", n = 2, 
                        simplify = F) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("first_name" = V1,
         "last_name" = V2)
row.names(polls_name) <- NULL

# add full name and cycle
polls_name <- polls_name %>% 
  mutate(cand_name_poll =  cand_state_year_dem$candidate_name_dem,
         cycle = cand_state_year_dem$cycle)

polls_name <- polls_name %>% 
  mutate(
    last_name = case_when(last_name == "lundergan grimes" ~ "grimes",
                          last_name == "ann radnofsky" ~ "radnofsky",
                          last_name == "nighthorse campbell" ~ "campbell",
                          last_name == "ray lujan" ~ "lujan",
                          last_name == "moseley braun" ~ "braun",
                          last_name == "cortez masto" ~ "masto",
                          last_name == "patrick moynihan" ~ "moynihan",
                          last_name == "edward brennan" ~ "brennan",
                          last_name == "alana mcginty" ~ "mcginty",
                          last_name == "combs weinberg" ~ "weinberg",
                          last_name == "michelle nunn" ~ "nunn",
                          last_name == "steven crumpton" ~ "crumpton",
                          last_name == "davis figures" ~ "figures",
                          last_name == "gordon ball" ~ "ball",
                          TRUE ~ last_name),
    first_name = case_when(last_name == "grimes" & first_name == "alison" ~ "alison grimes",
                           last_name == "radnofsky" & first_name == "barbara" ~ "barbara ann",
                           last_name == "campbell" & first_name == "ben" ~ "ben nighthorse",
                           last_name == "lujan" & first_name == "ben" ~ "ben ray",
                           last_name == "braun" & first_name == "carol" ~ "carol moseley",
                           last_name == "masto" & first_name == "catherine" ~ "catherine cortez",
                           last_name == "moynihan" & first_name == "daniel" ~ "daniel patrick",
                           last_name == "carter" & first_name == "jack" ~ "john william",
                           last_name == "martin" & first_name == "jim" ~ "james francis",
                           last_name == "sasser" & first_name == "jim" ~ "james ralph",
                           last_name == "brennan" & first_name == "joseph" ~ "joseph edward",
                           last_name == "mcginty" & first_name == "kathleen" ~ "kathleen alana",
                           last_name == "weinberg" & first_name == "lois" ~ "lois combs",
                           last_name == "nunn" & first_name == "mary" ~ "mary michelle",
                           last_name == "crumpton" & first_name == "ronald" ~ "ronald steven",
                           last_name == "figures" & first_name == "vivian" ~ "vivian davis",
                           last_name == "ball" & first_name == "william" ~ "william gordon",
                           TRUE ~ first_name),
         cand_name_full = paste0(last_name, " ", first_name)) %>% 
  unique()


#### Prepare campaign finance data ####

# expenditure data
exp_dem <- senate_exp1990_2020 %>% 
  subset(party == "DEM") %>% 
  mutate(cand_name_exp = str_remove_all(tolower(candidate_name), 
                                        " ii+| [a-z] [a-z]{2}| jr| $|^ |[.]| iv| [a-z]$| [a-z] $"),
         candidate_name = str_remove(candidate_name, " J JR| JR| [A-Z] | $")) %>% 
  unique()

# split first and last name
exp_name <- str_split(exp_dem$cand_name_exp, ",", n = 2) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("last_name" = V1,
         "first_name" = V2)
row.names(exp_name) <- NULL

exp_name <- exp_name %>% 
  mutate(cand_name_exp = exp_dem$candidate_name,
         cand_name_full = paste0(last_name, " ", first_name),
         election_year = exp_dem$election_year) %>% 
  unique()

# raising data
raising_dem <- senate_raising1990_2020 %>% 
  subset(party == "DEM") %>% 
  mutate(cand_name_raising = str_remove_all(tolower(candidate_name), ' ii+| [a-z] [a-z]{2}| jr| $|^ |[.]| iv| [a-z]$| [a-z] $'),
         candidate_name = str_remove(candidate_name, " [A-Z] | $")) %>% 
  unique()

raising_name <- str_split(raising_dem$cand_name_raising, ",", n = 2) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename("last_name" = V1,
         "first_name" = V2)
row.names(raising_name) <- NULL

raising_name <- raising_name %>% 
  mutate(cand_name_raising = raising_dem$candidate_name,
         cand_name_full = paste0(last_name, " ", first_name),
         election_year = raising_dem$election_year) %>% 
  unique()

# merge polls and expenditures
exp_name_match_dem <-lapply(seq(1990,2020,2), 
                            function(x) stringdist_join(polls_name %>% 
                                                          filter(cycle == x), 
                                                        exp_name %>% 
                                                          filter(election_year == x), 
                                                        by = c("cand_name_full"),
                                                        method = "jw",
                                                        max_dist = 99,
                                                        distance_col = "dist") %>%
                              group_by(cand_name_poll) %>%
                              slice_min(order_by = dist, n = 1) %>% 
                              unique()) %>% 
  bind_rows()

# check
check <- exp_name_match_dem[with(exp_name_match_dem, which(last_name.x != last_name.y)),]
exp_name_match_dem <- exp_name_match_dem[-with(exp_name_match_dem, which(last_name.x != last_name.y)),]

rm(exp_name, cand_state_year_dem, check)

# 5 miss-match: 
#   - Jim Rogers (2010): no FEC record
#   - Mark Claytom (2012): did not file any FEC records, raised $278 https://ballotpedia.org/Mark_Clayton
#   - Al Franken (2014):
#   - Mike Workman (2016): has no FEC record

# merge polls and raising
raising_name_match_dem <- lapply(seq(1990,2020,2), 
                                 function(x) stringdist_join(polls_name %>% 
                                                               filter(cycle == x), 
                                                             raising_name %>% 
                                                               filter(election_year == x), 
                                                             by = c("cand_name_full"),
                                                             method = "jw",
                                                             max_dist = 99,
                                                             distance_col = "dist") %>%
                                   group_by(cand_name_poll) %>%
                                   slice_min(order_by = dist, n = 1) %>% 
                                   unique()) %>% 
  bind_rows()

# check
check <- raising_name_match_dem[with(raising_name_match_dem, which(last_name.x != last_name.y)),]
raising_name_match_dem <- raising_name_match_dem[-with(raising_name_match_dem, which(last_name.x != last_name.y)),]

rm(raising_name, polls_name, check)


#### Merge ####

# polls with exp dem name
polls_dem <- merge(polls_rep_raising, exp_name_match_dem %>% 
                     select("cand_name_poll", "cand_name_exp", "election_year"),
                   by.x = c("candidate_name_dem", "cycle"), 
                   by.y = c("cand_name_poll", "election_year"),
                   all.x = T)

# polls with exp dem
polls_dem_exp <- merge(polls_dem, exp_dem %>% 
                         select(candidate_name, election_year, exp),
                       by.x = c("cand_name_exp", "cycle"), 
                       by.y = c("candidate_name", "election_year"), all.x = T) %>% 
  rename("exp_dem" = exp) %>% 
  select(-cand_name_exp)

# check
check <- polls_dem_exp[which(is.na(polls_dem_exp$exp_dem)),]

rm(exp_name_match_dem, exp_dem, polls_dem, check)

# polls with raising rep name
polls_dem <- merge(polls_dem_exp, raising_name_match_dem %>% 
                     select("cand_name_poll", "cand_name_raising", "election_year"),
                   by.x = c("candidate_name_dem", "cycle"), 
                   by.y = c("cand_name_poll", "election_year"),
                   all.x = T)

# polls with raising rep
polls_dem_raising <- merge(polls_dem, raising_dem %>% 
                             select(candidate_name, election_year, raising),
                           by.x = c("cand_name_raising", "cycle"), 
                           by.y = c("candidate_name", "election_year"), all.x = T) %>% 
  rename("raising_dem" = raising) %>% 
  select(-cand_name_raising)


# check
check <- polls_dem_raising[which(is.na(polls_dem_raising$raising_dem)),]

rm(raising_name_match_dem, raising_dem, polls_dem, check)


#### Save data ###

saveRDS(polls_dem_raising, "~/Documents/Uni/PollingError/us/senate/data/us_senate_1990_2020_finance.RDS")
