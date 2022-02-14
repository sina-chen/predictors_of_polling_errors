#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Landtag election poll data preparation
# 
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(tidyverse) 


# Data --------------------------------------------------------------------

# poll data
polls <- readRDS("data/landtag_polls_wide94_21.RDS")

# result data
results <- readRDS("data/landtag_voteshare_wide94_21.RDS")


#-------------------------------------------------------------------------------

# Pre-processing ----------------------------------------------------------


# remove polls with missing date or election
polls <- subset(polls, !is.na(date) & !is.na(election_year)) # 27 polls

# add poll id
polls <- polls %>% 
  group_by(state, election_year,institute_clean) %>% 
  mutate(poll_id = paste0(state, election_year, institute_clean, 
                          seq(1:n()))) %>% 
  ungroup()

# Polls with AfD

## identify elections in which AfD participated
with_afd <- polls %>% 
  subset(!is.na(afd)) %>% 
  group_by(state, election_year) %>% 
  summarise(with_afd = 1) %>% 
  ungroup

## polls for election with afd
polls_afd <- merge(polls, with_afd, by = c("election_year", "state")) %>% 
  select(-"with_afd")

## reshape to long
polls_afd_long <- reshape2::melt(polls_afd, id.vars = c("state", 
                                                        "election_year",
                                                        "date", "sample_size", 
                                                        "institute_clean", 
                                                        "institute",  "client",
                                                        "poll_id"),
                             variable.name = "party", value.name = "support")

# impute missing sample size with institute mean sample size
polls_afd_long <- polls_afd_long  %>% 
  group_by(institute_clean) %>% 
  mutate(sample_size = ifelse(is.na(sample_size),
                              mean(sample_size, na.rm = TRUE), sample_size) %>%  
           round())

# impute sample size of polls without institute mean sample size with 1000
polls_afd_long <- polls_afd_long %>% 
  mutate(sample_size = if_else(is.na(sample_size) == T, 1000, sample_size))

rm(with_afd, polls_afd)


# Polls

## add afd to other
polls$oth_afd <- rowSums(polls[,c("afd", "oth")], na.rm = T)
polls <- polls %>% 
  select(-c("afd", "oth"))

## reshape to long
polls_long <- reshape2::melt(polls, id.vars = c("state", "election_year",
                                                "date", "sample_size", 
                                                "institute_clean", "institute", 
                                                "client", "poll_id"),
                             variable.name = "party", value.name = "support")

# impute missing sample size with institute mean sample size
polls_long <- polls_long  %>% 
  group_by(institute_clean) %>% 
  mutate(sample_size = ifelse(is.na(sample_size),
                              mean(sample_size, na.rm = TRUE), sample_size) %>%  
           round())

# impute sample size of polls without institute mean sample size with 1000
polls_long <- polls_long %>% 
  mutate(sample_size = if_else(is.na(sample_size) == T, 1000, sample_size))

rm(polls)


# Results with AfD

## reshape to long
res_afd_long <- reshape2::melt(results, id.vars = c("state", "election_year",
                                                    "election_date"),
                               variable.name = "party", 
                               value.name = "voteshare")


## Results 

## add afd to other
results$oth_afd <- rowSums(results[,c("afd", "oth")], na.rm = T)
results <- results %>% 
  select(-c("afd", "oth"))

## reshaoe to long
res_long <- reshape2::melt(results, id.vars = c("state", "election_year",
                                                "election_date"),
                           variable.name = "party", value.name = "voteshare")



# Merge -------------------------------------------------------------------


# with AfD

## merge results to polls and remove 
  # polls on future election 
  # with missing poll support
polls_afd_res <- merge(polls_afd_long, res_afd_long, 
                       by = c("election_year", "state", "party")) %>% 
  subset(is.na(support) == F & is.na(voteshare) == F)

# compute days to election
polls_afd_res$days_to_election <- difftime(as.Date(polls_afd_res$election_date, 
                                                   "%d.%m.%Y"),
                                           as.Date(polls_afd_res$date, 
                                                   "%Y-$m-%d")) %>% 
  as.numeric() 

# add party, party-state and party-state-year id
order_ks <- unique(sort(paste0(polls_afd_res$party,":",
                                  polls_afd_res$state)))
order_kr <- unique(sort(paste0(polls_afd_res$party,":",
                                     polls_afd_res$state,":", 
                                     polls_afd_res$election_year)))
order_r <- unique(sort(paste0(polls_afd_res$state,":",
                                  polls_afd_res$election_year)))

polls_afd_res <- polls_afd_res %>%  
  mutate(party = factor(party, levels = c("afd", "cdu", "fdp", "gru", "lin", 
                                          "oth", "spd")),
         state = factor(state, levels = c("baden-wuerttemberg", "bayern", 
                                          "berlin", "brandenburg", "bremen", 
                                          "hamburg", "hessen", 
                                          "mecklenburg-vorpommern", 
                                          "niedersachsen", "nrw", 
                                          "rheinland-pfalz", "saarland", 
                                          "sachsen", "sachsen-anhalt", 
                                          "schleswig-holstein", "thueringen")),
         k_int = as.integer(party),
         ks = factor(paste0(party,":",state), levels = order_ks),
         ks_int = as.integer(ks),
         kr = factor(paste0(party, ":", state, ":", election_year), 
                           levels = order_kr, ),
         kr_int = as.integer(kr),
         r = factor(paste0(state,":",election_year), levels = order_r),
         r_int = as.integer(r))

rm(polls_afd_long, results, res_afd_long, order_ks, order_kr, 
   order_r)


# without AfD

# merge results to polls and remove 
  # polls on future election 
  # with missing poll support
  # elections in which "LINKE" did not participate (15 elections (BW 1996, BW 2001, BY 1994, BY 1998, BY 2003, HB 1999, HH 2001, HH 2004, HE 1995, HE 1999, HE 2003, NW 2000, SH 1996, SH 2000, RP 2001), 273 polls)
polls_res <- merge(polls_long, res_long, by = c("election_year", "state", 
                                                "party")) %>% 
  subset(is.na(support) == F & is.na(voteshare) == F &
           !(state == "baden-wuerttemberg" & election_year <= 2001) &
           !(state == "bayern" & election_year <= 2003) &
           !(state == "bremen" & election_year <= 1999) &
           !(state == "hamburg" & election_year <= 2004) &
           !(state == "hessen" & election_year <= 2003) &
           !(state == "nrw" & election_year <= 2000) &
           !(state == "schleswig-holstein" & election_year <= 2000) &
           !(state == "rheinland-pfalz" & election_year <= 2001))

# compute days to election
polls_res$days_to_election <- difftime(as.Date(polls_res$election_date, 
                                               "%d.%m.%Y"),
                                       as.Date(polls_res$date, "%Y-$m-%d")) %>% 
  as.numeric() 

# add party, party-state and party-state-year id
order_ks <- unique(sort(paste0(polls_res$party,":",polls_res$state)))
order_kr <- unique(sort(paste0(polls_res$party,":",polls_res$state,":", 
                                     polls_res$election_year)))
order_r <- unique(sort(paste0(polls_res$state,":",polls_res$election_year)))


polls_res <- polls_res %>%  
  mutate(party = factor(party, levels = c("cdu", "fdp", "gru", "lin", "oth_afd", 
                                          "spd")),
         state = factor(state, levels = c("baden-wuerttemberg", "bayern", 
                                          "berlin", "brandenburg", "bremen", 
                                          "hamburg", "hessen", 
                                          "mecklenburg-vorpommern", 
                                          "niedersachsen", "nrw", 
                                          "rheinland-pfalz", "saarland", 
                                          "sachsen", "sachsen-anhalt", 
                                          "schleswig-holstein", "thueringen")),
         pt_id = as.integer(party),
         ks = factor(paste0(party,":",state), levels = order_ks),
         ks_int = as.integer(ks),
         kr = factor(paste0(party, ":", state, ":", election_year), 
                           levels = order_kr, ),
         kr_int = as.integer(kr),
         r = factor(paste0(state,":",election_year), levels = order_r),
         r_int = as.integer(r))

rm(polls_long, res_long, order_ks, order_kr, 
   order_r)


# Save data ---------------------------------------------------------------

# saveRDS(polls_res, "data/polls_lt_1994_2021.RDS")
# saveRDS(polls_afd_res, "data/polls_lt_afd_1994_2021.RDS")
