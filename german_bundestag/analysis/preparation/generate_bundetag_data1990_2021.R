#-------------------------------------------------------------------------------
# Generate final data frame for German Bundestag election poll analysis
# 
# Author: Peter Selb, John Körtner, Philipp Bosch, Sina Chen
#
# Data source: 
#   - https://github.com/zweitstimme/btw-2017
#   - https://www.wahlrecht.de
#-------------------------------------------------------------------------------

#### Libraries ####

library(dplyr)
library(reshape2)
library(data.table)

#### Data ###

# zweitstimme
load("data/german_bundestag/polls_comb_results.RData")
load("data/german_bundestag/ger_model_df.RData")

# wahlrecht scrape
bundestag_polls_1998_2021 <- readRDS("data/german_bundestag/btw_polls_1998_2021_civey.RDS")


#-------------------------------------------------------------------------------
#### Pre-processing ####

### Zweitstimme ###

# subset polls from 1990 to 1998 and select relevant variables
polls_zs <- ger_polls_results %>% 
  subset(year >= 1990 & year <= 1998 & is.na(support) == F, 
         select = c("year", "party", "sample_size", "institute", "date", 
                    "support")) %>% 
  rename(election = year)

# add other support share 
polls_zs_wide <- reshape2::dcast(polls_zs, election + institute + date + sample_size ~ party, 
                       value.var = "support")
polls_zs_wide$party_sum <- rowSums(polls_zs_wide[,c("cdu", "spd", "fdp", "gru", 
                                                 "lin")], na.rm = T)
polls_zs_wide$oth <- 100 - polls_zs_wide$party_sum

# add poll id 
polls_zs_wide <- polls_zs_wide %>% 
  mutate(poll_id = seq(1:nrow(polls_zs_wide)),
         poll_id = paste0(election, institute, poll_id))

# reshape to long 
polls_zs_long <- polls_zs_wide %>% 
  subset(select = -party_sum) %>% 
  reshape2::melt(id.vars = c("election", "institute", "date", "sample_size", 
                             "poll_id"), 
       variable.name = "party", value.name = "support") %>% 
  subset(is.na(support) == F) %>% 
  mutate(institute = factor(institute),
         date = as.Date(date,"%Y-%m-%d"))

# adjust institute names
levels(polls_zs_long$institute) <-  list(dimap = "infratest_dimap", 
                                         politbarometer = "fgruppe_wahlen",
                                         allensbach = "allensbach",
                                         emnid = "emnid",
                                         forsa = "forsa")
  
# add other vote share
results_zs <- ger_df_long %>% 
  subset(year == 1990 | year == 1994 | year == 1998,
         select = c("year", "party", "voteshare")) %>% 
  rename(election = year)

# add election result
polls_zs_res <- merge(polls_zs_long, results_zs, by = c("election", "party")) 

rm(polls_zs, polls_zs_wide, polls_zs_long, results_zs, 
   ger_df_long, ger_polls_results, ger_results_lag)


### Wahlrecht ###

## Without AfD 2002 - 2021 ##

bundestag_polls_1998_2021$forecast <- as.numeric(bundestag_polls_1998_2021$forecast)

# subset polls from 2002 - 2021 and select relevant variables
polls_wr <- bundestag_polls_1998_2021 %>% 
  subset(election >= 2002  &
           party %in% c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "SPD"),
         # & exact_date == 1,
         select = c("election", "party", "sample_size", "institute", "date", 
                    "forecast", "result", "poll_id")) %>% 
  rename(support = forecast,
         voteshare = result) %>% 
  droplevels()

# adjust party names
levels(polls_wr$party) <-  list(cdu = "CDU/CSU", 
                                spd = "SPD", 
                                gru = "GRÜNE",
                                fdp = "FDP", 
                                lin = "LINKE")


# compute other support share
polls_wr_wide <- data.table::dcast(data = setDT(polls_wr), 
                          formula =  election + institute + date + 
                            sample_size + poll_id ~ party, 
                          value.var = "support", 
                          fun.aggregate = identity, fill = NA)

polls_wr_wide$party_sum <- rowSums(polls_wr_wide[,c("cdu", "spd", "fdp", "gru", 
                                                    "lin")], na.rm = T)
polls_wr_wide$oth <- 100 - polls_wr_wide$party_sum

# reshape to long 
polls_wr_long <- polls_wr_wide %>% 
  subset(select = -party_sum) %>% 
  reshape2::melt(id.vars = c("election", "institute", "date", "sample_size", 
                             "poll_id"), 
       variable.name = "party", value.name = "support") %>% 
  subset(is.na(support) == F)

# compute other vote share
results_wr_wide <- select(polls_wr, c("election", "party", "voteshare")) %>% 
  unique %>% 
  reshape2::dcast(election ~ party, value.var = "voteshare") %>% 
  mutate(oth = 100 - (cdu + fdp + gru + lin + spd))
 
# reshape to long
results_wr_long <- reshape2::melt(results_wr_wide, id.vars = "election", 
                        variable.name = "party", value.name = "voteshare")

# add election result
polls_wr_res <- merge(polls_wr_long, results_wr_long, 
                      by = c("election", "party"))

rm(polls_wr, polls_wr_long, polls_wr_wide, results_wr_long, results_wr_wide)


#-------------------------------------------------------------------------------
#### 1990 - 2021 ####

# merge
polls1990_2021 <- rbind(polls_zs_res, polls_wr_res)

# compute days to election
polls1990_2021 <- polls1990_2021 %>% 
  mutate(days_to_election = case_when(
    election == 1990 ~ difftime(as.Date("12/02/1990","%m/%d/%Y"), date, 
                                units = "days"),
    election == 1994 ~ difftime(as.Date("10/16/1994","%m/%d/%Y"), date, 
                                units = "days"),
    election == 1998 ~ difftime(as.Date("09/27/1998","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2002 ~ difftime(as.Date("09/22/2002","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2005 ~ difftime(as.Date("09/18/2005","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2009 ~ difftime(as.Date("09/27/2009","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2013 ~ difftime(as.Date("09/22/2013","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2017 ~ difftime(as.Date("09/24/2017","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2021 ~ difftime(as.Date("09/26/2021","%m/%d/%Y"), date, 
                                units = "days")))

# impute sample size
polls1990_2021 <- polls1990_2021  %>% 
  group_by(institute) %>% 
  mutate(imp_sample_size = if_else(is.na(sample_size) == T, 1, 0),
         sample_size = ifelse(is.na(sample_size),
                              mean(sample_size, na.rm=TRUE), sample_size))

#### 2013 - 2021 ####

# compute days to election

polls2013_2021 <- polls_afd_wr_res %>% 
  mutate(days_to_election = case_when(
    election == 2013 ~ difftime(as.Date("09/22/2013","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2017 ~ difftime(as.Date("09/24/2017","%m/%d/%Y"), date, 
                                units = "days"),
    election == 2021 ~ difftime(as.Date("09/26/2021","%m/%d/%Y"), date, 
                                units = "days")))

# impute sample size
polls2013_2021 <- polls2013_2021  %>% 
  group_by(institute) %>% 
  mutate(imp_sample_size = if_else(is.na(sample_size) == T, 1, 0),
         sample_size = ifelse(is.na(sample_size),
                              mean(sample_size, na.rm=TRUE), sample_size))


rm(polls_wr_res, polls_zs_res)


#### Save results #####

saveRDS(polls1990_2021, "data/german_bundestag/polls1990_2021.RDS")
saveRDS(polls2013_2021, "data/german_bundestag/polls2013_2021.RDS")


