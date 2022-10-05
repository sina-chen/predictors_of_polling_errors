#-------------------------------------------------------------------------------
#
# US Senate poll accuracy 1990 - 2020: 
#   prepare context data
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(dplyr)
library(usdata)
library(readr)
library(readxl)
library(stringr)


# Data --------------------------------------------------------------------

polls <- readRDS("~/data/us/senate/us_senate_polls_swing.RDS") # polls
turnout <- readRDS("~/data/us/senate/senate_turnout1990_2020.RDS") # turnout
covi <- read_excel("~/data/us/COVI Values 1996-2020old and new.xlsx") # covi score
state_control <- readRDS("~/data/us/senate/us_senate_state_control.RDS") # state control
grumbach <- read_csv("data/us/senate/grumbach_state_democracy_index.csv") # state democracy index


#-------------------------------------------------------------------------------

# Preparation -------------------------------------------------------------

# compute covariates 
polls <- polls  %>% 
  group_by(state, cycle) %>% 
  mutate(front_rep = sum(if_else(end_date == min(end_date) & 
                                   pct2_rep >= 0.525,1,0)),
         front_dem = sum(if_else(end_date == min(end_date) & 
                                   pct2_rep < 0.475,1,0)),
         early_date3 = list(sort(end_date, decreasing = F)[1:3]),
         early_date5 = list(sort(end_date, decreasing = F)[1:5])) %>% 
  ungroup() %>% 
  mutate(inc = case_when(candidate_name_rep == senator ~ "Rep. incumbent",
                         candidate_name_dem == senator ~ "Dem. incumbent",
                         TRUE ~ "Open seat") %>% 
           factor(levels = c("Dem. incumbent","Open seat",  "Rep. incumbent")),
         minority = case_when(race_rep == 1 & race_dem == 0 ~ "Rep. minority",
                              race_rep == 0 & race_dem == 1 ~ "Dem. minority",
                              race_rep == 0 & race_dem == 0 | 
                                race_rep == 1 & race_dem == 1 ~ "Neither/both minority") %>% 
           factor(levels = c("Dem. minority","Neither/both minority",  "Rep. minority")),
         minority4 = case_when(race_rep == 1 & race_dem == 0 ~ "Rep. minority",
                               race_rep == 0 & race_dem == 1 ~ "Dem. minority",
                               race_rep == 0 & race_dem == 0 ~ "Neither minority", 
                               race_rep == 1 & race_dem == 1 ~ "Both minority") %>% 
           factor(levels = c("Dem. minority", "Rep. minority",
                             "Neither minority", "Both minority")),
         gender = case_when(gender_rep == 1 & gender_dem == 0 ~ "Rep. female",
                            gender_rep == 0 & gender_dem == 1 ~ "Dem. female",
                            gender_rep == 0 & gender_dem == 0 | 
                              gender_rep == 1 & 
                              gender_dem == 1 ~ "Neither/both female") %>% 
           factor(levels = c("Dem. female", "Neither/both female", "Rep. female")),
         gender4 = case_when(gender_rep == 1 & gender_dem == 0 ~ "Rep. female",
                             gender_rep == 0 & gender_dem == 1 ~ "Dem. female",
                             gender_rep == 0 & 
                               gender_dem == 0 ~ "Neither female", 
                             gender_rep == 1 & 
                               gender_dem == 1 ~ "Both female") %>% 
           factor(levels = c("Dem. female", "Rep. female", "Neither female",
                             "Both female")),
         front = case_when(front_rep == 0 & front_dem >= 1 ~ "Dem. front runner",
                           front_rep == 0 & front_dem == 0 |
                             front_rep == 1 & front_dem == 1 ~ "No front runner",
                           front_rep >= 1 & front_dem == 0 ~ "Rep. front runner"),
         swing_winner_prev = factor(winner3_rep_prev, 
                                    levels = c("solid_dem", "swing_state", "solid_rep"),
                                    labels = c("Swing state", "Safe Dem.", "Safe Rep.")),
         state_abb = state2abbr(state),
         early_poll3 = if_else(end_date %in% unlist(early_date3), 1, 0),
         early_poll5 = if_else(end_date %in% unlist(early_date5), 1, 0)) %>% 
  select(-c(early_date3, early_date5)) %>% 
  group_by(state, cycle) %>% 
  mutate(front3 = case_when(all(pct2_rep[which(early_poll3 == 1)] > 0.525) ~ "Rep. front runner",
                            all(pct2_dem[which(early_poll3 == 1)] > 0.525) ~ "Dem. front runner",
                            TRUE ~ "No front runner"),
         front5 = case_when(all(pct2_rep[which(early_poll5 == 1)] > 0.525) ~ "Rep. front runner",
                            all(pct2_dem[which(early_poll5 == 1)] > 0.525) ~ "Dem. front runner",
                            TRUE ~ "No front runner")) %>% 
  ungroup() 


# turnout
turnout <- turnout %>% 
  mutate(state = str_to_title(state))

polls <- merge(polls, turnout, all.x = T, by.x = c("state", "cycle"), 
               by.y = c("state", "year"))
rm(turnout)

# covi index
covi <- covi %>% 
  select(state, year, FinalCOVI)

polls <- merge(polls, covi, by.x = c("state_abb", "cycle"), 
               by.y = c("state", "year"), all.x = T)
rm(covi)

# state control
state_control <- state_control %>% 
  mutate(prev_year = as.numeric(year) - 2,
         state_abb = state2abbr(state))

polls <- merge(polls, state_control, by.x = c("state","state_abb", "cycle"), 
               by.y = c("state", "state_abb", "year"), all.x = T)

state_control <- state_control %>% 
  rename("prev_state_control" = state_control)

polls <- merge(polls, state_control, by.x = c("state","state_abb", "cycle"), 
               by.y = c("state", "state_abb", "prev_year"), all.x = T)

rm(state_control)

# state democracy index
grumbach <- grumbach %>% 
  mutate(prev_year = year - 1)

polls <- merge(polls, grumbach[,c("state", "year", "democracy_mcmc")], 
               by.x = c("state", "cycle"), by.y = c("state", "year"), all.x = T)

grumbach <- grumbach %>% 
  rename("prev_democracy" = "democracy_mcmc")

polls <- merge(polls, grumbach[,c("state", "prev_year", "prev_democracy")], 
               by.x = c("state", "cycle"), by.y = c("state", "prev_year"), 
               all.x = T) 

rm(grumbach)


# Save data ---------------------------------------------------------------

saveRDS(polls, "~/data/us/senate/us_senate_polls_context.RDS")
