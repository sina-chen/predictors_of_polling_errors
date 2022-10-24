#-------------------------------------------------------------------------------
# Merge US 538 Senate polls 1990-2020 with swing proxies
#
# Source: 538, Bonica
# Author: Sina Chen
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)

# Data --------------------------------------------------------------------

swing_proxi <- readRDS("~/data/us/senate/us_senate_swing_proxi.RDS")
polls_fte_scores <- readRDS("~/data/us/senate/polls_fte_scores.RDS")


#-------------------------------------------------------------------------------

# Preparation -------------------------------------------------------------

swing_proxi <- swing_proxi %>% 
  subset(special == F|special == T & state == "ILLINOIS" & year == 2010) %>% 
  select(state, year, margin2_rep_prev, winner3_rep_prev) %>% 
  mutate(state = str_to_title(state))



# Merge -------------------------------------------------------------------

# set previous margin to 0 for AR 2010 and GA 1992 and 2002 since no Rep. candidate in previous election
polls_swing <- merge(polls_fte_scores, swing_proxi,by.x = c("state", "cycle"), 
               by.y = c("state", "year"), all.x = T) %>% 
  mutate(margin2_rep_prev = if_else(state == "Georgia" & cycle %in% c(1992, 2002) |
                                      state == "Arkansas" & cycle == 2010, 0, margin2_rep_prev))

# save
saveRDS(polls_swing, "~/data/us/senate/us_senate_polls_swing.RDS")

