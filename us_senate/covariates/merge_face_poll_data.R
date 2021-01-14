# --------------------------------------------------------------------------- #
# Merge with Senate Polls
# --------------------------------------------------------------------------- #

library(tidyverse)

# --------------------------------------------------------------------------- #

# polls are not part of github as they are fee-based
polls <- read.csv('data/us_senate/senate_wiki_merged.csv') 
faces <- read.csv('data/us_senate/race_gender_checked.csv')

# --------------------------------------------------------------------------- #

polls$dem_candidate <- str_remove_all(polls$dem_candidate, ',')
polls$dem_candidate <- str_replace_all(polls$dem_candidate, 'á', 'a')

faces <- faces %>% 
  select(candidate, gender, race)

# dem_candidate
faces <- rename(faces, dem_candidate = candidate, 
                clarifai_gender_dem = gender, 
                clarifai_race_dem = race)

polls <- merge(x = polls, y = faces, by = "dem_candidate", all.x = TRUE)

# rep_candidate
faces <- rename(faces, rep_candidate = dem_candidate,
                clarifai_gender_rep = clarifai_gender_dem, 
                clarifai_race_rep = clarifai_race_dem)

polls <- merge(x = polls, y = faces, by = "rep_candidate", all.x = TRUE)

# save data (commented out as poll data can't be shared on github)

#write.csv(polls, 'senate_polls_merged.csv')
