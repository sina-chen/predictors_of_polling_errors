# --------------------------------------------------------------------------- #
# Merge with Senate Polls
# --------------------------------------------------------------------------- #

library(tidyverse)

# --------------------------------------------------------------------------- #

polls <- read.csv('senate_wiki_merged.csv')
faces <- read.csv('race_gender_checked.csv')

# --------------------------------------------------------------------------- #

polls$dem_candidate <- str_remove_all(polls$dem_candidate, ',')

faces <- faces %>% 
  select(candidate, gender_checked, race_checked)

# dem_candidate
faces <- rename(faces, dem_candidate = candidate, 
                clarifai_gender_dem = gender_checked, 
                clarifai_race_dem = race_checked)

polls <- merge(x = polls, y = faces, by = "dem_candidate", all.x = TRUE)

# rep_candidate
faces <- rename(faces, rep_candidate = dem_candidate,
                clarifai_gender_rep = clarifai_gender_dem, 
                clarifai_race_rep = clarifai_race_dem)

polls <- merge(x = polls, y = faces, by = "rep_candidate", all.x = TRUE)

#write.csv(polls, 'senate_polls_merged.csv')
