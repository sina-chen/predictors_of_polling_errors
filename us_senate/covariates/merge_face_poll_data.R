# --------------------------------------------------------------------------- #
# Merge with Senate Polls
# --------------------------------------------------------------------------- #

library(tidyverse)

# --------------------------------------------------------------------------- #

polls <- read.csv('senate_wiki_merged.csv')
faces <- read.csv('clarifai_race.csv')

# --------------------------------------------------------------------------- #

faces <- faces %>% 
  select(candidate, clarifai_gender, clarifai_race)

# dem_candidate
faces <- rename(faces, dem_candidate.y = candidate, 
                clarifai_gender_dem = clarifai_gender, 
                clarifai_race_dem = clarifai_race)

polls <- merge(x = polls, y = faces, by = "dem_candidate.y", all.x = TRUE)

# rep_candidate
faces <- rename(faces, rep_candidate.y = dem_candidate.y,
                clarifai_gender_rep = clarifai_gender_dem, 
                clarifai_race_rep = clarifai_race_dem)

polls <- merge(x = polls, y = faces, by = "rep_candidate.y", all.x = TRUE)

# write.csv(polls, 'senate_polls_merged.csv')
