# merge poll-wiki data to election results data

senate_wiki_merged %>% 
  left_join(senate_results_final, by = c("election_year.x" = "election_year", 
                                         "state" = "state")) -> merged_results_phil

merged_results_phil %>% 
  drop_na(rep_result2.y) -> df_wiki_polls_results


polls <- df_wiki_polls_results
faces <- read_csv("Documents/SEDS/Polling Error/Data Science Project/predictors_of_polling_errors/data/senate/clarifai_race.csv")

# --------------------------------------------------------------------------- #
# merge poll-wiki-results data to clarifai demographics data

polls <- df_wiki_polls_results
faces <- read_csv("Documents/SEDS/Polling Error/Data Science Project/predictors_of_polling_errors/data/senate/clarifai_race.csv")

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

write.csv(polls, 'wiki_polls_results_clarifai_senate.csv')
