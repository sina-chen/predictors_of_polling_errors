### gender prediction by name

# read in data
df_final <- read_csv("data/senate/wiki_results/wiki_senate_covariates.csv")


# create variables with separate first name using the help function

source(file = "us_senate/covariates/helper_functions/helper_function_predict_gender.R")

df_final <- df_final %>% 
  sep_function(dem_candidate) %>% 
  sep_function(lib_candidate) %>% 
  sep_function(ind_candidate) %>% 
  sep_function(rep_candidate) %>% 
  sep_function(green_candidate) %>% 
  sep_function(other_candidate) %>% 
  add_column(min_year = 1930, max_year = 1965)






### predicting gender


# predict gender and join back to original df

rep_gender <- predict_gender(df_final, "rep") %>% 
  rename(rep_gender = gender) %>% 
  select(rep_gender, name)

dem_gender <- predict_gender(df_final, "dem") %>% 
  rename(dem_gender = gender) %>% 
  select(dem_gender, name)

# joining

df_final %>% 
  left_join(rep_gender, by = c("first_rep" = "name")) %>% 
  distinct() -> df_final

df_final %>% 
  left_join(dem_gender, by = c("first_dem" = "name")) %>% 
  distinct() -> df_final


# prepare data to join with the scraped polls

df_join <- df_final %>% 
  select(ends_with("gender"), incumbency, dem_candidate, rep_candidate,
         no_candidates, election_year, State, Senator) %>% 
  rename(state_long = State, senator = Senator) %>% 
  mutate(election_year = as.integer(election_year))

# save data as csv for join

write_csv(df_join, "data/senate/wiki_results/wikipedia_covariates_join.csv")
