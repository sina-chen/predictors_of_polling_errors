################################################################################
# Preprocess raw wikipedia tables about senate elections
# Author: Philipp Bosch
# Note: set your working directory to root folder of github repo
################################################################################


# packages ----------------------------------------------------------------


install.packages("tidyverse")


library(tidyverse)



# read in files -----------------------------------------------------------

# it is assumed that the working directory is set to the root folder of this github repo
# (predictors_of_polling_errors)

file_list <- list.files(path = "data/us_senate/wikipedia/senate", pattern = "X")

# efficiently read in all the files

setwd("data/us_senate/wikipedia/senate")

all_files_purr <- purrr::map(file_list, ~readr::read_csv(.x, skip = 1) %>% 
                               rename(State = 1) %>% 
                               select(-4)) %>% 
                               set_names(file_list)


# work on list of dataframes ----------------------------------------------

# split candidate variable at '%' sign and return list of all candidates 
map(all_files_purr, ~.x[["Candidates"]] %>% 
      strsplit("%")) -> splitted_candidates




# extract candidate name, party and voting percentage with helper function

setwd("./../../..") # set working directory to root folder again

source(file = "us_senate/covariates/helper_functions/helper_function_candidate_info.R")

# apply function to list of candidate names to get list of tidy dataframes

map(splitted_candidates, ~extract_candidate_info(.x)) -> clean_candidate_info


## extract state names from initial list of wikipedia tables

map(all_files_purr, ~.x[["State"]]) -> list_of_states

## extract senator names from initial list of wikipedia tables


map(all_files_purr, ~.x[["Senator"]]) -> list_of_senators


### use helper function to connect single election years with name of state and senators

source(file = "us_senate/covariates/helper_functions/helper_function_reshape_elections.R")


### loop over regular and special elections

# create separate lists for regular and special elections
list_of_general <- clean_candidate_info[!grepl("special", names(clean_candidate_info))] 
list_of_special <- clean_candidate_info[grepl("special", names(clean_candidate_info))]


# apply function to each regular election in list
for (i in seq_along(list_of_general)) {
  
  tmp <- (substr(names(list_of_general[i]), start = 2, stop = 5))
  
  list_of_general[[i]] <- reshape_states(list_of_general[[i]], tmp) %>% 
    mutate_all(as.character)
  
}

# apply function to each special election in list
for (i in seq_along(list_of_special)) {
  
  tmp <- (substr(names(list_of_special[i]), start = 2, stop = 5))
  
  list_of_special[[i]] <- reshape_states_special(list_of_special[[i]], tmp) %>% 
    mutate_all(as.character)
  
}

### create data frames from nested list objects
bind_rows(list_of_general) -> df_general

bind_rows(list_of_special) -> df_special

### clean state names in special elections

df_special$State <- gsub("\\([^()]*\\)", "", df_special$State) 



### bind general and special elections together
bind_rows(df_general, df_special) -> df_final


# cleaning of variables ---------------------------------------------------


source(file = "us_senate/covariates/helper_functions/clean_variables.R")

df_join <- df_final %>% 
  select(incumbency, dem_candidate, rep_candidate,
         no_candidates, election_year, State, Senator) %>% 
  rename(state_long = State, senator = Senator) %>% 
  mutate(election_year = as.integer(election_year))

# save file for gender prediction -----------------------------------------

write_csv(df_join, "data/us_senate/wiki_results/wiki_senate_covariates.csv")















