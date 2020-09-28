### file to read in wikipedia tables and set data up for gender
### prediction


# packages ----------------------------------------------------------------



install.packages("genderdata", repos = "http://packages.ropensci.org")
install.packages("gender")
install.packages("tidyverse")


library(tidyverse)
library(genderdata)
library(gender)


# read in files -----------------------------------------------------------

# it is assumed that the working directory is set to the root folder of this github repo
# (predictors_of_polling_errors)

file_list <- list.files(path = "data/wikipedia/senate", pattern = "X")

# efficiently read in all the files

all_files_purr <- purrr::map(file_list, ~readr::read_csv(.x, skip = 1) %>% 
                               rename(State = 1) %>% 
                               select(-4)) %>% 
                               set_names(file_list)


# work on list of dataframes ----------------------------------------------

# split candidate variable at '%' sign and return list of all candidates 
map(all_files_purr, ~.x[["Candidates"]] %>% 
      strsplit("%")) -> splitted_candidates




# extract candidate name, party and voting percentage with helper function

source(file = "us_senate/covariates/helper_function_candidate_info.R")

# apply function to list of candidate names to get list of tidy dataframes

map(splitted_candidates, ~extract_candidate_info(.x)) -> clean_candidate_info


## extract state names from initial list of wikipedia tables

map(all_files_purr, ~.x[["State"]]) -> list_of_states

## extract senator names from initial list of wikipedia tables


map(all_files_purr, ~.x[["Senator"]]) -> list_of_senators


### use helper function to connect single election years with name of state and senators

source(file = "us_senate/covariates/helper_function_reshape_elections.R")


## general elections

reshape_states(clean_candidate_info[["X1998_senate.csv"]], 1998) %>% 
  mutate_all(as.character) -> general_1998

reshape_states(clean_candidate_info[["X2000_senate.csv"]], 2000) %>% 
  mutate_all(as.character) -> general_2000

reshape_states(clean_candidate_info[["X2002_senate.csv"]], 2002) %>% 
  mutate_all(as.character)-> general_2002

reshape_states(clean_candidate_info[["X2004_senate.csv"]], 2004) %>% 
  mutate_all(as.character)-> general_2004

reshape_states(clean_candidate_info[["X2006_senate.csv"]], 2006) %>% 
  mutate_all(as.character)-> general_2006

reshape_states(clean_candidate_info[["X2008_senate.csv"]], 2008) %>% 
  mutate_all(as.character)-> general_2008

reshape_states(clean_candidate_info[["X2010_senate.csv"]], 2010) %>% 
  mutate_all(as.character)-> general_2010

reshape_states(clean_candidate_info[["X2012_senate.csv"]], 2012) %>% 
  mutate_all(as.character)-> general_2012

reshape_states(clean_candidate_info[["X2014_senate.csv"]], 2014) %>% 
  mutate_all(as.character)-> general_2014

reshape_states(clean_candidate_info[["X2016_senate.csv"]], 2016) %>% 
  mutate_all(as.character)-> general_2016

reshape_states(clean_candidate_info[["X2018_senate.csv"]], 2018) %>% 
  mutate_all(as.character)-> general_2018



## special elections


reshape_states_special(clean_candidate_info[["X2000_senate_special.csv"]], 2000) %>% 
  mutate_all(as.character)-> special_2000

reshape_states_special(clean_candidate_info[["X2002_senate_special.csv"]], 2002) %>% 
  mutate_all(as.character)-> special_2002

reshape_states_special(clean_candidate_info[["X2008_senate_special.csv"]], 2008) %>% 
  mutate_all(as.character)-> special_2008

reshape_states_special(clean_candidate_info[["X2010_senate_special.csv"]], 2010) %>% 
  mutate_all(as.character)-> special_2010

reshape_states_special(clean_candidate_info[["X2014_senate_special.csv"]], 2014) %>% 
  mutate_all(as.character)-> special_2014

reshape_states_special(clean_candidate_info[["X2018_senate_special.csv"]], 2018) %>% 
  mutate_all(as.character)-> special_2018


bind_rows(general_1998, general_2000, general_2002, general_2004, general_2006,
          general_2008, general_2010, general_2012, general_2014, general_2016,
          general_2018) -> df_general

bind_rows(special_2000, special_2002, special_2008, special_2010, special_2014,
          special_2018) -> df_special

### clean state names in special elections

df_special$State <- gsub("\\([^()]*\\)", "", df_special$State) 
  


### bind general and special elections together
bind_rows(df_general, df_special) -> df_final



# cleaning of variables ---------------------------------------------------


source(file = "us_senate/covariates/clean_variables.R")



# save file for gender prediction -----------------------------------------

write_csv(df_final, "data/senate/wiki_results/wiki_senate_covariates.csv")















