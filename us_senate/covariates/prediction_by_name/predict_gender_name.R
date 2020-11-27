################################################################################
# Predict gender by name
# Author: Philipp Bosch
#
# note: It is assumed, your working directory is set to the root folder of this 
#       github repo
################################################################################

### packages
library("devtools")
install_github("eamoncaddigan/GenderGuesser")
library(tidyverse)
library("GenderGuesser")
library(reticulate)


### read in data
wiki_senate_covariates <- read_csv("data/us_senate/wiki_results/wiki_senate_covariates.csv")


### reshape data and clean last names
wiki_senate_covariates %>% 
  pivot_longer(cols = ends_with("candidate"), 
               names_to = "party", 
               values_to = "name") %>%
  drop_na() %>% 
  select(state_long, party, name, election_year) %>% 
  separate(name, 
           into = c("first_name", "rest_name"),
           remove = FALSE,
           sep = "\\s", extra = "merge") %>%
  mutate(rest_name = str_remove_all(rest_name, c("Jr.|,|III"))) %>% 
  mutate(rest_name = str_trim(rest_name)) %>% 
  mutate(rest_name = word(rest_name, -1)) -> predict_df

### save file as csv
write_csv(predict_df, "data/us_senate/predict_df_by_name.csv")

### guess gender
guessGender(predict_df$first_name, countryCode = "US") 


### guess race
use_virtualenv("r-reticulate")
py_install("ethnicolr", pip = TRUE)
py_install("pandas")
virtualenv_install("r-reticulate", "ethnicolr")
virtualenv_install(envname = "r-reticulate", "ethnicolr")

### source python file
source_python("us_senate/covariates/helper_functions/ethnicolr_function.py")

### call python function
predict_race_census("data/us_senate/predict_df_by_name.csv")

