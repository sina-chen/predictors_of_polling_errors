### purrr Senate Lists from Wikipedia ### 

install.packages("genderdata", repos = "http://packages.ropensci.org")
install.packages("gender")
install.packages("tidyverse")
install.packages("visdat")
install.packages("naniar")

library(tidyverse)
library(genderdata)
library(gender)
library(visdat)
library(naniar)



# setwd("~/Documents/SEDS/Polling Error/Data") 

file_list <- list.files(pattern = "X")

# read in files

all_files_purr <- purrr::map(file_list, ~readr::read_csv(.x, skip = 1) %>% 
                               rename(State = 1) %>% 
                               select(-4)) %>% 
  set_names(file_list)

# path to scraped polls

polls_senate1998_2018 <- readRDS("~/Documents/SEDS/Polling Error/Data/polls_senate1998_2018.RDS")

# splitting the Candidates variable of each df in list

map(all_files_purr, ~.x[["Candidates"]] %>% 
      strsplit("%")) -> temp



my_function <- function(my_list_element){
 my_list_element %>% 
    map(function(x) tibble(dta = x)) %>%
    map(function(x) separate(x, col=dta, sep="[()]", into = c("name","party", "pct")) %>%
          mutate(name = trimws(name),
                 party = trimws(party))) %>%
    bind_rows(.id = "frame") %>% 
    drop_na() %>% 
    add_count(frame, name = "no_candidates") -> result
  
  return(result)
}



map(temp, ~my_function(.x)) -> output_of_function



## extract state names

map(all_files_purr, ~.x[["State"]]) -> list_of_states

map(all_files_purr, ~.x[["Senator"]]) -> list_of_senators


## write function to reshape single election years


reshape_states <- function(list_element, election_year){
  
  paste("X", election_year, "_senate.csv", sep = "") -> temporal_name
  
  list_element %>% 
  group_by(frame) %>% 
  slice_head(n = 3) %>% 
  pivot_wider(c(frame, no_candidates), names_from = party, values_from = name) %>% 
  ungroup() %>% 
  mutate(state_number = as.numeric(frame)) %>% 
  select(-frame) %>% 
  arrange(state_number) %>% 
  add_column(election_year = election_year) %>% 
  cbind(State = list_of_states[[temporal_name]]) %>% 
  cbind(Senator = list_of_senators[[temporal_name]]) %>% 
  select(-state_number) -> reshaped_election
  
  return(reshaped_election)
  
}



reshape_states_special <- function(list_element, election_year){
  
  paste("X", election_year, "_senate_special.csv", sep = "") -> temporal_name
  
  list_element %>% 
    group_by(frame) %>% 
    slice_head(n = 3) %>% 
    pivot_wider(c(frame, no_candidates), names_from = party, values_from = name) %>% 
    ungroup() %>% 
    mutate(state_number = as.numeric(frame)) %>% 
    select(-frame) %>% 
    arrange(state_number) %>% 
    add_column(election_year = election_year) %>% 
    cbind(State = list_of_states[[temporal_name]]) %>% 
    cbind(Senator = list_of_senators[[temporal_name]]) %>% 
    select(-state_number) -> reshaped_election
  
  return(reshaped_election)
  
}



## general elections

reshape_states(output_of_function[["X1998_senate.csv"]], 1998) %>% 
  mutate_all(as.character) -> general_1998

reshape_states(output_of_function[["X2000_senate.csv"]], 2000) %>% 
  mutate_all(as.character) -> general_2000

reshape_states(output_of_function[["X2002_senate.csv"]], 2002) %>% 
  mutate_all(as.character)-> general_2002

reshape_states(output_of_function[["X2004_senate.csv"]], 2004) %>% 
  mutate_all(as.character)-> general_2004

reshape_states(output_of_function[["X2006_senate.csv"]], 2006) %>% 
  mutate_all(as.character)-> general_2006

reshape_states(output_of_function[["X2008_senate.csv"]], 2008) %>% 
  mutate_all(as.character)-> general_2008

reshape_states(output_of_function[["X2010_senate.csv"]], 2010) %>% 
  mutate_all(as.character)-> general_2010

reshape_states(output_of_function[["X2012_senate.csv"]], 2012) %>% 
  mutate_all(as.character)-> general_2012

reshape_states(output_of_function[["X2014_senate.csv"]], 2014) %>% 
  mutate_all(as.character)-> general_2014

reshape_states(output_of_function[["X2016_senate.csv"]], 2016) %>% 
  mutate_all(as.character)-> general_2016

reshape_states(output_of_function[["X2018_senate.csv"]], 2018) %>% 
  mutate_all(as.character)-> general_2018



## special elections


reshape_states_special(output_of_function[["X2000_senate_special.csv"]], 2000) %>% 
  mutate_all(as.character)-> special_2000

reshape_states_special(output_of_function[["X2002_senate_special.csv"]], 2002) %>% 
  mutate_all(as.character)-> special_2002

reshape_states_special(output_of_function[["X2008_senate_special.csv"]], 2008) %>% 
  mutate_all(as.character)-> special_2008

reshape_states_special(output_of_function[["X2010_senate_special.csv"]], 2010) %>% 
  mutate_all(as.character)-> special_2010

reshape_states_special(output_of_function[["X2014_senate_special.csv"]], 2014) %>% 
  mutate_all(as.character)-> special_2014

reshape_states_special(output_of_function[["X2018_senate_special.csv"]], 2018) %>% 
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





### cleaning the variables 

# change "NULL" to NA for further analysis and trim leading whitespace

df_final %>% 
  mutate(across(where(is.character), trimws, "left")) -> df_final


df_final %>%
  mutate_all(na_if, "NULL") -> df_final
  
  


# remove string artefacts by Wikipedia ('[]')
df_final <- as_tibble(lapply(df_final, function(x) {
                  gsub("\\[[^][]*]", "", x)
                }))


## merging parties with same meaning to fit the scrape data


# democratic party
df_final$dem_candidate <- coalesce(df_final$Democratic, df_final$DFL)

df_final$dem_candidate <- coalesce(df_final$dem_candidate, df_final$`Democratic Farmer-Labor`)

df_final$dem_candidate <- coalesce(df_final$dem_candidate, df_final$`Democratic-Farmer-Labor`)

df_final$dem_candidate <- coalesce(df_final$dem_candidate, df_final$`Democratic-NPL`)


# merge independent candidates (from party names)

df_final$ind_candidate <- coalesce(df_final$Independent, df_final$`Independent American`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$Independence)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$IPD)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$independent)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`American Independent`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`Independent Green`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`Connecticut for Lieberman`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`Alaskan Independence`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$IAP)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`independent/Write-in`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$MIP)


# republican

# Only one case in "Write-in" -> candidate wasn't chosen to be republican nominee
# ran as write-in in election

df_final$rep_candidate <- coalesce(df_final$Republican, df_final$`Write-in`)


# green party does not need merging

df_final$green_candidate <- df_final$Green

# pooling rest of parties in other


vecs <- list(df_final$`Natural Law`, df_final$Reform, df_final$Marijuana, 
             df_final$`Marijuana Reform`, df_final$`Marijuana[c]`, df_final$Constitution,
             df_final$`U.S. Taxpayers`, df_final$`Concerned Citizens`, df_final$Conservative,
             df_final$`Peace and Freedom`, df_final$Veterans, df_final$politician,
             df_final$Mountain, df_final$Nebraska, df_final$Tea, df_final$W.F.,
             df_final$`Constitution Party`, df_final$`UT Justice`, df_final$Country,
             df_final$`Legal Marijuana Now`, df_final$American) 

df_final$other_candidate <- coalesce(!!!vecs)

# libertarian party

df_final$lib_candidate <- df_final$Libertarian


# create dummy variable if incumbent runs for re-election

df_final <- df_final %>% 
  rowwise() %>% 
  mutate(incumbency = dem_candidate %in% Senator | rep_candidate %in% Senator |
           lib_candidate %in% Senator | green_candidate %in% Senator |
         ind_candidate %in% Senator | other_candidate %in% Senator) 

# again remove white space

df_final %>% 
  mutate(across(where(is.character), trimws, "left")) -> df_final


# remove elections where more than one candidate from the major parties runs for office

df_final <- df_final[grep("c(", df_final$dem_candidate, invert = TRUE, fixed = TRUE) , ]

df_final <- df_final[grep("c(", df_final$rep_candidate, invert = TRUE, fixed = TRUE) , ]

### gender prediction by name


# create variables with separate first name

sep_function <- function(df, party_name){
  
  var_name <- deparse(substitute(party_name)) 
  var_name <- str_split(var_name, pattern = "_")[[1]][1]
  
  print(var_name)
  
  df %>% 
    separate({{party_name}}, into = c(paste("first", var_name, sep = "_"), 
                                      paste("rest", var_name, sep = "_")),
             remove = FALSE,
             sep = "\\s", extra = "merge") -> df
  return(df)
  
}

df_final <- df_final %>% 
  sep_function(dem_candidate) %>% 
  sep_function(lib_candidate) %>% 
  sep_function(ind_candidate) %>% 
  sep_function(rep_candidate) %>% 
  sep_function(green_candidate) %>% 
  sep_function(other_candidate) %>% 
  add_column(min_year = 1930, max_year = 1965)
  
  
# check how the separation worked



# df_final %>% 
#   select(starts_with("first_")) %>% 
#   mutate_all(list(no_characters = ~ nchar(.))) %>% 
#   arrange(first_rep_no_characters) %>% View()




# predicting gender

predict_gender <- function(df, party){
  df %>% 
    gender_df(name_col = paste("first", party, sep = "_"),
              year_col = c("min_year", "max_year"),
              method = "ssa") -> df_gender
  return(df_gender)
}


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



#### working on the scraped polls to enable join


# clean state names / remove white space

polls_senate1998_2018$state_long <- 
  recode(polls_senate1998_2018$state_long, NewJersey = "New Jersey",
       NewMexico = "New Mexico", NewYork = "New York",
       NorthDakota = "North Dakota", RhodeIsland = "Rhode Island",
       NewHampshire = "New Hampshire", SouthCarolina = "South Carolina",
       SouthDakota = "South Dakota") %>% 
  trimws()

# check if worked
unique(polls_senate1998_2018$state_long) 
  


# merge gender data to polls

df_join <- df_final %>% 
  select(ends_with("gender"), incumbency, dem_candidate, rep_candidate,
         no_candidates, election_year, State, Senator) %>% 
        rename(state_long = State, senator = Senator) %>% 
  mutate(election_year = as.integer(election_year))


polls_enriched <- polls_senate1998_2018 %>% 
                    left_join(df_join, by = c("state_long", "election_year"))


# checking for consistency in final data set

vis_miss(polls_enriched) # missings either show failed merge or missing rep/dem candidate


## extract observations where the merge failed 
na_df <- polls_enriched[is.na(polls_enriched$senator),]

# Missings in case of Alaska 2010 come from the fact that two republicans got
# the most votes in the elections -> those elections were excluded
# Same holds for Louisiana 2004 -> Two Democrats in best three

# Missings in the case of 2010 Texas show error in scrape as there was no senate
# election in Texas in 2010. Same holds for Massachusets in 2010.


# remove duplicates in enriched polls

polls_enriched <- distinct(polls_enriched)


write_csv(polls_enriched, "senate_polls_1998_2018_enriched.csv")


## partial string matching from names

# # levenstein distance
# levenstein <- tibble(.rows  = nrow(polls_enriched)) %>%
#   cbind(polls_enriched$rep_candidate.x) %>% 
#   cbind(polls_enriched$rep_candidate.y)
# 
# levenstein$rep_test <- mapply(adist, polls_enriched$rep_candidate.x, polls_enriched$rep_candidate.y)
# 
# 
# # true partial matching
# 
# polls_enriched %>%
#   separate(rep_candidate.y, into = c("first_rep", "rest_rep"),
#            remove = FALSE,
#            sep = "\\s", extra = "merge") %>% 
#   mutate(matched = 
#            str_detect(rep_candidate.x, first_rep) |
#            str_detect(rep_candidate.x, rest_rep)
#   ) %>% select(rep_candidate.y, rep_candidate.x, first_rep, rest_rep, matched) %>% 
#   View()






### republicans


# rep_compare <- polls_enriched %>%
#   separate(rep_candidate.y, into = c("first_rep", "second_rep", "rest_rep"),
#            remove = FALSE,
#            sep = "\\s", extra = "merge") %>% 
#   rowwise() %>%
#   mutate(matched = 
#            grepl(first_rep, rep_candidate.x, fixed = TRUE) ||
#            grepl(second_rep, rep_candidate.x, fixed = TRUE) ||
#            grepl(rest_rep, rep_candidate.x, fixed = TRUE)
#          
#   ) %>% 
#   select(rep_candidate.x, rep_candidate.y, first_rep, second_rep, rest_rep, matched)
# 
# rep_compare %>% 
#   filter(is.na(matched) || matched == FALSE) %>% View()

### recode the found artefacts (Tom Campbell, Bill McCollum, Rocky Raczkowski,
### ,Chuck Grassley, Charles E. Summers, Jr.)


recode(polls_enriched$rep_candidate.x, "Camp- bell (R)" = "Tom Campbell",
       "McCol- lum (R)" = "Bill McCollum", "Andrew Racz-kowski (R)" = "Rocky Raczkowski",
       "Grassely (R)" = "Chuck Grassley", 
       "Charlie Summers (R)" = "Charles E. Summers, Jr.") -> polls_enriched$rep_candidate.x




# dems

# dem_compare <- polls_enriched %>%
#   separate(dem_candidate.y, into = c("first_dem", "second_dem", "rest_dem"),
#            remove = FALSE,
#            sep = "\\s", extra = "merge") %>% 
#   rowwise() %>%
#   mutate(matched = 
#            grepl(first_dem, dem_candidate.x, fixed = TRUE) ||
#            grepl(second_dem, dem_candidate.x, fixed = TRUE) ||
#            grepl(rest_dem, dem_candidate.x, fixed = TRUE)
#          
#   ) %>% 
#   select(dem_candidate.x, dem_candidate.y, first_dem, second_dem, rest_dem, matched)
# 
# dem_compare %>% 
#   filter(is.na(matched) || matched == FALSE) %>% View()

# everywhere the match did not work (NA or False) we must check if it is because
# of artefacts of the scrape or a real difference between the candidates
# This check yields only two artefacts: Bob Casey, Jr. & Dianne Feinstein

recode(polls_enriched$dem_candidate.x, "Fein- stein (D)" = "Dianne Feinstein",
       "Casey" = "Bob Casey, Jr.") -> polls_enriched$dem_candidate.x


### create match variables for republican and democrat

# dems
polls_enriched %>% 
  separate(dem_candidate.y, into = c("first_dem", "second_dem", "rest_dem"),
           remove = FALSE,
           sep = "\\s", extra = "merge") %>% 
  rowwise() %>%
  mutate(matched = 
           grepl(first_dem, dem_candidate.x, fixed = TRUE) ||
           grepl(second_dem, dem_candidate.x, fixed = TRUE) ||
           grepl(rest_dem, dem_candidate.x, fixed = TRUE)) %>% 
  filter(matched == TRUE) %>% 
  select(-c(first_dem, second_dem, rest_dem, matched)) -> polls_enriched


# reps

polls_enriched %>% 
  separate(rep_candidate.y, into = c("first_rep", "second_rep", "rest_rep"),
           remove = FALSE,
           sep = "\\s", extra = "merge") %>% 
  rowwise() %>%
  mutate(matched = 
           grepl(first_rep, rep_candidate.x, fixed = TRUE) ||
           grepl(second_rep, rep_candidate.x, fixed = TRUE) ||
           grepl(rest_rep, rep_candidate.x, fixed = TRUE)) %>% 
  filter(matched == TRUE) %>% 
  select(-c(first_rep, second_rep, rest_rep, matched)) -> polls_enriched



### write to csv

write_csv(polls_enriched, "senate_polls_1998_2018_enriched.csv")


### write lists of unique dems and reps to csv

as.data.frame(unique(polls_enriched$dem_candidate.y)) %>% 
  rename(dem_senator = 1)-> dem_col
as.data.frame(unique(polls_enriched$rep_candidate.y)) %>% 
  rename(rep_senator = 1) -> rep_col

write_csv(dem_col, "democrat_senators.csv")
write_csv(rep_col, "republican_senators.csv")

# ### read in hand cleaned data set (only run this part if we want the names from 
#     the scraped polls)
# 
# senate_polls_1998_2018_enriched_cleaned <- read_delim("senate_polls_1998_2018_enriched_cleaned.csv", 
#                                                       ";", escape_double = FALSE, trim_ws = TRUE)
# 
# 
# senate_polls_1998_2018_enriched_cleaned %>% 
#   rename(rep_candidate_most_votes = rep_candidate.y,
#          dem_candidate_most_votes = dem_candidate.y,
#          rep_candidate = rep_candidate.x,
#          dem_candidate = dem_candidate.x) %>%
#   add_column(min_year = 1930, max_year = 1965) %>% 
#   sep_function(dem_candidate) %>% 
#   sep_function(rep_candidate) -> df_poll_names
#   
# 
# # predict gender of poll names and join back to original df
# 
# rep_gender_poll <- predict_gender(df_poll_names, "rep") %>% 
#   rename(rep_gender_poll = gender) %>% 
#   select(rep_gender_poll, name)
# 
# dem_gender_poll <- predict_gender(df_poll_names, "dem") %>% 
#   rename(dem_gender_poll = gender) %>% 
#   select(dem_gender_poll, name)
# 
# # joining
# 
# df_poll_names %>% 
#   left_join(rep_gender_poll, by = c("first_rep" = "name")) %>% 
#   distinct() -> df_poll_names
# 
# df_poll_names %>% 
#   left_join(dem_gender_poll, by = c("first_dem" = "name")) %>% 
#   distinct() -> df_poll_names
# 
# 
# # relocate/drop variables for better overview
# df_poll_names %>% 
#   select(-c(min_year, max_year)) %>% 
#   rename(rep_gender_most_votes = rep_gender, 
#          dem_gender_most_votes = dem_gender) %>% 
#   relocate(rep_gender_poll, .after = rep_candidate) %>% 
#   relocate(dem_gender_poll, .after = dem_candidate) -> df_poll_names
# 
# write_csv(df_poll_names, "senate_polls_1998_2018_covariates.csv")



# predict race by surname


polls_enriched %>%
  separate(dem_candidate.y,c("start","lastname"), sep="\\s+(?=\\S*$)") %>% 
  select(start, lastname) -> dem_lname

polls_enriched %>%
  separate(rep_candidate.y,c("start","lastname"), sep="\\s+(?=\\S*$)") %>% 
  select(start, lastname) -> rep_lname


# recode jr. to real last name for dems


dem_lname$lastname[dem_lname$start == "Bob Casey"] <- "Casey"
dem_lname$lastname[dem_lname$start == "Bob Casey,"] <- "Casey"
dem_lname$lastname[dem_lname$start == "Harold Ford"] <- "Ford"

# recode reps

rep_lname$lastname[rep_lname$start == "Jack E. Robinson"] <- "Robinson"
rep_lname$lastname[rep_lname$start == "Howard Mills"] <- "Mills"
rep_lname$lastname[rep_lname$start == "Thomas Kean"] <- "Kean"
rep_lname$lastname[rep_lname$start == "Charles E. Summers,"] <- "Summers"


write_csv(rep_lname, "republican_lastname.csv")

write_csv(dem_lname, "democrat_lastname.csv")


### create full split with names

dem_lname %>%
  separate(start,c("surname", "middle"), sep = "\\s", extra = "merge") -> dem_full_split


rep_lname %>%
  separate(start,c("surname", "middle"), sep = "\\s", extra = "merge") -> rep_full_split


write_csv(dem_full_split, "democrat_full_name_split.csv")

write_csv(rep_full_split, "republican_full_name_split.csv")










