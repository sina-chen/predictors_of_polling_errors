### read in wiki data from 2020 ###


# read file
file_list <- list.files(path = "data/us_senate/wikipedia/senate", pattern = "X2020")


senate_election_results2020 <- readRDS("/data/us_senate/senate_election_results2020.RDS")
setwd("data/us_senate/wikipedia/senate")

all_files_purr <- purrr::map(file_list, ~readr::read_csv(.x, skip = 1) %>% 
                               rename(State = 1) %>% 
                               select(-4)) %>% 
  set_names(file_list)


# extract candidate info
map(all_files_purr, ~.x[["Candidates"]] %>% 
      strsplit("\\[[^][]*]")) -> splitted_candidates


splitted_candidates[["X2020_senate.csv"]] %>% 
  map(function(x) tibble(dta = x)) %>% 
  map(function(x) separate(x, col=dta, sep = "\\(", into = c("name","party")) %>% 
      mutate(
             name = str_remove_all(name, "On ballot|Write-in"),
             name = trimws(name),
             party = str_remove_all(party, "\\d+\\.\\d"),
             party = trimws(party),
             party = str_remove_all(party, "\\)|write-in"))) %>% 
  bind_rows(.id = "frame") %>% 
  filter(!stringi::stri_isempty(name)) %>% 
  drop_na() %>% 
  add_count(frame, name = "no_candidates") -> df_senate_2020

# recode one observation from independent to democratic because the candidate was
# actually endorsed by the democratic party

df_senate_2020$party[4] <- "Democratic"

## extract state names from initial list of wikipedia tables

map(all_files_purr, ~.x[["State"]]) -> list_of_states

## extract senator names from initial list of wikipedia tables


map(all_files_purr, ~.x[["Senator"]]) -> list_of_senators

## check in which races there were more/less than 2 candidates from the major parties
df_senate_2020 %>% 
  filter(str_detect(party, "Democrat|Repub")) %>% 
  add_count(frame) %>% 
  filter(n == 2) -> df_senate_2020_clean


df_senate_2020 %>% 
  filter(str_detect(party, "Democrat|Repub")) %>% 
  add_count(frame) %>% 
  filter(n != 2) %>% 
  filter(str_detect(name, "Cassidy|Perkins|Sasse|Janice|Cotton")) %>% 
  bind_rows(df_senate_2020_clean) %>% 
  select(-n)-> df_senate_2020_final

## reshape dataframe to long format

df_senate_2020_final %>% 
  group_by(frame) %>% 
  pivot_wider(c(frame, no_candidates), names_from = party, values_from = name) %>% 
  ungroup() %>% 
  mutate(state_number = as.numeric(frame)) %>% 
  select(-frame) %>% 
  arrange(state_number) %>% 
  add_column(election_year = 2020) %>% 
  bind_cols(list_of_states[["X2020_senate.csv"]]) %>% 
  bind_cols(list_of_senators[["X2020_senate.csv"]]) %>% 
  select(-state_number) %>% 
  rename(state_long = 5, incumbent = 6) -> df_2020

### append to existing wikipedia data and write to wik_senate_covariates.csv

wiki_senate_covariates <- read_csv("data/us_senate/wiki_results/wiki_senate_covariates.csv")

df_2020 %>% 
  rename(rep_candidate = "Republican", dem_candidate = "Democratic",
         senator = "incumbent") %>% 
  rowwise() %>% 
  mutate(incumbency = dem_candidate %in% senator | rep_candidate %in% senator) -> df_2020

wiki_senate_covariates %>% 
  bind_rows(df_2020) %>% 
  write_csv("data/us_senate/wiki_results/wiki_senate_covariates.csv")



### merge with results
#abbrevations_states <- read_delim("~/Downloads/abbrevations_states.csv",
#                                  ";", escape_double = FALSE, trim_ws = TRUE) %>% 
#  select(X2, X3) %>% mutate(X2 = tolower(X2))

#df_2020 %>% 
#  mutate(state_long = tolower(state_long)) %>% 
#  left_join(abbrevations_states, by = c("state_long" = "X2")) %>% 
# rename(state_short = X3) -> df_2020_states

#df_2020_states <- df_2020_states %>% 
#  rowwise() %>% 
#  mutate(incumbency = Democratic %in% incumbent | Republican %in% incumbent) 



### merge with election results
#df_2020_states %>% 
#  left_join(senate_election_results2020, by = c("state_short" = "state")) %>% 
#  filter(!state_short == "GA", !state_short == "AR") %>% 
#  select(-election_year.y) %>% 
#  rename(election_year = "election_year.x")-> df_join

### write file to data folder

# write_csv(df_join, "data/us_senate/wiki_results_2020.csv")
