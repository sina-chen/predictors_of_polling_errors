################################################################################
# Join Wikipedia Data with scraped polls from pollingreport.com
# Author: Philipp Bosch
# Note: set your working directory to root folder of github repo
################################################################################


# read in data from different folders (git & nextcloud)
wikipedia_covariates_join <- read_csv("data/senate/wiki_results/wiki_senate_covariates.csv")

polls_senate1998_2018_clean <- readRDS("path_to_scraped_polls")

# ready to merge
str_replace_all(string = wikipedia_covariates_join$state_long,
                pattern=" ", repl="") -> wikipedia_covariates_join$state_long

# merge poll data with wiki data
polls_senate1998_2018_clean %>% 
  left_join(wikipedia_covariates_join, 
            by = c("election_year", "state_long"),
            keep = TRUE) %>% distinct() -> join_df


# filter only matching polls
join_df %>% 
  separate(dem_candidate.y, into = c("first_dem", "second_dem", "rest_dem"),
           remove = FALSE,
           sep = "\\s", extra = "merge") %>% 
  rowwise() %>%
  mutate(matched = 
           grepl(first_dem, dem_candidate.x, fixed = TRUE) ||
           grepl(second_dem, dem_candidate.x, fixed = TRUE) ||
           grepl(rest_dem, dem_candidate.x, fixed = TRUE)) %>% 
  filter(matched == TRUE) %>% 
  select(-c(first_dem, second_dem, rest_dem, matched)) -> join_dem_split



join_dem_split %>% 
  separate(rep_candidate.y, into = c("first_rep", "second_rep", "rest_rep"),
           remove = FALSE,
           sep = "\\s", extra = "merge") %>% 
  rowwise() %>%
  mutate(matched = 
           grepl(first_rep, rep_candidate.x, fixed = TRUE) ||
           grepl(second_rep, rep_candidate.x, fixed = TRUE) ||
           grepl(rest_rep, rep_candidate.x, fixed = TRUE)) %>% 
  filter(matched == TRUE) %>% 
  select(-c(first_rep, second_rep, rest_rep, matched)) -> senate_wiki_merged

# remove double variables and rename

senate_wiki_merged %>%
  rename(election_year = election_year.x,
         state_long = state_long.x,
         rep_candidate = rep_candidate.y,
         dem_candidate = dem_candidate.y) %>%
  subset(select = -c(election_year.y, 
                     state_long.y, 
                     rep_candidate.x, 
                     dem_candidate.x)) -> senate_wiki_merged  

# write final merged and enriched data-set (commented out, as the poll data 
# can't be shared on github)

# write_csv(senate_wiki_merged, "data/senate/senate_wiki_merged.csv")
