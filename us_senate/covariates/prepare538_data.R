#-------------------------------------------------------------------------------
#
# Preprocess historical 538 data 
# 
#-------------------------------------------------------------------------------

#### Libraries ####

library(tidyverse)
library(readxl)
library(data.table)
library(fuzzyjoin)
library(usdata)


#### Data ####

# 538 polling data
data <- read_csv("~/data/538/senate_polls_all.csv", col_names = T)[-1,]

# elections in which third party recieved at least 5%
third_party <- read_excel("~/data/538/senate_third_party.xlsx")

# indicator whether 
#   candidate was one of the 2 major candidates in the general election and
#   whether there are more than one candidate from the same party
general <- read.csv("~/data/538/candidates_general_fte.csv", sep = ";")

# race_gender (preliminary)
race_gender <- readRDS("~/data/race_gender/race_gender_all.RDS")

# election results
load("~/data/mit_senate_results_1976_2020.RData")

# incumbents 
incumbents <-read_delim("data/538/incumbents_senate1990_2020.csv", delim = ";")


#-------------------------------------------------------------------------------
#### Process Data ####

# subset polls form 1990:2020
data <- subset(data, cycle %in% seq(1990, 2020, 2)  & 
                 candidate_party %in% c("REP", "DEM") & 
                 stage == "general" &
                 candidate_name != "Generic Candidate") 

# special election dummy with 1 indicating a special election
data <- data %>% 
  mutate(special = if_else(state == "Hawaii" & cycle == 1990 |
                             state == "Indiana" & cycle == 1990 |
                             state == "North Dakota" & cycle == 1992 & candidate_name %in% c("Kent Conrad", "Jack Dalrymple") |
                             state == "California" & cycle == 1992 & candidate_name %in% c("Dianne Feinstein", "John Seymour") |
                             cycle %in% c(1993, 2013, 2017) |
                             state == "Tennessee" & cycle == 1994 & candidate_name %in% c("Fred Thompson", "Jim Cooper") |
                             state == "Oklahoma" & cycle == 1994 |
                             state == "Kansas" & cycle == 1996 & candidate_name %in% c("Sam Brownback", "Jill Docking") |
                             state == "Georgia" & cycle == 2000 |
                             state == "Missouri" & cycle == 2002 |
                             state == "Wyoming" & cycle == 2008 & candidate_name %in% c("John Barrasso", "Nicholas H. Carter") |
                             state == "Mississippi" & cycle == 2008 & candidate_name %in% c("Roger F. Wicker", "Ronnie Musgrove") |
                             state == "West Virginia" & cycle == 2010 |
                             state == "Massachusetts" & cycle == 2010 |
                             state == "Delaware" & cycle == 2010 |
                             state == "New York" & cycle == 2010 & candidate_name %in% c("Kirsten E. Gillibrand", "Joseph J. DioGuardi",
                                                                                         "Harold E. Ford Jr.", "George Pataki", 
                                                                                         "Bruce Blakeman", "David Robert Malpass",
                                                                                         "William Colridge Thompson") |
                             state == "South Carolina" & cycle == 2014 & candidate_name %in% c("Tim Scott", "Joyce Dickerson",
                                                                                               "Jill Bossi") |
                             state == "Hawaii" & cycle == 2014 |
                             state == "Oklahoma" & cycle == 2014 & candidate_name %in% c("James Lankford", "Constance N. Johnson", 
                                                                                         "Jim Rogers", "Mark Beard") |
                             state == "Minnesota" & cycle == 2018 & candidate_name %in% c("Tina Smith", "Karin Housley", "Sarah Wellington",
                                                                                          "Jerry Trooien") |
                             state == "Mississippi" & cycle == 2018 & candidate_name %in% c("Cindy Hyde-Smith", "Mike Espy", 
                                                                                            "Chris McDaniel", "David Baria",
                                                                                            "Jason Shelton", "Tobey Bartee") |
                             state == "Arizona" & cycle == 2020 | 
                             state == "Georgia" & cycle == 2020 & candidate_name %in% c("Raphael Warnock", "Kelly Loeffler", "Doug Collins",
                                                                                        "Matt Lieberman", "Ed Tarver",  
                                                                                        "Brian Richard Slowinski", "A. Wayne Johnson", 
                                                                                        "Richard Dien Winfield", "Jon Ossoff", 
                                                                                        "David A. Perdue", "Joy Felicia Slade", 
                                                                                        "Valencia Stovall", "Al Bartell", "John Fortuin", 
                                                                                        "Tamara Y. Johnson-Shealey", "Deborah A. Jackson", 
                                                                                        "Annette L. Davis Jackson", "Michael Todd Greene",
                                                                                        "Derrick E. Grayson", "Allen Buckley", "Jamesia James", 
                                                                                        "Kandiss Taylor", "Teresa Tomlinson"), 1, 0  
                             
  ))


table(data$special)

# third party candidate dummy with 1 indicating a third party candidate received >= 5% 
third_party$third_party <- 1

data <- merge(data, third_party[, c("State", "Year", "third_party")], by.x = c("state", "cycle"), 
              by.y = c("State", "Year"), all.x = T) %>% 
  mutate(third_party = if_else(!is.na(third_party), 1, 0))


table(data$third_party)

# general candidate dummy with 1 indicting running in the general election
# multi candidate dummy with 1 indicating that more than one candidate from the same party

data <- merge(data, general, by = c("candidate_name", "state", "cycle", 
                                    "candidate_party"))

table(data$general)
table(data$multi_cand)

# subset relevant polls 
data <- data %>% 
  subset(cycle %in% seq(1990, 2020, 2) & 
           candidate_party %in% c("DEM", "REP") &
           multi_cand == 0 & 
           general == 1 & 
           special == 0 &
           third_party == 0)

# remove polls of general election candidates for primary election
n_question <- count(data, question_id) %>% 
  rename(n_question = n)

data <- merge(data, n_question, by = "question_id", all.x = T) %>% 
  subset(n_question == 2)

# add number of polls per candidate and election
n_poll <- count(data, cycle, state, candidate_name) %>% 
  rename (n_poll = n)

# subset elections with at least 5 polls
data <- merge(data, n_poll, by = c ("cycle", "state", "candidate_name")) %>% 
  subset(n_poll >= 5)

rm(third_party, general, n_question, n_poll)

#-------------------------------------------------------------------------------
#### Add Gender & Race ####

# remove middle name from race gender
race_gender_clean <- race_gender %>% 
  mutate(candidate_name = str_remove_all(candidate_name, 
                                         "[A-Z][.] | Jr[.]| III")) %>% 
  unique()

# subset relevant polls
data <- data %>% 
  mutate(candidate_name = str_remove_all(candidate_name, "[A-Z][.] | Jr[.]| MD") %>% 
           str_replace_all("Art Small", "Arthur Small")%>% 
           str_replace_all("Alfonse D'Amato", "Al D'Amato") %>% 
           str_replace_all("Benjamin Cardin", "Ben Cardin") %>% 
           str_replace_all("Moseley-", "Moseley ") %>% 
           str_replace_all("Chele Farley", "Chele Chiavacci Farley")  %>% 
           str_replace_all("á", "a") %>% 
           str_replace_all("Barbara Goolsbee Bollier", "Barbara Bollier")  %>% 
           str_replace_all("Bill Redmond", "William Redmond")  %>% 
           str_replace_all("Bob Flanders", "Robert Flanders") %>% 
           str_replace_all("Charles Bradley Hutto", "Brad Hutto") %>% 
           str_replace_all("Christopher Bond", "Kit Bond") %>% 
           str_replace_all("Christopher Coons", "Chris Coons") %>% 
           str_replace_all("Christopher Dodd", "Chris Dodd") %>% 
           str_replace_all("Christopher Murphy", "Chris Murphy") %>% 
           str_replace_all("Christopher Rothfuss", "Chris Rothfuss") %>% 
           str_replace_all("Daniel Coats", "Dan Coats") %>% 
           str_replace_all("Douglas Forrester", "Doug Forrester") %>% 
           str_replace_all("Ed Bernstein", "Edward Bernstein") %>% 
           str_replace_all("Edward Kennedy", "Ted Kennedy") %>% 
           str_replace_all("Edward Markey", "Ed Markey") %>% 
           str_replace_all("Gordon Douglas Jones", "Doug Jones") %>% 
           str_replace_all("Hillary Rodham Clinton", "Hillary Clinton") %>% 
           str_replace_all("Jacob Hoogendyk", "Jack Hoogendyk") %>% 
           str_replace_all("James Inhofe", "Jim Inhofe") %>% 
           str_replace_all("James Jeffords", "Jim Jeffords") %>% 
           str_replace_all("James Oberweis", "Jim Oberweis") %>% 
           str_replace_all("James Risch", "Jim Risch") %>% 
           str_replace_all("Jeffrey Beatty", "Jeff Beatty") %>% 
           str_replace_all("John Neely Kennedy", "John Kennedy") %>% 
           str_replace_all("John Wright Hickenlooper", "John Hickenlooper") %>% 
           str_replace_all("Jon Stevens Corzine", "Jon Corzine") %>% 
           str_replace_all("Joseph Biden", "Joe Biden") %>% 
           str_replace_all("Joseph Heck", "Joe Heck") %>% 
           str_replace_all("Joseph Lieberman", "Joe Lieberman") %>% 
           str_replace_all("Kurt Patrick Bills", "Kurt Bills") %>% 
           str_replace_all("Margaret Wood Hassan", "Maggie Hassan") %>% 
           str_replace_all("Marvin Bailey Scott", "Marvin Scott") %>% 
           str_replace_all("Mary Jennings Hegar", "MJ Hegar") %>% 
           str_replace_all("Michael DeWine", "Mike DeWine") %>% 
           str_replace_all("Michael Enzi", "Mike Enzi") %>% 
           str_replace_all("Misty Katherine Snow", "Misty Snow") %>% 
           str_replace_all("Phil Giordano", "Philip Giordano") %>% 
           str_replace_all("Richard Durbin", "Dick Durbin") %>% 
           str_replace_all("Rikin Mehta", "Rik Mehta") %>% 
           str_replace_all("Robert Bennett", "Bob Bennett") %>% 
           str_replace_all("Robert Casey", "Bob Casey") %>% 
           str_replace_all("Robert Gerald Lorge", "Robert Lorge") %>% 
           str_replace_all("Robert Menendez", "Bob Menendez") %>% 
           str_replace_all("Robert Tuke", "Bob Tuke") %>% 
           str_replace_all("Rodney Britz Glassman", "Rodney Glassman") %>% 
           str_replace_all("Stevan Pearce", "Steve Pearce") %>% 
           str_replace_all("Thomas Allen", "Tom Allen") %>% 
           str_replace_all("Thomas Carper", "Tom Carper") %>% 
           str_replace_all("Thomas Roland Tillis", "Thom Tillis"),
         candidate_party = tolower(candidate_party)) 



# merge race and gender
data_race_gender <- fuzzy_left_join(data, race_gender_clean, 
                         by = "candidate_name", match_fun = str_detect) %>% 
  rename(candidate_name = candidate_name.x) %>% 
  select(-candidate_name.y)


# check for missings
which(is.na(data_race_gender$race))

rm(race_gender, race_gender_clean, data)

#-------------------------------------------------------------------------------
#### Add Incumbent ####

data_inc <- merge(data_race_gender, incumbents, by = c("cycle", "state"), 
                  all.x = T) %>% 
  rename(senator = incumbent) %>% 
  mutate(senator = str_remove_all(senator, "[A-Z][.] | III| Jr[.]") %>% 
           str_replace_all("Mark Hatfield", "Mark Odom Hatfield") %>% 
           str_replace_all("Chuck Robb", "Charles Robb") %>% 
           str_replace_all("Chuck Schumer", "Charles Schumer") %>% 
           str_replace_all("Joe Manchin", "Joe Manchin, III") %>% 
           str_replace_all("Pat Toomey", "Patrick Toomey") %>% 
           str_replace_all("Fritz Hollings", "Ernest Hollings"),
         inc = if_else(senator == candidate_name, 1, 0))

rm(incumbents, data_race_gender)
#-------------------------------------------------------------------------------
#### Add Result ####

# pre process results
results <- x %>% 
  mutate(special = if_else(state == "ILLINOIS" & year == 2010, FALSE, as.logical(special))) %>% 
  subset(party_simplified %in% c("REPUBLICAN", "DEMOCRAT") &
           special == F & writein == F & stage == "gen" & 
           year %in% seq(1990, 2020, 2)) %>% 
  select(state, year, party_simplified, candidatevotes, totalvotes) %>% 
  rename(cycle = year,
         candidate_party = party_simplified) %>% 
  mutate(state = str_to_title(state),
         candidate_party = if_else(candidate_party == "REPUBLICAN", "rep", 
                                   "dem"),
         vote = candidatevotes/totalvotes) %>% 
  group_by(cycle, state, candidate_party) %>% 
  summarise(vote = max(vote))

# merge
data_result <- merge(data_inc, results, by = c("cycle", "state", 
                                                      "candidate_party"), 
                     all.x =T)

rm(x, data_inc, results)

#-------------------------------------------------------------------------------
#### Add Geographic Entities ####

data_geo <- data_result %>% 
  mutate(state_short = state2abbr(state),
         division = case_when(state_short %in% c("WA", "OR", "CA", "HI", "AK") ~ "Pacific", 
                            state_short %in% c("MT", "ID", "NV", "WY", "UT", 
                                               "AZ", "CO", "NM") ~ "Mountain",
                            state_short %in% c("ND", "SD", "NE", "KS", "MN", 
                                               "IA", "MO") ~ "West North Central", 
                            state_short %in% c("WI", "IL", "MI", "IN", "OH") ~ "East North Central",
                            state_short %in% c("NY", "PA", "NJ") ~ "Middle Atlantic", 
                            state_short %in% c("VT", "NH", "MA", "CT", "RI", 
                                               "ME") ~ "New England",
                            state_short %in% c("OK", "TX", "AR", "LA") ~ "West South Central", 
                            state_short %in% c("KY", "TN", "MS", "AL") ~ "East South Central", 
                            state_short %in% c("WV", "VA", "MD", "DE", "NC", "SC", "GA", 
                                               "FL") ~ "South Atlantic"),
         region = case_when(state_short %in% c("WA", "OR", "CA", "HI", "AK", 
                                                 "MT", "ID", "NV", "WY", "UT", 
                                                 "AZ", "CO", "NM") ~ "West",
                              state_short %in% c("ND", "SD", "NE", "KS", "MN", 
                                                 "IA", "MO", "WI", "IL", "MI", 
                                                 "IN", "OH") ~ "Midwest",
                              state_short %in% c("NY", "PA", "NJ", "VT", "NH", 
                                                 "MA", "CT", "RI", "ME") ~ "Northeast",
                              state_short %in% c("OK", "TX", "AR", "LA", "KY", 
                                                 "TN", "MS", "AL", "WV", "VA", 
                                                 "MD", "DE", "NC", "SC", "GA", 
                                                 "FL") ~ "South"))

rm(data_result)


#-------------------------------------------------------------------------------
#### Prepare data in wide format for analysis ####

data_wide <- dcast(setDT(subset(data_geo)), 
                   state + cycle + sample_size + end_date + election_date + 
                     question_id + fte_grade + population + methodology + 
                     seat_name + internal + n_poll + senator + division + 
                     region ~ candidate_party , 
                   value.var = c("pct", "vote", "candidate_name", "race", 
                                 "gender")) %>% 
  mutate(end_date = as.Date(end_date, "%m/%d/%y"),
         election_date = as.Date(election_date, "%m/%d/%y"),
         t = difftime(election_date, end_date),
         pct_dem = as.numeric(pct_dem),
         pct_rep = as.numeric(pct_rep),
         pct2_dem = pct_dem/(pct_dem + pct_rep),
         pct2_rep = pct_rep/(pct_dem + pct_rep),
         vote2_dem = vote_dem/(vote_dem + vote_rep),
         vote2_rep = vote_rep/(vote_dem + vote_rep),
         sample_size = as.double(sample_size)) %>% 
  group_by(state, cycle) %>% 
  mutate(sample_size = if_else(is.na(sample_size) == T, 
                               mean(sample_size, na.rm = T), 
                               sample_size)) %>% 
  ungroup()



#saveRDS(data_wide, "~/data/data_wide_analysis_fte.RDS")


