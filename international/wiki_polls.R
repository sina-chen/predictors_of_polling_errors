### get polls from wiki ###

library(tidyverse)
library(polite)
library(rvest)
library(lubridate)
library(vctrs)

# read html from wiki -------------------------------------------------



# Australia ---------------------------------------------------------------



url_aus_10 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2010_Australian_federal_election"

aus_session_10 <- polite::bow(url = url_aus_10, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

aus_session_10 %>% 
  polite::scrape() -> aus_results_10

aus_results_10 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]/tbody") %>% 
  html_table() %>% 
  .[1:8] %>% 
  rename(date = 1, ALP = 2, Lib = 3, Nat = 4, green = 5, Oth = 6, alp_TPP = 7, "L/NP_TPP" = 8) %>% 
  slice(-1) %>% 
  filter(date != "2007 Election") %>%
  filter(row_number() <= n() - 1) %>% 
  mutate(Nat = na_if(Nat, "<")) %>% 
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(!c(date), type.convert)) %>%
  mutate(date = str_extract(date, "(?<=–|-).*")) %>% 
  mutate(date = str_squish(date)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(institute = "Newspoll") %>% 
  janitor::clean_names() -> aus_polls_2010_1



aus_results_10 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[5]/tbody") %>% 
  html_table() %>% 
  .[1:8] %>% 
  rename(date = 1, ALP = 2, Lib = 3, Nat = 4, green = 5, Oth = 6, alp_TPP = 7, "L/NP_TPP" = 8) %>% 
  slice(-1) %>% 
  filter(row_number() <= n() - 1) %>% 
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(!c(date), type.convert)) %>%
  mutate(date = str_extract(date, "(?<=–|-).*")) %>% 
  mutate(date = str_squish(date)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(institute = "Roy Morgan") %>% 
  janitor::clean_names() -> aus_polls_2010_2

bind_rows(aus_polls_2010_1, aus_polls_2010_2) %>% 
  mutate(mode = NA, sample_size = NA) -> polls_aus_2010


### Australia 2013

url_aus_13 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2013_Australian_federal_election"

aus_session_13 <- polite::bow(url = url_aus_13, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

aus_session_13 %>% 
  polite::scrape() -> aus_results_13

aus_results_13 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]/tbody") %>% 
  html_table() %>% 
  .[1:8] %>% 
  rename(date = 1, institute = 2, ALP = 3, "L/NP" = 4, green = 5, Oth = 6, alp_TPP = 7, "L/NP_TPP" = 8) %>% 
  slice(-c(1:3)) %>% 
  filter(row_number() <= n() - 5) %>%
  mutate(institute = str_remove_all(institute, pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)")) %>% 
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = str_sub(date, start = -11)) %>% 
  mutate(date = str_remove(date, "–|-")) %>% 
  mutate(date = str_squish(date)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(mode = NA, sample_size = NA) %>% 
  janitor::clean_names() %>% 
  mutate(l_np_tpp = parse_number(l_np_tpp)) -> polls_aus_2013


### Australia 2016

url_aus_16 <- "https://en.wikipedia.org/wiki/National_opinion_polling_for_the_2016_Australian_federal_election"

aus_session_16 <- polite::bow(url = url_aus_16, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

aus_session_16 %>% 
  polite::scrape() -> aus_results_16

aus_results_16 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]/tbody") %>% 
  html_table() %>% 
  .[1:11] %>%
  rename(date = 1, institute = 2, "L/NP" = 3, "ALP" = 4, green = 5, Oth = 6, "L/NP_TPP" = 7, "ALP_TPP" = 8, 
         sample_size = 9, margin_of_error = 10, mode = 11) %>% 
  select(-margin_of_error) %>% 
  slice(-c(1:2)) %>% 
  filter(str_detect(ALP, "%")) %>% 
  filter(row_number() <= n() - 4) %>%
  mutate(across(everything(),~ na_if(.x, "?"))) %>%
  mutate(institute = str_remove_all(institute, pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)")) %>% 
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = str_sub(date, start = -11)) %>% 
  mutate(date = str_remove(date, "–|-")) %>% 
  mutate(date = str_squish(date)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  janitor::clean_names() %>% 
  mutate(sample_size = parse_number(sample_size)) -> polls_aus_2016


### Australia 2019

url_aus_19 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Australian_federal_election"

aus_session_19 <- polite::bow(url = url_aus_19, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

aus_session_19 %>% 
  polite::scrape() -> aus_results_19

aus_results_19 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]/tbody") %>% 
  html_table() %>% 
  .[1:9] %>% 
  rename(date = 1, institute = 2, "L/NP" = 3, "ALP" = 4, green = 5, ONP = 6, OTH = 7,
         "L/NP_TPP" = 8, "ALP_TPP" = 9) %>% 
  slice(-c(1:4)) %>% 
  filter(str_detect(ALP, "%")) %>% 
  filter(row_number() <= n() - 6) %>%
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = str_sub(date, start = -11)) %>% 
  mutate(date = str_remove(date, "–|-")) %>% 
  mutate(date = str_squish(date)) %>%
  mutate(ONP = na_if(ONP, "–")) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  janitor::clean_names() -> polls_aus_2019


### bind together

final_australia <- bind_rows(polls_aus_2010, polls_aus_2013, polls_aus_2016, polls_aus_2019)

rm(list = grep("^final", ls(), value = TRUE, invert = TRUE))

# Austria -----------------------------------------------------------------



### Austria

### 2006

url_austria_06 <- "https://en.wikipedia.org/wiki/2006_Austrian_legislative_election#Opinion_polling"

austria_session_06 <- polite::bow(url = url_austria_06, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

austria_session_06 %>% 
  polite::scrape() -> austria_results_06

austria_results_06 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  .[1:8] %>% 
  rename(institute = 1, date = 2, greens = Grüne) %>% 
  janitor::clean_names() %>% 
  slice(-1) %>% 
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(matin = na_if(matin, "–")) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = str_squish(date)) %>%
  mutate(date = lubridate::dmy(date)) -> polls_austria_2006


### 2008

url_austria_08 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2008_Austrian_legislative_election"

austria_session_08 <- polite::bow(url = url_austria_08, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

austria_session_08 %>% 
  polite::scrape() -> austria_results_08


austria_results_08 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2, greens = GRÜNE) %>% 
  filter(str_detect(institute, "Election|Results", negate = TRUE)) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\(|\\)"))) %>% 
  mutate(across(everything(), ~ na_if(.x, "—"))) %>% 
  mutate(across(everything(), ~ na_if(.x, "~"))) %>% 
  mutate(across(everything(), ~ na_if(.x, "#"))) %>% 
  filter(str_detect(fpo, "–", negate = TRUE)) %>% 
  filter(str_detect(bzo, "<|>", negate = TRUE)) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(matin = as.character(matin),
         matin = as.numeric(matin)) %>% 
  mutate(date = str_squish(date)) %>%
  mutate(date = lubridate::ymd(date)) -> polls_austria_2008

### 2013

url_austria_13 <- "https://en.wikipedia.org/wiki/2013_Austrian_legislative_election#Opinion_polling"

austria_session_13 <- polite::bow(url = url_austria_13, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

austria_session_13 %>% 
  polite::scrape() -> austria_results_13

austria_results_13 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(!c(date), type.convert)) %>%
  mutate(date = str_squish(date)) %>%
  mutate(date = lubridate::ymd(date)) -> polls_austria_2013


### 2017

url_austria_17 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2017_Austrian_legislative_election"

austria_session_17 <- polite::bow(url = url_austria_17, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

austria_session_17 %>% 
  polite::scrape() -> austria_results_17

austria_results_17 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2, greens = Grüne) %>%
  tail(-2) %>% 
  filter(institute != "") %>% 
  select(-Lead) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  filter(str_detect(fpo, "–", negate = TRUE)) %>% 
  mutate(across(!c(date), type.convert)) %>%
  mutate(date = str_squish(date)) %>%
  mutate(date = lubridate::ymd(date)) -> polls_austria_2017


### polls prior to 2017 but after 2013 election



austria_results_17 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2, greens = Grüne) %>% 
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  select(-Lead) %>% 
  tail(-1) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(!c(date), type.convert)) %>%
  mutate(date = str_squish(date)) %>%
  mutate(date = lubridate::ymd(date)) -> polls_austria_14_16


### 2019

url_austria_19 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Austrian_legislative_election"

austria_session_19 <- polite::bow(url = url_austria_19, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

austria_session_19 %>% 
  polite::scrape() -> austria_results_19

austria_results_19 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2, greens = Grüne) %>%
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  select(-Lead) %>% 
  tail(-1) %>% 
  filter(str_detect(date, "–")) %>% 
  mutate(date = str_extract(date, "(?<=–).*")) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), ~ na_if(.x, "?"))) %>%
  mutate(across(!c(date), type.convert)) %>%
  mutate(date = str_squish(date)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(samplesize = parse_number(samplesize)) %>% 
  rename(sample_size = samplesize) -> polls_austria_2019


### bind together

final_austria <- vec_rbind(polls_austria_2006, polls_austria_2008, polls_austria_2013,
          polls_austria_14_16, polls_austria_2017, polls_austria_2019)


rm(list = grep("^final", ls(), value = TRUE, invert = TRUE))


# Belgium -----------------------------------------------------------------



### Belgium: discuss with Sina


# Canada ------------------------------------------------------------------



### Canada


url_canada_2000 <- "https://en.wikipedia.org/wiki/2000_Canadian_federal_election#Opinion_polls"

canada_session_2000 <- polite::bow(url = url_canada_2000, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

canada_session_2000 %>% 
  polite::scrape() -> canada_results_2000


canada_results_2000 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2, LPC = 3, CA = 4,	BQ = 5, NDP = 6, PC = 7) %>% 
  filter(str_detect(institute, "Firm|Election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  janitor::clean_names() %>% 
  select(-selected_polls_during_the_campaign) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>% 
  mutate(date = str_remove(date, "Oct. 31–"),
         date = str_remove(date, "Oct. 27–")) %>% 
  mutate(date = str_remove(date, "\ (.*?)–")) %>% 
  separate(date, into = c("month", "day"), sep = "\\.") %>% 
  mutate(across(everything(), ~ str_squish(.x))) %>% 
  mutate(month = case_when(
    month == "Nov" ~ 11,
    month == "Oct" ~ 10
  )) %>% 
  bind_cols(year = 2000) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  select(-c(month, day, year)) %>% 
  mutate(across(!c(date), type.convert)) -> polls_canada_2000
  
### 2004

url_canada_2004 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2004_Canadian_federal_election"

canada_session_2004 <- polite::bow(url = url_canada_2004, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

canada_session_2004 %>% 
  polite::scrape() -> canada_results_2004


canada_results_2004 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  select(1:7) %>% 
  rename(institute = 1, date = 2) %>%
  janitor::clean_names() %>% 
  filter(str_detect(institute, "firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election", negate = TRUE)) %>% 
  filter(str_detect(institute, "Campaign", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(date = case_when(
    date == "April 26–28, 2004" ~ "April 28, 2004",
    date == "March 29–April 18, 2004" ~ "April 18, 2004",
    TRUE ~ date
  )) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::mdy(date)) -> polls_canada_2004



### 2006

bind_cols(x_1 = 1:12, x_2 = month.name) -> numeric_month_df


url_canada_2006 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2006_Canadian_federal_election"

canada_session_2006 <- polite::bow(url = url_canada_2006, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

canada_session_2006 %>% 
  polite::scrape() -> canada_results_2006

canada_results_2006 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  select(c(1:8, Samplesize, "Polling method")) %>% 
  rename(institute = 1, date = 2) %>%
  select(-Link) %>% 
  filter(str_detect(institute, "firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>% 
  mutate(across(everything(), ~ na_if(.x, "—"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  separate(col = date, into = c("day", "month", "year"), sep = " ") %>% 
  mutate(year = case_when(
    month == "January" ~ 2006,
    TRUE  ~ 2005
  )) %>% 
  left_join(numeric_month_df, by = c("month" = "x_2")) %>% 
  mutate(date = make_date(year = year, month = x_1, day = day)) %>% 
  select(-c(day, month, year, x_1)) %>% 
  janitor::clean_names() %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(samplesize = parse_number(samplesize)) %>% 
  rename(sample_size = samplesize)-> polls_canada_2006

### 2008

url_canada_2008 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2008_Canadian_federal_election"

canada_session_2008 <- polite::bow(url = url_canada_2008, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

canada_session_2008 %>% 
  polite::scrape() -> canada_results_2008

canada_results_2008 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names() %>% 
  select(-c(3, na)) %>% 
  rename(institute = 1, date = 2, CPC = 3, LPC = 4,	NDP = 5, BQ = 6, GPC = 7) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  filter(str_detect(institute, "without", negate = TRUE)) %>% 
  mutate(institute =  str_remove_all(institute,  pattern = "\\([\\s\\S]*\\)")) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::mdy(date)) -> polls_canada_2008


### 2011

url_canada_2011 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2011_Canadian_federal_election"

canada_session_2011 <- polite::bow(url = url_canada_2011, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

canada_session_2011 %>% 
  polite::scrape() -> canada_results_2011

### election campaign

canada_results_2011 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  select(-X3) %>% 
  rename(institute = 1, date = 2, CPC = 3, LPC = 4,	
         NDP = 5, BQ = 6, GPC = 7, Other = 8, polling_method = 9) %>% 
  select(1:9) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::mdy(date)) -> polls_canada_2011

### pre-election campaign

canada_results_2011 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  select(-X3) %>% 
  rename(institute = 1, date = 2, CPC = 3, LPC = 4,	
         NDP = 5, BQ = 6, GPC = 7) %>% 
  select(1:7) %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  janitor::clean_names() %>% 
  bind_rows(polls_canada_2011) -> polls_canada_2011


### 2015  

url_canada_2015 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_Canadian_federal_election"

canada_session_2015 <- polite::bow(url = url_canada_2015, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

canada_session_2015 %>% 
  polite::scrape() -> canada_results_2015


canada_results_2015 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  select(-Link) %>% 
  rename(institute = 1, date = 2, CPC = 3, NDP = 4,	LPC = 5, BQ = 6, GPC = 7,
         sample_size = 9, polling_method = 10) %>% 
  select(c(1:7, 9,10)) %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  janitor::clean_names() %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  mutate(sample_size = parse_number(sample_size)) -> polls_canada_2015

### pre-election

canada_results_2015 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  select(-Link) %>% 
  rename(institute = 1, date = 2, CPC = 3, NDP = 4,	LPC = 5, BQ = 6, GPC = 7,
         sample_size = 9, polling_method = 10) %>% 
  select(c(1:7, 9,10)) %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  janitor::clean_names() %>% 
  mutate(sample_size = parse_number(sample_size)) %>% 
  bind_rows(polls_canada_2015) -> polls_canada_2015


### 2019

url_canada_2019 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Canadian_federal_election"

canada_session_2019 <- polite::bow(url = url_canada_2019, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

canada_session_2019 %>% 
  polite::scrape() -> canada_results_2019


canada_results_2019 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  select(-Link) %>% 
  rename(institute = 1, date = 2) %>% 
  janitor::clean_names() %>% 
  select(c(1:8, 10,11)) %>% 
  rename(sample_size = samplesize_3, polling_method = polling_method_4) %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(sample_size = parse_number(sample_size)) %>% 
  mutate(date = lubridate::mdy(date)) -> polls_canada_2019

### pre-election

canada_results_2019 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  select(-Link) %>% 
  rename(institute = 1, date = 2, PPC = 8) %>% 
  janitor::clean_names() %>% 
  select(c(1:8, 10,11)) %>% 
  rename(sample_size = samplesize_3, polling_method = polling_method_4) %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>% 
  filter(str_detect(institute, "\\(", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  mutate(sample_size = parse_number(sample_size)) %>% 
  bind_rows(polls_canada_2019) -> polls_canada_2019





### bind together

final_canada <- vec_rbind(polls_canada_2000, polls_canada_2004, polls_canada_2006, 
                          polls_canada_2008, polls_canada_2011, polls_canada_2015,
                          polls_canada_2019)

rm(list = grep("^final", ls(), value = TRUE, invert = TRUE))


# Finland -----------------------------------------------------------------



### Finland


url_finland_2011 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2011_Finnish_parliamentary_election"

finland_session_2011 <- polite::bow(url = url_finland_2011, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

finland_session_2011 %>% 
  polite::scrape() -> finland_results_2011


finland_results_2011 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from = rowname, values_from = value) %>% 
  select(1:9) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  filter(Party != 2007) %>%
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  bind_cols(institute = "Taloustutkimus") %>% 
  rename(date = Party, kesk = "Centre Party", kok = "National Coalition Party",
         sdp = "Social Democrats", vas = "Left Alliance", vihr = "Green League",
         kd = "Christian Democrats", sfp = "Swedish People's Party of Finland",
         ps = "True Finns", ) %>% 
  mutate(date = parse_date_time(date, "my")) %>% 
  mutate(across(!c(date), type.convert)) -> polls_finland_2011


### finland 2015


url_finland_2015 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_Finnish_parliamentary_election"

finland_session_2015 <- polite::bow(url = url_finland_2015, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

finland_session_2015 %>% 
  polite::scrape() -> finland_results_2015

# create year variable
tibble(year = rep(2015, 12)) -> col_2015
tibble(year = rep(2014, 23)) -> col_2014
tibble(year = rep(2013, 16)) -> col_2013
tibble(year = rep(2012, 12)) -> col_2012
tibble(year = rep(2011, 8)) -> col_2011

bind_rows(col_2015, col_2014, col_2013, col_2012, col_2011) -> year_df

finland_results_2015 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  rename(date = 1, institute = 2) %>% 
  select(-Lead) %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  bind_cols(year_df) %>% 
  filter(str_detect(institute, "20|Election", negate = TRUE)) %>% 
  janitor::clean_names() %>% 
  mutate(date = str_remove(date, ".*–")) %>% 
  mutate(date = str_remove(date, ".*-")) %>% 
  mutate(year = as.character(year)) %>% 
  unite(col = "date", date, year, sep = " ") %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  janitor::clean_names() %>% 
  mutate(across(!c(date), type.convert)) -> polls_finland_2015



### 2019

url_finland_2019 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Finnish_parliamentary_election"

finland_session_2019 <- polite::bow(url = url_finland_2019, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

finland_session_2019 %>% 
  polite::scrape() -> finland_results_2019


tibble(year = rep(2019, 13)) -> col_2019
tibble(year = rep(2018, 35)) -> col_2018
tibble(year = rep(2017, 23)) -> col_2017
tibble(year = rep(2016, 24)) -> col_2016
tibble(year = rep(2015, 17)) -> col_2015

bind_rows(col_2019, col_2018, col_2017, col_2016, col_2015) -> year_df_2019


finland_results_2019 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  rename(date = 1, institute = 2) %>% 
  select(-c(Lead, `Gov.`, `Opp.`)) %>% 
  filter(str_detect(institute, "firm|Firm", negate = TRUE)) %>% 
  filter(str_detect(date, "Election", negate = TRUE)) %>% 
  bind_cols(year_df_2019) %>% 
  filter(str_detect(institute, "20|Election", negate = TRUE)) %>% 
  janitor::clean_names() %>% 
  mutate(date = str_remove(date, ".*–")) %>% 
  mutate(date = str_remove(date, ".*-")) %>% 
  mutate(year = as.character(year)) %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  unite(col = "date", date, year, sep = " ") %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  janitor::clean_names() %>% 
  mutate(across(!c(date), type.convert)) -> polls_finland_2019



### bind together

final_finland <- vctrs::vec_rbind(polls_finland_2011, polls_finland_2015, polls_finland_2019)

rm(list = grep("^final", ls(), value = TRUE, invert = TRUE))


# France ------------------------------------------------------------------


### France

# --> question to Sina




# Greece ------------------------------------------------------------------

### 2007

url_greece_2007 <- "https://en.wikipedia.org/wiki/2007_Greek_legislative_election#Opinion_polls,_January%E2%80%93August_2007"

greece_session_2007 <- polite::bow(url = url_greece_2007, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

greece_session_2007 %>% 
  polite::scrape() -> greece_results_2007



greece_results_2007 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[4]") %>% 
  html_table() %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  janitor::row_to_names(row_number = 1) %>% 
  rename(institute = 1, date = 2) %>% 
  mutate(institute = str_remove(institute, "\\*")) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  janitor::clean_names() %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(across(!c(date), type.convert)) -> polls_greece_2007


### 2012

url_greece_2012 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2012_Greek_legislative_elections"

greece_session_2012 <- polite::bow(url = url_greece_2012, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

greece_session_2012 %>% 
  polite::scrape() -> greece_results_2012


greece_parties_2012 <- c("PASOK", "ND", "KKE", "LAOS", "SYRIZA", "Ecologist_Greens", 
                         "Golden_Dawn", "DIMAR", "Democratic_Alliance", "PARMAP", "ANEL")

greece_results_2012 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]/tbody") %>% 
  html_table() -> raw_greece_2012

colnames(raw_greece_2012)[4:14] <- greece_parties_2012


raw_greece_2012 %>% 
  rename(institute = 1, date = 2) %>% 
  select(-c(Lead)) %>% 
  slice(-c(1:11)) %>% 
  mutate(across(everything(), 
                ~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(date = str_remove(date, ".*–")) %>% 
  mutate(date = str_remove(date, ".*-")) %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>% 
  mutate(across(everything(), ~ na_if(.x, "?"))) %>% 
  janitor::clean_names() %>% 
  mutate(across(!c(institute, date, sample_size), ~ str_extract(.x, pattern = "\\d{1,2}\\.\\d{1}"))) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(sample_size = parse_number(sample_size)) -> polls_greece_2009_2012


### may to june (re-election)

greece_results_2012 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]/tbody") %>% 
  html_table() -> raw_greece_may_2012


greece_parties_may_2012 <- c("ND", "SYRIZA", "PASOK", "ANEL", "KKE", "Golden_Dawn", 
                             "DIMAR", "Ecologist_Greens", "LAOS", "Democratic_Alliance",
                             "DIXA", "Drassi")

colnames(raw_greece_may_2012)[4:15] <- greece_parties_may_2012

raw_greece_may_2012 %>% 
  rename(institute = 1, date = 2) %>% 
  select(-c(Lead)) %>% 
  slice(-c(1:10)) %>% 
  mutate(across(everything(), 
                ~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(date = str_remove(date, ".*–")) %>% 
  mutate(date = str_remove(date, ".*-")) %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>% 
  mutate(across(everything(), ~ na_if(.x, "?"))) %>% 
  janitor::clean_names() %>% 
  mutate(across(!c(institute, date, sample_size), ~ str_extract(.x, pattern = "\\d{1,2}\\.\\d{1}"))) %>%
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(sample_size = parse_number(sample_size)) -> polls_greece_2012


### 2015 January

url_greece_2015_jan <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_January_2015_Greek_legislative_election"

greece_session_2015_jan <- polite::bow(url = url_greece_2015_jan, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

greece_session_2015_jan %>% 
  polite::scrape() -> greece_results_2015_jan

greece_parties_jan_2015 <- c("ND", "SYRIZA", "PASOK", "ANEL", "Golden_Dawn", 
                             "DIMAR", "KKE",  "Potami", "Kinima"
                             )

greece_results_2015_jan %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() -> raw_greece_jan_2015

colnames(raw_greece_jan_2015)[3:11] <- greece_parties_jan_2015


raw_greece_jan_2015 %>% 
  rename(institute = 1, date = 2) %>% 
  select(-c(Lead, "Marginof Error")) %>% 
  slice(-c(1:12)) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), 
                ~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(date = str_remove(date, ".*–")) %>% 
  mutate(date = str_remove(date, ".*-")) %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>% 
  mutate(across(!c(institute, date, sample_size), ~ str_extract(.x, pattern = "\\d{1,2}\\.\\d{1}"))) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(sample_size = parse_number(sample_size)) -> polls_greece_jan_2015


### 2015 September

url_greece_2015_september <- "https://en.wikipedia.org/wiki/September_2015_Greek_legislative_election#Opinion_polls"

greece_session_2015_september <- polite::bow(url = url_greece_2015_september, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

greece_session_2015_september %>% 
  polite::scrape() -> greece_results_2015_september

greece_results_2015_september %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() -> raw_greece_2015_september


greece_parties_2015_september <- c("SYRIZA", "ND", "Golden_Dawn", "Potami", "KKE", "ANEL",  
                         "PASOK", "KIDISO", "Union_of_centrists", "Popular Unity")


colnames(raw_greece_2015_september)[4:13] <- greece_parties_2015_september

raw_greece_2015_september %>% 
  rename(institute = 1, date = 2) %>% 
  select(-c(Lead)) %>% 
  slice(-c(1:12)) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), 
                ~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(date = str_remove(date, ".*–")) %>% 
  mutate(date = str_remove(date, ".*-")) %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>% 
  mutate(across(everything(), ~ na_if(.x, "?"))) %>% 
  mutate(across(!c(institute, date, sample_size), ~ str_extract(.x, pattern = "\\d{1,2}\\.\\d{1}"))) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(sample_size = parse_number(sample_size)) -> polls_greece_2015_september


### 2019

url_greece_2019 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Greek_legislative_election"

greece_session_2019 <- polite::bow(url = url_greece_2019, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

greece_session_2019 %>% 
  polite::scrape() -> greece_results_2019


greece_results_2019 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() -> raw_greece_2019


greece_parties_2019 <- c("SYRIZA", "ND", "Golden_Dawn","DISI", "KKE",  "Potami",
                         "ANEL", "Union_of_centrists", "Popular Unity", "ANT",
                         "PE", "EL", "KINAL", "MERA25")


colnames(raw_greece_2019)[4:17] <- greece_parties_2019

raw_greece_2019 %>% 
  rename(institute = 1, date = 2) %>% 
  select(-c(Lead)) %>% 
  slice(-c(1:8)) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), 
                ~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  filter(str_detect(institute, "election", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  mutate(date = str_remove(date, ".*–")) %>% 
  mutate(date = str_remove(date, ".*-")) %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>% 
  mutate(across(everything(), ~ na_if(.x, "?"))) %>% 
  mutate(across(!c(institute, date, sample_size), ~ str_extract(.x, pattern = "\\d{1,2}\\.\\d{1}"))) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(sample_size = parse_number(sample_size)) -> polls_greece_2019



### bind together

final_greece <- vctrs::vec_rbind(polls_greece_2007, polls_greece_2009_2012, polls_greece_2012,
                 polls_greece_jan_2015, polls_greece_2015_september, polls_greece_2019)


rm(list = grep("^final", ls(), value = TRUE, invert = TRUE))


# Israel ------------------------------------------------------------------

# caution: seats not percentages

### 2009

url_israel_2009 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2009_Israeli_legislative_election"

israel_session_2009 <- polite::bow(url = url_israel_2009, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

israel_session_2009 %>% 
  polite::scrape() -> israel_results_2009


israel_results_2009 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  rename(date = 1, institute = 3) %>% 
  select(-c(Sources, Other)) %>% 
  janitor::clean_names() %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  filter(date > as.Date("2009-01-11")) %>% 
  filter(str_detect(labor, "-", negate = TRUE)) %>% 
  filter(str_detect(likud, "-", negate = TRUE)) %>% 
  filter(str_detect(institute, "Results", negate = TRUE)) %>% 
  filter(hadash < 8) %>% # delete observation where arabic parties are taken together
  mutate(across(.cols = everything(), ~ str_squish(.x))) %>% 
  mutate(across(everything(), 
                ~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(!c(date), type.convert)) -> polls_israel_2009


### 
### important: National union and Jewish home are polled together as well 
### as Likud-Yisrael Beiteinu

url_israel_2013 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2013_Israeli_legislative_election"

israel_session_2013 <- polite::bow(url = url_israel_2013, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

israel_session_2013 %>% 
  polite::scrape() -> israel_results_2013


israel_results_2013 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x")) %>% 
  rename(date = 1, institute = 2) %>% 
  filter(str_detect(institute, "results|Knesset|Poll|poll", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  filter(institute != "") %>% 
  filter(institute != kadima) %>% 
  unite(col = "likud_yisrael_beiteinu", 4,5) %>% 
  mutate(likud_yisrael_beiteinu = str_sub(likud_yisrael_beiteinu, end = 2)) %>% 
  filter(national_union == jewish_home) %>% 
  unite(col = "national_union_jewish_home", national_union, jewish_home) %>% 
  mutate(national_union_jewish_home = str_remove(national_union_jewish_home, ".*_")) %>% 
  filter(hadash < 8) %>% # delete observation where arabic parties are taken together
  slice(1:53) %>% 
  mutate(year = case_when(
    str_detect(date, "Jan") ~ 2013,
    TRUE ~ 2012
  )) %>% 
  unite(date, date, year, sep = " ") %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_israel_2013


### 2015

### important: labor hatnua run together as joint list
### islamic parties as well

url_israel_2015 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_Israeli_legislative_election"

israel_session_2015 <- polite::bow(url = url_israel_2015, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

israel_session_2015 %>% 
  polite::scrape() -> israel_results_2015


israel_results_2015 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x")) %>% 
  rename(date = 1, institute = 2) %>% 
  filter(str_detect(institute, "results|Seats|Poll|poll", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  filter(institute != "") %>% 
  filter(institute != likud) %>% 
  rename(hadash = hadash_a, ual_taal = ual_taal_a, balad = balad_a, otzma = otzma_b,
         yachad = yachad_b) %>% 
  unite("labor_hatnuah", labor, hatnuah) %>% 
  mutate(labor_hatnuah = str_remove(labor_hatnuah, ".*_")) %>% 
  slice(1:63) %>% 
  unite("arabic_list", hadash, ual_taal, balad) %>% 
  mutate(arabic_list = str_remove(arabic_list, ".*_")) %>% 
  unite("otzma_yachad", otzma, yachad) %>% 
  mutate(otzma_yachad = str_remove(otzma_yachad, ".*_")) %>% 
  mutate(date = paste(date, "2015", sep = " ")) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_israel_2015


### 2019 April


url_israel_2019_april <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_April_2019_Israeli_legislative_election"

israel_session_2019_april <- polite::bow(url = url_israel_2019_april, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

israel_session_2019_april %>% 
  polite::scrape() -> israel_results_2019_april


israel_results_2019_april %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x")) %>% 
  rename(date = 1, institute = 2) %>% 
  select(-c(r, l)) %>% 
  filter(str_detect(institute, "results|Seats|seats|Poll|poll|silence|Outgoing", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  filter(institute != likud) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "-") ~ str_extract(date, "(?<=-).*"),
    TRUE ~ date
  )) %>% 
  mutate(date = paste(date, "2019", sep = " ")) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_israel_2019_april


### 2019 September

url_israel_2019_sept <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_September_2019_Israeli_legislative_election"

israel_session_2019_sept <- polite::bow(url = url_israel_2019_sept, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

israel_session_2019_sept %>% 
  polite::scrape() -> israel_results_2019_sept


israel_results_2019_sept %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x")) %>% 
  rename(date = 1, institute = 2) %>% 
  select(-c(publisher, gov_a)) %>% 
  filter(str_detect(institute, "results|Seats|seats|Poll|poll", negate = TRUE)) %>% 
  filter(institute != "") %>% 
  filter(institute != likud) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(date = paste(date, "2019", sep = " ")) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_israel_2019_sept


### 2020

url_israel_2020 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2020_Israeli_legislative_election"

israel_session_2020 <- polite::bow(url = url_israel_2020, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

israel_session_2020 %>% 
  polite::scrape() -> israel_results_2020

israel_results_2020 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x")) %>% 
  rename(date = 1, institute = 2) %>% 
  select(-c(publisher, gov)) %>% 
  filter(institute != "") %>% 
  filter(institute != blue_white) %>% 
  filter(str_detect(institute, "results|seats|firm", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  filter(date < as.Date("2020-03-02")) -> polls_israel_2020 # filter exit polls after election silence



### 2021

url_israel_2021 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_Israeli_legislative_election"

israel_session_2021 <- polite::bow(url = url_israel_2021, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

israel_session_2021 %>% 
  polite::scrape() -> israel_results_2021


israel_results_2021 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x")) %>% 
  rename(date = 1, institute = 2, religious_zionist = religious_zionist_a) %>% 
  select(-publisher) %>% 
  filter(institute != "") %>% 
  filter(institute != likud) %>% 
  filter(str_detect(institute, "results|seats|firm", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  filter(date <= as.Date("2021-03-19")) -> polls_israel_2021




# Italy -------------------------------------------------------------------

### 2006


url_italy_2006 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2006_Italian_general_election"

italy_session_2006 <- polite::bow(url = url_italy_2006, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

italy_session_2006 %>% 
  polite::scrape() -> italy_results_2006


italy_results_2006 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-lead) %>% 
  rename(date = 1, institute = 2, cdl = cd_l) %>% 
  filter(str_detect(institute, "results|firm", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = paste(date, "2006", sep = " ")) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_italy_2006

# rest prior to 2006

italy_results_2006 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-lead) %>% 
  rename(date = 1, institute = 2, cdl = cd_l) %>% 
  filter(str_detect(institute, "results|firm", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = paste(date, "2005", sep = " ")) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  bind_rows(polls_italy_2006) %>% 
  arrange(desc(date)) -> polls_italy_2006


### 2008

url_italy_2008 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2008_Italian_general_election"

italy_session_2008 <- polite::bow(url = url_italy_2008, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

italy_session_2008 %>% 
  polite::scrape() -> italy_results_2008


italy_results_2008 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-lead) %>% 
  rename(date = 1, institute = 2, pdl = pd_l, idv = id_v) %>% 
  filter(str_detect(institute, "results|firm", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = paste(date, "2008", sep = " ")) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_italy_2008

# 2007

italy_results_2008 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-lead) %>% 
  rename(date = 1, institute = 2, pdci = pd_ci, idv = id_v, fdv = fd_v) %>% 
  filter(str_detect(institute, "results|firm", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = paste(date, "2007", sep = " ")) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  bind_rows(polls_italy_2008) -> polls_italy_2008


# 2006

italy_results_2008 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-lead) %>% 
  rename(date = 1, institute = 2, pdci = pd_ci, idv = id_v, fdv = fd_v) %>% 
  filter(str_detect(institute, "results|firm", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]|\\([\\s\\S]*\\)"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = paste(date, "2006", sep = " ")) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  bind_rows(polls_italy_2008) %>% 
  arrange(desc(date)) -> polls_italy_2008


### 2013

url_italy_2013 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2013_Italian_general_election"

italy_session_2013 <- polite::bow(url = url_italy_2013, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

italy_session_2013 %>% 
  polite::scrape() -> italy_results_2013


italy_results_2013 %>% 
  rvest::html_nodes("table") %>% 
  html_table() -> italy_dfs_2013

italy_years_2013 <- list(2013, 2012, 2011, 2010, 2009, 2008)

map(.x = italy_dfs_2013[-c(7,8)], .f = janitor::clean_names) %>% 
  map(.f = . %>% mutate(across(.cols = everything(), ~ as.character(.))))  %>% 
  map2(italy_years_2013, ~ bind_cols(.x, year = .y)) %>% 
  map_dfr(bind_rows) %>% 
  mutate(new_other = coalesce(oth, others)) %>% 
  rename(date = 1, institute = 2) %>% 
  select(-c(oth, others, lead)) %>% 
  rename(other = new_other) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "Did not exist"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(date = paste(date, year, sep = " ")) %>% 
  select(-year) %>% 
  rename(pdl = pd_l, udc = ud_c, fdi = fd_i, idv = id_v) %>% 
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_italy_2013



  
### 2018

url_italy_2018 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2018_Italian_general_election"

italy_session_2018 <- polite::bow(url = url_italy_2018, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

italy_session_2018 %>% 
  polite::scrape() -> italy_results_2018
  
  
italy_results_2018 %>% 
  rvest::html_nodes("table") %>% 
  html_table() -> italy_dfs_2018

italy_years_2018 <- list(2018, 2017, 2016, 2015, 2014, 2013)

italy_dfs_2018 %>% 
  .[[1]] %>% 
  janitor::row_to_names(1) -> italy_dfs_2018[[1]]


map(.x = italy_dfs_2018[c(1:6)], .f = janitor::clean_names) %>% 
  map(.f = . %>% mutate(across(.cols = everything(), ~ as.character(.)))) %>% 
  map2(italy_years_2018, ~ bind_cols(.x, year = .y)) %>% 
  map_dfr(bind_rows) %>%
  rename(date = 1, institute = 2) %>% 
  select(-lead) %>% 
  rename(udc = ud_c, fdi = fd_i, nci = nc_i, 
         leu = le_u, pap = pa_p, pdl_fi = pd_l_fi) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "Did not exist"))) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(.cols = !c(date, institute, samplesize), .fns = ~ case_when( 
                  str_detect(., "w.") ~ NA_character_,
                  TRUE ~ .))) %>% 
  mutate(date = paste(date, year, sep = " ")) %>% 
  mutate(samplesize = na_if(samplesize, "–")) %>% 
  mutate(institute = str_remove(institute, " Archived 2019-09-01 at the Wayback Machine")) %>% 
  select(-year) %>% 
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(samplesize = readr::parse_number(samplesize)) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_italy_2019




# New Zeeland -------------------------------------------------------------


url_nz_1999 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_1999_New_Zealand_general_election"

nz_session_1999 <- polite::bow(url = url_nz_1999, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_1999 %>% 
  polite::scrape() -> nz_results_1999


nz_results_1999 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nz_1999

### 2002
  

url_nz_2002 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2002_New_Zealand_general_election"

nz_session_2002 <- polite::bow(url = url_nz_2002, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_2002 %>% 
  polite::scrape() -> nz_results_2002

nz_results_2002 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  rename(institute = 1, date = 2) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  filter(institute != date) %>% 
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(.cols = !c(date, institute), .fns = ~ case_when( 
    str_detect(., "<") ~ NA_character_,
    TRUE ~ .))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nz_2002




### 2005


url_nz_2005 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2005_New_Zealand_general_election"

nz_session_2005 <- polite::bow(url = url_nz_2005, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_2005 %>% 
  polite::scrape() -> nz_results_2005


nz_results_2005 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(institute = 1, date = 2) %>% 
  select(-starts_with("x")) %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  filter(institute != date) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(date = case_when(
      str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
      TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  filter(!is.na(date)) -> polls_nz_2005


### 2008

url_nz_2008 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2008_New_Zealand_general_election"

nz_session_2008 <- polite::bow(url = url_nz_2008, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_2008 %>% 
  polite::scrape() -> nz_results_2008


nz_results_2008 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(institute = 1, date = 2) %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  filter(institute != date) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nz_2008


### 2011

url_nz_2011 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2011_New_Zealand_general_election"

nz_session_2011 <- polite::bow(url = url_nz_2011, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_2011 %>% 
  polite::scrape() -> nz_results_2011

nz_results_2011 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(institute = 1, date = 2) %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  filter(institute != date) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(.cols = !c(date, institute), .fns = ~ case_when( 
    str_detect(., "<") ~ NA_character_,
    TRUE ~ .))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nz_2011


### 2014

url_nz_2014 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2014_New_Zealand_general_election"

nz_session_2014 <- polite::bow(url = url_nz_2014, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_2014 %>% 
  polite::scrape() -> nz_results_2014


nz_results_2014 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(institute = 1, date = 2) %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  filter(institute != date) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(.cols = !c(date, institute), .fns = ~ case_when( 
    str_detect(., "<") ~ NA_character_,
    TRUE ~ .))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nz_2014


### 2017

url_nz_2017 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2017_New_Zealand_general_election"

nz_session_2017 <- polite::bow(url = url_nz_2017, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_2017 %>% 
  polite::scrape() -> nz_results_2017

nz_results_2017 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(date = 1, institute = 2, national = nat, labour = lab, green = grn,
         nz_first = nzf, maori = mri, united_future = unf, mana = mna) %>% 
  filter(date != "") %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  filter(institute != national) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(.cols = !c(date, institute), .fns = ~ case_when( 
    str_detect(., "<") ~ NA_character_,
    TRUE ~ .))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nz_2017



### 2020


url_nz_2020 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2020_New_Zealand_general_election"

nz_session_2020 <- polite::bow(url = url_nz_2020, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nz_session_2020 %>% 
  polite::scrape() -> nz_results_2020


nz_results_2020 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(date = 1, institute = 2, national = nat, labour = lab, green = grn,
         nz_first = nzf, maori = mri) %>% 
  select(-lead) %>% 
  filter(date != "") %>% 
  filter(str_detect(institute, "result", negate = TRUE)) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  filter(institute != national) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(.cols = !c(date, institute), .fns = ~ case_when( 
    str_detect(., "<") ~ NA_character_,
    TRUE ~ .))) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  filter(!is.na(date)) -> polls_nz_2020



# Norway ------------------------------------------------------------------

### important: date variable is not coded as date because sometimes only month and
### year are given

### 2009

url_nw_2009 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2009_Norwegian_parliamentary_election"

nw_session_2009 <- polite::bow(url = url_nw_2009, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nw_session_2009 %>% 
  polite::scrape() -> nw_results_2009


nw_results_2009 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(institute = 1, date = 2, ap = dna) %>% 
  select(-source) %>% 
  mutate(across(.cols = everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(!c(date), type.convert)) -> polls_nw_2009



### 2013

url_nw_2013 <- "https://en.wikipedia.org/wiki/2013_Norwegian_parliamentary_election"

nw_session_2013 <- polite::bow(url = url_nw_2013, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nw_session_2013 %>% 
  polite::scrape() -> nw_results_2013

nw_results_2013 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  janitor::row_to_names(row_number = 2) %>% 
  janitor::clean_names() %>% 
  select(1:9) %>% 
  rename(date = 1) %>% 
  filter(str_detect(date, "period", negate = TRUE)) %>% 
  filter(str_detect(date, "election", negate = TRUE)) %>% 
  filter(date != "") %>% 
  mutate(across(.cols = everything(), ~ str_remove(.x, "%"))) %>% 
  separate(col = date, into = c("month", "year"), sep = " ") %>% 
  mutate(month = as.integer(factor(month, levels = month.name))) %>%
  unite(col = date, month, year, sep = "-") %>% 
  mutate(across(!c(date), type.convert)) -> polls_nw_2013


### 2017

url_nw_2017 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2017_Norwegian_parliamentary_election"

nw_session_2017 <- polite::bow(url = url_nw_2017, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nw_session_2017 %>% 
  polite::scrape() -> nw_results_2017


nw_results_2017 %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(institute = 1, date = 2, resp_rate = resp) %>% 
  select(-c(lead, red, blue, lead_2)) %>% 
  filter(str_detect(institute, "firm|election", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nw_2017



# 2021

url_nw_2021 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_Norwegian_parliamentary_election#Graphical_summary"

nw_session_2021 <- polite::bow(url = url_nw_2021, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

nw_session_2021 %>% 
  polite::scrape() -> nw_results_2021


nw_results_2021 %>% 
  rvest::html_nodes("table") %>% 
  html_table() %>% 
  .[1:5] %>% 
  map(.f = janitor::clean_names) %>% 
  map(.f = . %>% mutate(across(.cols = everything(), ~ as.character(.)))) %>% 
  map_dfr(bind_rows) %>% 
  rename(institute = 1, date = 2, resp_rate = resp) %>% 
  select(-lead) %>% 
  filter(str_detect(institute, "firm|election", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(samplesize = str_remove(samplesize, ",")) %>% 
  mutate(samplesize = str_remove(samplesize, "\\.")) %>% 
  mutate(across(!c(date), type.convert)) %>% 
  mutate(date = lubridate::dmy(date)) -> polls_nw_2021




# Poland ------------------------------------------------------------------

### important: only elections after 2007 are considered, as the polls before
### average parties together

### 2011

url_pl_2011 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2011_Polish_parliamentary_election"

pl_session_2011 <- polite::bow(url = url_pl_2011, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

pl_session_2011 %>% 
  polite::scrape() -> pl_results_2011

pl_results_2011 %>% 
  rvest::html_nodes("table") %>% 
  .[1:2] %>% 
  html_table() %>% 
  map(.f = janitor::clean_names) %>% 
  map(.f = . %>% mutate(across(.cols = everything(), ~ as.character(.)))) %>% 
  map_dfr(bind_rows) %>% 
  rename(date = 1, institute = 2, po = 3, pis = 4, dla = 5, psl = 6,
         your_movement = 7, pjn = 8) %>% 
  select(-lead) %>% 
  filter(str_detect(date, "Dates", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|elections", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>%
  separate(col = date, into = c("month", "rest"), sep = " ", extra = "merge") %>% 
  mutate(rest = case_when(
    str_detect(rest, "–") ~ str_extract(rest, "(?<=–).*"),
    TRUE ~ rest
  )) %>% 
  mutate(rest = str_replace(rest, ",", "-")) %>% 
  mutate(month = as.integer(factor(month, levels = month.name))) %>%
  unite(col = "date", month, rest, sep = "-") %>% 
  mutate(across(!c(date), type.convert)) -> polls_pl_2011


### 2015

# important: only last year of polls is sraped, as parties merged etc.

url_pl_2015 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_Polish_parliamentary_election"

pl_session_2015 <- polite::bow(url = url_pl_2015, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

pl_session_2015 %>% 
  polite::scrape() -> pl_results_2015


pl_results_2015 %>% 
  rvest::html_nodes("table") %>% 
  rvest::html_table() %>% 
  purrr::pluck(1) %>% 
  janitor::clean_names() %>% 
  rename(date = 1, institute = 2, pis = pi_s, psl = x, dla = x_2,
         your_movement = x_3, kukiz_15 = x_4, modern = x_5, razem = x_6) %>% 
  select(-lead) %>% 
  filter(str_detect(date, "Dates", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|elections|Late|Exit", negate = TRUE)) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>% 
  mutate(across(everything(), ~ na_if(.x, "?"))) %>% 
  mutate(across(everything(), ~ na_if(.x, "15+5"))) %>% 
  mutate(date = paste(date, "2015", sep = " ")) %>% 
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(across(!c(date), type.convert)) -> polls_pl_2015


### 2019

# important again only last year due to merging of parties


url_pl_2019 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Polish_parliamentary_election"

pl_session_2019 <- polite::bow(url = url_pl_2019, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

pl_session_2019 %>% 
  polite::scrape() -> pl_results_2019


pl_results_2019 %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-c(starts_with("x"), lead)) %>% 
  rename(institute = 1, date = 2) %>% 
  filter(str_detect(institute, "Firm|Results|Election", negate = TRUE)) %>% 
  filter(date != "") %>% 
  slice(-1) %>% 
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(), ~ na_if(.x, "-"))) %>%
  mutate(across(everything(), ~ na_if(.x, "N/A"))) %>% 
  mutate(across(everything(), ~ na_if(.x, ""))) %>% 
  mutate(across(everything(), ~ na_if(.x, "?"))) %>% 
  mutate(date = case_when(
    str_detect(date, "-") ~ str_extract(date, "(?<=-).*"),
    TRUE ~ date
  )) %>% 
  separate(col = date, into = c("day", "month", "year"), sep = " ") %>% 
  mutate(month = as.integer(factor(month, levels = month.abb))) %>%
  unite(date, day, month, year, sep = "-") %>% 
  mutate(sample_size = str_remove(sample_size, ",")) %>% 
  mutate(across(!c(date), type.convert)) -> polls_pl_2019



# South Korea -------------------------------------------------------------


url_sk_2012 <- "https://en.wikipedia.org/wiki/2012_South_Korean_legislative_election#Opinion_polling"

sk_session_2012 <- polite::bow(url = url_sk_2012, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

sk_session_2012 %>% 
  polite::scrape() -> sk_results_2012

sk_results_2012 %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  rename(nfp = 3, dup = 4, upp = 5, lfp = 6, kvp = 7, npp = 8) %>% 
  mutate(year = str_sub(date, start = -4)) %>% 
  mutate(month = str_extract_all(date, "[a-zA-Z]+")) %>%
  mutate(month = sapply(month, toString)) %>% 
  mutate(month = case_when(
    str_detect(month, ",") ~ str_extract(month, "(?<=,).*"),
    TRUE ~ month
  )) %>% 
  mutate(month = str_squish(month)) %>% 
  mutate(day = str_extract_all(date, "[\\d]+")) %>% 
  mutate(day = sapply(day, toString)) %>% 
  mutate(comma_count = str_count(day, ",")) %>% 
  mutate(day = case_when(
    comma_count == 1 ~ str_extract(day, "(.*?),"),
    comma_count == 2 ~ str_extract(day, ",(.*?),")
  )) %>% 
  mutate(day = str_remove_all(day, ","),
         day = str_squish(day)) %>% 
  select(-comma_count) %>% 
  unite(date, day, month, year, sep = " ") %>% 
  mutate(date = lubridate::dmy(date)) %>%   
  mutate(across(everything(), ~ str_remove(.x, "%"))) %>% 
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(across(!c(date, institute), type.convert)) -> polls_sk_2012


### 2016


url_sk_2016 <- "https://en.wikipedia.org/wiki/2016_South_Korean_legislative_election#Opinion_polls"

sk_session_2016 <- polite::bow(url = url_sk_2016, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

sk_session_2016 %>% 
  polite::scrape() -> sk_results_2016



sk_results_2016 %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[4]") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  select(-c(starts_with("x"), lead)) %>% 
  rename(date = 1, institute = 2, democratic = democratic_n_2, others = oth) %>% 
  filter(str_detect(date, "Date", negate = TRUE)) %>% 
  filter(str_detect(institute, "Election|election", negate = TRUE)) %>%
  filter(date != "") %>%
  mutate(across(everything(), ~ na_if(.x, "–"))) %>%
  mutate(across(everything(),~ str_remove_all(.x,  pattern = "\\[[\\s\\S]*\\]"))) %>%
  mutate(date = case_when(
    str_detect(date, "–") ~ str_extract(date, "(?<=–).*"),
    TRUE ~ date
  )) %>% 
  mutate(date = lubridate::dmy(date)) %>%  
  mutate(across(!c(date, institute), type.convert)) %>% 
  View()



### 2020

### important!!! election laws changed in south korea so satellite parties were
### created and merged in a constant manner


url_sk_2020 <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2020_South_Korean_legislative_election"

sk_session_2020 <- polite::bow(url = url_sk_2020, user_agent = "Polling error - philipp.bosch@uni-konstanz.de")

sk_session_2020 %>% 
  polite::scrape() -> sk_results_2020

sk_results_2020 %>% 
  html_nodes("table") %>% 
  html_table()
