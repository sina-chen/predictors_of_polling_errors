#-------------------------------------------------------------------------------
# Scraping the 1994 to 2021 German Landtags Election Polls - Helper functions
#
# Source: https://www.wahlrecht.de/umfragen/landtage/
# Author: Sina Chen
#
#-------------------------------------------------------------------------------

#### Polls ####

# download raw polls
get_raw_lt <- function(bl) {
  
  # define url
  bl_url <- paste0('https://www.wahlrecht.de/umfragen/landtage/',bl,'.htm')
  
  # dl html
  bl_html <- getURL(bl_url)
  
  # get number of tables with polls
  n_tables <- read_html(bl_url) %>% 
    html_table(fill = T) %>% length()
  
  # extract html tables
  bl_raw <- lapply(2:n_tables, function(x) bl_html %>% 
                     htmltab(which = x, rm_nodata_cols = T)) %>% 
    suppressWarnings()
  
  return(bl_raw)  
  
}

# subset polls 
subset_polls <- function(raw_poll){
  
  # remove election results
  res_pos <- which(str_detect(raw_poll[,1], 'Landtag') |
                     str_detect(raw_poll[,1], 'Abgeordnetenhauswahl')|
                     str_detect(raw_poll[,1], 'BÃ¼rgerschaftswahl')|
                     str_detect(raw_poll[,3], 'Nachwahl')|
                     str_detect(raw_poll[,3], 'Werte')|
                     str_detect(raw_poll[,5], 'â€“')|
                     str_detect(raw_poll[,5], '[?]') |
                     str_detect(raw_poll[,6], '[?]') |
                     str_detect(raw_poll[,7], '[?]') |
                     is.na(raw_poll[,5]) == T |
                     is.na(raw_poll[,6]) == T |
                     is.na(raw_poll[,7]) == T |
                     is.na(raw_poll[,8]) == T
                    )
  
  sonstige_pos <- which(colnames(raw_poll) %in% c('Sonstige', 'Sonstige.1', 'FW',
                                                  'PIRATEN', 'NPD', 'SSW',
                                                  'Pro DMSchill', 'Offensive(Ex-Schill)'))
  
  polls <- raw_poll[- res_pos, -sonstige_pos] 
  
  # remove duplicated columns
  polls <- polls[!duplicated(as.list(polls))]
  
  # rename columns
  if(any(str_detect(colnames(polls), 'Quelle')) == T) {
    polls <- polls %>% 
      rename(institute = 'Institut',
             client = 'Quelle',
             sample_size = 'Befragte',
             date = 'Datum')
  } else if (any(str_detect(colnames(polls), 'Auftraggeber')) == T) {
    polls <- polls %>% 
      rename(institute = 'Institut',
             client = 'Auftraggeber',
             sample_size = 'Befragte',
             date = 'Datum')
  }
  
  
  
  # reshape to long
  polls_long <- melt(polls, id.vars = c('institute', 'client', 
                                        'sample_size', 'date'),
                     value.name = 'support',
                     variable.name = 'party') %>% 
    mutate(support = if_else(str_detect(support, '[[:digit:]]+\\,*[[:digit:]]*\\s[%][[:digit:]]+\\,*[[:digit:]]*\\s[%][*][*]') == T,
                             str_replace_all(support, ',', '.') %>% 
                               str_extract_all('[[:digit:]]+\\.*[[:digit:]]*') %>% 
                               lapply(function(x) as.numeric(x)) %>% 
                               lapply(function(x) sum(x)) %>% 
                               unlist(), 
                             str_remove_all(support, '%|[,][?]|[*][*]|[*]') %>% 
                               str_replace_all(',', '.') %>% 
                               as.numeric),
           date = as.Date(date, '%d.%m.%Y'),
           party = fct_recode(party, cdu = 'CDU',
                                       cdu = 'CSU',
                                       spd = 'SPD',
                                       gru = 'GRÃœNE',
                                       gru = 'GRÜNE',
                                       gru = 'GAL',
                                       fdp = 'FDP',
                                       lin = 'LINKE',
                                       afd = 'AfD')) %>% 
    suppressWarnings() 
  
  return(polls_long)
  
}


# add others support share 
add_oth <- function(poll) {
  
  # reshape to wide to compute col sums
  poll_wide <- reshape2::dcast(poll, institute + client + sample_size + date + rowid(party) ~ party, 
                               value.var = 'support')
  
  # compute col sums
  poll_wide$oth <- 100 - rowSums(poll_wide[,6:length(poll_wide)], na.rm = T)
  
  # reshape to long
  polls_long <- reshape2::melt(poll_wide, id.vars = c('institute', 'client', 
                                                      'sample_size', 'date', 'rowid(party)'),
                               value.name = 'support',
                               variable.name = 'party') %>% 
    select(-`rowid(party)`)
  
  return(polls_long)
}


# add state identifier
add_state <- function(state_list) {
  
  for(i in 1:length(state_list)){
    
    # extract name
    name = names(state_list[i])
    
    # assign name
    state_list_name <- lapply(state_list[i], 
                              function(x) lapply(x, function(y) mutate(y, state = name)))
    state_list[i] <- state_list_name
    
  }
  
  return(state_list)
  
}


# clean sample size
clean_sample_size <- function(poll){
  
  poll_clean <- poll %>%
    mutate(sample_size = str_remove_all(sample_size, "\\d{2}[.]\\d{2}[.]â€“\\d{2}[.]\\d{2}[.]|\\d{2}[.]\\d{1}[.]â€“\\d{2}[.]\\d{2}[.]|\\d{2}[.]\\d{1}[.]â€“\\d{2}[.]\\d{1}[.]|\\d{2}[.]\\d{2}[.]â€“\\d{2}[.]\\d{2}$|\\d{2}[.]\\d{2}[.][–]\\d{2}[.]\\d{2}[.]|KW\\s\\d{2}|\\d{2}[.]\\d{2}[.][–]\\d{2}[.]\\d{2}$|\\d{2}[.]\\d{2}[.][-]\\d{2}[.]\\d{2}[.]|\\d{2}[.]\\d{2}[-]\\d{2}[.]\\d{2}[.]|\\d{2}[.]\\d{2}[.]\\d{4}|\\d{2}[.]\\d{2}[–]\\d{2}[.]\\d{2}[.]|\\d{2}[.]\\d{2}[.]$|[A-Za-z]\\s\\d{4}|[A-Za-z]+\\s[A-Za-z]+[.]\\s\\d{4}|\\d{2}[.]\\sKW|â€™\\d{2}$"),
           sample_size = if_else(str_detect(sample_size, '[?]') == T, NA_character_, sample_size) %>% 
             str_remove_all('[^0-9]') %>% 
             as.numeric())
  return(poll_clean)
}


# add election year 

add_election <- function(polls_state_list){
  
  # rbind each state
  polls_state_long <- lapply(polls_state_list,
                             function(x) do.call(what =  "rbind", x))
  
  # reshape to wide
  polls_state_wide <- lapply(polls_state_long, 
                             function(x) reshape2::dcast(x, state +
                                                           date + sample_size + 
                                                           institute + client +
                                                           rowid(party) ~ party, 
                                                         value.var = 'support'))
  
  # rbind all states
  polls_wide <- do.call(what = rbind, polls_state_wide) %>% 
    select(-'rowid(party)')
  
  # add election year
  polls_wide <- polls_wide %>%
    mutate(election_year = case_when(state == 'baden-wuerttemberg' &
                                       date <= as.Date('24.03.1996', '%d.%m.%Y') ~ 1996,
                                     state == 'baden-wuerttemberg' &
                                       date > as.Date('24.03.1996', '%d.%m.%Y') &
                                       date <= as.Date('25.03.2001', '%d.%m.%Y') ~ 2001,
                                     state == 'baden-wuerttemberg' &
                                       date > as.Date('25.03.2001', '%d.%m.%Y') &
                                       date <= as.Date('26.03.2006', '%d.%m.%Y') ~ 2006,
                                     state == 'baden-wuerttemberg' &
                                       date > as.Date('26.03.2006', '%d.%m.%Y') &
                                       date <= as.Date('27.03.2011', '%d.%m.%Y') ~ 2011,
                                     state == 'baden-wuerttemberg' &
                                       date > as.Date('27.03.2011', '%d.%m.%Y') &
                                       date <= as.Date('13.03.2016', '%d.%m.%Y') ~ 2016,
                                     state == 'baden-wuerttemberg' &
                                       date > as.Date('13.03.2016', '%d.%m.%Y') &
                                       date <= as.Date('14.03.2021', '%d.%m.%Y') ~ 2021,
                                     # bayern
                                     state == 'bayern' &
                                       date <= as.Date('25.09.1994', '%d.%m.%Y') ~ 1994,
                                     state == 'bayern' &
                                       date > as.Date('25.09.1994', '%d.%m.%Y') &
                                       date <= as.Date('13.09.1998', '%d.%m.%Y') ~ 1998,
                                     state == 'bayern' &
                                       date > as.Date('13.09.1998', '%d.%m.%Y') &
                                       date <= as.Date('21.09.2003', '%d.%m.%Y') ~ 2003,
                                     state == 'bayern' &
                                       date > as.Date('21.09.2003', '%d.%m.%Y') &
                                       date <= as.Date('28.09.2008', '%d.%m.%Y') ~ 2008,
                                     state == 'bayern' &
                                       date > as.Date('28.09.2008', '%d.%m.%Y') &
                                       date <= as.Date('15.09.2013', '%d.%m.%Y') ~ 2013,
                                     state == 'bayern' &
                                       date > as.Date('15.09.2013', '%d.%m.%Y') &
                                       date <= as.Date('14.10.2018', '%d.%m.%Y') ~ 2018,
                                     state == 'bayern' &
                                       date > as.Date('14.10.2018', '%d.%m.%Y') ~ 2023,
                                     # berlin
                                     state == 'berlin' &
                                       date <= as.Date('21.10.2001 ', '%d.%m.%Y') ~ 2001,
                                     state == 'berlin' &
                                       date > as.Date('21.10.2001', '%d.%m.%Y') &
                                       date <= as.Date('17.09.2006 ', '%d.%m.%Y') ~ 2006,
                                     state == 'berlin' &
                                       date > as.Date('17.09.2006', '%d.%m.%Y') &
                                       date <= as.Date('18.09.2011', '%d.%m.%Y') ~ 2011,
                                     state == 'berlin' &
                                       date > as.Date('18.09.2011', '%d.%m.%Y') &
                                       date <= as.Date('18.09.2016 ', '%d.%m.%Y') ~ 2016,
                                     state == 'berlin' &
                                       date > as.Date('18.09.2016', '%d.%m.%Y') ~ 2021,
                                     # brandenburg
                                     state == 'brandenburg' &
                                       date <= as.Date('11.09.1994 ', '%d.%m.%Y') ~ 1994,
                                     state == 'brandenburg' &
                                       date > as.Date('11.09.1994', '%d.%m.%Y') &
                                       date <= as.Date('05.09.1999 ', '%d.%m.%Y') ~ 1999,
                                     state == 'brandenburg' &
                                       date > as.Date('05.09.1999', '%d.%m.%Y') &
                                       date <= as.Date('19.09.2004 ', '%d.%m.%Y') ~ 2004,
                                     state == 'brandenburg' &
                                       date > as.Date('19.09.2004', '%d.%m.%Y') &
                                       date <= as.Date('27.09.2009 ', '%d.%m.%Y') ~ 2009,
                                     state == 'brandenburg' &
                                       date > as.Date('27.09.2009', '%d.%m.%Y') &
                                       date <= as.Date('14.09.2014 ', '%d.%m.%Y') ~ 2014,
                                     state == 'brandenburg' &
                                       date > as.Date('14.09.2014', '%d.%m.%Y') &
                                       date <= as.Date('01.09.2019 ', '%d.%m.%Y') ~ 2019,
                                     state == 'brandenburg' &
                                       date > as.Date('01.09.2019', '%d.%m.%Y') ~ 2024,
                                     # bremen
                                     state == 'bremen' &
                                       date <= as.Date('25.05.2003 ', '%d.%m.%Y') ~ 2003,
                                     state == 'bremen' &
                                       date > as.Date('25.05.2003', '%d.%m.%Y') &
                                       date <= as.Date('13.05.2007 ', '%d.%m.%Y') ~ 2007,
                                     state == 'bremen' &
                                       date > as.Date('13.05.2007', '%d.%m.%Y') &
                                       date <= as.Date('22.05.2011 ', '%d.%m.%Y') ~ 2011,
                                     state == 'bremen' &
                                       date > as.Date('22.05.2011', '%d.%m.%Y') &
                                       date <= as.Date('10.05.2015 ', '%d.%m.%Y') ~ 2015,
                                     state == 'bremen' &
                                       date > as.Date('10.05.2015', '%d.%m.%Y') &
                                       date <= as.Date('26.05.2019 ', '%d.%m.%Y') ~ 2019,
                                     state == 'bremen' &
                                       date > as.Date('26.05.2019', '%d.%m.%Y') ~ 2023,
                                     # hamburg
                                     state == 'hamburg' &
                                       date <= as.Date('23.09.2001 ', '%d.%m.%Y') ~ 2001,
                                     state == 'hamburg' &
                                       date > as.Date('23.09.2001', '%d.%m.%Y') &
                                       date <= as.Date('29.02.2004 ', '%d.%m.%Y') ~ 2004,
                                     state == 'hamburg' &
                                       date > as.Date('29.02.2004', '%d.%m.%Y') &
                                       date <= as.Date('24.02.2008 ', '%d.%m.%Y') ~ 2008,
                                     state == 'hamburg' &
                                       date > as.Date('24.02.2008', '%d.%m.%Y') &
                                       date <= as.Date('20.02.2011 ', '%d.%m.%Y') ~ 2011,
                                     state == 'hamburg' &
                                       date > as.Date('20.02.2011', '%d.%m.%Y') &
                                       date <= as.Date('15.02.2015 ', '%d.%m.%Y') ~ 2015,
                                     state == 'hamburg' &
                                       date > as.Date('15.02.2015', '%d.%m.%Y') &
                                       date <= as.Date('23.02.2020 ', '%d.%m.%Y') ~ 2020,
                                     state == 'hamburg' &
                                       date > as.Date('23.02.2020', '%d.%m.%Y') ~ 2025,
                                     # hessen,
                                     state == 'hessen' &
                                       date <= as.Date('19.02.1995 ', '%d.%m.%Y') ~ 1995,
                                     state == 'hessen' &
                                       date > as.Date('19.02.1995', '%d.%m.%Y') &
                                       date <= as.Date('07.02.1999 ', '%d.%m.%Y') ~ 1999,
                                     state == 'hessen' &
                                       date > as.Date('07.02.1999', '%d.%m.%Y') &
                                       date <= as.Date('02.02.2003 ', '%d.%m.%Y') ~ 2003,
                                     state == 'hessen' &
                                       date > as.Date('02.02.2003', '%d.%m.%Y') &
                                       date <= as.Date('27.01.2008 ', '%d.%m.%Y') ~ 2008,
                                     state == 'hessen' &
                                       date > as.Date('27.01.2008', '%d.%m.%Y') &
                                       date <= as.Date('18.01.2009 ', '%d.%m.%Y') ~ 2009,
                                     state == 'hessen' &
                                       date > as.Date('18.01.2009', '%d.%m.%Y') &
                                       date <= as.Date('22.09.2013 ', '%d.%m.%Y') ~ 2013,
                                     state == 'hessen' &
                                       date > as.Date('22.09.2013', '%d.%m.%Y') &
                                       date <= as.Date('28.10.2018 ', '%d.%m.%Y') ~ 2018,
                                     state == 'hessen' &
                                       date > as.Date('28.10.2018', '%d.%m.%Y')  ~ 2023,
                                     # meck-pom
                                     state == 'mecklenburg-vorpommern' &
                                       date <= as.Date('16.10.1994 ', '%d.%m.%Y') ~ 1994,
                                     state == 'mecklenburg-vorpommern' &
                                       date > as.Date('16.10.1994', '%d.%m.%Y') &
                                       date <= as.Date('27.09.1998 ', '%d.%m.%Y') ~ 1998,
                                     state == 'mecklenburg-vorpommern' &
                                       date > as.Date('27.09.1998', '%d.%m.%Y') &
                                       date <= as.Date('22.09.2002 ', '%d.%m.%Y') ~ 2002,
                                     state == 'mecklenburg-vorpommern' &
                                       date > as.Date('22.09.2002', '%d.%m.%Y') &
                                       date <= as.Date('17.09.2006 ', '%d.%m.%Y') ~ 2006,
                                     state == 'mecklenburg-vorpommern' &
                                       date > as.Date('17.09.2006', '%d.%m.%Y') &
                                       date <= as.Date('04.09.2011 ', '%d.%m.%Y') ~ 2011,
                                     state == 'mecklenburg-vorpommern' &
                                       date > as.Date('04.09.2011', '%d.%m.%Y') &
                                       date <= as.Date('04.09.2016', '%d.%m.%Y') ~ 2016,
                                     state == 'mecklenburg-vorpommern' &
                                       date > as.Date('04.09.2016', '%d.%m.%Y') ~ 2021,
                                     # niedersachsen
                                     state == 'niedersachsen' &
                                       date <= as.Date('02.02.2003', '%d.%m.%Y') ~ 2003,
                                     state == 'niedersachsen' &
                                       date > as.Date('02.02.2003', '%d.%m.%Y') &
                                       date <= as.Date('27.01.2008', '%d.%m.%Y') ~ 2008,
                                     state == 'niedersachsen' &
                                       date > as.Date('27.01.2008', '%d.%m.%Y') &
                                       date <= as.Date('20.01.2013', '%d.%m.%Y') ~ 2013,
                                     state == 'niedersachsen' &
                                       date > as.Date('20.01.2013', '%d.%m.%Y') &
                                       date <= as.Date('15.10.2017', '%d.%m.%Y') ~ 2017,
                                     state == 'niedersachsen' &
                                       date > as.Date('15.10.2017', '%d.%m.%Y') ~ 2022,
                                     # nrw
                                     state == 'nrw' &
                                       date <= as.Date('14.05.2000', '%d.%m.%Y') ~ 2000,
                                     state == 'nrw' &
                                       date > as.Date('14.05.2000', '%d.%m.%Y') &
                                       date <= as.Date('22.05.2005', '%d.%m.%Y') ~ 2005,
                                     state == 'nrw' &
                                       date > as.Date('22.05.2005', '%d.%m.%Y') &
                                       date <= as.Date('09.05.2010', '%d.%m.%Y') ~ 2010,
                                     state == 'nrw' &
                                       date > as.Date('09.05.2010', '%d.%m.%Y') &
                                       date <= as.Date('13.05.2012', '%d.%m.%Y') ~ 2012,
                                     state == 'nrw' &
                                       date > as.Date('13.05.2012', '%d.%m.%Y') &
                                       date <= as.Date('14.05.2017', '%d.%m.%Y') ~ 2017,
                                     state == 'nrw' &
                                       date > as.Date('14.05.2017', '%d.%m.%Y') ~ 2022,
                                     # rheinland-pfalz,
                                     state == 'rheinland-pfalz' &
                                       date <= as.Date('26.03.2006', '%d.%m.%Y') ~ 2006,
                                     state == 'rheinland-pfalz' &
                                       date > as.Date('26.03.2006', '%d.%m.%Y') &
                                       date <= as.Date('27.03.2011', '%d.%m.%Y') ~ 2011,
                                     state == 'rheinland-pfalz' &
                                       date > as.Date('27.03.2011', '%d.%m.%Y') &
                                       date <= as.Date('13.03.2016', '%d.%m.%Y') ~ 2016,
                                     state == 'rheinland-pfalz' &
                                       date > as.Date('13.03.2016', '%d.%m.%Y') &
                                       date <= as.Date('14.03.2021', '%d.%m.%Y') ~ 2021,
                                     # saarland,
                                     state == 'saarland' &
                                       date <= as.Date('05.09.2004', '%d.%m.%Y') ~ 2004,
                                     state == 'saarland' &
                                       date > as.Date('05.09.2004', '%d.%m.%Y') &
                                       date <= as.Date('30.08.2009', '%d.%m.%Y') ~ 2009,
                                     state == 'saarland' &
                                       date > as.Date('30.08.2009', '%d.%m.%Y') &
                                       date <= as.Date('25.03.2012', '%d.%m.%Y') ~ 2012,
                                     state == 'saarland' &
                                       date > as.Date('25.03.2012', '%d.%m.%Y') &
                                       date <= as.Date('26.03.2017', '%d.%m.%Y') ~ 2017,
                                     state == 'saarland' &
                                       date > as.Date('26.03.2017', '%d.%m.%Y') ~ 2022,
                                     # sachsen
                                     state == 'sachsen' &
                                       date <= as.Date('11.09.1994', '%d.%m.%Y') ~ 1994,
                                     state == 'sachsen' &
                                       date > as.Date('11.09.1994', '%d.%m.%Y') &
                                       date <= as.Date('19.09.1999', '%d.%m.%Y') ~ 1999,
                                     state == 'sachsen' &
                                       date > as.Date('19.09.1999', '%d.%m.%Y') &
                                       date <= as.Date('19.09.2004', '%d.%m.%Y') ~ 2004,
                                     state == 'sachsen' &
                                       date > as.Date('19.09.2004', '%d.%m.%Y') &
                                       date <= as.Date('30.08.2009', '%d.%m.%Y') ~ 2009,
                                     state == 'sachsen' &
                                       date > as.Date('30.08.2009', '%d.%m.%Y') &
                                       date <= as.Date('31.08.2014', '%d.%m.%Y') ~ 2014,
                                     state == 'sachsen' &
                                       date > as.Date('31.08.2014', '%d.%m.%Y') &
                                       date <= as.Date('01.09.2019', '%d.%m.%Y') ~ 2019,
                                     state == 'sachsen' &
                                       date > as.Date('01.09.2019', '%d.%m.%Y') ~ 2024,
                                     # sachsen-anhalt,
                                     state == 'sachsen-anhalt' &
                                       date <= as.Date('21.04.2002', '%d.%m.%Y') ~ 2002,
                                     state == 'sachsen-anhalt' &
                                       date > as.Date('21.04.2002', '%d.%m.%Y') &
                                       date <= as.Date('26.03.2006', '%d.%m.%Y') ~ 2006,
                                     state == 'sachsen-anhalt' &
                                       date > as.Date('26.03.2006', '%d.%m.%Y') &
                                       date <= as.Date('20.03.2011', '%d.%m.%Y') ~ 2011,
                                     state == 'sachsen-anhalt' &
                                       date > as.Date('20.03.2011', '%d.%m.%Y') &
                                       date <= as.Date('13.03.2016', '%d.%m.%Y') ~ 2016,
                                     state == 'sachsen-anhalt' &
                                       date > as.Date('13.03.2016', '%d.%m.%Y')  ~ 2021,
                                     # schleswig-holstein
                                     state == 'schleswig-holstein' &
                                       date <= as.Date('24.03.1996', '%d.%m.%Y') ~ 1996,
                                     state == 'schleswig-holstein' &
                                       date > as.Date('24.03.1996', '%d.%m.%Y') &
                                       date <= as.Date('27.02.2000', '%d.%m.%Y') ~ 2000,
                                     state == 'schleswig-holstein' &
                                       date > as.Date('27.02.2000', '%d.%m.%Y') &
                                       date <= as.Date('20.02.2005', '%d.%m.%Y') ~ 2005,
                                     state == 'schleswig-holstein' &
                                       date > as.Date('20.02.2005', '%d.%m.%Y') &
                                       date <= as.Date('27.09.2009', '%d.%m.%Y') ~ 2009,
                                     state == 'schleswig-holstein' &
                                       date > as.Date('27.09.2009', '%d.%m.%Y') &
                                       date <= as.Date('06.05.2012', '%d.%m.%Y') ~ 2012,
                                     state == 'schleswig-holstein' &
                                       date > as.Date('06.05.2012', '%d.%m.%Y') &
                                       date <= as.Date('07.05.2017', '%d.%m.%Y') ~ 2017,
                                     state == 'schleswig-holstein' &
                                       date > as.Date('07.05.2017', '%d.%m.%Y') ~ 2022,
                                     # thueringen,
                                     state == 'thueringen' &
                                       date <= as.Date('13.06.2004', '%d.%m.%Y') ~ 2004,
                                     state == 'thueringen' &
                                       date > as.Date('13.06.2004', '%d.%m.%Y') &
                                       date <= as.Date('30.08.2009', '%d.%m.%Y') ~ 2009,
                                     state == 'thueringen' &
                                       date > as.Date('30.08.2009', '%d.%m.%Y') &
                                       date <= as.Date('14.09.2014', '%d.%m.%Y') ~ 2014,
                                     state == 'thueringen' &
                                       date > as.Date('14.09.2014', '%d.%m.%Y') &
                                       date <= as.Date('27.10.2019', '%d.%m.%Y') ~ 2019,
                                     state == 'thueringen' &
                                       date > as.Date('27.10.2019', '%d.%m.%Y') ~ 2021
    ))
  
  return(polls_wide)
  
}


# clean institute names
clean_institute <- function(polls_lt){
  
  polls_lt_clean <- polls_lt %>% mutate(institute_clean = str_remove_all(institute, '\\s[*]|[*]') %>% 
                                          tolower() %>% 
                                          str_replace_all('\\s', '_'),
                                        institute_clean = if_else(institute_clean == 'forschâ€™gr.wahlen' |
                                                                    institute_clean == 'forschungs-gruppe_wahlen'|
                                                                    institute_clean == 'forschungs-gruppewahlen'|
                                                                    institute_clean == 'forschungsgruppewahlen' |
                                                                    institute_clean == 'fgwtelefonfeld', 'fgw', institute_clean),
                                        institute_clean = if_else(institute_clean == 'infratest_burke' |
                                                                    institute_clean == 'infratestburke'|
                                                                    institute_clean == 'infratestdimap'|
                                                                    institute_clean == 'infratestpolitik-forschung' |
                                                                    institute_clean == 'infratestpolitikforschung' |
                                                                    institute_clean == 'infratestsozialforschung'|
                                                                    institute_clean == 'tns_forschung'|
                                                                    institute_clean == 'tns_infratest'|
                                                                    institute_clean == 'tnsinfratest'|
                                                                    institute_clean == 'nfo_infratest', 'infratest',institute_clean),
                                        institute_clean = if_else(institute_clean == 'gessphone_&_field', 'gess', institute_clean),
                                        institute_clean = if_else(institute_clean == 'ifmleipzig', 'ifm_leipzig', institute_clean),
                                        institute_clean = if_else(institute_clean == 'universitã¤tkiel', 'uni_kiel', institute_clean),
                                        institute_clean = if_else(institute_clean == 'universitã¤thamburg', 'uni_hamburg', institute_clean),
                                        institute_clean = if_else(institute_clean == 'polis', 'polis_sinus', institute_clean),
                                        institute_clean = if_else(institute_clean == 'polis+sinus', 'polis_sinus', institute_clean),
                                        institute_clean = if_else(institute_clean == 'mifmmã¼nchen', 'mifm_muenchen', institute_clean),
                                        institute_clean = if_else(institute_clean == 'inra', 'ipsos', institute_clean)
                                        
  )
  
}


#### Results ####

# subset polls 
subset_res <- function(raw_poll){
  
  # remove election results
  res_pos <- which(str_detect(raw_poll[,1], 'Landtag') |
                     str_detect(raw_poll[,1], 'Abgeordnetenhauswahl')|
                     str_detect(raw_poll[,1], 'BÃ¼rgerschaftswahl')
  )
  
  # remove last entry (duplicated)
  res_pos <- res_pos[-length(res_pos)]
  
  sonstige_pos <- which(colnames(raw_poll) %in% c('Sonstige', 'Sonstige.1', 'Datum', 'FW',
                                                  'PIRATEN', 'NPD', 'SSW',
                                                  'Pro DMSchill', 'Offensive(Ex-Schill)'))
  
  res <- raw_poll[res_pos,-sonstige_pos] 

  return(res)
  
}

# add other voteshare
add_oth_res <- function(res) {
  
  # remove duplicated col
  res <- res[!duplicated(as.list(res)) | colnames(res) == 'GRÃœNE']
  
  if (!'LINKE' %in% colnames(res)) {
    res$LINKE <- NA
  }
    
  if (!'AfD' %in% colnames(res)) {
    res$AfD <- NA
  }
  
  res <- res %>% 
    mutate(LINKE = if_else(str_detect(LINKE, '[[:digit:]]+\\,*[[:digit:]]*\\s[%][[:digit:]]+\\,*[[:digit:]]*\\s[%][*][*]') == T,
                           str_replace_all(LINKE, ',', '.') %>% 
                             str_extract_all('[[:digit:]]+\\.*[[:digit:]]*') %>% 
                             lapply(function(x) as.numeric(x)) %>% 
                             lapply(function(x) sum(x)) %>% 
                             unlist(), 
                           str_remove_all(LINKE, '[%]|[*][*]|â€“') %>% 
                             str_replace_all(',', '.') %>% 
                             as.numeric())) %>%  
    suppressWarnings()
  
  # remove %
  res <- lapply(res, function(x) str_remove_all(str_replace_all(x,'[,]', '.'), 
                                                pattern= '[%]|[*][*]|â€“')) %>% 
    as.data.frame()
  
  # conver voteshares to numeric
  res[-1] <- lapply(res[-1], function(x) as.numeric(x)) %>%  
    as.data.frame() %>% 
    suppressWarnings()
  
  # compute col sums
  res$oth <- 100 - rowSums(res[,2:length(res)], na.rm = T)
  
  return(res)
}


# add election info
add_election_info <- function(res) {
  
  # election date 
  res$election_date <- str_remove_all(res[,1], '[^[.]\\d]')
  
  # election year
  res$election_year <- str_extract(res$election_date, '\\d{4}')
  
  # remove first column
  res <- res[,-1]
  
  return(res)
  
}

# relabel parties 
party_names <- function(res_state_list){
  
  # reshape each state
  res_state_long <- lapply(res_state_list, 
                           function(x) lapply(x, 
                                              function(y) reshape2::melt(y, id.vars = c('election_year', 'state', 'election_date'),
                                                                         variable.name = 'party', value.name = 'voteshare')) %>% 
                             do.call(what =  "rbind"))
  
  res_long <- do.call(what = rbind, res_state_long)
  
  res_long <- res_long %>% 
    mutate(party = fct_recode(party, cdu = 'CDU',
                              cdu = 'CSU',
                              spd = 'SPD',
                              gru = 'GRÃœNE',
                              gru = 'GAL',
                              gru = 'GRÜNE',
                              fdp = 'FDP',
                              lin = 'LINKE',
                              afd = 'AfD'
    ) )
  
  res_wide <- reshape2::dcast(res_long, 
                              election_year + state + election_date ~ party, 
                              value.var = 'voteshare')
  
  return(res_wide)
}

