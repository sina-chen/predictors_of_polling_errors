#-------------------------------------------------------------------------------
# Helper functions scraping polls for the German Bundestagswahl 1998 to 2021
#
# Source code: https://github.com/simonmunzert/gerpol-forecasting-2013-election-polls 
# Author: Peter Selb and Simon Munzert
#
# Source polls: wahlrecht.de
#
# Modified: Sina Chen
#-------------------------------------------------------------------------------

#### Get all polls conducted by one institute #### ####

get_institute_polls <- function(institute){
  
  # get html
  inst.url <- getURL(paste0("https://www.wahlrecht.de/umfragen/", institute, '.htm'))

  # get individual year links
  if (institute != 'insa' & institute != 'yougov') {
    siteHtml2 <- htmlTreeParse(inst.url, useInternalNodes=TRUE)
    reg.year <- xpathSApply(siteHtml2, "//a/@href")
    reg.year <- reg.year[str_detect(reg.year, institute[1])]
    reg.year <- reg.year[str_detect(reg.year, "[0-9]+")]
    year.links <- paste("https://www.wahlrecht.de/umfragen/", reg.year,sep="")
    year.links[length(year.links) + 1] <- paste0("https://www.wahlrecht.de/umfragen/", institute, '.htm')
  } else {
    year.links <- paste0("https://www.wahlrecht.de/umfragen/", institute, '.htm')
  }
  
  list.institute.polls <- lapply(year.links, get_polls, institute = institute) 

  return(list.institute.polls)
}


#### Get polls from a single website ####

get_polls <- function(link, institute){
  
  tables <- link %>%  
    read_html() %>% 
    html_table(header = T)  
  
  # get table with polls
  polls <- tables[[2]]
  
  keep1 <- apply(polls[1:(length(polls))], 1,
                 function(x) length(unique(x[!is.na(x)])) != 1) 
  keep2 <- sapply(1:nrow(polls), function(x) all(polls[x,] == colnames(polls)))
  
  polls <- polls[which(keep1 == T & keep2 == F),]
  
  # remove empty columns
  polls <- polls[!apply(polls, 2, function(x) all(is.na(x)) | all(x==""))]
  polls <- polls[-which((colnames(polls)==""))]
  
  # assign name to date column
  colnames(polls)[1] <- 'date_raw'
  
  # add year for processing dates (temporary)
  if (str_detect(link, '\\d+') == T & 
      institute != 'insa' & 
      institute != 'yougov') {
    year <- str_extract_all(link, '\\d+')
  } else if (str_detect(link, '\\d+') == F & 
             institute != 'insa' & 
             institute != 'yougov') {
    year <- 2021
  } else {
    year <- NA
  }
  
  # add institute name
  polls <- polls %>% 
    mutate(institute = institute,
           year = year)
  
  # clean column names
  colnames(polls) <- colnames(polls) %>% 
    str_replace_all("Ãœ","Ü") %>% 
    str_replace_all("Ã¤","ä") %>% 
    str_replace_all('Linke.PDS|PDS', 'LINKE') 
  
  # remove 	"Nichtwähler/Unentschl." column
  if (any(str_detect(colnames(polls), 'Nichtwähler/Unentschl.')) == T) {
    polls <- polls %>% 
      select(-`Nichtwähler/Unentschl.`)
  }
  
  # add "Befragte" and "Zeitraum" if missing
  if(any(str_detect(colnames(polls), 'Befragte')) == F & 
     any(str_detect(colnames(polls), 'Zeitraum')) == T ) {
    polls$Befragte <- NA
  } else if (any(str_detect(colnames(polls), 'Befragte')) == F & 
             any(str_detect(colnames(polls), 'Zeitraum')) == T) {
    polls$Zeitraum <- NA
  } else if (any(str_detect(colnames(polls), 'Befragte')) == F & 
             any(str_detect(colnames(polls), 'Zeitraum')) == F) {
    polls$Befragte <- NA
    polls$Zeitraum <- NA
  } 
  
  polls <- polls %>% 
    rename(sample_size = 'Befragte',
           period = 'Zeitraum')
  
  # remove election result from previous election
  if (isTRUE(polls[nrow(polls), 'sample_size'] == 'Bundestagswahl')) {
    polls <- polls[-nrow(polls),]
  }
  
  # clean polls
  polls <- polls %>% 
    mutate_all(~str_remove_all(.x,'%')) %>% 
    mutate_all(~str_replace_all(.x,'â€“', '-')) %>% 
    mutate_all(~str_replace_all(.x,',', '.'))
  
  # get smaller parties
  small_party <- which(!colnames(polls) %in% c('CDU/CSU', 'SPD','GRÜNE', 
                                               'LINKE', 'FDP', 'Sonstige',
                                               'date_raw', 'sample_size', 
                                               'period', 'institute', 'year'))

  # add column with name of smaller parties
  if (length(small_party) == 1) { # one smaller party
    
    sonstige_party <- ifelse(polls[colnames(polls[small_party])] == '–' | 
                              polls[colnames(polls[small_party])] == '-', T, F) %>% 
      as.data.frame() 

    sonstige_party$id <- seq(1:nrow(sonstige_party))
    
    sonstige_party <- sonstige_party %>% gather(sonst_parties, value, -id) %>%  # change to long format
      filter(value) %>%  # filter for value which are TRUE
      group_by(id) %>%
      summarise(sonst_parties = paste0(sonst_parties, collapse=","))

    polls$id <-  seq(1:nrow(polls))

    polls <- merge(polls, sonstige_party, all = T) %>%
      select(-id)

  } else if (length(small_party) > 1) { # > 1 smaller parties
    
    sonstige_party <- apply(polls[colnames(polls[small_party])], 
                            1, 
                            function(x) x == '–' | x == '-') %>% 
      t() %>%  
      as.data.frame() 
    
    sonstige_party$id <- seq(1:nrow(sonstige_party))
    
    sonstige_party <- sonstige_party %>% gather(sonst_parties, value, -id) %>%  # change to long format
      filter(value) %>%  # filter for value which are TRUE
      group_by(id) %>%
      summarise(sonst_parties = list(sonst_parties)) # store multiple smaller parties as list
    
    polls$id <-  seq(1:nrow(polls))
    
    polls <- merge(polls, sonstige_party, all = T) %>% 
      select(-id)
    
  } else {
    
    polls$sonst_parties <- NA_character_
    
  }
  
  # clean year
  polls <- polls %>% 
    mutate(year = if_else(is.na(str_detect(date_raw, '\\d{4}')) == F & year != str_extract(date_raw, '\\d{4}'),
                          str_extract(date_raw, '\\d{4}'), year) %>% 
             unlist())
  
  polls$poll_id <- seq(1:nrow(polls))
  polls <- polls %>% 
    mutate(poll_id = seq(1:nrow(polls)),
           poll_id = paste0(year, institute, poll_id))
  
  # reshape to long format
  polls_long <- polls %>% 
    reshape2::melt(id.vars = c("date_raw", "sample_size", "period", "institute", 
                               "year", "sonst_parties", "poll_id"),
         value.name = "forecast",
         variable.name = "party")
  
  return(polls_long)
  
}

### Get instutute names ####
get_institutes <- function(url_wahlrecht){
  
  url <- getURL(url_wahlrecht)
  siteHtml <- htmlTreeParse(url, useInternalNodes=TRUE)
  reg <- xpathSApply(siteHtml,"//th/a/@href")
  institutes <- str_remove_all(reg, '.htm')
  
  return(institutes)
}


### Add election results ####

add_results <- function(polls, results) {
  
  # merge results to polls with explicit pthers ("sonstige") share
  df_results <- merge(polls, results, 
                      by = c('election', 'party', 'institute'), all.x = T) %>% 
    mutate(result = if_else(is.na(sonst_parties) == F & party == 'Sonstige', NA_real_, result))
  
  # identify others
  na_sonst <- which(is.na(df_results$result) == T & df_results$election < 2021)
  
  # add number of parties included in others category
  df_results$length_sonstige <- sapply(df_results$sonst_parties, length)
  
  # add share of others
  for(i in na_sonst) {
    
    sonstige_sum <- results[which(results$election == df_results[i, 'election'] & 
                                    results$institute == df_results[i, 'institute'] &
                                    results$party == 'Sonstige'), 'result']
    
    
    for(j in 1:df_results[i, 'length_sonstige']){
      sonstige_parties <- df_results[i, 'sonst_parties'[[1]]] %>%  unlist
      sonstige_result <- results[which(results$election == df_results[i, 'election'] & 
                                         results$institute == df_results[i, 'institute'] &
                                         results$party == sonstige_parties[j]), 'result']
      
      if (length(sonstige_result) < 1) {
        sonstige_result <- 0.0
      }
      sonstige_sum <- sonstige_sum + sonstige_result
      
    }
    
    
    df_results[i, 'result'] <- sonstige_sum
  }
  
  df_results <- df_results %>% 
    select(-length_sonstige)
  
  return(df_results)
  
}


#### Get election results #####

get_btw_results <- function(polls_raw){
  results <- polls_raw[which(polls_raw$date_raw == 'Wahl 1998' | 
                               polls_raw$sample_size == 'Bundestagswahl' |
                               polls_raw$date == '27.09.1998'),] %>% 
    mutate(election = str_extract(date_raw, '\\d{4}'),
           result = str_remove_all(forecast, '%') %>% 
             str_replace_all(',', '.') %>% 
             str_replace_all('-|â€“|–', '0'),
           result = if_else(str_detect(result, "FW") == T & election == 2021, "8.7", result)) %>% 
    subset(str_detect(forecast, 'PIR') == F)  %>% 
    select(-c(sample_size, period, date_raw, year, forecast, sonst_parties, 
              poll_id)) %>% 
    unique()  %>% 
    mutate(result = as.numeric(result))

  return(results)
}

#### Clean data #####

clean_btw_polls <- function(polls_raw){
  
  # remove election results
  clean_polls <- polls_raw[-which(polls_raw$date_raw == 'Wahl 1998' | 
                                    polls_raw$sample_size == 'Bundestagswahl' |
                                    polls_raw$date_raw == '27.09.1998'),]
  
  # sample_size, clean period, forecast and year
  clean_polls <- clean_polls %>% 
    mutate(sample_size = if_else(str_detect(sample_size, '[?]') == T, 
                                 NA_character_, 
                                 sample_size) %>%
             str_remove_all('[^0-9]') %>% 
             as.numeric(),
           year = if_else(is.na(str_detect(date_raw, '\\d{4}')) == F & 
                            year != str_extract(date_raw, '\\d{4}'),
                          str_extract(date_raw, '\\d{4}'), year) %>% 
             unlist(),
           forecast = str_replace_all(forecast, '-|^[?]|–', NA_character_))
  
  # remove characters from forecast
  clean_polls[which(str_detect(clean_polls$forecast, '[[:alpha:]]')),] <- clean_polls[which(str_detect(clean_polls$forecast, '[[:alpha:]]')),] %>% 
    mutate(forecast = str_remove_all(forecast, '[^0-9]') %>% 
             str_remove('^ ') %>% 
             strsplit(split = "") %>%  
             lapply(function(x) sum(as.numeric(x))) %>% 
             unlist())
  
  # set empty cells to NA
  clean_polls[clean_polls == ""] <- NA
  
  # convert date 
  clean_polls <- clean_polls %>% 
    mutate(date = as.Date(str_replace_all(date_raw, '[?][?]', '01'), '%d.%m.%Y'))
  
  # fill missing dates with field period information
  clean_polls <- clean_polls %>% 
    mutate(date = if_else(is.na(date) == T & is.na(period) == F, 
                          as.Date(paste0(period,year), '%d.%m.–%d.%m.%Y'),
                          date),
           date_raw = date_raw %>% 
             str_replace_all('Mrz', 'Mar') %>% 
             str_replace_all('Okt', 'Oct') %>% 
             str_replace_all('Dez', 'Dec') %>% 
             str_replace_all('Mai', 'May.'))
  
  # insert date without information on day
  clean_polls <- clean_polls %>%  
    mutate(date = if_else(str_detect(date_raw, '\\w+[.] \\d+') == T & 
                            is.na(date) == T,
                          as.Date(as.yearmon(date_raw, '%b. %Y'), frac = 1), 
                          date ))
  
  # add election and poll id
  clean_polls <- clean_polls %>% 
    mutate(election = case_when(date <= as.Date('27.09.1998', '%d.%m.%Y') |
                                  date_raw == 'Wahl 1998' ~ 1998,
                                date > as.Date('27.09.1998', '%d.%m.%Y') &
                                  date <= as.Date('22.09.2002', '%d.%m.%Y') ~ 2002,
                                date > as.Date('22.09.2002', '%d.%m.%Y') &
                                  date <= as.Date('18.09.2005', '%d.%m.%Y') |
                                  year == 2004 ~ 2005,
                                date > as.Date('18.09.2005', '%d.%m.%Y') &
                                  date <= as.Date('27.09.2009', '%d.%m.%Y') |
                                  year == 2005 ~ 2009, # polls with missing information on date from emnid in 2005 were conducted after the election 2005 (source: wahlrecht)
                                date > as.Date('27.09.2009', '%d.%m.%Y') &
                                  date <= as.Date('22.09.2013', '%d.%m.%Y') ~ 2013,
                                date > as.Date('22.09.2013', '%d.%m.%Y') &
                                  date <= as.Date('24.09.2017', '%d.%m.%Y') ~ 2017,
                                date > as.Date('24.09.2017', '%d.%m.%Y') &
                                  date <= as.Date('26.09.2021', '%d.%m.%Y') ~ 2021,
                                date > as.Date('26.09.2021', '%d.%m.%Y') ~ 2025),
           poll_id = if_else(str_detect(poll_id, "NA") == T, 
                             str_replace(poll_id, "NA", as.character(election)), 
                             poll_id))
  
  
  # remove NA forecast
  clean_polls <- clean_polls %>% 
    subset(is.na(forecast) == F | forecast != '0') %>% 
    mutate(forecast = as.numeric(forecast)) %>% 
    unique() %>% 
    select(-c(date_raw, year)) 
  
  return(clean_polls)
} 


