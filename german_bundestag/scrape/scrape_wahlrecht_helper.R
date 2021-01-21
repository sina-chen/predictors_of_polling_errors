################################################################################
# Helper functions scraping polls for the German Bundestagswahl 1998 to 2020
#
# Source code: https://github.com/simonmunzert/gerpol-forecasting-2013-election-polls 
# Author: Peter Selb and Simon Munzert
#
# Source polls: wahlrecht.de
#
# Modified: Sina Chen
################################################################################

#### Get all polls conducted by one institute #### ####

get_institute_polls <- function(institute){
  
  # get html
  inst.url <- getURL(paste0("https://www.wahlrecht.de/umfragen/", institute, '.htm'))

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
  
  url.year <- getURL(link)
  
  # get table with polls
  polls <- url.year %>%  
    htmltab(which = 2, rm_nodata_cols = F) %>% 
    suppressWarnings()
  
  keep <- apply(polls[2:(length(polls) - 3)], 1,
                function(x) length(unique(x[!is.na(x)])) != 1) 
  
  polls <- polls[which(keep == T),]
  
  # remove empty columns
  polls <- polls[!apply(polls, 2, function(x) all(is.na(x)) | all(x==""))]
  
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
      summarise(sonst_parties = list(sonst_parties)) # stire multiple smaller parties as list
    
    polls$id <-  seq(1:nrow(polls))
    
    polls <- merge(polls, sonstige_party, all = T) %>% 
      select(-id)
    
  } else {
    
    polls$sonst_parties <- NA_character_
    
  }
  
  # reshape to long format
  polls_long <- polls %>% 
    reshape2::melt(id.vars = c('date_raw', 'sample_size', 'period', 'institute', 'year', 'sonst_parties'),
         value.name = 'forecast',
         variable.name = 'party')
  
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


