# --------------------------------------------------------------------------- #
# OpenFEC API
# --------------------------------------------------------------------------- #

library(readr)
library(jsonlite)
library(tidyverse)
library(httr)

# --------------------------------------------------------------------------- #

# get_fec_response
get_openFEC_response <- function(path, query = list(), key) {
  base <- 'http://api.open.fec.gov/v1'
  url <- modify_url(url = paste0(base, path, '?api_key=', key), query = query)
  response <- GET(url)
  return(response)
}

# get_fec_data
get_openFEC_data <- function(path = '/elections/search/', query=list(page=1), key) {
  response <- get_openFEC_response(path=path, query = query, key=key)
  pages <- fromJSON(content(response, 'text', encoding='UTF-8'))$pagination$pages
  # pagination
  if(pages > 1){
    data <- list()
    for(i in 1:pages){
      query$page <- i
      response <- get_openFEC_response(path=path, query = query, key=key)
      data <- bind_rows(data, fromJSON(content(response, 'text', encoding='UTF-8'))$results)
      print(paste('page:', i, '/', pages))
      # delay API calls
      Sys.sleep(.5)
    }
  } else {
    data <- fromJSON(content(response, 'text', encoding='UTF-8'))$results
  }
  print(data)
}

# --------------------------------------------------------------------------- #

# test
# response <- get_openFEC_response(path = '/elections/search/', key=api_key)
# data <- get_openFEC_data(key = api_key)

# --------------------------------------------------------------------------- #