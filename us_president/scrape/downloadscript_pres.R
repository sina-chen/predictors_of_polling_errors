#-------------------------------------------------------------------------------
#
# Download script: Presidential election polls from 2000 to 2016
# Author: Sina Chen
# Source: pollingreport.com 
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------


library(RCurl)
library(XML)
library(stringr)
library(stringi)
library(pryr)
library(tidyverse)



# Preparation -------------------------------------------------------------

# working directory
setwd('your_working_directory')

# username & password
usr_pwd <- read_file('usr_pwd.txt')

# debug gatherer
debuginfo <- debugGatherer()

# curl handle + identification
handle <- getCurlHandle(cookiejar="",
                        followlocation = TRUE,
                        autoreferer = TRUE,
                        debugfunc = debuginfo$update,
                        verbose = TRUE,
                        httpheader=list(
                          from = "your_mail_address",
                          "user-agent" = str_c(R.version$version.string,
                                              ", ", R.version$platform)
                        ) )


# Functions ---------------------------------------------------------------

# Get list of states by year

get_state_urls <- function(year){
  url <- paste0('https://www.pollingreport.us/sub/',year,'.htm')
  url <- getURL(url, userpwd = usr_pwd, followlocation = TRUE, curl = handle)
  url_parsed <- htmlParse(url)
  links <- xpathSApply(url_parsed,path = "//a", xmlGetAttr, "href")
  links_year <- links[which(lapply(links, function(x) 
    str_detect(x, pattern = paste0('[[:alpha:]]+', year, '|', 
                                   year, '[[:alpha:]]+[.]htm$|',
                                   year,'c[.]htm#Maine$')))==T)]
  links_comp <- as.list(stri_c("https://www.pollingreport.us/sub/", links_year))
  return(links_comp)
}

# Download and write polls for each state and year

dl_polls <-function(url_list, write_dir){
  if(!file.exists(write_dir)){dir.create(write_dir, recursive = T)}
  urls <- lapply(url_list, getURL, userpwd = usr_pwd,
                 followlocation=TRUE, curl=handle)
  i <- 1
  urls_write <- lapply(urls, function(x, i){
    i <-  get("i", parent.frame())
    i <<- i + 1
    write(x, str_c(write_dir, i, ".html"))
  }
  )
}

#-------------------------------------------------------------------------------


# Download & write --------------------------------------------------------

# get url lists for each year
{
  urllist2000 <- get_state_urls(2000)
  urllist2004 <- get_state_urls(2004)
  urllist2008 <- get_state_urls(2008)
  urllist2012 <- get_state_urls(2012)
  urllist2016 <- get_state_urls(2016)  
  urllist2020 <- get_state_urls(2020)  
}

# download & write
{
  dl_polls(urllist2000, 'year2000/html/')
  dl_polls(urllist2004, 'year2004/html/')
  dl_polls(urllist2008, 'year2008/html/')
  dl_polls(urllist2012, 'year2012/html/')
  dl_polls(urllist2016, 'year2016/html/')
  dl_polls(urllist2020, 'year2020/html/')
}

# check
length(list.files("year2000/html/")) # 48
length(list.files("year2004/html/")) # 50
length(list.files("year2008/html/")) # 31
length(list.files("year2012/html/")) # 23
length(list.files("year2016/html/")) # 12 
length(list.files("year2020/html/")) # 7 
