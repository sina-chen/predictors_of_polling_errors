########################################################################################
# Download script: Senat Polls  from 1998 to 2018 
# Author: Sina Chen
# Source: www.pollingreport.com 
#
########################################################################################

#### Libraries ####

library(xml2)
library(rvest)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(functional)
library(RCurl)
library(XML)
library(stringr)
library(stringi)
library(pryr)
library(tidyverse)


#### Directory ####
setwd('your working directory')


#### Preparation ####

# debug gatherer
debuginfo <- debugGatherer()

# curl handle + identification
usr_pwd <- read_file('usr_pwd.txt')

handle <- getCurlHandle(cookiejar="",
                        followlocation = TRUE,
                        autoreferer = TRUE,
                        debugfunc = debuginfo$update,
                        verbose = TRUE,
                        httpheader=list(
                          from = "your mai address",
                          'user-agent' = str_c(R.version$version.string,
                                               ", ",R.version$platform)
                        ) )



#### Helper functions ####

# Get list of states by year

get_state_urls <- function(year){
  if(year != 1998){
    url <- paste0('http://www.pollingreport.us/sub/',year,'.htm')
    url <- getURL(url, userpwd = usr_pwd, followlocation=TRUE, curl=handle)
    url_parsed <- htmlParse(url)
    links <- xpathSApply(url_parsed,path = "//a",xmlGetAttr,"href")
    links_year <- links[which(grepl(paste0('[[:alpha:]]+',year),links) |
                                grepl(paste0(year, '[[:alpha:]]+'),links) & grepl('#',links)==F)]
    links_comp <- as.list(stri_c("http://www.pollingreport.us/sub/",links_year))
    return(links_comp)
  } else {
    url <- paste0('http://www.pollingreport.us/sub/','Campaign_Update','.htm')
    url <- getURL(url, userpwd = usr_pwd, followlocation=TRUE, curl=handle)
    url_parsed <- htmlParse(url)
    links <- xpathSApply(url_parsed,path = "//a",xmlGetAttr,"href")
    links_year <- links[which(unlist(lapply(links,function(x) str_detect(x,'\\d+|index|issues|Harris|pollingreport', negate = T))))]
    links_comp <- as.list(stri_c("http://www.pollingreport.us/sub/",links_year))
    return(links_comp)
    
  }
  
}


# Dowload and write polls for each state and year

dl_polls <-function(url_list,write_dir){
  dir.create(write_dir, recursive = T)
  urls <- lapply(url_list,getURL,userpwd = usr_pwd,followlocation=TRUE,curl=handle)
  i <- 1
  urls_write <- lapply(urls, function(x,i){
    i <-  get("i", parent.frame())
    i <<- i + 1
    write(x, str_c(write_dir, i, ".html"))
  }
  )
}


#### Download & write ####

# Get url lists for each year

{
  urllist1998 <- get_state_urls(1998)
  urllist2000 <- get_state_urls(2000)
  urllist2002 <- get_state_urls(2002)
  urllist2004 <- get_state_urls(2004)
  urllist2006 <- get_state_urls(2006)
  urllist2006 <- c(list('http://www.pollingreport.us/sub/2006a.htm'),urllist2006) # 2006a is stored differentely
  urllist2008 <- get_state_urls(2008)
  urllist2010 <- get_state_urls(2010)
  urllist2012 <- get_state_urls(2012)
  urllist2014 <- get_state_urls(2014)
  urllist2016 <- get_state_urls(2016)  
  urllist2018 <- get_state_urls(2018)
}

 
# Download & write 

{
  dl_polls(urllist1998,'year1998/html/')
  dl_polls(urllist2000,'year2000/html/')
  dl_polls(urllist2002,'year2002/html/')
  dl_polls(urllist2004,'year2004/html/')
  dl_polls(urllist2006,'year2006/html/')
  dl_polls(urllist2008,'year2008/html/')
  dl_polls(urllist2010,'year2010/html/')
  dl_polls(urllist2012,'year2012/html/')
  dl_polls(urllist2014,'year2014/html/')
  dl_polls(urllist2016,'year2016/html/') 
  dl_polls(urllist2018,'year2018/html/') 
}


# Making sure all states are downloaded

length(list.files("year1998/html/")) # 6
length(list.files("year2000/html/")) # 48
length(list.files("year2002/html/")) # 47
length(list.files("year2004/html/")) # 50
length(list.files("year2006/html/")) # 27
length(list.files("year2008/html/")) # 30
length(list.files("year2010/html/")) # 23
length(list.files("year2012/html/")) # 23
length(list.files("year2014/html/")) # 14
length(list.files("year2016/html/")) # 12 
length(list.files("year2018/html/")) # 5  


