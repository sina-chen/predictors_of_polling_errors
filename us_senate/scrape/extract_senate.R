########################################################################################
# Scrape script: Senate general election olls from 1998 to 2016
# Author: Sina Chen
# Source: www.pollingreport.com 
#
########################################################################################

#### Libraries ####

library(stringr)
library(xml2)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(functional)
library(rvest)
library(openintro)

#### Directory ####

senate_wd <- 'your working directory'
setwd(senate_wd)

source('helper_func_subset_senate.R')
source('helper_func_senate.R')

#### Data ####
{
  polls_raw1998 <- extract_senate1998(paste0(senate_wd,'/year1998/txt'))
  polls_raw2000 <- extract_senate(paste0(senate_wd,'/year2000/txt'))
  polls_raw2002 <- extract_senate(paste0(senate_wd,'/year2002/txt'))
  polls_raw2004 <- extract_senate(paste0(senate_wd,'/year2004/txt'))
  polls_raw2006 <- extract_senate(paste0(senate_wd,'/year2006/txt'))
  polls_raw2008 <- extract_senate(paste0(senate_wd,'/year2008/txt'))
  polls_raw2010 <- extract_senate(paste0(senate_wd,'/year2010/txt'))
  polls_raw2012 <- extract_senate(paste0(senate_wd,'/year2012/txt'))
  polls_raw2014 <- extract_senate(paste0(senate_wd,'/year2014/txt'))
  polls_raw2016 <- extract_senate(paste0(senate_wd,'/year2016/txt'))
  polls_raw2018 <- extract_senate(paste0(senate_wd,'/year2018/txt'))
}



#### 1998 ####
polls_senate1998 <- polls_raw1998 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Fong', demC = '[()]D[)]|Boxer', 
                       year = 1998)

#### 2000 ####
polls_senate2000 <- polls_raw2000 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Lazio', demC = '[()]D[)]|Clinton', 
                       year = 2000)

#### 2002 ####
polls_senate2002 <- polls_raw2002 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]', demC = '[()]D[)]', year = 2002)

#### 2004 ####
polls_senate2004 <- polls_raw2004 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|^Orchulli$|Repub- lican|Keyes|Scott|Grassely|Grassley|Bunning|Bond|Ziser|Mills|Voinovich|Thune|Nethercutt|Michels|Republican|Pipkin',
                       demC = '[()]D[)]|^Dodd$|Obama|Bayh|Small|Mongiardo|Farmer|Reid|Schumer|Fingerhut|Daschle|Murray|Feingold|Mikulski',
                       year = 2004)

#### 2006 ####
polls_senate2006 <- polls_raw2006 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Republican|Lugar|Dubie|Rainville|
                       Capito', 
                       demC = '[()]D[)]|Democratic|Roemer|Byrd', 
                       indC = '[(]I[)]|Independent|Ind.|Sanders',
                       year = 2006)

#### 2008 ####
polls_senate2008 <- polls_raw2008 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Republican|Cornyn', 
                       demC = '[()]D[)]|Democrat|Lautenberg', 
                       year = 2008)

#### 2010 ####
polls_senate2010 <- polls_raw2010 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Republican|Voinovich', 
                       demC = '[()]D[)]|Democrat|Murray', 
                       writeC = 'Murkowski|write',
                       year = 2010)

#### 2012 ####
polls_senate2012 <- polls_raw2012 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Republican', 
                       demC = '[()]D[)]|Democrat|Menendez|Brown|Casey', 
                       year = 2012)

#### 2014 ####
polls_senate2014 <- polls_raw2014 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Republican', 
                       demC = '[()]D[)]', 
                       year = 2014)

#### 2016 ####
polls_senate2016 <- polls_raw2016 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]|Republican|Kennedy|Fleming|Boustany|
                       Angelle|Maness|Young', 
                       demC = '[()]D[)]|Landrieu|Campbell|Fayard', 
                       year = 2016)

#### 2018 ####
polls_senate2018 <- polls_raw2018 %>% 
  clean_split() %>% 
  lapply( rm_nonSenate) %>%
  convert_to_dataframe(repC = '[()]R[)]', 
                       demC = '[()]D[)]', 
                       year = 2018)



#### All senate polls ####

polls_senate1998_2018 <- rbind(polls_senate1998, polls_senate2000,
                                 polls_senate2002, polls_senate2004, 
                                 polls_senate2006, polls_senate2008,
                                 polls_senate2010, polls_senate2012,
                                 polls_senate2014, polls_senate2016,
                               polls_senate2018)

# Save polls
saveRDS(polls_senate1998_2018, paste0(senate_wd,"/polls_senate1998_2018.RDS"))

