#-------------------------------------------------------------------------------
# Scrape US senate
# Author: Sina Chen
# Notes: 
#-------------------------------------------------------------------------------

setwd("~/Documents/Uni/PollingError/us/senate/code/finances")


# Libraries ---------------------------------------------------------------

library(RSelenium)
library(rvest)
library(xml2)
library(tidyverse)
library(stringr)


# Functions ---------------------------------------------------------------

source('helper_scrape_senate_exp.R')



# Set up remote driver ----------------------------------------------------

# run "docker run -d -p 4445:4444 selenium/standalone-chrome" in terminal

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()



# Total expenditures ------------------------------------------------------

{
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=1990")
  Sys.sleep(20)
  senate_exp1990 <- get_exp_senate(1990)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=1992")
  Sys.sleep(20)
  senate_exp1992 <- get_exp_senate(1992)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=1994")
  Sys.sleep(20)
  senate_exp1994 <- get_exp_senate(1994)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=1996")
  Sys.sleep(30)
  senate_exp1996 <- get_exp_senate(1996)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=1998")
  Sys.sleep(20)
  senate_exp1998 <- get_exp_senate(1998)
 
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2000")
  Sys.sleep(20)
  senate_exp2000 <- get_exp_senate(2000)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2002")
  Sys.sleep(20)
  senate_exp2002 <- get_exp_senate(2002)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2004")
  Sys.sleep(20)
  senate_exp2004 <- get_exp_senate(2004)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2006")
  Sys.sleep(20)
  senate_exp2006 <- get_exp_senate(2006)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2008")
  Sys.sleep(20)
  senate_exp2008 <- get_exp_senate(2008)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2010")
  Sys.sleep(20)
  senate_exp2010 <- get_exp_senate(2010)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2012")
  Sys.sleep(20)
  senate_exp2012 <- get_exp_senate(2012)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2014")
  Sys.sleep(20)
  senate_exp2014 <- get_exp_senate(2014)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2016")
  Sys.sleep(20)
  senate_exp2016 <- get_exp_senate(2016)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2018")
  Sys.sleep(20)
  senate_exp2018 <- get_exp_senate(2018)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2020")
  Sys.sleep(20)
  senate_exp2020 <- get_exp_senate(2020)
  
  remDr$navigate("https://www.fec.gov/data/spending-bythenumbers/?office=S&election_year=2022")
  Sys.sleep(20)
  senate_exp2022 <- get_exp_senate(2022)  
}


# aggregate in data frame
senate_exp1990_2022 <- rbind(senate_exp1990, senate_exp1992, senate_exp1994, 
                             senate_exp1996, senate_exp1998, senate_exp2000,
                             senate_exp2002, senate_exp2004, senate_exp2006,
                             senate_exp2008, senate_exp2010, senate_exp2012,
                             senate_exp2014, senate_exp2016, senate_exp2018,
                             senate_exp2020, senate_exp2022) %>%
  mutate(exp = as.numeric(str_remove_all(exp, ',')),
         party = str_remove_all(party, " ")) %>%
  subset(exp > 0 & party %in% c("REP", "DEM"))

rm(senate_exp1990, senate_exp1992, senate_exp1994, senate_exp1996, 
   senate_exp1998, senate_exp2000, senate_exp2002, senate_exp2004, 
   senate_exp2006, senate_exp2008, senate_exp2010, senate_exp2012,
   senate_exp2014, senate_exp2016, senate_exp2018, senate_exp2020,
   senate_exp2022)

# save exp
saveRDS(senate_exp1990_2022, "~/Documents/Uni/PollingError/us/senate/data/covariates/finances/senate_exp1990_2022.RDS")



# Total receipts ----------------------------------------------------------

{
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=1990")
  Sys.sleep(20)
  senate_raising1990 <- get_raising_senate(1990)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=1992")
  Sys.sleep(20)
  senate_raising1992 <- get_raising_senate(1992)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=1994")
  Sys.sleep(20)
  senate_raising1994 <- get_raising_senate(1994)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=1996")
  Sys.sleep(20)
  senate_raising1996 <- get_raising_senate(1996)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=1998")
  Sys.sleep(20)
  senate_raising1998 <- get_raising_senate(1998)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2000")
  Sys.sleep(20)
  senate_raising2000 <- get_raising_senate(2000)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2002")
  Sys.sleep(20)
  senate_raising2002 <- get_raising_senate(2002)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2004")
  Sys.sleep(20)
  senate_raising2004 <- get_raising_senate(2004)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2006")
  Sys.sleep(20)
  senate_raising2006 <- get_raising_senate(2006)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2008")
  Sys.sleep(20)
  senate_raising2008 <- get_raising_senate(2008)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2010")
  Sys.sleep(20)
  senate_raising2010 <- get_raising_senate(2010)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2012")
  Sys.sleep(20)
  senate_raising2012 <- get_raising_senate(2012)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2014")
  Sys.sleep(20)
  senate_raising2014 <- get_raising_senate(2014)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2016")
  Sys.sleep(20)
  senate_raising2016 <- get_raising_senate(2016)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2018")
  Sys.sleep(20)
  senate_raising2018 <- get_raising_senate(2018)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2020")
  Sys.sleep(20)
  senate_raising2020 <- get_raising_senate(2020)
  
  remDr$navigate("https://www.fec.gov/data/raising-bythenumbers/?election_year=2022")
  Sys.sleep(20)
  senate_raising2022 <- get_raising_senate(2022)  
}


remDr$close()


senate_raising1990_2022 <- rbind(senate_raising1990, senate_raising1992, 
                                 senate_raising1994, senate_raising1996, 
                                 senate_raising1998, senate_raising2000,
                                 senate_raising2002, senate_raising2004, 
                                 senate_raising2006, senate_raising2008, 
                                 senate_raising2010, senate_raising2012,
                                 senate_raising2014, senate_raising2016, 
                                 senate_raising2018, senate_raising2020,
                                 senate_raising2022)  %>% 
  mutate(raising = as.numeric(str_remove_all(raising, ',')),
         party = str_remove_all(party, " ")) %>%
  subset(raising > 0 & party %in% c("REP", "DEM"))



rm(senate_raising1990, senate_raising1992, senate_raising1994, senate_raising1996, 
   senate_raising1998, senate_raising2000, senate_raising2002, senate_raising2004, 
   senate_raising2006, senate_raising2008, senate_raising2010, senate_raising2012,
   senate_raising2014, senate_raising2016, senate_raising2018, senate_raising2020,
   senate_raising2022)

# save raising

saveRDS(senate_raising1990_2022, "~/Documents/Uni/PollingError/us/senate/data/covariates/finances/senate_raising1990_2022.RDS")




