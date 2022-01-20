# ---------------------------------------------------------------------------- #
# Get senate expenditures by purpose
# Author: Sina Chen
# Source: https://api.open.fec.gov/developers/#/disbursements/get_schedules_schedule_b_by_purpose_
# Notes: 
# ---------------------------------------------------------------------------- #

#### Libraries ####

library(dplyr)
library(readr)
library(jsonlite)
library(tidyverse)
library(httr)


#### Functions ####

source("open_fec.R")
source("open_fec_adv.R")


#### Data ####

api_key <- read_file('api-key.txt')


# ---------------------------------------------------------------------------- #

#### Get relevant senate candidates and committees ####

# get senate candidates 
query <- list(page=1, office = 'S')
candidates <- get_openFEC_data(path = '/candidates/search/', 
                               query = query, key = api_key)

# get principal campaign committee(s) for each candidate (receipts are only accessed if PCC is reported)
cand_cmte <- data.frame()

for(i in 1:nrow(candidates)){
  cmte <- candidates[i,'principal_committees'][[1]] 
  
  if(length(cmte) > 0){
    cmte <- subset(cmte, select = c('candidate_ids', 'committee_id', 'cycles'))
    cand_cmte <- rbind(cand_cmte, cmte)
  }
}


# ---------------------------------------------------------------------------- #

#### Get advertisment data ####

# API restricted to 1000 calls per hour --> split requests up 

cand_cmte1 <- cand_cmte[1:999,]
# get all receipts for each committee id
totals1 <- list()
for (id in cand_cmte1$committee_id){
  cmte_path <- paste0('/schedules/schedule_b/by_purpose/?sort_hide_null=false&purpose=ADVERTISING&sort_null_only=false&sort_nulls_last=false&page=1&per_page=100&committee_id=', id)
  data <- get_openFEC_data_adv(path = cmte_path, key = api_key)
  totals1 <- bind_rows(totals1, data)
}

cand_cmte2 <- cand_cmte[1000:1999,]
# get all receipts for each committee id
totals2 <- list()
for (id in cand_cmte2$committee_id){
  cmte_path <- paste0('/schedules/schedule_b/by_purpose/?sort_hide_null=false&purpose=ADVERTISING&sort_null_only=false&sort_nulls_last=false&page=1&per_page=100&committee_id=', id)
  data <- get_openFEC_data_receipts(path = cmte_path, key = api_key)
  totals2 <- bind_rows(totals2, data)
}

cand_cmte3 <- cand_cmte[2000:2999,]
# get all receipts for each committee id
totals3 <- list()
for (id in cand_cmte3$committee_id){
  cmte_path <- paste0('/schedules/schedule_b/by_purpose/?sort_hide_null=false&purpose=ADVERTISING&sort_null_only=false&sort_nulls_last=false&page=1&per_page=100&committee_id=', id)
  data <- get_openFEC_data_receipts(path = cmte_path, key = api_key)
  totals3 <- bind_rows(totals3, data)
}

cand_cmte4 <- cand_cmte[3000:3999,]
# get all receipts for each committee id
totals4 <- list()
for (id in cand_cmte4$committee_id){
  cmte_path <- paste0('/schedules/schedule_b/by_purpose/?sort_hide_null=false&purpose=ADVERTISING&sort_null_only=false&sort_nulls_last=false&page=1&per_page=100&committee_id=', id)
  data <- get_openFEC_data_receipts(path = cmte_path, key = api_key)
  totals4 <- bind_rows(totals4, data)
}

cand_cmte5 <- cand_cmte[4000:4746,]
# get all receipts for each committee id
totals5 <- list()
for (id in cand_cmte5$committee_id){
  cmte_path <- paste0('/schedules/schedule_b/by_purpose/?sort_hide_null=false&purpose=ADVERTISING&sort_null_only=false&sort_nulls_last=false&page=1&per_page=100&committee_id=', id)
  data <- get_openFEC_data_receipts(path = cmte_path, key = api_key)
  totals5 <- bind_rows(totals5, data)
}


totals <- rbind(totals1, totals2, totals3, totals4, totals5)

#saveRDS(totals, 'totals_senate_adv.RDS')

