# point to your specific path
load("~/Nextcloud/PollingError/Data/senate/1976-2018-senate.RData")

# assign results
senate_results <- x[,c('year', 'state_po', 'party', 'candidatevotes', 'totalvotes','special')] %>% 
  subset(str_detect(party,'republican') == T & year >= 1998 |
           str_detect(party,'democrat') == T & year >= 1998) 

senate_results$party <-  unlist(str_extract_all(senate_results$party,
                                                'republican|democrat'), 
                                recursive = F)


senate_results %>% 
  filter(special == FALSE) %>% 
  select(-special) %>% 
  group_by(year, state_po) %>% 
  slice_max(candidatevotes, n = 2) %>% 
  mutate(res_share = candidatevotes/totalvotes) -> senate_results

senate_results %>% 
  ungroup() %>% 
  pivot_wider(names_from = party, values_from = res_share) -> reshaped_results

reshaped_results %>% 
  filter(!is.na(republican)) %>% 
  select(year, state_po, republican) -> rep_results

reshaped_results %>% 
  filter(!is.na(democrat)) %>% 
  select(year, state_po, democrat) %>% 
  left_join(rep_results) %>% 
  rename(rep_result  = 'republican',
         dem_result = 'democrat',
         election_year  = 'year',
         state = 'state_po') %>%
  mutate(rep_result2 = rep_result/(rep_result + dem_result),
         dem_result2 = dem_result/(rep_result + dem_result)) -> senate_results_final


write_csv(senate_results_final, "senate_results.csv")



          