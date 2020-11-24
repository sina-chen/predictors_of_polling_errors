# this function performs for each data frame in a list a transformation of the 
# "candidate" column and returns a list of dataframes with a variable to identify
# the state of the candidates (frame), a name, party and percentage column for every
# candidate and another variabel which indicates the number of candidates competing
# in the senate election

extract_candidate_info <- function(list_element){
  list_element %>% 
    map(function(x) tibble(dta = x)) %>%
    map(function(x) separate(x, col=dta, sep="[()]", into = c("name","party", "pct")) %>%
          mutate(name = trimws(name),
                 party = trimws(party))) %>%
    bind_rows(.id = "frame") %>% 
    drop_na() %>% 
    add_count(frame, name = "no_candidates") -> result
  
  return(result)
}