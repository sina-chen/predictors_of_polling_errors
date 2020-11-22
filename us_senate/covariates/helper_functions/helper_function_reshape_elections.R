# this function needs a dataframe from a list and the according election year
# it reshapes the data in a manner that each candidate gets assigned to the 
# according political party and the right state is associated with all candidates


reshape_states <- function(list_element, election_year){
  
  paste("X", election_year, "_senate.csv", sep = "") -> temporal_name
  
  list_element %>% 
    group_by(frame) %>% 
    slice_head(n = 3) %>% 
    pivot_wider(c(frame, no_candidates), names_from = party, values_from = name) %>% 
    ungroup() %>% 
    mutate(state_number = as.numeric(frame)) %>% 
    select(-frame) %>% 
    arrange(state_number) %>% 
    add_column(election_year = election_year) %>% 
    cbind(State = list_of_states[[temporal_name]]) %>% 
    cbind(Senator = list_of_senators[[temporal_name]]) %>% 
    select(-state_number) -> reshaped_election
  
  return(reshaped_election)
  
}

# this functions behaves similar to the one above and takes care of special elections

reshape_states_special <- function(list_element, election_year){
  
  paste("X", election_year, "_senate_special.csv", sep = "") -> temporal_name
  
  list_element %>% 
    group_by(frame) %>% 
    slice_head(n = 3) %>% 
    pivot_wider(c(frame, no_candidates), names_from = party, values_from = name) %>% 
    ungroup() %>% 
    mutate(state_number = as.numeric(frame)) %>% 
    select(-frame) %>% 
    arrange(state_number) %>% 
    add_column(election_year = election_year) %>% 
    cbind(State = list_of_states[[temporal_name]]) %>% 
    cbind(Senator = list_of_senators[[temporal_name]]) %>% 
    select(-state_number) -> reshaped_election
  
  return(reshaped_election)
  
}
