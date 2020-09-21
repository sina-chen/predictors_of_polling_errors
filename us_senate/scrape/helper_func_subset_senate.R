########################################################################################
# Scrape script - helper function: Senate general election polls from 1998 to 2016
# Author: Sina Chen
# Source: pollingreport.com 
#
########################################################################################

#### Helper function senate polls 2000-2018 ####

# Function used inside helper funxtion to split states
split_st <- function(list, names_states){
  
  # Define split points
  split_point <- which(sapply(list, function(x) 
    lapply(x, str_detect, pattern = names_states)) %>%  
                         lapply(Compose(function(x) lapply(x, any)==T, 
                                        any)) == T)
  
  # Split if applicable
  if(length(split_point)>1){
    states <- vector(mode = 'list', length = length(split_point))
    split_point <- c(split_point, (length(list)+1))
    for(i in 1:(length(split_point)-1)){
      states[i] <- list(list[split_point[i]:(split_point[(i + 1)] - 1)])
    } 
  } else {
    states <- list(list)
  }
  return(states)
}

# Extract senate polls from 2000 to 2018
extract_senate <- function(txt_dir){
  
  ### Get raw txt content ###
  
  # Set working directory to read files
  setwd(txt_dir)
  
  # State names
  states <- c('Alaska', 'Alabama', 'Arkansas', 'Arizona', 'California', 
              'Colorado',  'Connecticut', 'Delaware', 'Florida', 'Georgia', 
              'Hawaii', 'Iowa', 'Idaho', 'Illinois', 'Indiana', 'Kansas', 
              'Kentucky', 'Louisiana',  'Massachusetts', 'Maryland', 'Maine', 
              'Michigan', 'Minnesota',   'Missouri', 'Mississippi', 'Montana', 
              'North Carolina',  'North Dakota', 'Nebraska', 'New Hampshire', 
              'New Jersey',  'New Mexico', 'Nevada', 'New York', 'Ohio', 
              'Oklahoma', 'Oregon',  'Pennsylvania', 'Rhode Island', 
              'South Carolina', 'South Dakota',  'Tennessee', 'Texas', 'Utah',
              'Virginia', 'Vermont', 'Washington', 'Wisconsin', 'West Virginia',
              'Wyoming', 'District of Columbia') %>% 
    as.factor()
  
  # Read directory content
  txt_names <- dir(txt_dir)
  polls_raw <- lapply(txt_names, read_html) %>% 
    lapply(function(x) html_table(x, fill = T))
  
  ### Clean data ###

  # Remove erronous patterns
  polls_clean <- lapply(polls_raw, function(x) 
    lapply(x, function(y) 
      as.data.frame(lapply(y, str_remove_all, pattern = '^[.]|^Â|\n'), 
                    stringsAsFactors=FALSE)))
  
  # Set erronous patterns to NA
  polls_clean <- lapply(polls_raw, function(x) 
    lapply(x, function(y) 
      as.data.frame(lapply(y, str_replace_all, 
                           pattern = '^[.]$|^$|^Â$', 
                           replacement = NA_character_), 
                    stringsAsFactors=FALSE)))
  
  # Replace multiple spacing by single spacing
  polls_clean <- lapply(polls_clean, function(x) 
    lapply(x, function(y) 
      as.data.frame(lapply(y, str_replace_all, 
                           pattern='\\s+', replacement = ' '), 
                    stringsAsFactors=FALSE)))
  
  # Remove empty data frames from list
  polls_clean <- lapply(polls_clean, function(x) 
    lapply(x, function(y) 
      y[rowSums(is.na(y)) != ncol(y),])) %>% 
    lapply(Filter, f = NROW)
  
  ### Split multiple states & subset states with senate election ###
  
  # Define names to subset
  st_names <- paste0('^', paste(states, collapse = '$|^'),'$|^',
                     paste0(states, ' \\d+', collapse = '$|^'), '$')
  
  # Define position of state names as split point
  split_point <- lapply(polls_clean, function(x) 
    which(sapply(x, function(x) 
      lapply(x, str_detect, pattern = st_names)) %>%
        lapply(Compose( function(x) lapply(x,any) == T, any)) == T))
  
  # Subset polls containing senate related polls
  if(any(lengths(split_point) > 1) == T) { # of multiple states in one list
    st_single <- lapply(polls_clean, split_st, names_states = st_names) %>% 
      unlist(recursive = F)
    senate_pos <- suppressWarnings(str_which(st_single, 'U.S. SENATE'))
    st_senate <- st_single[senate_pos]
    senate_names  <- lapply(st_senate, function(x) 
      lapply(c(x[[1]], x[[2]], x[[3]]), str_extract, pattern = st_names)) %>% 
      unlist() %>% 
      na.omit() %>% 
      unique()
    names(st_senate) <- senate_names
  } else { # if each list refers to only one state
    st_single <- polls_clean
    senate_pos <- suppressWarnings(str_which(st_single, 'U.S. SENATE'))
    st_senate <- st_single[senate_pos]
    names(st_senate) <- lapply(txt_names[senate_pos], 
                               str_remove_all, pattern = '.txt')
  }

  ### Clean data 2 ###
  
  # Replace empty cells with NA
  senate_clean <- lapply(st_senate, function(x) 
    lapply(x, function(y) if(isTRUE(nrow(y) < 3)) {
      as.data.frame(lapply(y, gsub, pattern = '^$', replacement = NA),
                    stringsAsFactors=FALSE)
      } else y))
  
  ### Subset Senate polls within each state ###
  
  # Get position of first senate election related poll
  senate_start <- sapply(senate_clean, function(x) 
    lapply(x, function(y) str_detect(y[[1]], pattern = 'U.S. SENATE') )) %>%  
    lapply(function(x) sapply(x, any)) %>% 
    lapply(which) %>% 
    lapply(min) 
  
  # Get position of last senate election related poll
  senate_end <- sapply(senate_clean, function(x) 
    sapply(x, str_detect, 
           pattern = c('CD:|HOUSE|BALLOT|REFERENDUM|
                       AMENDMENT|SPECIAL|U[.]S[.] SENATE[:] Special Election|PROPOSED AMENDMENT'))) %>%  
    sapply(function(x) sapply(x, any)) %>% sapply(which) # Define end position
  
  senate_end <- lapply(names(senate_end), function(x) 
                         if(length(senate_end[[x]]) == 0) {
                           state_name <- names(senate_end[x])
                           x = list(length(senate_clean[[x]]) + 1)
                           setNames(x,state_name)
                         } 
                       else {
                         senate_end[x]
                         }
                       ) %>%
    unlist(recursive = F)

  senate_end <- lapply(names(senate_end), function(x) 
    min(senate_end[[x]][which(unlist(senate_end[x]) > senate_start[x])])) 
  names(senate_end) <- names(senate_start)
  
  # Subset senate polls
  senate <- lapply(names(senate_clean), function(x) 
    x = senate_clean[[x]][(senate_start[[x]] + 1):(senate_end[[x]] - 1)])
  senate <- lapply(senate, function(x) lapply(x, function(y) y[nrow(y) > 2,]))
  senate <- lapply(senate, Filter, f = NROW)
  names(senate) <- names(senate_start)
  
  return(senate)
}


#### Helper function senate polls 1998 ####

# Function used inside helper funxtion to split states
split_st1998 <- function(df, names_states){
  
  # Define split points
  split_point <- which(str_detect(df[,1], names_states))
  
  # Split if applicable
  if(length(split_point) > 1) {
    states <- vector(mode = 'list', length = length(split))
    split_point <- c(split_point, (nrow(df) + 1))
    for(i in 1:(length(split_point)-1)) {
      states[i] <- list(df[split_point[i]:(split_point[(i + 1)] - 1),])
    } 
  } else {
    states <- list(df)
  }
  return(states)
}

# Function used inside helper funxtion to split elections
split_election1998 <- function(df){
  
  # Define split points
  split_points <- which(str_detect(df[,1], pattern = 'U.S. SENATE|GOVERNOR|CD'))
  
  # Split if applicable
  if(length(split_points)>1) {
    split_points[length(split_points) + 1] <- nrow(df) + 1
    senate_start <- which(str_detect(df[,1], pattern = 'U.S. SENATE'))
    senate_end <- split_points[which(split_points == senate_start) + 1]
    df_senate <- df[(senate_start + 1):(senate_end - 1), ]
  } else {
    df_senate <- df[(split_points + 1):nrow(df),]
  }
  return(df_senate)
}

# Function used inside helper funxtion to split polls
split_polls1998 <- function(df) {
  
  # Get columns with poll info (= start poll)
  split_points <-
    which(df[,1] != '' & df[,1] != 'General Election Trial Heat:' & 
            df[,1] != 'General Election Trial Heat (among likely voters):' &
            df[,1] != 'General Election Trial Heat (among most-likely voters):') 
 
  # Split if applicable
  if(length(split_points) > 1){
    polls <- vector(mode = 'list', length = length(split_points))
    split_points[length(split_points) + 1] <- (nrow(df) + 1)
    for(i in 1:(length(split_points) - 1)) {
      polls[[i]] <- df[split_points[i]:(split_points[i + 1] - 1),]
    }
  } else {
    polls <- list(df)
  }
  return(polls)
}

# Extract senate polls 1998
extract_senate1998 <- function(txt_dir){
  
  ### Get raw txt content ###
  
  # Set working directory to read files
  setwd(txt_dir)

  # State names
  states <- c('Alaska', 'Alabama', 'Arkansas', 'Arizona', 'California', 
              'Colorado',  'Connecticut', 'Delaware', 'Florida', 'Georgia', 
              'Hawaii', 'Iowa', 'Idaho', 'Illinois', 'Indiana', 'Kansas', 
              'Kentucky', 'Louisiana',  'Massachusetts', 'Maryland', 'Maine', 
              'Michigan', 'Minnesota',   'Missouri', 'Mississippi', 'Montana', 
              'North Carolina',  'North Dakota', 'Nebraska', 'New Hampshire', 
              'New Jersey',  'New Mexico', 'Nevada', 'New York', 'Ohio', 
              'Oklahoma', 'Oregon',  'Pennsylvania', 'Rhode Island', 
              'South Carolina', 'South Dakota',  'Tennessee', 'Texas', 'Utah',
              'Virginia', 'Vermont', 'Washington', 'Wisconsin', 'West Virginia',
              'Wyoming', 'District of Columbia') %>% 
    as.factor()
  
  # Read directory content
  txt_names <- dir(txt_dir)
  polls_raw <- lapply(txt_names, read_html) %>% 
    lapply(function(x) html_table(x, fill = T))
  
  ### Clean data ###
  
  # Remove erronous patterns  
  polls_clean <- lapply(polls_raw, function(x) 
    lapply(x, function(y) 
      as.data.frame(lapply(y, str_remove_all, pattern = '^[.]|^Â|\n|Â'), 
                    stringsAsFactors = F)))
  
  # Replace multiple spacing by single spacing
  polls_clean <- lapply(polls_clean, function(x) 
    lapply(x, function(y) 
      as.data.frame(lapply(y, str_replace_all, pattern = '\\s+', 
                           replacement = ' '), stringsAsFactors = F)))
  
  # Split multiple states
  st_singel <- lapply(polls_clean, function(x) x = x[-1]) %>% 
    unlist(recursive = F)
  
  ### Split multiple states & subset states with senate election ###

  # Define names to subset
  st_names <-  paste0('^', paste(states, collapse = '$|^'),'$')

  # Split polls
  st_singel2 <- lapply(st_singel, split_st1998, names_states = st_names) %>% 
    unlist(recursive = F)
  
  # Subset states with senate polls
  st_senate <- st_singel2[lapply(st_singel2, function(x) 
    any(str_detect(x[[1]], pattern = 'U.S. SENATE'))) %>% 
      unlist() %>% 
      which()]
  
  # Set state names
  st_names2  <- lapply(st_senate, function(x) 
    lapply(x[[1]], str_extract, pattern = st_names)) %>% 
    unlist() %>% 
    na.omit() %>% 
    unique()
  names(st_senate) <- st_names2
  
  ### Subset senate polls within each state ###
  senate <- lapply(st_senate, split_election1998)
  
  ### Split multiple polls within in state data frame  ###
  senate <- lapply(senate, split_polls1998)
  
  return(senate)
}
  
