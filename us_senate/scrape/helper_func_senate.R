########################################################################################
# Helper functions to extract senate polls 1998 - 2018
# Author: Sina Chen
# Notes: 
#
########################################################################################

### Cleaning & Splitting ###

# Clean
clean <- function(dt){
  dt<- as.data.frame(lapply(dt, gsub, pattern = '\n|\\s+|:', replacement = ' '), 
                     stringsAsFactors=FALSE)
  dt<- as.data.frame(lapply(dt, gsub, pattern = '^[.]|Â|[?]-|Ã\u0082|Ã|¢', 
                            replacement=''), stringsAsFactors = FALSE)
  dt[is.na(dt)] <- ''
  
  if( grepl('General Election Trial Heat', dt[3,1]) == T & 
      grepl('General Election Trial Heat',dt[4,1]) == T) {
    dt <- dt[-3,]
  }
  
  # remove . after abbreviations
  dt[1,1] <- str_replace(dt[1,1], 'St[.]', 'St')
  dt[1,1] <- str_replace(dt[1,1], 'H[.]', 'H')
  dt[1,1] <- str_replace(dt[1,1], 'S. Back Corp.', 'S Back Corp')
  dt[1,1] <- str_replace(dt[1,1], 'R[.]L[.]', 'R L ')

  return(dt)
}


# Split several polls in one data frame
split_polls <- function(dt){
  
  # split based on first column enries
  split <- which(dt[,1] != '')
  if(isTRUE(split[2] == 2 & split[3] == 3)) {
    split <- split[-3]
  }
  split[length(split) + 1] <- nrow(dt) + 1
  
  if(length(split) > 3) {
    
    head_tbl <- dt[split[1]:(split[2] - 1),] # Head with meta data 
    list_dt <- vector(mode = 'list', length = length(split) - 2)
    
    # split
    for(index_split in 2:(length(split) - 1)) {
      
      if(dt[(split[index_split+1] - 1), 3] != '%') {
        split_data <- dt[split[index_split]:(split[index_split + 1] - 1),]
        split_data <- rbind(head_tbl, split_data)
        list_dt[index_split - 1] <- list(split_data)
      } else {
        split_data <- dt[split[index_split]:(split[index_split + 1] - 3),]
        split_data <- rbind(head_tbl, split_data)
        list_dt[index_split - 1] <- list(split_data)
      }
    }
  } else { # do not split if not applicable 
    list_dt <- list(dt)
  } 
  return(list_dt)
}

split_voter <- function(dt){
  
  # split based on voter type
  dt[,c(2, 3)][is.na(dt[,c(2, 3)])] <- ''
  
  if(identical(dt[,2], dt[,3]) == T){
    dt <- dt[,-2]
  }
  
  split <- which(grepl('Among registered voters', dt[-c(1:5),2]) |
                    grepl('All registered voters',dt[-c(1:5),2])|
                    grepl('Among active voters',dt[-c(1:5),2])|
                    grepl('Among White Voters Only',dt[-c(1:5),2])|
                    grepl('Without leaners',dt[-c(1:5),2])) + 5

  split <- c(6, split, nrow(dt) + 1)
  
  if(any(grepl('Among registered voters',dt[-c(1:5),2])) == T | 
     any(grepl('Likely voters',dt[-c(1:5),2])) == T) {
    split <- 0
  }

  split_ALL <-  which(grepl('ALL$', dt[-c(1:5),2])) + 5

  if(length(split) > 2) {
    head_tbl <- dt[1:5,] # Head with meta data for every data frame
    list_dt <- vector(mode = 'list', length = length(split) - 1)
    
    # split
    for(i in 1:(length(split) - 1)) {
      split_data <- dt[(split[i]):(split[i + 1] - 1),]
      split_data <- rbind(head_tbl, split_data)
      
      if(grepl('Among registered voters', split_data[6,2]) == T) {
        split_data[1,1] <- paste0(split_data[1,1], split_data[6,2])
        split_data <- split_data[-6,]
      }
      
      if(any(grepl('Among White Voters Only',split_data[,2])) == T) {
        split_data[1,1] <- paste(split_data[1,1], 'Among White Voters Only')
      }
      
      if(any(grepl('Without leaners', split_data[,2])) == T) {
        split_data[1,1] <- paste(split_data[1,1], 'Without leaners')
      }
      list_dt[i] <- list(split_data)
    }
    
  } else if (length(split_ALL) > 1) {
    head_ALL <- dt[1:4,] # Head with meta data for every data frame
    list_dt <- vector(mode = 'list', length = length(split_ALL))
    
    split_ALL[length(split_ALL) + 1] <- nrow(dt)  + 2
    
    for(i in 1:(length(split_ALL) - 1)) {
      split_dataALL <- rbind(head_ALL,dt[split_ALL[i]:(split_ALL[i + 1] - 2),])
      list_dt[i] <- list(split_dataALL)
    }
    
  } else {
    list_dt <- list(dt)
  }  
  
  return(list_dt)
}

split_pct <- function(dt){
  
  # split based on %
  
  split <- which(grepl('%', dt[-c(1:3),3])) + 3
  split[length(split) + 1] <- nrow(dt) + 2
  
  if(length(split) > 2) {
    head_tbl <- dt[1:3,] # Head with meta data for every data frame
    list_dt <- vector(mode = 'list', length = length(split) - 1)
    
    # split
    for(i in 1:(length(split) - 1)) {
      split_data <- dt[(split[i] - 1):(split[i + 1] - 2),]
      split_data <- rbind(head_tbl, split_data)
      
      list_dt[i] <- list(split_data)
    }
  } else {
    list_dt <- list(dt)
  }  
  return(list_dt)
}


split_cand <- function(dt){
  
  # split based Democrat
  
  splitDem <- which(grepl('[(]D[)]', dt[-c(1:4),2])) + 4
  splitDem[length(splitDem) + 1] <- nrow(dt) + 1
  
  splitRep <- which(grepl('[(]R[)]', dt[-c(1:4),2])) + 4
  splitRep[length(splitRep)+1] <- nrow(dt) + 1
  
  split_und <-  which(grepl('Undecided', dt[-c(1:2),2])) + 2
  split_und <- c((which(dt[,1] != '')[2]), split_und)
  
  if(length(splitDem) > 2 & length(splitRep) < 3) {
    head_tbl <- dt[1:4,] # Head with meta data for every data frame
    list_split <- vector(mode = 'list', length = length(splitDem) - 1)
    
    # split
    for(index_split4 in 1:(length(splitDem)-1)) {
      split_data4 <- dt[(splitDem[index_split4]):(splitDem[index_split4 + 1] - 1),]
      split_data4 <- rbind(head_tbl,split_data4)
      
      list_split[index_split4] <- list(split_data4)
    }
  } else if(length(split_und) > 2) {
    head_tbl <- dt[1:3,]
    list_split <- vector(mode = 'list', length = length(split_und) - 1)
    
    # split
    for(i in 1:(length(split_und) - 1)) {
      split_data <- dt[(split_und[i] + 1):(split_und[i + 1]),]
      split_data <- rbind(head_tbl,split_data)
      list_split[i] <- list(split_data)
    }
      
  } else {
    list_split <- list(dt)
  }  
  
  return(list_split)
}

# Remove subgroups of respondents
rm_resp <- function(dt){
  
  pos_pct <- which(grepl('%', dt[-c(1,2),3])) + 2
  
  # remove polls among subgroups
  rm_row <-which(grepl('Los Angeles County',dt[-c(1:4),2]) == T | # 1998
                         grepl('Other Southern California',dt[-c(1:4),2]) == T |
                         grepl('Central Valley',dt[-c(1:4),2]) == T |
                         grepl('San Francisco Bay Area',dt[-c(1:4),2]) == T |
                         grepl('Conservative',dt[-c(1:4),2]) == T |
                         grepl('Middle-of-the-road',dt[-c(1:4),2]) == T |
                         grepl('Liberal',dt[-c(1:4),2]) == T |
                         grepl('Non-white',dt[-c(1:4),2]) == T |
                         grepl('18-29 years',dt[-c(1:4),2]) == T |
                         grepl('30-39',dt[-c(1:4),2]) == T |
                         grepl('40-49',dt[-c(1:4),2]) == T |
                         grepl('50-59',dt[-c(1:4),2]) == T |
                         grepl('60 & older',dt[-c(1:4),2]) == T |
                         grepl('Republicans',dt[-c(1:4),2]) == T | #2000
                         grepl('Democrats',dt[-c(1:4),2]) == T |
                         grepl('Independents',dt[-c(1:4),2]) == T |
                         grepl('Independent',dt[-c(1:4),2]) == T |
                         grepl('None/Minor',dt[-c(1:4),2]) == T |
                         grepl('Men',dt[-c(1:4),2]) == T |
                         grepl('Women',dt[-c(1:4),2]) == T |
                         grepl('No party',dt[-c(1:4),2]) == T |
                         grepl('Nonpartisan/Other',dt[-c(1:4),2]) == T|
                         grepl('Trend',dt[-c(1:4),2]) == T |
                         grepl('Jersey',dt[-c(1:4),2]) == T |
                         grepl('Whites',dt[-c(1:4),2]) == T |
                         grepl('Blacks',dt[-c(1:4),2]) == T |
                         grepl('active voters',dt[-c(1:4),2]) == T &  
                   grepl('active voters statewide',dt[1,1]) == F |
                         grepl('turnout scenarios ',dt[-c(1:4),2]) == T | # 2002
                         grepl('black',dt[-c(1:4),2]) == T & 
                   grepl('ALL',dt[-c(1:4),2]) ==F  |
                         grepl('White',dt[-c(1:4),2]) == T |
                         grepl('Hispanics',dt[-c(1:4),2]) == T |
                         grepl('Anglos',dt[-c(1:4),2]) == T |
                         grepl('High-propensity voters',dt[-c(1:4),2]) == T |
                         grepl('voters',dt[-c(1:4),2]) == T & 
                   grepl('voters',dt[-c(1:4),3]) == T |
                         grepl('African-Americans',dt[-c(1:4),2]) == T |#2006
                         grepl('Virginia',dt[-c(1:4),2]) == T |#2006
                         grepl('Unaffiliated',dt[-c(1:4),2]) == T |#2006
                         grepl('Most likely to vote',dt[-c(1:4),2]) == T |#2006
                         grepl('Interest',dt[-c(1:4),2]) == T |#2008
                         grepl('Extremely',dt[-c(1:4),2]) == T |#2010
                         grepl('Very',dt[-c(1:4),2]) == T |#2010
                         grepl('Somewhat',dt[-c(1:4),2]) == T |#2010
                         grepl('Unenrolled',dt[-c(1:4),2]) == T |#2010
                         grepl('Undeclared',dt[-c(1:4),2]) == T |#2010
                         grepl('Others',dt[-c(1:4),2]) == T|#2010
                         grepl('Democratic surge',dt[-c(1:4),2]) == T |#2010
                         grepl('Low turnout',dt[-c(1:4),2]) == T |#2010
                         grepl('Most-likely voters',dt[-c(1:4),2]) == T) + 4 #2010
  
  
  
  if(length(rm_row) > 0){
    dt <- dt[-c(rm_row),]
  }
  rm_row2 <- which(grepl('Undecideds allocated',dt[-c(1:3),2]) == T) + 3 
  
  if(length(rm_row2) > 0) {
    dt <- dt[-c(rm_row2),]
  }
  rm_col <- which(grepl('Men ', dt[pos_pct-1,]) == T |
                          grepl('Women', dt[pos_pct-1,]) == T |
                          grepl('Likely Voters', dt[pos_pct-1,]) == T |
                          grepl('With Leaners', dt[pos_pct-1,]) == T |
                          grepl('NYC',dt[pos_pct-1,]) == T |
                          grepl('Suburbs', dt[pos_pct-1,]) == T |
                          grepl('Upstate', dt[pos_pct-1,]) == T |
                          grepl('Whites', dt[pos_pct-1,]) == T |
                          grepl('Blacks', dt[pos_pct-1,]) == T |
                          grepl('Northern', dt[pos_pct-1,]) == T |
                          grepl('Blacks', dt[pos_pct-1,]) == T | # 2002
                          grepl('White ', dt[pos_pct-1,]) == T & 
                    grepl('Bill', dt[pos_pct-1,]) == F | # 2002
                          grepl('Chicago', dt[pos_pct-1,]) == T | # 2002
                          grepl('Collar Counties', dt[pos_pct-1,]) == T | # 2002
                          grepl('Down- state', dt[pos_pct-1,]) == T | # 2002
                          grepl('Chronic', dt[pos_pct-1,]) == T | # 2002
                          grepl('Men$', dt[pos_pct-1,]) == T
  )
  
  if(length(rm_col)>0) {
    dt <- dt[,-c(rm_col)]
  }
  
  return(dt)
}

# Combine C
clean_split <- function(list_states){
  states_split <- lapply(list_states, function(x) 
    lapply(x, Compose(clean, split_polls))) %>%
    lapply(unlist, recursive = F)
  states_voter <- lapply(states_split, function(x) 
    lapply(x, split_voter)) %>%
    lapply(unlist, recursive = F)  
  states_pct <- lapply(states_voter, function(x) 
    lapply(x, split_pct)) %>% lapply(unlist, recursive = F)
  states_cand <- lapply(states_pct, function(x) 
    lapply(x, split_cand)) %>%
    lapply(unlist, recursive = F)
  
  states_rm <- lapply(states_cand, function(x) lapply(x, rm_resp))
  return(states_rm)
}



### Remove non Sneate general elecion trial heat polls

rm_nonSenate <- function(list_states){
  
  rm_id1 <- which(lapply(list_states, function(x) any(str_detect(as.matrix(x), pattern = 'Favorability|Job|job|Favor- able|positive|replace &! race to replace him|like to see someone else|Someone new deserves a change|your opinion of|concerned|needs|experience|approve|deserves|Probably|Time |prefer a change|White Voters Only|thermometer|tax| Primary |primary|Favorable|elected senator|or would you rather see someone else|performance|Vote-To-Reelect|favorable|Kerry were elected president|best Republican candidate|appoint someone|best Democratic candidate|oppose|First choice|Against|Yes|Agree|nominate someone else|policies|[(]D[)] &![(]R[)]|[(]R[)] &! [(]D[)]|Likely|Too|legislative initiatives|chance|direction|run for president|appropriate|fair|appeal|lawsuit|someone new|would you like to personally see|age|satisfied|a number of Democratic candidates|health care|in touch|honest|should|run for &! reelection|inclined|favorably|consider|Without leaners|Less likely|vacant seat|Preference for Republican Senate Nominee|election to choose the Republican candidate|approve|Republican Runoff Trial Heat|good thing|opponent|Consider|deserve|replace|Depends who runs|special election|Preference for Republican Nominee|remain governor|political beliefs|a Democrat were to win|not at all likely|Vote to reelect Schumer|aware|involved|worse|guilty|closely|bribery|prosecutors|Would vote to reelect|Have heard of|not seek reelection|extreme'))) == T)
  
  rm_id2 <- sapply(list_states, function(x) 
    all(any(str_detect(as.matrix(x), "[(]D[)]")) &!
          any(str_detect(as.matrix(x),"[(]R[)]"))) ) %>% 
    which()
  
  rm_id <- c(rm_id1, rm_id2)
  
  if(length(rm_id)>0){
    list_states <- list_states[-rm_id]
  } 
  return(list_states)
}



#### Poll details ####

poll_details <- function(dt){
  
  details <- paste(dt[1,1], dt[2,1], dt[3,1])
  details_add <- which(grepl('N=|MoE', dt[-c(1:3),2])) + 3
  
  if(length(details_add) > 0) {
    details <- paste(details, dt[details_add,2])
  }
  
  # Sample Size
  n  <-  parse_number(unlist(str_replace(
    unlist(str_extract(details,'N+[=]+\\d+ |N+[=]+\\d+[,]+\\d+|N+[=]approx[.]\\s+\\d+[,]\\d+|N+[=]approx[.]\\s+\\d+|N[=]\\d+')),',|[.]','')))
  if(isTRUE(is.na(n))|length(n) == 0){
    n <- '-'
  } 
  
  # Margin of Error
  MoE <- parse_number(str_replace(str_extract(details,'MoE [±] +\\d+[.]\\d+|MoE [±] +\\d+|MoE [+][/][-] \\d+[.]\\d+|MoE [+][/][-] \\d+|MoE [±] approx. \\d+[.]\\d+|argin of error [±] +\\d+[.]\\d+|argin of error [±] +\\d+[.]|error is [±] +\\d+|error \\d+[.]\\d+|error [±] \\d+[)]'),'[[:alpha:]][.]|[-]',''))
  if(isTRUE(is.na(MoE))){
    MoE <- '-'
  } 
  
  # Respondents
  if(grepl('Among registered', details) == T|
     grepl('among registered', details) == T) {
    resp <- 'RV'
  } else if(grepl('Among likely', details) == T |
            grepl('among likely', details) == T) {
    resp <- 'LV'
  } else {
    resp <- paste(unlist(str_extract_all(details,'Statewide|Sttewide|statewide|(?<!LV =) likely voters|(?<!RV =) registered|Nader supporters|	Without Nader')),collapse = ' ')
  }
  
  if(is.na(resp) == T) {
    resp <- '-'
  }
  
  # Polling firm 
  pf <- str_extract(details, "[^.]+")
  
  details_complete <- list(n = n, MoE = MoE, resp = resp, pf = pf)
  return(details_complete)
}


#### Convert date to appropriate format

convert_date <- function(date_raw,year){
  date_raw <- str_replace(date_raw,'Sept.','Sep.')
  date_raw <- str_replace(date_raw,'July','Jul.')
  date_raw <- str_replace(date_raw,'June','Jun.')
  
  if(grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+[-]\\d+[/]\\d+')), collapse =  ''),' |t','')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d & %m/%d-%d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+ [&] \\d+[-]\\d+[/]\\d+')), collapse =  '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d & %d-%d/%y'))
  } else if(grepl('\\d+[/]\\d+ [&] \\d+[/]\\d+$',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+ [&] \\d+[/]\\d+')), collapse =  '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d & %d/%y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+[,] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+[,] \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%d, %d, %d,%Y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[,] \\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+[,] \\d+[-]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d, %d-%d/%y'))
  } else if(grepl('\\d+[/]\\d$',date_raw) == T){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'\\d+[/]\\d$')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y/%m/%d'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d & %d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+$',date_raw) == T){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+$')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y/%m/%d-%d'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%b. %d, %Y'))
  } else if(grepl('[[:alpha:]]+ \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]] \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b %d -%b. %d,%Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-][[:alpha:]]+ \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-][[:alpha:]]+ \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d -%b %d,%Y'))
  } else if(grepl('[[:alpha:]]+ \\d+[-]\\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[-]\\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b %d-%d,%Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+[-]\\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+[-]\\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%d, %d-%d, %Y'))
  } else if(grepl('[[:alpha:]]+[.]\\d+[-]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.]\\d+[-]\\d+')),',2000', collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d - %d ,%Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+$',date_raw) == T){
    date_unformated <- str_replace_all(paste0('2000','/',unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%Y %b.%d'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[/]\\d$',date_raw) == T){
    date_unformated <- paste0('2000/',unlist(str_extract_all(date_raw,'[[:digit:]]|[/]|[-]')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y%m/%d-%m/%d'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:digit:]]|[/]|[-]')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[,] \\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+[,] \\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d,%d/%y'))
  } else if(grepl('\\d+[/]\\d+[,] \\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[,] \\d+[-]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d,%d-%d/%y'))
  } else if(grepl('\\d+[/]\\d+ [-] \\d+[/]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+ [-] \\d+[/]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d - %m/%d/%y'))
  } else if(grepl('\\d+[/]\\d+[,] \\d+[,] \\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[,] \\d+[,] \\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d,%d,%d/%y'))
  } else if(grepl('\\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0('1/',unlist(str_extract_all(date_raw,'\\d+[-]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%d/%m-%m/%y'))
  }  else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+')), collapse =  ''),' ','')
    date_unformated <- str_replace(date_unformated,'Sept.','Sep.')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%d,%Y'))
  } else if(grepl('[1][0-2][/][0-9]$|1[0-9]$|2[0-9]$|3[0-1]$',date_raw) == T){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y/%m/%d'))
  } else if(grepl('[[:alpha:]]+ \\d+[-][[:alpha:]]+ \\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[-][[:alpha:]]+ \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b %d-%b %d, %Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[-]\\d+ [,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[-]\\d+ [,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%d & %d-%d, %Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[-]\\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[-]\\d+[,] \\d+')), collapse = '')
    date_unformated <- str_replace(date_unformated,'Sept.','Sep.')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d - %d & %d - %d, %Y'))
  } else if(grepl(paste('[[:alpha:]]+[.]',year),date_raw) == T){
    date_unformated <- paste0('01.',unlist(str_extract_all(date_raw,paste('[[:alpha:]]+[.]',year))), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%d. %b. %Y'))
  } else if(grepl('[[:alpha:]]+ \\d+[-][[:alpha:]]+ \\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[-][[:alpha:]]+ \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b %d-%b %d, %Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[,] \\d+[.]',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d, %Y'))
  } else if(grepl('\\d+[/]\\d+[/]\\d+$',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[/]\\d+$')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d/%y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+[.]',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+[.]')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%b. %d, %Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[,] \\d+ [&] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[,] \\d+ [&] \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d, %d & %d, %Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[,] \\d+[,]',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[,] \\d+[,]')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d, %Y'))
  } else if(grepl('[[:alpha:]]+ \\d+[,] \\d+[.]',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[,] \\d+[.]')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b %d, %Y'))
  } else if(grepl('\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0('1/',unlist(str_extract_all(date_raw,'\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%d/%m/%y'))
  } else {
    print('Date not processed')
    date_processed <- date_raw
  }
  return(date_processed)
  
}


#### Convert each poll data frame to one row ####


reshape_to_row <- function(dt, repC, demC, 
                           greenC = 'green|Green|[(]G[)]', 
                           libC = 'lib.|Lib.|Libertarian|libertarian|[(]L[)]', 
                           reformC = 'Reform|reform|ref.', 
                           indC = '[(]I[)]|Independent|Ind.', 
                           writeC,
                           year){
  
  candidates <- paste0(repC,'|',demC)
  details <- poll_details(dt)
  
  if(identical(c(dt[3,]), c(dt[4,])) == T){
    dt <- dt[-4,]
  }
  
  if (any(grepl('%',dt)) == F & grepl(candidates,dt[2,3]) == T & 
      grepl('General Election Trial Heat', dt[2,2]) == T) {
    dt <- rbind(dt[1:2,], c(rep('',length(dt))),dt[3:nrow(dt),])
    dt[3,3:length(dt)] <-'%'
  } else if (any(grepl('%', dt[,3])) == F & grepl(candidates, dt[5,3]) == F) {
    dt <- rbind(dt[1:3,], c(rep('', length(dt))),dt[4:nrow(dt),])
    dt[4,3:length(dt)] <-'%'
  } else if (any(grepl('%',dt[,3])) == F & grepl(candidates, dt[5,3]) == T) {
    dt <- rbind(dt[1:5,], c(rep('', length(dt))), dt[6:nrow(dt),])
    dt[6,3:length(dt)] <-'%'
  } 
  
  dt <- dt %>% mutate_at(vars(names(dt)), na_if, '') %>% 
    filter_all(any_vars(!is.na(.))) 
  dt <- dt %>% mutate_at(vars(names(dt)), na_if, 'n/a') %>% 
    filter_all(any_vars(!is.na(.))) 
  pos_pct <- which(grepl('^%$',dt[-(1:2),3])) + 2
  dt_final <- data.frame()
  
 #print(details$pf)
  
  # candidate names in row
  if(any(grepl(candidates, dt[-c(1:(pos_pct-1)),2])) == F){
    #print('row')
    for(i in (pos_pct+1):nrow(dt)) { 
      
      rep <- 0
      repV <- 0
      dem <- 0
      demV <- 0
      undecided_pattern <- 'undecided|Undecided|Not Sure|Unsure'
      undecided <- 0
      refused_pattern <- 'refused|Refused'
      refused <- 0
      green <- NA
      greenV <- 0
      lib <- NA
      libV <- 0
      reform <- NA
      reformV <- 0
      ind <- NA
      indV <- 0
      write <- NA
      writeV <- 0
      other <- 0
      
      for(j in 3:length(dt)){
        
        if(grepl(repC,dt[(pos_pct-1),j]) == T) {
          rep <- dt[(pos_pct-1),j] 
          repV <- dt[i,j]
        } else if(any(grepl(demC,dt[(pos_pct-1),j])) == T){
          dem <- dt[(pos_pct-1),j] 
          demV <- dt[i,j]
        } else if(grepl(undecided_pattern,dt[(pos_pct-1),j]) == T){
          undecided_vote  <- suppressWarnings(as.numeric(dt[i,j]))
          undecided_vote <- ifelse(is.na(undecided_vote),0,undecided_vote)
          undecided <- undecided + undecided_vote
        } else if(grepl(refused_pattern,dt[(pos_pct-1),j]) == T){
          refused_vote  <-suppressWarnings(as.numeric(dt[i,j]))
          refused_vote <- ifelse(is.na(refused_vote),0,refused_vote)
          refused <- refused + refused_vote
        } else if(any(grepl(greenC,dt[(pos_pct-1),j])) == T){
          green <- dt[(pos_pct-1),j] 
          greenV <- dt[i,j]
          greenV <- ifelse(is.na(greenV)|greenV=='-',0,greenV)
        } else if(any(grepl(libC,dt[(pos_pct-1),j])) == T){
          lib <- dt[(pos_pct-1),j] 
          libV <- dt[i,j]
          libV <- ifelse(is.na(libV)|libV=='-',0,libV)
        } else if(any(grepl(reformC,dt[(pos_pct-1),j])) == T){
          reform <- dt[(pos_pct-1),j] 
          reformV <- dt[i,j]
          reformV <- ifelse(is.na(reformV)|reformV=='-',0,reformV)
        } else if(any(grepl(indC,dt[(pos_pct-1),j])) == T){
          ind <- dt[(pos_pct-1),j] 
          indV <- dt[i,j]
          indV <- ifelse(is.na(indV)|indV=='-',0,indV)
        } else if(any(grepl(writeC,dt[(pos_pct-1),j])) == T){
          write <- dt[(pos_pct-1),j] 
          writeV <- dt[i,j]
          writeV <- ifelse(is.na(writeV) | writeV == '-', 0, writeV)
        } else if(grepl(demC,dt[(pos_pct-1),j])==F & 
                  grepl(repC,dt[(pos_pct-1),j])==F &
                  grepl(undecided_pattern,dt[(pos_pct-1),j])==F &
                  grepl(refused_pattern,dt[(pos_pct-1),j])==F &
                  grepl(greenC,dt[(pos_pct-1),j])==F &
                  grepl(libC,dt[(pos_pct-1),j])==F &
                  grepl(reformC,dt[(pos_pct-1),j])==F &
                  grepl(indC,dt[(pos_pct-1),j])==F &
                  grepl(writeC,dt[(pos_pct-1),j])==F &
                  grepl("[[:digit:]]", dt[i,j]) == T){
          other_vote <- as.numeric(dt[i,j])
          other <- other + other_vote
        }
      }
      
      if(grepl('RV|LV|AV|2nd|Adults|Likely voters|Definite voters|With undecideds|Probable voters|registered voters|Registered voters|leaners|Unleaned',dt[i,2]) == T){
        resp <- paste(unlist(str_extract_all(dt[i,2],'Adults|[A-Z]+|with leaners|with leaners|without leaners|With leaners|Unleaned')),collapse=" ")
        date <- convert_date(dt[1,1], year = year)

      } else if(grepl('ALL',dt[i,2]) == T|
                grepl('undecideds',dt[i,2]) == T|
                grepl('active',dt[i,2]) == T|
                grepl('Standard midterm',dt[i,2]) == T|
                grepl('All likely voters',dt[i,2]) == T
                ){
        date <- convert_date(dt[1,1], year = year)
        
      } else {
        date <- convert_date(dt[i,2], year = year)
      }
      
      dt_row <- data.frame(
        'date' = date, 
        'election_year' = year, 
        'state' = NA,
        'pollFirm' = details$pf, 
        'n' = details$n,
        'respondents' = details$resp,
        'rep_candidate' = rep, 
        'dem_candidate' = dem, 
        'rep_vote' = repV,
        'dem_vote' = demV, 
        'undecided' = undecided,
        'refused' = refused,
        'green_candidate' = green,
        'green_vote' = greenV,
        'lib_candidate' = lib,
        'lib_vote' = libV,
        'reform_candidate' = reform,
        'reform_vote' = reformV,
        'ind_candidate' = ind,
        'ind_vote' = indV,
        'write_in_candidate' = write,
        'write_in_vote' = writeV,
        'other' = other,
        'MoE' = details$MoE,
        stringsAsFactors = FALSE
      )
      dt_final <- rbind(dt_final, dt_row)
    }
    
    return(dt_final)
  } 
  
  #### Candidate names in col ####
  
    if(any(grepl(candidates,dt[-c(1:pos_pct),2])) == T) {
    #print('col')
    col_filled <- which(is.na(dt[3,]))

    if(length(col_filled) < 3 & isTRUE(dt[5,4] != '') & 
              is.na(dt[3,length(dt)]) == F) {
      col_filled[3] <- length(dt) + 1
    } else if(length(col_filled) < 3 & 
              isTRUE(dt[5,4] != '' & 
                     isTRUE(dt[3,min(col_filled)] == ''))) {
      col_filled[3] <- min(col_filled) - 1
    }
    
    if(length(col_filled) < 3) {
      col_filled[3] <- 4
    }
    
    for(k in 3:(col_filled[3] - 1)){ 
      rep <- 0
      repV <- 0
      dem <- 0
      demV <- 0
      undecided_pattern <- 'undecided|Undecided|Not Sure|Unsure'
      undecided <- 0
      refused_pattern <- 'refused|Refused'
      refused <- 0
      green <- NA
      greenV <- 0
      lib <- NA
      libV <- 0
      reform <- NA
      reformV <- 0
      ind <- NA
      indV <- 0
      write <- NA
      writeV <- 0
      other <- 0
      
      for(l in 3:nrow(dt)){
        if(grepl(repC,dt[l,2]) == T)
        {
          if (rep == 0){
          rep <- dt[l,2]
          } else {
          rep <- 'Several Rep. candidates'
          }
          repVtemp <- suppressWarnings(as.numeric(dt[l,k])) 
          repVtemp <- ifelse(is.na(repVtemp), 0, repVtemp)
          repV <- repV + repVtemp
        } else if(any(grepl(demC,dt[l,2])) == T) {
          
          if (dem == 0) {
            dem <- dt[l,2]
          } else {
            dem <- 'Several Dem. candidates'
          }
          
          demVtemp <- suppressWarnings(as.numeric(dt[l,k])) 
          demVtemp <- ifelse(is.na(demVtemp), 0, demVtemp)
          demV <- demV + demVtemp
          
        } else if(grepl(undecided_pattern,dt[l,2]) == T) {
          undecided_vote  <- suppressWarnings(as.numeric(dt[l,k]))
          undecided_vote <- ifelse(is.na(undecided_vote), 0, undecided_vote)
          undecided <- undecided + undecided_vote
        } else if(grepl(refused_pattern, dt[l,2]) == T) {
          refused_vote  <-suppressWarnings(as.numeric(dt[l,k]))
          refused_vote <- ifelse(is.na(refused_vote), 0, refused_vote)
          refused <- refused + refused_vote
        } else if(any(grepl(greenC,dt[l,2])) == T) {
          green <- dt[l,2] 
          greenV <- dt[l,k]
          greenV <- ifelse(is.na(greenV)|greenV=='-', 0, greenV)
        } else if(any(grepl(libC, dt[l,2])) == T) {
          lib <- dt[l,2] 
          libV <- dt[l,k]
          libV <- ifelse(is.na(libV)|libV=='-', 0, libV)
        } else if(any(grepl(reformC, dt[l,2])) == T){
          reform <- dt[l,2] 
          reformV <- dt[l,k]
          reformV <- ifelse(is.na(reformV)|reformV=='-', 0, reformV)
        } else if(any(grepl(indC, dt[l,2])) == T){
          if (is.na(ind)){
            ind <- dt[l,2]
          } else {
            ind <- 'Several Ind. candidates'
          }
          
          indVtemp <- suppressWarnings(as.numeric(dt[l,k])) 
          indVtemp <- ifelse(is.na(indVtemp), 0, indVtemp)
          indV <- indV + indVtemp
          indV <- ifelse(is.na(indV)|indV == '-', 0, indV)
          
        } else if(any(grepl(writeC, dt[l,2])) == T) {
          write <- dt[l,2] 
          writeV <- dt[l,k]
          writeV <- ifelse(is.na(writeV) | writeV == '-', 0, writeV)
        } else if(grepl(demC, dt[l,2]) == F & 
                  grepl(repC, dt[l,2]) == F &
                  grepl(undecided_pattern, dt[l,2]) == F &
                  grepl(refused_pattern, dt[l,2]) == F &
                  grepl(greenC, dt[l,2]) == F &
                  grepl(libC, dt[l,2]) == F &
                  grepl(reformC, dt[l,2]) == F &
                  grepl(indC, dt[l,2]) == F &
                  grepl(writeC, dt[l,2]) == F &
                  grepl("[[:digit:]]", dt[l,k]) == T) {
          other_vote <-suppressWarnings(as.numeric(dt[l,k]))
          other_vote <- ifelse(is.na(other_vote), 0, other_vote)
          other <- other + other_vote
        }
      }
      
      if(grepl('RV|LV|AV|2nd|Adults|active', dt[2,k]) == T) {
        resp <- paste(unlist(str_extract_all(dt[2,k], 'Adults|[A-Z]+|with leaners|without leaners|')), collapse=" ")
        date <- convert_date(dt[1,1], year = year)
      } else if(grepl('General Election Trial Heat', dt[pos_pct - 1, k]) == T |
         grepl('ALL', dt[pos_pct - 1, k]) == T |
         is.na(dt[pos_pct-1,k]) == T|
         grepl('vote', dt[pos_pct-1,k]) == T |
         grepl('^$', dt[pos_pct-1,k]) == T |
         grepl('State- wide', dt[pos_pct - 1, k]) == T |
         grepl('Trial Heat', dt[pos_pct - 1,k]) == T |
         grepl('Undecideds Allocated', dt[pos_pct - 1, k]) == T |
         grepl('Reg. Voters',dt[pos_pct - 1, k]) == T) {
        date <- convert_date(dt[1,1], year = year)
      } else {
        date <- convert_date(dt[pos_pct-1,k], year = year)
        if(any(grepl('[?]', date)) == T) {
          date <- convert_date(dt[1,1], year = year)
        }
      }
      
      dt_row <- data.frame(
        'date' = date, 
        'election_year' = year, 
        'state' = NA,
        'pollFirm' = details$pf, 
        'n' = details$n,
        'respondents' = details$resp,
        'rep_candidate' = rep, 
        'dem_candidate' = dem, 
        'rep_vote' = repV,
        'dem_vote' = demV, 
        'undecided' = undecided,
        'refused' = refused,
        'green_candidate' = green,
        'green_vote' = greenV,
        'lib_candidate' = lib,
        'lib_vote' = libV,
        'reform_candidate' = reform,
        'reform_vote' = reformV,
        'ind_candidate' = ind,
        'write_in_vote' = writeV,
        'write_in_candidate' = write,
        'ind_vote' = indV,'other' = other,
        'MoE' = details$MoE,
        stringsAsFactors = FALSE
      )
      dt_final <- rbind(dt_final, dt_row)
      
    }
    return(dt_final)
  }
} 

reshape_rbind <- function(dt_list, repC, demC, 
                          greenC , 
                          libC, 
                          reformC, 
                          indC, 
                          writeC, 
                          year){
  row_list <- lapply(dt_list, reshape_to_row, 
                     repC = repC,
                     demC = demC,
                     indC = indC,
                     reformC = reformC,
                     greenC = greenC,
                     writeC = writeC,
                     year = year)
  row_dt <- do.call('rbind', row_list)
}

convert_to_dataframe <- function(list_states, repC, demC, 
                                 greenC = 'green|Green|[(]G[)]', 
                                 libC = 'lib.|Lib.|Libertarian|libertarian|[(]L[)]', 
                                 reformC = 'Reform|reform|ref.', 
                                 indC = '[(]I[)]|Independent|Ind.', 
                                 writeC = 'write',
                                 year){
  states_df <- lapply(list_states, reshape_rbind, 
                      repC = repC,
                      demC = demC,
                      indC = indC,
                      reformC = reformC,
                      greenC = greenC,
                      writeC = writeC,
                      year = year)
  states_df <-  do.call("rbind", states_df)
  states_df$state_long <- str_replace_all(rownames(states_df), '\\d+|[.]','')
  rownames(states_df) <- c()
  return(states_df)
}


# Format respondents

resp <- function(x){
  if(grepl('registered', x) == T & grepl('likely', x) == F |
     grepl('RV',x) == T & grepl('likely', x) == F){
    x <- 'RV'
  } else if(grepl('registered', x) == F & grepl('likely',x) == T |
            grepl('registered',x) == F & grepl('LV',x) == T ){
    x <- 'LV'
  } else {
    x <- 'Statewide'
  }
  return(x)
}
