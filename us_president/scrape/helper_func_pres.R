#-------------------------------------------------------------------------------
#
# Helper functions to extract presidential polls
# Author: Sina Chen
# Notes:
#
#-------------------------------------------------------------------------------


#### Get  txt content ###

read_pres <- function(txt_dir){

  # Set working directory to read files
  setwd(txt_dir)

  # Read files
  txt_names <- dir(txt_dir)
  polls_raw <- lapply(txt_names, read_html, encoding = 'UTF-8') %>%
    lapply(function(x) html_table(x, fill = T))

  # Set state names
  state_names <- sapply(txt_names, function(x) str_remove(x, pattern = '\\d+.txt'))
  names(polls_raw) <- state_names

  return(polls_raw)
}

#### Clean & Split ####

# Clean
clean <- function(dt){
  dt <- as.data.frame(lapply(dt, gsub, pattern='\n|\\s+|:', replacement=' '),
                     stringsAsFactors=FALSE)
  dt <- as.data.frame(lapply(dt, gsub, pattern='^[.]|Â|^[.][.]$|[?][-]',
                             replacement=''), stringsAsFactors=FALSE)
  dt <- as.data.frame(lapply(dt, gsub, pattern='Buch- anan',
                            replacement='Buchanan'), stringsAsFactors=FALSE)

  if(identical(dt[,2],dt[,3]) == T) {
    dt <- dt[,-2]
  }

  if(all(dt[2,] == '') & all(dt[3,] == '')) {
    dt <- dt[-3,]
  }

  # remove "." after abbreviations
  dt[1,1] <- str_replace(dt[1,1], 'Corp[.]', 'Corp')
  dt[1,1] <- str_replace(dt[1,1], 'St[.]', 'St')
  dt[1,1] <- str_replace(dt[1,1], 'D[.]', 'D')
  dt[1,1] <- str_replace(dt[1,1], 'J[.]', 'J ')
  dt[1,1] <- str_replace(dt[1,1], 'Co[.]', 'Co')
  dt[1,1] <- str_replace(dt[1,1], 'Prof[.]', 'Prof')
  dt[1,1] <- str_replace(dt[1,1], 'R[.]L[.]', 'R L ')
  dt[1,1] <- str_replace(dt[1,1], 'L[.]', 'L ')
  dt[1,1] <- str_replace(dt[1,1], 'U[.]S[.]', 'US')
  dt[1,1] <- str_replace(dt[1,1], 'H[.]', 'H')
  dt[1,1] <- str_replace(dt[1,1], 'U[.]', 'U')

  # remove rong party affiliation
  dt[,4] <- str_replace_all(dt[,4],'Obama [(]R[)]', 'Obama (D)')

  # remove unneccesary date information
  pos_digit <-  which(str_detect(dt[-c(1:3),2], '\\d+')) + 3
  pos_date <- which(sapply(pos_digit, function(x)
    length(unique(c(dt[x,2:length(dt)]))) == 1) ==T)

  if(length(pos_date) > 0 &
     isTRUE(all(dt[pos_digit[pos_date] + 1,2] == 'STATEWIDE'))) {

    for(i in 1: length(pos_date)) {
      dt[pos_digit[pos_date[i] + 1],2] <-  dt[pos_digit[pos_date[i]],2]
    }

    dt <- dt[-c(pos_digit[pos_date]),]
  }

  # adjust date position
  for(i in 4:nrow(dt)){
    if(!is.na(dt[i,2]) &
       #!is.na(dt[i,3]) &
       !is.na(dt[(i + 1),2]) &
      str_detect(dt[i,2], '\\d+/\\d+[-]\\d+/\\d+|\\d+/\\d+ [-] \\d+/\\d+/\\d+') == T &
       #str_detect(dt[i,3], '\\d+/\\d+[-]\\d+/\\d+|\\d+/\\d+ [-] \\d+/\\d+/\\d+') == T &
       str_detect(dt[(i + 1),2],'ALL') == T){
      dt[(i + 1),2] <- dt[i,2]
      dt <- dt[-i,]
    }
  }


  return(dt)
}

# Split polls from multiple polling firms
split_general <- function(dt){

  # split based on 'General election'...
  split_general <- which(str_detect(dt[-c(1:3),1], 'General ')) + 3

  if(length(split_general) > 1) {
    list_dt <- vector(mode = 'list', length = length(split_general))
    split_general[length(split_general) + 1] <- nrow(dt) + 3

    for(i in 1:(length(split_general) - 1)){
      split_data <- dt[(split_general[i] - 2):(split_general[i + 1] - 3),]
      list_dt[i] <- list(split_data)
    }
  } else {
    list_dt <- list(dt)
  }

  return(list_dt)
}

# Split if polls from more than one polling firm is in one dataframe
split_df <- function(dt){

  split <- which(dt[,1] != '')

  if(length(split) > 3 & (split[2] - split[1]) <= 2 & (split[4] - split[3]) <= 2) {
    split[length(split) + 1] <- nrow(dt)
    split_pos <- split[seq(1,length(split) , 2)]
    list_dt <- vector(mode = 'list', length = length(split_pos) - 1)

    for(i in 1:(length(split_pos) - 1)) {
      split_data <- dt[split_pos[i]:(split_pos[i + 1] - 1),]
      list_dt[i] <- list(split_data)
    }
  } else {
    list_dt <- list(dt)
  }

  return(list_dt)
}


# Split polls from one polling firm
split_polls <- function(dt){

  # split based on first column enries
  split <- which(dt[,1] != '' &
                   dt[,1] != '"Former vice president Joe Biden"' &
                   dt[,1] != '"Vermont Senator Bernie Sanders"' &
                   dt[,1] != '"California Senator Kamala Harris"' &
                   dt[,1] != '"Massachusetts Senator Elizabeth Warren"' &
                   dt[,1] != '"New Jersey Senator Cory Booker"'  &
                   dt[,1] != '"Former Texas congressman Beto O\'Rourke"'  &
                   dt[,1] != '"Minnesota Senator Amy Klobuchar"'  &
                   dt[,1] != '"Former U.S. secretary of Housing and Urban Development Julian Castro"'  &
                   dt[,1] != '"Mayor of South Bend Indiana, Pete Buttigieg"'  &
                   dt[,1] != '"New York Senator Kirsten Gillibrand"' &
                   dt[,1] != '"Former Colorado Governor John Hickenlooper"' &
                   dt[,1] != '"Washington Governor Jay Inslee"')
  split[length(split) + 1] <- nrow(dt)

  if(length(split) > 3) {
    head_tbl <- dt[split[1]:(split[2] - 1),] # Head with meta data for every data frame
    list_dt <- vector(mode = 'list', length = length(split) - 2)

    # split
    for(i in 2:(length(split) - 1)) {
      split_data <- dt[split[i]:(split[i + 1] - 1),]
      split_data <- rbind(head_tbl, split_data)

      list_dt[i - 1] <- list(split_data)
    }
  } else { # do not split if not applicable
    list_dt <- list(dt)
  }

  return(list_dt)
}


# Split multiple polls form one polling firm
split_pct <- function(dt){

  # split based on %

  split2 <- which(grepl('%', dt[-c(1:3),3])) + 3

    if(length(split2) > 1 &
     length(unique(c(dt[(split2[2] - 3), 2:length(dt)]))) != 1 |
     isTRUE(dt[(split2[2] - 3),2] == ''))  {
    split2[length(split2) + 1] <- nrow(dt) + 2

    head_tbl2 <- dt[1:3,] # Head with meta data for every data frame
    list_dt_split2 <- vector(mode = 'list', length = length(split2) - 1)

    # split
    for(i in 1:(length(split2) - 1)){
      split_data2 <- dt[(split2[i] - 1):(split2[i + 1] - 2),]
      split_data2 <- rbind(head_tbl2,split_data2)

      list_dt_split2[i] <- list(split_data2)
    }
  } else if(length(split2) > 1 &
            length(unique(c(dt[(split2[2] - 3),2:length(dt)]))) == 1 &
            dt[(split2[2] - 3),2] != '') {
    head_tbl <- dt[1:2,] # Head with meta data for every data frame
    split2[length(split2)+1] <- nrow(dt) + 3

    list_dt_split2 <- vector(mode = 'list', length = length(split2) - 1)

    for(i in 1:(length(split2)-1)){
      split_data <- dt[(split2[i] - 3):(split2[i + 1] - 4),]
      split_data <- rbind(head_tbl, split_data)

      list_dt_split2[i] <- list(split_data)
    }

  } else {
    list_dt_split2 <- list(dt)
  }

  return(list_dt_split2)
}

# Split if polls are conducuted among different voter types
split_voter <- function(dt){

  # split based on voter type

  split <- which(str_detect(dt[-c(1:5),2],'Among registered voters')|
                    str_detect(dt[-c(1:5),2],'All registered voters')|
                    str_detect(dt[-c(1:5),2],'Among active voters')) + 5
  split <- c(6,split,nrow(dt) + 1)

  split2 <- which(str_detect(dt[-c(1:6),2],'Among likely voters')|
              str_detect(dt[-c(1:6),2],'Among registered voters')) + 6

  if(length(split) > 2 & length(split2) < 2) {
    head_tbl <- dt[1:5,] # Head with meta data for every data frame
    list_dt <- vector(mode = 'list', length = length(split) - 1)

    # split
    for(i in 1:(length(split) - 1)){
      split_data3 <- dt[(split[i]):(split[i + 1] - 1),]
      split_data3 <- rbind(head_tbl, split_data3)

      if(grepl('Among registered voters',split_data3[6,2]) == T){
        split_data3[1,1] <- paste0(split_data3[1,1], split_data3[6,2])
        split_data3 <- split_data3[-6,]
      }

      list_dt[i] <- list(split_data3)
    }
  } else if(length(split2) >= 2 &
            any(str_detect(dt[-c(1:6),2],'Among likely voters')) &
            any(str_detect(dt[-c(1:6),2],'Among registered voters'))) {
    head_tbl <- dt[1:6,] # Head with meta data for every data frame
    list_dt <- vector(mode = 'list', length = length(split2))

    split2 <- c(split2, nrow(dt) + 1)

    for(i in 1:(length(split2) - 1)){
      split_data <- dt[(split2[i]):(split2[i + 1] - 1),]
      split_data <- rbind(head_tbl, split_data)

      if(any(str_detect(dt[-c(1:6),2],'Among registered voters'))) {
        split_data[1,1] <- paste0(split_data[1,1], 'registered voters')
        split_data[1,1] <- str_remove_all(split_data[1,1],'likely')

      } else if (any(str_detect(dt[-c(1:6),2],'Among likely voters'))){
        split_data[1,1] <- paste0(split_data[1,1], 'likely voters')
        split_data[1,1] <- str_remove_all(split_data[1,1],'registered')
      }

      list_dt[i] <- list(split_data)
    }

  } else {
    list_dt <- list(dt)
  }

  return(list_dt)
}

# Split several polls form one polling firm based on multiple candidates from the same party
split_cand <- function(dt){

  split_rep <- which(str_detect(dt[-c(1:3),2], '[(]R[)]'))
  split_dem <- which(str_detect(dt[-c(1:3),2], '[(]D[)]'))
  pos_pct <- which(str_detect( dt[-c(1:3),3], '%')) + 3

  if(length(split_rep) > 1 & length(split_dem) > 1) {

    head_data <- dt[1:pos_pct[1],]

    split_point <- which(str_detect(dt[-c(1:(pos_pct[1] - 1)),2],
                                    '^$')) + pos_pct[1] - 1

    if(split_point[length(split_point)] != nrow(dt)) {
      split_point[length(split_point) + 1] <- nrow(dt) + 1
    }

    split_point <- split_point[1:3]

    list_dt <- vector(mode = 'list', length = length(split_point) - 1)

    for(i in 1:(length(split_point) - 1)) {
      split_data <- dt[(split_point[i] + 1):split_point[i + 1],]
      split_data <- rbind(head_data, split_data)

      rm_empty <- which(str_detect(
        split_data[(pos_pct[1] + 1),-c(1:3)], '^$')) + 3
      split_data[c(3:pos_pct[1]),rm_empty] <- ''

      list_dt[i] <- list(split_data)
    }

  } else {
    list_dt <- list(dt)
  }

  return(list_dt)

}



#### Remove subgroups of respondents ####
rm_resp <- function(dt){

  pos_pct <- which(grepl('%', dt[,3]))
  # remove polls among subgroups
  rem_row <- which(stringr::str_detect(dt[-c(1:4),2], 'Democrats|Republicans|Independents|Independent|None/Minor|Men|Women|Black|Blacks|Cubans/Hispanics|No party|Whites|Hispanics|1st CD|2nd CD|3rd CD|Union households|None/Minor parties|party affiliation|Unaffiliated|San Diego/Orange|L.A. County|Central Valley|White, non-Hispanic|S.F. Bay Area|Latino|Non-union households|Calif[.]|Nonpartisan/Other|Current Results|Jersey|Other|turnout|CD')) + 4

  rem_row2 <- which(stringr::str_detect(dt[-c(1:4),2], 'STATEWIDE') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'STATEWIDE') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'Among') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'Among') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'among') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'among') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'Trend') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'Trend') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'Without Nader') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'Without Nader') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'Registered') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'Registered') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'registered') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'registered') == T|
                      stringr::str_detect(dt[-c(1:4),2], 'results') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'results') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'Without') == T &
                      stringr::str_detect(dt[-c(1:4),3], 'Without') == T |
                      stringr::str_detect(dt[-c(1:4),2], 'Trend') == T &
                      stringr::str_detect(dt[-c(1:4),3], '^$') == T) + 4

  rem_row <- c(rem_row,rem_row2)

  if(length(rem_row) > 0) {
    dt <- dt[-c(rem_row),]
  }

  rem_col <- which(stringr::str_detect(
    as.matrix(dt[pos_pct - 1,]),'Rep[.]|Dem[.]|Ind[.]|Suburbs|Upstate|Men|Women|Most likely voters|^Republicans|^Democrats'))

  if(length(rem_col) > 0) {
    dt <- dt[,-c(rem_col)]
  }

  return(dt)
}

# Combine all clean & split functions
clean_split <- function(list_states){
  if(length(which(sapply(list_states, nrow)==1)) != 0) {list_states <- list_states[-which(sapply(list_states, nrow)==1)]}
  states_clean <-  lapply(list_states, clean)
  state_general <- lapply(states_clean, split_general) %>%
    unlist(recursive = F)
  state_df <- lapply(state_general, split_df) %>%
    unlist(recursive = F)
  states_polls <- lapply(state_df, split_polls) %>%
    unlist(recursive = F)
  states_pct <- lapply(states_polls, split_pct) %>%
    unlist(recursive = F)
  states_voter <- lapply(states_pct, split_voter) %>%
    unlist(recursive = F)
  states_cand <- lapply(states_voter, split_cand) %>%
    unlist(recursive = F)
  states_rmResp <- lapply(states_cand, rm_resp)

  return(states_rmResp)
}


#### Remove polls not covering vote intention ####

rm_other <- function(state){
  rm_id <-  which(state %>%
                    lapply(function(x)
                      any(str_detect(as.matrix(x),'Favorability|Job|job|Approve|Favor- able|direction|primary|Primary|favorable|Not Inclined|deserve|run for the Senate|Support|performance|Preference for Democratic Nominee|track|military force| definitely|against| Against|worth|like to see the Democrats nominate|potential Democratic candidates|replace|might run for|excellent|Excellent|wrong|or not|Yes|caucus| definite|Caucus|issues|following Democrats|another term|issue|nine|Vote-To-Reelect|effect|conservative|better|active Democratic voters|Certain|become the Democratic presidential nominee|certain|positive|satisfied|become the Democratic|like to be nominated|select a|feel|like to see the Republicans nominate|recalculated|possible|Among Democratic voters|Among Republican voters|success|would you like to see run|Republican Presidential Preference|Democratic Presidential Preference|Preference for Republican Nominee|Track|like to see the Democratic Party nominate|rather see as|presidential nomination|candidates are you planning to support|number of Democratic candidates|number of Republican candidates|Direction|Asked of respondents who|best for the nation|oppose|election for the Republican nomination|candidate for vice president|candidate for vice president|Preference for Republican nominee|much more likely|opponent|time to have someone else|prefer someone else|approve|prefer to see run as the Republican candidate|first choice for to be the Republican nominee|allocated|idea|potential candidates|stay|good|Asked of Republicans|oppose|Who is that candidate|difference|Republican presidential preference|Democrats will vote|Republicans will vote|likely Republican voters|Preferences, including leaners|list of candidates for the Democratic nomination|prospective candidates for the Republican nomination|temperament|who are Republican or lean Republican|Asked of Democrats|like to see as the Democratic presidential candidate|like to see as the Republican presidential candidate|like to see as the Republican candidate|mask |impeach|removed from office|justice|grade|Will you vote to reelect Donald Trump as president, or will you vote for someone else to be president|likely or unlikely|Donald Trump versus|identify|whistleblower|like to see run|New York politicians|continue|controlled|improper|acceptable|Obama - lean|like to be the next Democratic presidential nominee|like to see as the Democratic candidate|following Republicans|Just tell me the name'))) == T)

  if(length(rm_id) > 0) {
    state <- state[-rm_id]
  }

  return(state)
}



#### Poll details ####

poll_details <- function(dt){

  details <- paste(dt[1,1],dt[3,1])
  details_add <-which(grepl('N=|MoE',dt[-c(1:3),2])) + 3

  if(length(details_add) > 0){
    details <- paste(details, dt[details_add,2])
  }

    # Sample Size
  n <- parse_number(unlist(str_replace(unlist(str_extract(details,'N+[=]+\\d+ |N+[=]+\\d+[,]+\\d+|N+[=]approx[.]\\s+\\d+[,]\\d+|N+[=]approx[.]\\s+\\d+|N[=]\\d+')),',|[.]','')))
  if(isTRUE(is.na(n))|length(n)==0){
    n <- '-'
  }

  # Margin of Error
  MoE <- parse_number(str_replace(str_extract(details,'MoE [±] +\\d+[.]\\d+|MoE [±] +\\d+|MoE [+][/][-] \\d+[.]\\d+|MoE [+][/][-] \\d+|MoE [±] approx. \\d+[.]\\d+|argin of error [±] +\\d+[.]\\d+|argin of error [±] +\\d+[.]|error is [±] +\\d+|error \\d+[.]\\d+|error [±] \\d+[)]'),'[[:alpha:]][.]|[-]',''))
  if(isTRUE(is.na(MoE))){
    MoE <- '-'
  }

  # Respondents
  if(grepl('Among registered', details) == T |
     grepl('among registered', details) == T) {
    resp <- 'RV'
  } else if(grepl('Among likely', details) == T |
            grepl('among likely', details) == T) {
    resp <- 'LV'
  } else {
    resp <- paste(unlist(str_extract_all(details, 'Statewide|Sttewide|statewide|(?<!LV =) likely voters|(?<!RV =) registered|Nader supporters|	Without Nader')), collapse = ' ')
  }

  if(is.na(resp) == T) {
    resp <- '-'
  }

  # Polling firm
  pf <- str_extract(details, "[^.]+")
  details_complete <- list(n = n, MoE = MoE, resp = resp, pf = pf)
  return(details_complete)
}


#### Convert date to appropriate format ####

convert_date <- function(date_raw,year){
  date_raw <- str_replace(date_raw, 'Sept.', 'Sep.')
  date_raw <- str_replace(date_raw, 'July', 'Jul.')
  date_raw <- str_replace(date_raw, 'June', 'Jun.')

  if(grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+[-]\\d+[/]\\d+')), collapse =  ''),' |t','')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d & %m/%d-%d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[,] \\d+[-]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+[,] \\d+[-]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d, %d-%d/%y'))
  } else if(grepl('\\d+[/]\\d+[-] \\d+[/]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-] \\d+[/]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d- %m/%d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[,] \\d+ [-] \\d+[/]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+[,] \\d+ [-] \\d+[/]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d, %d - %m/%d/%y'))
  } else if(grepl('\\d+[/]\\d+[,] \\d+[,] \\d+ [-] \\d+[/]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[,] \\d+[,] \\d+ [-] \\d+[/]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d, %d, %d - %m/%d/%y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+[,] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+[,] \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%d, %d, %d,%Y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+$',date_raw) == T & year != 2000 |
            grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+$',date_raw) == T & year == 2000 & grepl('1$',date_raw) == F){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d & %d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+$',date_raw) == T & year == 2000 & grepl('1$',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+')),'/',year, collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d & %m/%d/%Y'))
  } else if(grepl('\\d+[/]\\d+[-] \\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-] \\d+[/]\\d+')),'/', year, collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d- %m/%d/%Y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+ [&] \\d+[/]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d & %m/%d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[/][1]$',date_raw) == T & year == 2016){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+[/][1]$')),'6', collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d-%d/%y'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+$',date_raw) == T){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+$')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y/%m/%d-%d'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%b. %d,%Y'))
  } else if(grepl('\\d+[/]\\d+[-] \\d+[/]\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-] \\d+[/]\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d- %m/%d/%y'))
  } else if(grepl('[[:alpha:]]+ \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[-][[:alpha:]]+[.] \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b %d-%b. %d,%Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-][[:alpha:]]+ \\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-][[:alpha:]]+ \\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d -%b %d,%Y'))
  } else if(grepl('[[:alpha:]]+ \\d+[-]\\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[-]\\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b %d-%d,%Y'))
  } else if(grepl('[[:alpha:]]+[.]\\d+[-]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.]\\d+[-]\\d+')),',',year, collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d - %d ,%Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+$',date_raw) == T){
    date_unformated <- str_replace_all(paste0(year,'/',unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%Y %b.%d'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[/]\\d$',date_raw) == T & grepl('16$', date_raw) == F & grepl('1$', date_raw) == F){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'[[:digit:]]|[/]|[-]')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y%m/%d-%m/%d'))
  }  else if(grepl('\\d+[/]\\d+[-]\\d+[/]\\d+$',date_raw) == T  & grepl('1$', date_raw) == T & year == 2000){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+[/]\\d+$')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y/%m/%d-%m/%d'))
  } else if(grepl('\\d+[/]\\d+[-]\\d+[/]\\d+',date_raw) == T ){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+[-]\\d+[/]\\d+')), collapse = '')
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
  } else if(grepl('\\d+[/]\\d+ [&] \\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+ [&] \\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d & %d/%y'))
  }  else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+',date_raw) == T){
    date_unformated <- str_replace_all(paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+[,] \\d+')), collapse =  ''),' ','')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%d,%Y'))
  } else if(grepl('[1][0-2][/][0-9]$|1[0-9]$|2[0-9]$|3[0-1]$',date_raw) == T){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y/%m/%d'))
  } else if(grepl('^\\d+[/]\\d+[/]\\d+$',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'^\\d+[/]\\d+[/]\\d+$')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d/%y'))
  } else if(grepl('\\d+[/]\\d+ [-] \\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'\\d+[/]\\d+ [-] \\d+[/]\\d+')),'/', year, collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%m/%d - %m/%d/%Y'))
  } else if(grepl('\\d+[/]\\d+',date_raw) == T){
    date_unformated <- paste0('1/',unlist(str_extract_all(date_raw,'\\d+[/]\\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%d/%m/%y'))
  } else if(grepl('[[:alpha:]]+ \\d+[-][[:alpha:]]+ \\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[-][[:alpha:]]+ \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b %d - %b %d,%Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[-]\\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[-]\\d+[,] \\d+')), collapse = '')
    date_unformated <- str_replace(date_unformated,'Sept.','Sep.')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d - %d & %d - %d, %Y'))
  } else if(grepl('[[:alpha:]]+ \\d+[-]\\d+ [&] \\d+[,] \\d+', date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+ \\d+[-]\\d+ [&] \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b %d - %d & %d, %Y'))
  } else if(grepl('[[:alpha:]]+[.][-][[:alpha:]]+ [[:alpha:]]+[,] \\d+', date_raw) == T){
    date_unformated <- paste0('01, ',unlist(str_extract_all(date_raw,'[[:alpha:]]+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%d, %B, %Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[,] \\d+', date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d, %Y'))
  } else if(grepl('\\d+[/]\\d$',date_raw) == T){
    date_unformated <- paste0(year,'/',unlist(str_extract_all(date_raw,'\\d+[/]\\d$')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%Y/%m/%d'))
  } else if(grepl('Late [[:alpha:]]+[.] \\d+[.]',date_raw) == T){
    date_unformated <- paste0('30. ',unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%d. %b. %Y'))
  } else if(grepl('[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[,] \\d+',date_raw) == T){
    date_unformated <- paste0(unlist(str_extract_all(date_raw,'[[:alpha:]]+[.] \\d+[-]\\d+ [&] \\d+[,] \\d+')), collapse = '')
    date_processed <- as.character(as.Date(date_unformated, '%b. %d-%d & %d, %Y'))
  } else {
    #print('Date not processed')
    date_processed <- date_raw
  }
  return(date_processed)

}
#### Assemble information from each poll data frame in one row ####

reshape_to_row <- function(dt, repC, demC, thirdC, year){

  candidates <- paste0(repC,'|',demC)
  details <- poll_details(dt)

  if(identical(dt[,2],dt[,3]) == T) {
    dt <- dt[,-3]
  }

  dt <- dt %>%
    mutate_at(vars(names(dt)), na_if, '') %>%
    filter_all(any_vars(!is.na(.)))
  dt <- dt %>%
    mutate_at(vars(names(dt)), na_if, 'n/a') %>%
    filter_all(any_vars(!is.na(.)))

  pos_pct <- which(grepl('%',dt[-c(1,2),3])) + 2

  if(length(pos_pct) < 1){
    row_pct <- dt[3,]
    row_pct[which(!is.na(dt[3,]))] <- '%'
    dt <- rbind(dt[1:3,], row_pct, dt[4:nrow(dt),])
    pos_pct <- which(grepl('%',dt[,3]))
  }

  dt_final <- data.frame()


   # candidate names in row
   if(any(grepl(candidates, dt[-c(1:(pos_pct-1)),2])) == F) {
     #print('row')
     for(i in (pos_pct+1):nrow(dt)){

       rep <- 0
       repV <- 0
       dem <- 0
       demV <- 0
       undecided_pattern <- 'undecided|Undecided|Not Sure|Unsure'
       undecided <- 0
       refused_pattern <- 'refused|Refused'
       refused <- 0
       third_party <- 0
       other <- 0

       for(j in 3:length(dt)) {

         if(grepl(repC,dt[(pos_pct-1),j]) == T) {
           rep <- dt[(pos_pct-1),j]
           repV <- dt[i,j]
         } else if(any(grepl(demC,dt[(pos_pct-1),j])) == T) {
           dem <- dt[(pos_pct-1),j]
           demV <- dt[i,j]
         } else if(grepl(undecided_pattern,dt[(pos_pct-1),j]) == T) {
           undecided_vote  <- suppressWarnings(as.numeric(dt[i,j]))
           undecided_vote <- ifelse(is.na(undecided_vote), 0, undecided_vote)
           undecided <- undecided + undecided_vote
         } else if(grepl(refused_pattern,dt[(pos_pct-1),j]) == T) {
           refused_vote  <-suppressWarnings(as.numeric(dt[i,j]))
           refused_vote <- ifelse(is.na(refused_vote), 0, refused_vote)
           refused <- refused + refused_vote
         } else if(any(grepl(thirdC,dt[(pos_pct-1),j])) == T) {
           third_party_vote <- suppressWarnings(as.numeric(dt[i,j]))
           third_party_vote <- ifelse(is.na(third_party_vote), 0, third_party_vote)
           third_party <- third_party + third_party_vote
         } else if(grepl(demC, dt[(pos_pct-1),j]) == F &
                   grepl(repC, dt[(pos_pct-1),j]) == F &
                   grepl(undecided_pattern, dt[(pos_pct-1),j]) == F &
                   grepl(refused_pattern, dt[(pos_pct-1),j]) == F &
                   grepl(thirdC, dt[(pos_pct-1),j]) == F &
                   grepl("[[:digit:]]", dt[i,j]) == T) {
           other_vote <- as.numeric(dt[i,j])
           other <- other + other_vote
         }
       }

       if(isTRUE(any(str_detect(dt[i,2], 'RV|LV|AV|2nd|Adults')))) {
         resp <- paste(unlist(
           str_extract_all(dt[i,2],
                           'Adults|[A-Z]+|with leaners|without leaners|')),
           collapse=" ")
         date <- convert_date(dt[i,2],year = year)
       } else if(isTRUE(any(str_detect(dt[i,2],
                                       'ALL|Registered|Likely|STATEWIDE')))) {
         date <- convert_date(dt[1,1], year = year)
         dt[1,1] <- paste0(dt[1,1], dt[i,2])
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
         'rep_poll' = repV,
         'dem_poll' = demV,
         'undecided' = undecided,
         'refused' = refused,
         'third_party' = third_party,
         'other' = other,
         'MoE' = details$MoE,
         stringsAsFactors = FALSE
       )
       dt_final <- rbind(dt_final, dt_row)
     }

     return(dt_final)

     ### Candidate names in column ###
   } else if(any(grepl(candidates, dt[-c(1:pos_pct),2])) == T) {

     if(isTRUE(str_detect(dt[3,2],'Among likely voters|Likely voters'))) {
       dt[3,2] <- NA
     }

     col_filled <- which(is.na(dt[3,]))
     # print('col')

     if(length(col_filled) < 3) {
       col_filled[3] <- length(dt) + 1
     }

     for(k in 3:(col_filled[3] - 1)) {
       rep <- 0
       repV <- 0
       dem <- 0
       demV <- 0
       undecided_pattern <- 'undecided|Undecided|Not Sure|Unsure'
       undecided <- 0
       refused_pattern <- 'refused|Refused'
       refused <- 0
       third_party <- 0
       other <- 0

       for(l in 3:nrow(dt)) {
         if(grepl(repC,dt[l,2]) == T){
           if(rep == 0 & repV ==0){
             rep <- dt[l,2]
             repV <- dt[l,k]
           } else {
             rep <- 'Several Rep. candidates'
             repV <- as.numeric(repV) + as.numeric(dt[l,k])
           }
         } else if(any(grepl(demC,dt[l,2])) == T){
           if(dem == 0 & demV ==0){
             dem <- dt[l,2]
             demV <- dt[l,k]
           } else {
             dem <- 'Several Dem. candidates'
             demV <- as.numeric(demV) + as.numeric(dt[l,k])
           }
         } else if(grepl(undecided_pattern, dt[l,2]) == T) {
           undecided_vote  <- suppressWarnings(as.numeric(dt[l,k]))
           undecided_vote <- ifelse(is.na(undecided_vote), 0, undecided_vote)
           undecided <- undecided + undecided_vote
         } else if(grepl(refused_pattern, dt[l,2]) == T) {
           refused_vote  <-suppressWarnings(as.numeric(dt[l,k]))
           refused_vote <- ifelse(is.na(refused_vote), 0, refused_vote)
           refused <- refused + refused_vote
         } else if(any(grepl(thirdC,dt[l,2])) == T){
           third_party_vote <- suppressWarnings(as.numeric(dt[l,k]))
           third_party_vote <- ifelse(is.na(third_party_vote), 0,
                                      third_party_vote)
           third_party <- third_party + third_party_vote
         } else if(grepl(demC,dt[l,2]) == F &
                   grepl(repC,dt[l,2]) == F &
                   grepl(undecided_pattern,dt[l,2]) == F &
                   grepl(refused_pattern,dt[l,2]) == F &
                   grepl(thirdC,dt[l,2]) == F &
                   grepl("[[:digit:]]", dt[l,k]) == T) {
           other_vote <-suppressWarnings(as.numeric(dt[l,k]))
           other_vote <- ifelse(is.na(other_vote), 0, other_vote)
           other <- other + other_vote
         }
       }


       if(isTRUE(any(str_detect(dt[2,k],'RV|LV|AV|2nd|Adults')))) {
         resp <- paste(unlist(
           str_extract_all(dt[2,k],
                           'Adults|[A-Z]+|with leaners|without leaners|')),
           collapse=" ")
         date <- convert_date(dt[2,k], year = year)
       } else if(isTRUE(any(str_detect(dt[pos_pct-1,k],
                                       'General Election Trial Heat|ALL')))) {
         date <- convert_date(dt[1,1], year = year)
       } else {
         date <- convert_date(dt[pos_pct-1,k], year = year)
         if(is.na(date) == T | str_detect(date, '[[:alpha:]]+') == T) {
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
         'rep_poll' = repV,
         'dem_poll' = demV,
         'undecided' = undecided,
         'refused' = refused,
         'third_party' = third_party,
         'other' = other,
         'MoE' = details$MoE,
         stringsAsFactors = FALSE
       )
       dt_final <- rbind(dt_final, dt_row)
     }

     return(dt_final)
   }
}


# Clean & Split
convert_to_dataframe <- function(list_states, repC, demC,
                                 thirdC, year){
  states_df <- lapply(list_states, function(x) lapply(x, reshape_to_row,
                                                      repC = repC,
                                                      demC = demC,
                                                      thirdC = thirdC,
                                                      year = year)) %>%
    unlist(recursive = F)
  states_df <-  do.call("rbind", states_df)
  states_df$states_long <- str_remove_all(rownames(states_df),'\\d+|[.]')
  rownames(states_df) <- NULL

  return(states_df)
}



#### Format respondents ####

resp <- function(x){
  if(grepl('registered',x)==T & grepl('likely',x)==F|grepl('RV',x)==T & grepl('likely',x)==F){
    x <- 'RV'
  } else if(grepl('registered',x)==F & grepl('likely',x)==T|grepl('registered',x)==F & grepl('LV',x)==T){
    x <- 'LV'
  } else {
    x <- 'Statewide'
  }
  return(x)
}


