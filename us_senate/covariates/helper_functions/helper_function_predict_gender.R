# function to create variables with separate first name

sep_function <- function(df, party_name){
  
  var_name <- deparse(substitute(party_name)) 
  var_name <- str_split(var_name, pattern = "_")[[1]][1]
  
  print(var_name)
  
  df %>% 
    separate({{party_name}}, into = c(paste("first", var_name, sep = "_"), 
                                      paste("rest", var_name, sep = "_")),
             remove = FALSE,
             sep = "\\s", extra = "merge") -> df
  return(df)
  
}

# function to predict gender with given first name
predict_gender <- function(df, party){
  df %>% 
    gender_df(name_col = paste("first", party, sep = "_"),
              year_col = c("min_year", "max_year"),
              method = "ssa") -> df_gender
  return(df_gender)
}