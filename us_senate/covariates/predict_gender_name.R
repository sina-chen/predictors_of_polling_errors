### gender prediction by name


# create variables with separate first name

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

df_final <- df_final %>% 
  sep_function(dem_candidate) %>% 
  sep_function(lib_candidate) %>% 
  sep_function(ind_candidate) %>% 
  sep_function(rep_candidate) %>% 
  sep_function(green_candidate) %>% 
  sep_function(other_candidate) %>% 
  add_column(min_year = 1930, max_year = 1965)


# check how the separation worked



# df_final %>% 
#   select(starts_with("first_")) %>% 
#   mutate_all(list(no_characters = ~ nchar(.))) %>% 
#   arrange(first_rep_no_characters) %>% View()




# predicting gender

predict_gender <- function(df, party){
  df %>% 
    gender_df(name_col = paste("first", party, sep = "_"),
              year_col = c("min_year", "max_year"),
              method = "ssa") -> df_gender
  return(df_gender)
}


# predict gender and join back to original df

rep_gender <- predict_gender(df_final, "rep") %>% 
  rename(rep_gender = gender) %>% 
  select(rep_gender, name)

dem_gender <- predict_gender(df_final, "dem") %>% 
  rename(dem_gender = gender) %>% 
  select(dem_gender, name)

# joining

df_final %>% 
  left_join(rep_gender, by = c("first_rep" = "name")) %>% 
  distinct() -> df_final

df_final %>% 
  left_join(dem_gender, by = c("first_dem" = "name")) %>% 
  distinct() -> df_final

