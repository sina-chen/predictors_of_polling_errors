# --------------------------------------------------------------------------- #
# US Senate - Faces
# --------------------------------------------------------------------------- #

library(tidyverse)
# library(stringr)

# prepare data -------------------------------------------------------------- #

# sen_faces <- read.csv('us_senator_pictures1998_2018/sen_faces.csv', sep = ",")
sen_faces <- read.csv('us_senator_pictures1998_2018/sen_faces_including_2020.csv', sep = ",")
sen_faces <- sen_faces %>% 
  select(candidate, jpg, party)

# classify images ----------------------------------------------------------- #

# gender
# jpgs <- list.files(path = "us_senator_pictures1998_2018/portraits")
jpgs <- list.files(path = "us_senator_pictures1998_2018/portraits_including_2020")
d_list <- list()
pb <- progress_bar$new(total = length(jpgs), show_after = 0)
for (i in 1:length(jpgs)){
  pb$tick()
  # jpg_path = paste0('us_senator_pictures1998_2018/portraits/', jpgs[i])
  jpg_path = paste0('us_senator_pictures1998_2018/portraits_including_2020/', jpgs[i])
  
  # https://github.com/jkortner/clarifai
  d <- face2gender(jpg_path, api_key)
  d[['jpg']] <- jpgs[i]
  d_list <- c(d_list, d)
  
}

df_gender <- d_list_to_df(d_list)
write.csv(df_gender, 'predictors_of_polling_errors/data/us_senate/clarifai_gender.csv')

# race
jpgs <- list.files(path = "us_senator_pictures1998_2018/portraits_including_2020")
d_list <- list()
pb <- progress_bar$new(total = length(jpgs), show_after = 0)
for (i in 1:length(jpgs)){
  pb$tick()
  jpg_path = paste0('us_senator_pictures1998_2018/portraits_including_2020/', jpgs[i])
  
  # https://github.com/jkortner/clarifai
  d <- face2race(jpg_path, api_key)
  d[['jpg']] <- jpgs[i]
  d_list <- c(d_list, d)
  
}

df_race <- d_list_to_df(d_list)
write.csv(df_race, 'predictors_of_polling_errors/data/us_senate/clarifai_race.csv')

# post-process data --------------------------------------------------------- #

df_gender$clarifai_gender <- names(df_gender)[1:2][max.col(df_gender[1:2], 'first')]
df_race$clarifai_race <- names(df_race)[1:7][max.col(df_race[1:7], 'first')]

df_gender <- df_gender %>% 
  select(jpg, clarifai_gender)

df_race <- df_race %>% 
  select(jpg, clarifai_race)

# merge --------------------------------------------------------------------- #

sen_faces <- merge(x = sen_faces, y = df_gender, by = "jpg", all.x = TRUE)
sen_faces <- merge(x = sen_faces, y = df_race, by = "jpg", all.x = TRUE)

write.csv(sen_faces, 'predictors_of_polling_errors/data/us_senate/clarifai_labels.csv')
