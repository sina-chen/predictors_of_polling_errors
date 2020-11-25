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

# jpgs <- list.files(path = "us_senator_pictures1998_2018/portraits")
jpgs <- list.files(path = "us_senator_pictures1998_2018/portraits_including_2020")
d_list <- list()
pb <- progress_bar$new(total = length(jpgs), show_after = 0)
for (i in 1:length(jpgs)){
  pb$tick()
  # jpg_path = paste0('us_senator_pictures1998_2018/portraits/', jpgs[i])
  jpg_path = paste0('us_senator_pictures1998_2018/portraits_including_2020/', jpgs[i])
  
  # https://github.com/jkortner/clarifai
  d <- face2race(jpg_path, api_key)
  d[['jpg']] <- jpgs[i]
  d_list <- c(d_list, d)
  
}

df <- d_list_to_df(d_list)
write.csv(df, 'predictors_of_polling_errors/data/senate/clarifai_demographics.csv')

# post-process data (race / gender) ----------------------------------------- #

df$clarifai_gender <- names(df)[4:5][max.col(df[4:5], 'first')]
df$clarifai_race <- names(df)[6:12][max.col(df[6:12], 'first')]

df <- df %>% 
  select(jpg, clarifai_gender, clarifai_race)

# merge --------------------------------------------------------------------- #

sen_faces <- merge(x = sen_faces, y = df, by = "jpg", all.x = TRUE)
write.csv(sen_faces, 'predictors_of_polling_errors/data/senate/clarifai_race.csv')
