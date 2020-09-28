# --------------------------------------------------------------------------- #
# US Senate - Faces
# --------------------------------------------------------------------------- #

library(tidyverse)
# library(stringr)

# prepare data -------------------------------------------------------------- #

sen_faces <- read.csv('sen_faces.csv')
sen_faces <- sen_faces %>% 
  select(candidate, jpg, party)

# classify images ----------------------------------------------------------- #

jpgs <- list.files(path = "portraits")
d_list <- list()
pb <- progress_bar$new(total = length(jpgs), show_after = 0)
for (i in 1:length(jpgs)){
  pb$tick()
  jpg_path = paste0('portraits/', jpgs[i])
  
  # https://github.com/ybml/clarifai
  d <- face2race(jpg_path, api_key)
  d[['jpg']] <- jpgs[i]
  d_list <- c(d_list, d)
  
}

df <- d_list_to_df(d_list)
# write.csv(df, 'clarifai_demographics.csv')

# post-process data (race / gender) ----------------------------------------- #

df$clarifai_gender <- names(df)[4:5][max.col(df[4:5], 'first')]
df$clarifai_race <- names(df)[6:12][max.col(df[6:12], 'first')]

df <- df %>% 
  select(jpg, clarifai_gender, clarifai_race)

# merge --------------------------------------------------------------------- #

sen_faces <- merge(x = sen_faces, y = df, by = "jpg", all.x = TRUE)
# write.csv(sen_faces, 'clarifai_race.csv')
