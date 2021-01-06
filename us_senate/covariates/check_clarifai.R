# --------------------------------------------------------------------------- #
# Make checked Race / Gender Prediction
# --------------------------------------------------------------------------- #

library(tidyverse)

sen_faces_clarifai <- read.csv('predictors_of_polling_errors/data/us_senate/clarifai_race.csv')
# sen_faces_john <- read.csv('data/us_senate/manually_race_gender_john.csv', sep = ';')

# --------------------------------------------------------------------------- #
sen_faces_clarifai <- sen_faces_clarifai %>%
  mutate(clarifai_race = replace(clarifai_race, candidate == 'Alan Schlesinger', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Dan Bongino', 'hispanic'),
         clarifai_race = replace(clarifai_race, candidate == 'Jim Huffman', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Linda Smith', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Lou Barletta', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Philip Giordano', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Rick Lazio', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Robert Lorge', 'hispanic'),
         clarifai_race = replace(clarifai_race, candidate == 'Barbara Boxer', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Bob Menendez', 'white'), # or hispanic
         clarifai_race = replace(clarifai_race, candidate == 'Catherine Cortez Masto', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Harold Ford Jr.', 'black'),
         clarifai_race = replace(clarifai_race, candidate == 'Ken Salazar', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Paul Hodes', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Rick Noriega', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Sam Granato', 'white'),
         # 2020
         clarifai_race = replace(clarifai_race, candidate == 'Ben Ray Lujan', 'hispanic'),
         clarifai_race = replace(clarifai_race, candidate == 'Dan Ahlers', 'white'),
         clarifai_race = replace(clarifai_race, candidate == 'Lauren Witzke', 'white')) %>%
  as.data.frame()

sen_faces_clarifai <- sen_faces_clarifai %>%
  mutate(clarifai_gender = replace(clarifai_gender, candidate == 'Misty K. Snow', 'feminine'),
         clarifai_gender = replace(clarifai_gender, candidate == 'Slade Gorton', 'masculine'),
         # 2020
         clarifai_gender = replace(clarifai_gender, candidate == 'Ben Ray Lujan', 'masculine'),
         clarifai_gender = replace(clarifai_gender, candidate == 'Dan Ahlers', 'masculine'),
         clarifai_gender = replace(clarifai_gender, candidate == 'Lauren Witzke', 'feminine'),
         clarifai_gender = replace(clarifai_gender, candidate == 'Merav Ben-David', 'feminine'),) %>%
  as.data.frame()

sen_faces_clarifai <- sen_faces_clarifai %>%
  rename(gender_checked = clarifai_gender, race_checked = clarifai_race)

write.csv(sen_faces_clarifai, "predictors_of_polling_errors/data/us_senate/race_gender_checked.csv")

# --------------------------------------------------------------------------- #
