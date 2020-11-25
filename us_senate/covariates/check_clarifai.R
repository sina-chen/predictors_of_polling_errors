# --------------------------------------------------------------------------- #
# Make checked Race / Gender Prediction
# --------------------------------------------------------------------------- #

library(tidyverse)

sen_faces_clarifai <- read.csv('data/us_senate/clarifai_race.csv')
sen_faces_john <- read.csv('data/us_senate/manually_race_gender_john.csv', sep = ';')

# --------------------------------------------------------------------------- #
sen_faces_clarifai <- sen_faces_clarifai %>%
  mutate(clarifai_race = replace(clarifai_race, clarifai_race == 'Alan Schlesinger', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Dan Bongino', 'hispanic'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Jim Huffman', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Linda Smith', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Lou Barletta', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Philip Giordano', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Rick Lazio', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Robert Lorge', 'hispanic'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Barbara Boxer', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Bob Menendez', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Catherine Cortez Masto', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Harold Ford Jr.', 'black'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Ken Salazar', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Paul Hodes', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Rick Noriega', 'white'),
         clarifai_race = replace(clarifai_race, clarifai_race == 'Sam Granato', 'white')) %>%
  as.data.frame()

sen_faces_clarifai <- sen_faces_clarifai %>%
  mutate(clarifai_gender = replace(clarifai_gender, clarifai_gender == 'Misty K. Snow', 'feminine'),
         clarifai_gender = replace(clarifai_gender, clarifai_gender == 'Slade Gorton', 'masculine')) %>%
  as.data.frame()

sen_faces_clarifai <- sen_faces_clarifai %>%
  rename(gender_checked = clarifai_gender, race_checked = clarifai_race)

write.csv(sen_faces_clarifai, "data/us_senate/race_gender_checked.csv")

# --------------------------------------------------------------------------- #
