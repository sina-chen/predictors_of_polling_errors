# --------------------------------------------------------------------------- #
# Make checked Race / Gender Prediction
# --------------------------------------------------------------------------- #

library(tidyverse)
library(ggplot2)
library(cvms)

sen_faces_clarifai <- read.csv('predictors_of_polling_errors/data/us_senate/clarifai_labels.csv')
sen_faces_human <- read.csv('predictors_of_polling_errors/data/us_senate/human_labels.csv', sep = ";")

sen_faces <- merge(x = sen_faces_clarifai, y = sen_faces_human, by = "jpg", all.x = TRUE)
sen_faces <- sen_faces %>% 
  select(candidate.x, jpg, party, clarifai_gender, clarifai_race, gender, race)
sen_faces <- rename(sen_faces, candidate = candidate.x)
# --------------------------------------------------------------------------- #

# clean: drop candidates not used in subsequent steps
drop_list <- c( "abby_broyles.jpg", "adrian_perkins.jpg", "alexi_giannoulias.jpg", "allen_waters.jpg", 
  "ben_sasse.jpg", "bill_hagerty.jpg", "charles_e_summers_jr.jpg", "chris_janicek.jpg", "cynthia_dill.jpg", 
  "dan_ahlers.jpg", "foster_campbell.jpg", "jean_carnahan.jpg", "jo_rae_perkins.jpg", "jon_ossoff.jpg", 
  "kelly_loeffler.jpg", "kevin_o'connor.jpg", "mack_mattingly.jpg", "mark_curran.jpg", "mark_kelly.jpg", 
  "marquita_bradshaw.jpg", "martha_coakley.jpg", "paula_jean_swearengin.jpg", "paulette_jordan.jpg", 
  "raphael_warnock.jpg", "ricky_dale_harrington.jpg", "shelley_moore_capito.jpg", "zell_miller.jpg")

sen_faces <- sen_faces[!sen_faces$jpg %in% drop_list, ]

# --------------------------------------------------------------------------- #

# gender
conf_mat_gender <- confusion_matrix(targets = sen_faces$gender,
                                    predictions = sen_faces$clarifai_gender)

plot_confusion_matrix(conf_mat_gender, add_sums = TRUE, palette = "Greens")

sen_faces[sen_faces$clarifai_gender != sen_faces$gender, ]

# race
conf_mat_race <- confusion_matrix(targets = sen_faces$race,
                             predictions = sen_faces$clarifai_race)

plot_confusion_matrix(conf_mat_race, add_sums = TRUE, palette = "Greens")

sen_faces[sen_faces$clarifai_race != sen_faces$race, ]

# --------------------------------------------------------------------------- #

sen_faces_checked <- sen_faces
sen_faces_checked$gender_checked <- sen_faces$gender
sen_faces_checked$race_checked <- sen_faces$race
  
sen_faces_checked <- sen_faces_checked %>%
  mutate(race_checked = replace(race_checked, candidate == 'Marco Rubio', 'Cuban'),
         race_checked = replace(race_checked, candidate == 'Ted Cruz', 'Cuban'),
         race_checked = replace(race_checked, candidate == 'Bob Menendez', 'Cuban'),
         race_checked = replace(race_checked, candidate == 'Dan Bongino', 'Italian'),
         race_checked = replace(race_checked, candidate == "Al D'Amato", 'Italian'),
         race_checked = replace(race_checked, candidate == 'Lou Barletta', 'Italian'),
         race_checked = replace(race_checked, candidate == 'Daniel Mongiardo', 'Italian'),
         race_checked = replace(race_checked, candidate == 'Philip Giordano', 'Italian'),
         race_checked = replace(race_checked, candidate == 'Rick Lazio', 'Italian'),
         race_checked = replace(race_checked, candidate == 'Mark Ronchetti', 'Italian'),
         race_checked = replace(race_checked, candidate == 'Pete Domenici', 'Italian'),
         race_checked = replace(race_checked, candidate == 'Gloria Tristani', 'Latino_Hispanic'),
         race_checked = replace(race_checked, candidate == 'Spencer Abraham', 'White'),
         race_checked = replace(race_checked, candidate == 'Sara Gideon', 'Indian')) %>%
  as.data.frame()

write.csv(sen_faces_checked, 'predictors_of_polling_errors/data/us_senate/race_gender_checked.csv')

# --------------------------------------------------------------------------- #

# overview

unique(sen_faces_checked$race_checked)

sen_faces_checked <- sen_faces_checked %>% 
  select(candidate, party, race_checked, gender_checked)

sen_faces_checked[sen_faces_checked$race_checked == "White", ]
sen_faces_checked[sen_faces_checked$race_checked == "Black", ]
sen_faces_checked[sen_faces_checked$race_checked == "Latino_Hispanic", ]
sen_faces_checked[sen_faces_checked$race_checked == "Cuban", ]
sen_faces_checked[sen_faces_checked$race_checked == "Italian", ]
sen_faces_checked[sen_faces_checked$race_checked == "East Asian", ]
sen_faces_checked[sen_faces_checked$race_checked == "Southeast Asian", ]

