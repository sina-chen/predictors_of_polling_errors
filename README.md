# predictors_of_poling_errors
The aim of this project is to develop a contextual understanding of polling errors and their triggers. Unlike most previous studies, we take a cross-election comparative perspective and put the theoretical focus on characteristics of the electoral contest which may encourage polling errors.

**Important**: Run scripts in following order:





It is assumed that THIS is your working directory.


This readme is structured as follows:

- [US President](https://github.com/SinaMaria412/predictors_of_polling_errors#us-president): Code and explanation for presidential elections
- [US Senate](https://github.com/SinaMaria412/predictors_of_polling_errors#us-senate): Code and explanation for senate elections

## US President



## US Senate




To add covariates to the senate scrapes you first run the script [preprocess_wiki_data.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/covariates/preprocess_wiki_data.R) and the afterwards the script [predict_gender_name.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/covariates/predict_gender_name.R). These scripts generate tables with covariates (including gender of candidates) to each senate election since 1998. For more details, see the readme file in the respective folder.


### Analysis

Excluded: 

- elections with more than one candidate per party
- elections with less than 4 polls?
- special elections?
