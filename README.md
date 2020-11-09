# predictors_of_poling_errors
The aim of this project is to develop a contextual understanding of polling errors and their triggers. Unlike most previous studies, we take a cross-election comparative perspective and put the theoretical focus on characteristics of the electoral contest which may encourage polling errors.

This repository is structured as follows:

- All data files are in the [data](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/data) folder. 
- The [us_president](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_president) contains code for
  - scraping US presidential polls from 2000 to 2016 in the [scrape](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_president/scrape) folder
  - adding covariates in the [covariates](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_president/covariates) folder
- The [us_senate](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate) folder contains code for:
  - scraping US senate polls from 1998 to 2018 in the [scrape](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/scrape) folder
  - cleaning the scraped polls and merging additional covariates in the [covariates](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/covariates) folder
  - a hierarchical bayesian model for analysing us senate polls [analysis](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/analysis).
  
Detailed descriptions for all code files can be found in the respective folders. 









To add covariates to the senate scrapes you first run the script [preprocess_wiki_data.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/covariates/preprocess_wiki_data.R) and the afterwards the script [predict_gender_name.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/covariates/predict_gender_name.R). These scripts generate tables with covariates (including gender of candidates) to each senate election since 1998. For more details, see the readme file in the respective folder.


