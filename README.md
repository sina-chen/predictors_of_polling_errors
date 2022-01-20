# predictors_of_poling_errors
The aim of this project is to develop a contextual understanding of polling errors and their triggers. Unlike most previous studies, we take a cross-election comparative perspective and put the theoretical focus on characteristics of the electoral contest which may encourage polling errors.

This repository is structured as follows:

- All data files are in the [data](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/data) folder. There is one subfolder for each election type investigated.
- The [us_president](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_president) folder contains code for
  - scraping US presidential polls from 2000 to 2016 in the [scrape](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_president/scrape) folder
  - adding covariates in the [covariates](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_president/covariates) folder
  - a hierarchical bayesian model for analysing us presidential polls in the [analysis](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_president/analysis) folder.

- The [us_senate](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate) folder contains code for:
  - scraping US senate polls from 1998 to 2020 in the [scrape](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/scrape) folder
  - analysis of US Senate candidate characteristics cen be found in the [candidate_characteristics](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/candidate_characteristics) folder
    - cleaning the scraped polls and merging additional covariates in the [covariates](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/candidate_characteristics/covariates) folder
    - a hierarchical bayesian model for analysing us senate polls in the [analysis](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/analysis) folder.
  -  analysis of Trumpists can be found in the [trumpists](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/us_senate/trumpists) folder
- The [german_bundestag](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/german_bundestag) folder contains code for:
  - scraping German Bundestag electiopn polls from 1994 to 2021 in the [scrape](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/german_bundestag/scrape) folder
  - a hiearchical bayesian model for analysing German Bundestag polls in the [analysis](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/german_bundestag/analysis) folder

- The [german_landtag](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/german_landtag) folder contains code for:
  - scraping German Landtag election polls from 1994 to 2021 in the [scrape](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/german_bundestag/scrape) folder
  - a hiearchical bayesian model for analysing German Landtag polls in the [analysis](https://github.com/SinaMaria412/predictors_of_polling_errors/tree/master/german_landtag/analysis) folder

  
Detailed descriptions for all code files can be found in the respective folders. 












