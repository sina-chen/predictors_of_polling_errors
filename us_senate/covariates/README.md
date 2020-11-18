# Instructions for cleaning polls and merging covariates on gender and ethnicity

**Important**: Run scripts in the outlined order.

It is assumed that THIS is your working directory.


## Detailed description of wikipedia covariate code

In addition to merging the datasets into one and cleaning them, some manipulation is done.
Data cleaning includes the removal of elections where more than one candidate from the two major parties runs for elections and ends up in the first three places in the election. This restriction is included, as the primary interest lies in the two party competition between Republicans and Democrats. Moreover, elections with two candidates from the same party on the final ballot are extremely rare due to election laws in the USA (except e.g. California).

One covariate of interest is gender, which is predicted with the [R-Package "gender"](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html). Thereby the surnames from the Wikipedia data are used.


- dem_name: string with full name of democratic candidate
- rep_name: string with full name of republican candidate
- no_candidates: integer with the number of candidates which received votes in election
- senator: name of incumbent
- incumbency: dummy variable if incumbent runs for re-election
- rep_gender: predicted gender of republican candidate
- dem_gender: predicted gender of democratic candidate

To add covariates to the senate scrapes you first run the script [preprocess_wiki_data.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/covariates/preprocess_wiki_data.R) and the afterwards the script [predict_gender_name.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/covariates/predict_gender_name.R). These scripts generate tables with covariates (including gender of candidates) to each senate election since 1998. For more details, see the readme file in the respective folder.
