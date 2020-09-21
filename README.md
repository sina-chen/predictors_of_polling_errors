# predictors_of_poling_errors
The aim of this project is to develop a contextual understanding of polling errors and their triggers. Unlike most previous studies, we take a cross-election comparative perspective and put the theoretical focus on characteristics of the electoral contest which may encourage polling errors.

**Important**

Run scripts in following order:
- 
- 
- 

It is assumed that THIS is your working directory.


This readme is structured as follows:

- [US President](https://github.com/SinaMaria412/predictors_of_polling_errors#us-president): Code and explanation for presidential elections
- [US Senate](https://github.com/SinaMaria412/predictors_of_polling_errors#us-senate): Code and explanation for senate elections

## US President

To run the folowiung script you need to subscribe to pollingreport.us and save your username and password in "usr_pwd.txt" (username:password).
First you need to run the [downloadscript_pres.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_president/scrape/downloadscript_pres.R). The websites with polling results from will be downloaded and stored separately for every year and state/group of states as html files. The folder structure is year/html/. In a next step, to convert these html files to txt for further processing run the [convert_txt_pres.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_president/scrape/convert_txt_pres.R) script. Running this script will create the subfolder "txt" within every year folder (year/txt/) with ine txt file per state. To extract the relvant information and combina all in one data frame run [extract_pres.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_president/scrape/extract_pres.R). This data frame includes information on :

- state: state abbreviation
- election_year: year, election took place
- date: last date of field period
- pollFirm: name of polling firm
- n: sample size (if reported)
- respondents: reported respondents, unformatted
- dem_candidate: reported name of Rep. candidaterep_candidate: reported name of Dem. candidate
- rep_poll: Rep. poll vote share 
- dem_poll: Dem. poll vote share 
- rep_poll2: two-party Rep. poll vote share 
- dem_poll2: two-party Dem. poll vote share 
- undecided: share of respondents answerung being undecided (if reported)
- refused: share of poll refusals (if reported)
- third_party: share of third party poll votes (if reported)
- other: share of other poll repsonses (if reported)
- MoE: reported margin of error
- states_long: state name
- dte: temportál distabce between last day of fueld period and election day in days
- state_year: state-level election identifier
- rep_result: Rep. election vote share
- dem_result: Dem. election vote share
- rep_result2: two-party Rep. election vote share
- dem_result2: two-party Dem. election vote share
- resp-formated: formated respondents type: LV = likely voters, RV = registered voters, Statewide = statewide polls
- turnout: turnout
- diff_to_prev: differemnce in turnout to previous state-level election
- to_mean: average state-level turnout from 2000 to 2016

In the [add_variables.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_president/scrape/add_variables.R) file, additional information is added:

- ev: number of electoral college votes
- ev_sc: between 0 and 1 scaled eelctoral college votes
- after_rep: dummy indicating whether the poll was conducted after (0) or before (1) the Rep. national convention
- after_dem: dummy indicating whether the poll was conducted after (0) or before (1) the Dem. national convention
- swing: dummy indicating swing states (1) and safe states (0)
- dte_sc: between 0 and 1 scaled days to election (based on data up to 365 days before election)

## US Senate

To run the folowiung script you need to subscribe to pollingreport.us and save your username and password in "usr_pwd.txt" (username:password). First you need to run the [downloadscript_senate1998_2018.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/scrape/downloadscript_senate1998_2018.R). The websites with senate polling results from 1998 to 2018 will be downloaded and stored separately for every year and state/group of states as html files. The folder structure is year/html/. In a next step, to convert these html files to txt for further processing run the [convert_txt_senate.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/scrape/convert_txt_senate.R) script. Running this script will create the sub folder "txt" within every year folder (year/txt/) with one txt file per state. To extract the relevant information and combine all in one data frame run [extract_senate.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/scrape/extract_senate.R). The files [helper_func_senate.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/scrape/helper_func_senate.R) and [helper_func_subset_senate.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/scrape/helper_func_subset_senate.R) include necessary functions to extract relevant information from the txt files and combine this information a data frame. This data frame includes information on:



To add covariates to the senate scrapes you run the script [senate_covariates_wikipedia.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_senate/covariates/senate_covariates_wikipedia.R). This script reads in tables from Wikipedia from all US Senate elections since 1998. These tables include information about final election results, name and number of candidates and incumbency. 

In addition to merging the datasets into one and cleaning them, some manipulation is done.
Data cleaning includes the removal of elections where more than one candidate from the two major parties runs for elections and ends up in the first three places in the election. This restriction is included, as the primary interest lies in the two party competition between Republicans and Democrats. Moreover, elections with two candidates from the same party on the final ballot are extremely rare due to election laws in the USA (except e.g. California).

One covariate of interest is gender, which is predicted with the [R-Package "gender"](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html). Thereby the surnames from the Wikipedia Data are used.

Finally the covariates are merged to the scraped polls and yield a dataset with the already described variables and:

- dem_name: string with full name of democratic candidate
- rep_name: string with full name of republican candidate
- no_candidates: integer with the number of candidates which received votes in election
- senator: name of incumbent
- incumbency: dummy variable if incumbent runs for re-election
- rep_gender: predicted gender of republican candidate
- dem_gender: predicted gender of democratic candidate
