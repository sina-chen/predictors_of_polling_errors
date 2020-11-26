### Code for scraping US presidential election polls 

This folder contains code for scraping US presidential pre-election polls from 2000 to 2020 published on [pollingreport.us](https://pollingreport.us).

To run the following script you need to subscribe to [pollingreport.us](https://pollingreport.us). Save your username and password as "usr_pwd.txt" in your working directory in the following format: username:password.
First you need to run the [downloadscript_pres.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_president/scrape/downloadscript_pres.R). The single websites with polling results  will be downloaded and stored separately for every year and state/group of states as html files. The folder structure is year/html/. In a next step, to convert these html files to txt for further processing run the [convert_txt_pres.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_president/scrape/convert_txt_pres.R) script. Running this script will create the subfolder "txt" within every year folder (year/txt/) with one txt file per state. To extract the relevant information and combine all in one data frame run [extract_pres.R](https://github.com/SinaMaria412/predictors_of_polling_errors/blob/master/us_president/scrape/extract_pres.R).
![workflow](scrape_pres_workflow.png)

This data frame will include information on:

- election_year: year, election took place
- date: last date of field period
- pollFirm: name of polling firm
- n: sample size (if reported)
- respondents: reported respondents, unformatted
- dem_candidate: reported name of Rep. candidate
- rep_candidate: reported name of Dem. candidate
- rep_poll: Rep. poll vote share in percentage
- dem_poll: Dem. poll vote share in percentage 
- undecided: share of respondents answering being undecided in percentage (if reported)
- refused: share of poll refusals in percentage (if reported)
- third_party: share of third party poll votes in percentage (if reported)
- other: share of other poll responses in percentage (if reported)
- MoE: reported margin of error
- states_long: state name
