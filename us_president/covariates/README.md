### Instructions to merge/generate additional covariates

Run the add_variables.R script to merge/generate additional covariates used in the analysis. Note that 2020 is not included in the analysis as of yet. Hence, information on several covariates is only included up to 2016 (see below for more information).    
Running this script will create the following variables:

- state: state abbreviation
- dte: temporal distance between last day of field period and election day in days
- dte_sc: between 0 and 1 scaled days to election (based on data up to 365 days before election)
- state_year: state-level election identifier
- rep_poll: Rep. poll vote share as decimal (rescaled)
- dem_poll: Dem. poll vote share as decimal (rescaled)
- rep_poll2: two-party Rep. poll vote share as decimal
- dem_poll2: two-party Dem. poll vote share as decimal
- undecided: share of respondents answering being undecided as decimal (rescaled)
- refused: share of poll refusals as decimal (rescaled)
- third_party: share of third party poll votes as decimal (rescaled)
- other: share of other poll responses as decimal (rescaled)
- rep_result: Rep. election vote share as decimal from 2000 to 2016
- dem_result: Dem. election vote share as decimal from 2000 to 2016
- rep_result2: two-party Rep. election vote share as decimal from 2000 to 2016
- dem_result2: two-party Dem. election vote share as decimal from 2000 to 2016
- resp-formated: formated respondent type: LV = likely voters, RV = registered voters, Statewide = statewide polls
- turnout: turnout  from 2000 to 2016
- diff_to_prev: difference in turnout to previous state-level election from 2000 to 2016
- to_mean: average state-level turnout from 2000 to 2016
- ev: number of electoral college votes from 2000 to 2016
- ev_sc: between 0 and 1 scaled electoral college votes from 2000 to 2016
- after_rep: dummy indicating whether the poll was conducted after (0) or before (1) the Rep. national convention
- after_dem: dummy indicating whether the poll was conducted after (0) or before (1) the Dem. national convention
- swing: dummy indicating safe states (0) and swing states (1)  
