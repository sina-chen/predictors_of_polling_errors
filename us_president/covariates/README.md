### Instructions to merge/generate additional covariates

Run the add_variables.R script to merge/generate additional covariates:

- ev: number of electoral college votes
- ev_sc: between 0 and 1 scaled electoral college votes
- after_rep: dummy indicating whether the poll was conducted after (0) or before (1) the Rep. national convention
- after_dem: dummy indicating whether the poll was conducted after (0) or before (1) the Dem. national convention
- swing: dummy indicating swing states (1) and safe states (0)
- dte_sc: between 0 and 1 scaled days to election (based on data up to 365 days before election)