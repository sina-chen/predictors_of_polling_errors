#-------------------------------------------------------------------------------
# Fit German Bundestag election polls 1990 - 2017
#
# Author: Sina Chen
#
#-------------------------------------------------------------------------------

#### Libraries ####

library(readr)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)
library(reshape2)


#### Data ####

# poll data
polls <- readRDS('~/data/polls1990_2017.RDS')


#-------------------------------------------------------------------------------

#### Pre-processing ####

# subset time window
polls <- polls %>% 
  subset(institute != 'civey' & election == 1990 & date >= as.Date('1990-01-01', '%Y-%m-%d')|
           institute != 'civey' & election == 1994 & date >= as.Date('1994-01-01', '%Y-%m-%d')|
           institute != 'civey' & election == 1998 & date >= as.Date('1998-01-01', '%Y-%m-%d')|
           institute != 'civey' & election == 2002 & date >= as.Date('2002-01-01', '%Y-%m-%d')|
           institute != 'civey' & election == 2005 & date >= as.Date('2005-01-01', '%Y-%m-%d')|
           institute != 'civey' & election == 2009 & date >= as.Date('2009-01-01', '%Y-%m-%d')|
           institute != 'civey' & election == 2013 & date >= as.Date('2013-01-01', '%Y-%m-%d')|
           institute != 'civey' & election == 2017 & date >= as.Date('2017-01-01', '%Y-%m-%d'))

# define order of groups
order_kr <- unique(sort(paste0(polls$party,':',polls$election)))
order_ki <- unique(sort(paste0(polls$party,':',polls$institute)))

# add party and party-election id
polls <- polls %>% 
  mutate(k_int = as.integer(as.factor(party)),
         kr = factor(paste0(party, ':', election), levels = order_kr),
         kr_int = as.integer(kr),
         ki = factor(paste0(party, ':', institute), levels = order_ki),
         ki_int = as.integer(ki))

# subset result data
results <- polls[,c('election', 'party', 'voteshare')] %>%  unique()


## generate 0 matrices for multivariate distributed party coefficients

zero1 <- matrix(nrow = 1, 
                ncol = length(unique(polls$party)), 0)

zero2 <- matrix(nrow = length(unique(polls$election)), 
               ncol = length(unique(polls$party)), 0)

zero3 <- matrix(nrow = length(unique(polls$institute)), 
                ncol = length(unique(polls$party)), 0)


#-------------------------------------------------------------------------------

#### Fit Model ####

# Stan data 
stan_dat <- list(
  
  N = nrow(polls),
  K = length(unique(polls$party)),
  R = length(unique(polls$election)),
  I = length(unique(polls$institute)),
  KR = length(unique(polls$kr)),
  KI = length(unique(polls$ki)),
  
  poll = polls$support/100,
  vote = results$voteshare/100,

  k_id = polls$k_int, 
  kr_id = polls$kr_int,
  ki_id = polls$ki_int,
  
  n = polls$sample_size,
  t = as.integer(polls$days_to_election)/max(as.integer(polls$days_to_election)),

  zero1 = zero1,
  zero2 = zero2,
  zero3 = zero3

)

sapply(stan_dat, length)
sapply(stan_dat, range)

#### Fit stan model ####

resStan <- stan(file = "~/fit_stan/stan_ml/ml_bundestag.stan", 
                data = stan_dat,
                chains = 8, iter = 5000,
                control = list(adapt_delta = 0.95,  max_treedepth = 12)
) 

#launch_shinystan(resStan)

 saveRDS(resStan, '~/fit_stan/resStan_bundestag01_1990_2017_inst.RDS') # 138 divergencies
