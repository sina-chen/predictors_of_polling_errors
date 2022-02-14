#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Fit German Bundestag election polls 2013-2021 
#     with AfD separately,
#     with redundantly-parameterized normal dist and
#     with splines
#
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(splines)
  library(dplyr)
  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  library(shinystan)
  library(reshape2)
}


# Data --------------------------------------------------------------------

polls <- readRDS('~/data/polls2013_2021.RDS')


# Pre-processing ----------------------------------------------------------

# subset polls
poll_id_afd <- polls[which(polls$party == "afd" & 
                             !is.na(polls$support)),]$poll_id 

polls <- polls %>% 
  subset(poll_id %in% poll_id_afd &
           !is.na(support) & 
           !is.na(date) & 
           election < 2025) %>% 
  group_by(poll_id) %>% 
  mutate(n_party = n()) %>% 
  ungroup() %>% 
  subset(n_party == 7 
  ) %>% 
  droplevels() %>% 
  mutate(poll_id_int = as.integer(as.factor(poll_id)))

rm(poll_id_afd)

# order by poll id and party
polls <- polls[order(polls$poll_id_int, polls$party),]

# specify levels for election-party id
order_kr <-paste0(rep(c( 2013, 2017, 2021), 
                      each = 7),
                  rep(c("cdu", "fdp", "gru", "lin","spd", "afd", "oth"), 3)) 

# compute election-party id
polls <- polls %>% 
  mutate(kr = factor(paste0(election, party), levels = order_kr),
         kr_id = as.integer(kr),
         l_id = as.integer(as.factor(institute)),
         rl = paste0(election, institute),
         rl_id = as.integer(as.factor(rl)),
         t_sc = as.numeric(days_to_election)/max(as.numeric(days_to_election)),
         r_id = case_when(election == 2013 ~ 1,
                          election == 2017 ~ 2,
                          election == 2021 ~ 3),
         k_id = as.integer(factor(party, levels = c("cdu", "fdp", "gru", "lin",
                                                    "spd", "afd", "oth"))))  
# poll level data
poll_level <- polls %>% 
  group_by(poll_id_int, election, institute, t_sc, sample_size, r_id, l_id) %>% 
  summarise() %>% 
  ungroup() 

# election-party level data
election_party_level <- polls %>%  
  group_by(election, party, voteshare, kr, kr_id, k_id, r_id) %>%  
  summarise()

election_party_level <- election_party_level[order(election_party_level$kr_id),] # order election party level data

# create b-splines design matrix
B <- t(bs(poll_level$t_sc, knots = quantile(poll_level$t_sc, 
                                            probs= seq(0.1,0.9,0.1)), 
          degree = 3,
          Boundary.knots = c(0,1)))


# generate 0 matrices for multivariate distributed party coefficients
zero_r <- matrix(nrow = length(unique(polls$election)), 
                 ncol = length(unique(polls$party)) - 1, 0)

zero_l <- matrix(nrow = length(unique(polls$institute)),
                 ncol = length(unique(polls$party)) - 1, 0)

# Model input -------------------------------------------------------------

stan_dat <- list(
  
  N = nrow(polls),                           # number of party-poll estimates
  K = length(unique(polls$party)),           # number of parties
  R = length(unique(polls$election)),        # number of elections
  P = length(unique(polls$poll_id_int)),     # number of polls
  L = length(unique(polls$institute)),       # number of institutes
  KR = length(unique(polls$kr)),             # number of party-election results
  n_B = nrow(B),                             # number of b-splines
  
  B = B,                                     # matrix of B-splines
  n = poll_level$sample_size,                # sample size at poll level
  poll = polls$support/100,                  # poll support in percentage
  vote = election_party_level$voteshare/100, # election result in percentage
  
  kr_id = polls$kr_id,                       # party-election id
  r_id = poll_level$r_id,                    # election id for each poll
  l_id = poll_level$l_id,                    # institute id for each poll
  p_id = polls$poll_id_int,                  # poll id for each party-poll estimate
  k_id = polls$k_id,                         # party id for each party-poll estimate
  
  zero_r = zero_r,                           # zero mean matrix for mvn distribution
  zero_l = zero_l                            # zero mean matrix for mvn distribution
  
)

# check model input
sapply(stan_dat, length)
sapply(stan_dat, range)


# Fit stan model ----------------------------------------------------------

resStan <- stan(file = "~/fit_stan/stan_ml/ml_btw_splines_final.stan", 
                data = stan_dat,
                chains = 3, iter = 10000,
                #warmup = 5000,
                seed = 9421,
                control = list(adapt_delta = 0.99, max_treedepth = 12)
) 

#launch_shinystan(resStan)
saveRDS(resStan, '~/fit_stan/resStan_btw13_21_redundant_splines_final.RDS') # 25 divergencies with delta 0.98 and treedepth 12

