#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Fit German Landtag election polls 1994 - 2021
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
}


# Data --------------------------------------------------------------------

polls <- readRDS('~/data/polls_lt_1994_2021.RDS')


# Pre-processing ----------------------------------------------------------

# subset polls
polls <- polls %>% 
  subset(!is.na(support) & 
           !is.na(date) &
           support != 0) %>% 
  group_by(poll_id) %>% 
  mutate(n_party = n()) %>% 
  ungroup() %>% 
  subset(n_party == 6) %>% 
  droplevels() %>% 
  mutate(poll_id_int = as.integer(as.factor(poll_id)),
         party = factor(party, levels = c("cdu", "fdp", "gru", "lin","spd",
                                          "oth_afd")),
         k_id = as.integer(party),
         t_sc = days_to_election/max(days_to_election))

# order by poll id and party
polls <- polls[order(polls$poll_id_int, polls$party),]

# specify levels for election-party id
order_kr <-paste0(rep(unique(polls$r),
                      each = 6),
                  rep(c("cdu", "fdp", "gru", "lin","spd", "oth_afd"), 71))

# compute election-party id
polls <- polls %>%
  mutate(kr = factor(paste0(r, party), levels = order_kr),
         kr_id = as.integer(kr),
         t_sc = as.numeric(days_to_election)/max(as.numeric(days_to_election))) 

# poll level data
poll_level <- polls %>% 
  group_by(poll_id_int, election_year, r, r_int, t_sc, sample_size) %>% 
  summarise() %>% 
  ungroup()

# election-party level data
election_party_level <- polls %>%  
  group_by(r, party, voteshare, kr, kr_id, k_id) %>%  
  summarise()

# create b-splines design matrix
B <- t(bs(poll_level$t_sc, knots = quantile(poll_level$t_sc, 
                                            probs= seq(0.1,0.9,0.1)), 
          degree = 3,
          Boundary.knots = c(0,1)))

# zero matrix for multivariate normal
# zero_Kr <- matrix(nrow = length(unique(polls$r)), 
#                  ncol = length(unique(polls$party)), 0)
zero_r <- matrix(nrow = length(unique(polls$r)), 
                 ncol = length(unique(polls$party))-1, 0)

# Model input -------------------------------------------------------------

stan_dat <- list(
  
  N = nrow(polls),                           # number of party-poll estimates
  K = length(unique(polls$party)),           # number of parties
  R = length(unique(polls$r)),               # number of elections
  P = length(unique(polls$poll_id_int)),     # number of polls
  KR = length(unique(polls$kr)),             # number of party-election results
  n_B = nrow(B),                             # number of b-splines
  
  B = B,                                     # matrix of B-splines
  n = poll_level$sample_size,                # sample size
  poll = polls$support/100,                  # poll support in percentage
  vote = election_party_level$voteshare/100, # election result in percentage
  
  zero_r = zero_r,                             # vector of zeros mvn
  # zero_Kr = zero_Kr,                             # vector of zeros mvn

  kr_id = polls$kr_id,                       # party-election id
  r_id = poll_level$r_int,                   # election id for each poll
  k_id = polls$k_id,                         # party id for each party-poll estimate
  p_id = polls$poll_id_int                   # institute id for each poll
  
)

# check model input
sapply(stan_dat, length)
sapply(stan_dat, range)


# Fit stan model ----------------------------------------------------------

resStan <- stan(file = "~/fit_stan/stan_ml/ml_ltw_splines_final.stan", 
                data = stan_dat,
                chains = 3, iter = 5000,
                seed = 9421,
                control = list(adapt_delta = 0.95, max_treedepth = 12)
) 

# launch_shinystan(resStan)
saveRDS(resStan, '~/fit_stan/resStan_ltw_splines_final.RDS') # xx divergencies with delta 0.95 and treedepth 12
