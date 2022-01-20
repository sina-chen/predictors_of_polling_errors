#-------------------------------------------------------------------------------
# Trumpists did not fare worse in the polls: results visualization
# 
# Author: Sina Chen
#
#-------------------------------------------------------------------------------

#### Libraries ####

library(rv) 
library(gtools) 
library(tidyverse) 
library(rstan) 
library(scales)
library(extrafont)
library(gridExtra)


#### Data #### 

# polls
polls <- readRDS("~/data/senate_polls1998_2020_score.RDS") 

# stan fit obj.
resStan_end <- readRDS('~/fit_stan/resStan_senate_end_trump.RDS')
resStan_inc <- readRDS('~/fit_stan/resStan_senate_inc_trump.RDS')
resStan_bat <- readRDS('~/fit_stan/resStan_senate_bat_trump.RDS')
resStan_end_inc <- readRDS('~/fit_stan/resStan_senate_end_trump_inc.RDS')
resStan_end_bat <- readRDS('~/fit_stan/resStan_senate_end_trump_battle.RDS')


#### Functions ####

# extra rv functins 
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)
exp.rv <- function(x) rvmapply(FUN = exp, x)
log.rv <- function(x) rvmapply(FUN = log, x)


#-------------------------------------------------------------------------------

#### Pre-processing #### 

# polls

## subset relevant polls and generate endorsement variable
polls <- polls  %>%
  mutate(n = na_if(n, '-'))  %>%
  subset(is.na(n) == F & election_year >= 2018)  %>% 
  mutate(state_year = paste0(state, election_year),
         state_year_int = as.integer(as.factor(state_year)),
         end_trump = if_else(state == 'WV' & election_year == 2020 |
                               state == 'LA' & election_year == 2020|
                               state == 'TX' & election_year == 2020|
                               state == 'MT' & election_year == 2020|
                               state == 'IA' & election_year == 2020|
                               state == 'CO' & election_year == 2020|
                               state == 'SC' & election_year == 2020|
                               state == 'TN' & election_year == 2020|
                               state == 'MS' & election_year == 2020|
                               state == 'OK' & election_year == 2020|
                               state == 'MN' & election_year == 2020|
                               state == 'WY' & election_year == 2020|
                               state == 'KS' & election_year == 2020|
                               state == 'KY' & election_year == 2020|
                               state == 'NH' & election_year == 2020|
                               state == 'GA' & election_year == 2020|
                               state == 'ID' & election_year == 2020|
                               state == 'SD' & election_year == 2020|
                               state == 'NE' & election_year == 2020|
                               state == 'AK' & election_year == 2020|
                               state == 'NC' & election_year == 2020|
                               state == 'AL' & election_year == 2020|
                               state == 'AZ' & election_year == 2018|
                               state == 'IN' & election_year == 2018|
                               state == 'FL' & election_year == 2018|
                               state == 'MI' & election_year == 2018|
                               state == 'MN' & election_year == 2018|
                               state == 'MS' & election_year == 2018|
                               state == 'MO' & election_year == 2018|
                               state == 'MT' & election_year == 2018|
                               state == 'NE' & election_year == 2018|
                               state == 'NV' & election_year == 2018|
                               state == 'NJ' & election_year == 2018|
                               state == 'ND' & election_year == 2018|
                               state == 'OH' & election_year == 2018|
                               state == 'PA' & election_year == 2018|
                               state == 'TN' & election_year == 2018|
                               state == 'TX' & election_year == 2018|
                               state == 'UT' & election_year == 2018|
                               state == 'WV' & election_year == 2018|
                               state == 'WI' & election_year == 2018|
                               state == 'WY' & election_year == 2018, 1, 0),
         inc_rep = if_else(rep_candidate == senator, 1, 0),
         battle = if_else(state == 'AZ' & election_year == 2018|
                            state == 'CA' & election_year == 2018|
                            state == 'FL' & election_year == 2018|
                            state == 'IN' & election_year == 2018|
                            state == 'MN' & election_year == 2018|
                            state == 'MO' & election_year == 2018|
                            state == 'MS' & election_year == 2018|
                            state == 'MT' & election_year == 2018|
                            state == 'ND' & election_year == 2018|
                            state == 'NJ' & election_year == 2018|
                            state == 'NM' & election_year == 2018|
                            state == 'NV' & election_year == 2018|
                            state == 'TN' & election_year == 2018|
                            state == 'TX' & election_year == 2018|
                            state == 'WV' & election_year == 2018|
                            state == 'AL' & election_year == 2020|
                            state == 'CO' & election_year == 2020|
                            state == 'GA' & election_year == 2020|
                            state == 'IA' & election_year == 2020|
                            state == 'KS' & election_year == 2020|
                            state == 'KN' & election_year == 2020|
                            state == 'ME' & election_year == 2020|
                            state == 'MI' & election_year == 2020|
                            state == 'MN' & election_year == 2020|
                            state == 'MT' & election_year == 2020|
                            state == 'NH' & election_year == 2020|
                            state == 'NC' & election_year == 2020|
                            state == 'SC' & election_year == 2020|
                            state == 'TX' & election_year == 2020, 1, 0),
         end_inc = case_when(end_trump == 1 & inc_rep == 1 ~ 'endorsed & incumbent',
                             end_trump == 1 & inc_rep == 0 ~ 'endorsed & challenger',
                             end_trump == 0 & inc_rep == 1 ~ 'not endorsed & incumbent',
                             end_trump == 0 & inc_rep == 0 ~ 'not endorsed & challenger'),
         end_bat = case_when(end_trump == 1 & battle == 1 ~ 'endorsed & battleground',
                             end_trump == 1 & battle == 0 ~ 'endorsed & safe state',
                             end_trump == 0 & battle == 1 ~ 'not endorsed & battleground',
                             end_trump == 0 & battle == 0 ~ 'not endorsed & safe state'))

## election-level data 
vote_sy <- polls %>%
  group_by(state_year, state_year_int,  election_year, state, rep_result2,
           end_trump, inc_rep, battle, end_inc, end_bat) %>%
  summarise(n_poll = n()) 

## stan data 
stan_dat <- list(
  
  N = nrow(polls),                             # number of polls
  SY = length(unique(polls$state_year)),       # number of elections (state x election year)
  
  poll = polls$rep_poll2,                      # two-party poll share
  vote = vote_sy$rep_result2,                  # two-party vote share
  
  end_trump = vote_sy$end_trump,               # dummy Rep. cand. endosed by trump
  inc_rep = vote_sy$inc_rep,                   # dummy Rep. cand. endosed by trump
  battle = vote_sy$battle,                   # dummy Rep. cand. endosed by trump
  t = polls$t/max(polls$t),                    # between 0 and 1 scaled days to election
  
  sample_size = as.numeric(polls$n) * 
    (polls$rep_result + polls$dem_result),     # sample size adjusted for Rep. & Dem. poll support
  
  sy_id = polls$state_year_int                 # state-year identifier
  
)

## state-year identifier
sy <- polls$state_year_int 
sy_id <- vote_sy$state_year_int

# generate random variable of estimated parameters from stan fit object
postrv_end <- as.rv(resStan_end)  
postrv_inc <- as.rv(resStan_inc)  
postrv_bat <- as.rv(resStan_bat)  
postrv_end_inc <- as.rv(resStan_end_inc)  
postrv_end_bat <- as.rv(resStan_end_bat)  


#### Posterior calculations ####

# compute p[i] and p0[i]

## p_i
p_e_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end$alpha[sy] + 
    postrv_end$beta1 * stan_dat$end_trump[sy] + 
    postrv_end$beta2[sy] * stan_dat$t 
)

p_i_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_inc$alpha[sy] + 
    postrv_inc$beta1 * stan_dat$inc_rep[sy] + 
    postrv_inc$beta2[sy] * stan_dat$t 
)

p_b_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_bat$alpha[sy] + 
    postrv_bat$beta1 * stan_dat$battle[sy] + 
    postrv_bat$beta2[sy] * stan_dat$t 
)

p_ei_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_inc$alpha[sy] + 
    postrv_end_inc$beta1 * stan_dat$end_trump[sy] + 
    postrv_end_inc$beta2[sy] * stan_dat$t +
    postrv_end_inc$beta3 * stan_dat$inc_rep[sy] +
    postrv_end_inc$beta4 * stan_dat$end_trump[sy] * stan_dat$inc_rep[sy]
)

p_eb_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_bat$alpha[sy] + 
    postrv_end_bat$beta1 * stan_dat$end_trump[sy] + 
    postrv_end_bat$beta2[sy] * stan_dat$t +
    postrv_end_bat$beta3 * stan_dat$battle[sy] +
    postrv_end_bat$beta4 * stan_dat$end_trump[sy] * stan_dat$battle[sy]
)


# result + bias on election day
p0_e_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end$alpha[sy] + 
    postrv_end$beta1 * stan_dat$end_trump[sy] 
)

p0_i_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_inc$alpha[sy] + 
    postrv_inc$beta1 * stan_dat$inc_rep[sy] 
)

p0_b_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_bat$alpha[sy] + 
    postrv_bat$beta1 * stan_dat$inc_rep[sy] 
)

p0_ei_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_inc$alpha[sy] + 
    postrv_end_inc$beta1 * stan_dat$end_trump[sy] + 
    postrv_end_inc$beta3 * stan_dat$inc_rep[sy] +
    postrv_end_inc$beta4 * stan_dat$end_trump[sy] * stan_dat$inc_rep[sy]
)

p0_eb_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_bat$alpha[sy] + 
    postrv_end_bat$beta1 * stan_dat$end_trump[sy] + 
    postrv_end_bat$beta3 * stan_dat$battle[sy] +
    postrv_end_bat$beta4 * stan_dat$end_trump[sy] * stan_dat$battle[sy]
)


# endorsement, incumebncy & battleground
p_e_i_end <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end$beta1 * stan_dat$end_trump[sy] 
)

p_i_i_inc <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_inc$beta1 * stan_dat$inc_rep[sy] 
)

p_b_i_bat <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_bat$beta1 * stan_dat$battle[sy] 
)

p_ei_end <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_inc$beta1 * stan_dat$end_trump[sy] 
)

p_ei_inc <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_inc$beta3 * stan_dat$inc_rep[sy] 
)

p_ei_end_inc <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_inc$beta1 * stan_dat$end_trump[sy] + 
    postrv_end_inc$beta3 * stan_dat$inc_rep[sy] + 
    postrv_end_inc$beta4 * stan_dat$end_trump[sy] * stan_dat$inc_rep[sy] 
)

p_eb_end <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_bat$beta1 * stan_dat$end_trump[sy] 
)

p_eb_bat <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_bat$beta3 * stan_dat$battle[sy] 
)

p_ei_end_bat <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_end_bat$beta1 * stan_dat$end_trump[sy] + 
    postrv_end_bat$beta3 * stan_dat$battle[sy] + 
    postrv_end_bat$beta4 * stan_dat$end_trump[sy] * stan_dat$battle[sy] 
)

# total standard error
sig_e_i <- sqrt(
  exp.rv(
    log.rv((p_e_i * (1 - p_e_i) / stan_dat$sample_size)) + postrv_end$tau[sy]
  )
)

sig_i_i <- sqrt(
  exp.rv(
    log.rv((p_i_i * (1 - p_i_i) / stan_dat$sample_size)) + postrv_inc$tau[sy]
  )
)

sig_b_i <- sqrt(
  exp.rv(
    log.rv((p_b_i * (1 - p_b_i) / stan_dat$sample_size)) + postrv_bat$tau[sy]
  )
)

sig_ei_i <- sqrt(
  exp.rv(
    log.rv((p_ei_i * (1 - p_ei_i) / stan_dat$sample_size)) + postrv_end_inc$tau[sy]
  )
)

sig_eb_i <- sqrt(
  exp.rv(
    log.rv((p_eb_i * (1 - p_eb_i) / stan_dat$sample_size)) + postrv_end_inc$tau[sy]
  )
)

# srs standard error
srs_e_i <- sqrt(
  exp.rv(
    log.rv((p_e_i * (1 - p_e_i) / stan_dat$sample_size)) 
  )
)

srs_i_i <- sqrt(
  exp.rv(
    log.rv((p_i_i * (1 - p_i_i) / stan_dat$sample_size)) 
  )
)

srs_b_i <- sqrt(
  exp.rv(
    log.rv((p_b_i * (1 - p_b_i) / stan_dat$sample_size)) 
  )
)

srs_ei_i <- sqrt(
  exp.rv(
    log.rv((p_ei_i * (1 - p_ei_i) / stan_dat$sample_size)) 
  )
)

srs_eb_i <- sqrt(
  exp.rv(
    log.rv((p_eb_i * (1 - p_eb_i) / stan_dat$sample_size)) 
  )
)

# election level bias 
{
  b_e_r <- rv(length(unique(polls$state_year)))
  b0_e_r <- rv(length(unique(polls$state_year)))
  b_i_r <- rv(length(unique(polls$state_year)))
  b0_i_r <- rv(length(unique(polls$state_year)))
  b_b_r <- rv(length(unique(polls$state_year)))
  b0_b_r <- rv(length(unique(polls$state_year)))
  b_ei_r <- rv(length(unique(polls$state_year)))
  b0_ei_r <- rv(length(unique(polls$state_year)))
  b_eb_r <- rv(length(unique(polls$state_year)))
  b0_eb_r <- rv(length(unique(polls$state_year)))
  sig_e_r <- rv(length(unique(polls$state_year)))
  sig_i_r <- rv(length(unique(polls$state_year)))
  sig_b_r <- rv(length(unique(polls$state_year)))
  sig_ei_r <- rv(length(unique(polls$state_year)))
  sig_eb_r <- rv(length(unique(polls$state_year)))
  srs_e_r <- rv(length(unique(polls$state_year)))
  srs_i_r <- rv(length(unique(polls$state_year)))
  srs_b_r <- rv(length(unique(polls$state_year)))
  srs_ei_r <- rv(length(unique(polls$state_year)))
  srs_eb_r <- rv(length(unique(polls$state_year)))
  ex_e_r <- rv(length(unique(polls$state_year)))
  ex_i_r <- rv(length(unique(polls$state_year)))
  ex_b_r <- rv(length(unique(polls$state_year)))
  ex_ei_r <- rv(length(unique(polls$state_year)))
  ex_eb_r <- rv(length(unique(polls$state_year)))
}

for (i in 1:length(unique(stan_dat$sy_id))) {
  b_e_r[i] <- mean(p_e_i[i == stan_dat$sy_id]) - (stan_dat$vote[i]) 
  b0_e_r[i] <- mean(p0_e_i[i == stan_dat$sy_id]) - (stan_dat$vote[i])
  b_i_r[i] <- mean(p_i_i[i == stan_dat$sy_id]) - (stan_dat$vote[i]) 
  b0_i_r[i] <- mean(p0_i_i[i == stan_dat$sy_id]) - (stan_dat$vote[i])
  b_b_r[i] <- mean(p_b_i[i == stan_dat$sy_id]) - (stan_dat$vote[i]) 
  b0_b_r[i] <- mean(p0_b_i[i == stan_dat$sy_id]) - (stan_dat$vote[i])  
  b_ei_r[i] <- mean(p_ei_i[i == stan_dat$sy_id]) - (stan_dat$vote[i]) 
  b0_ei_r[i] <- mean(p0_ei_i[i == stan_dat$sy_id]) - (stan_dat$vote[i])
  b_eb_r[i] <- mean(p_eb_i[i == stan_dat$sy_id]) - (stan_dat$vote[i]) 
  b0_eb_r[i] <- mean(p0_eb_i[i == stan_dat$sy_id]) - (stan_dat$vote[i])
  sig_e_r[i]  <-  mean(sig_e_i[i == stan_dat$sy_id])
  sig_i_r[i]  <-  mean(sig_i_i[i == stan_dat$sy_id])
  sig_b_r[i]  <-  mean(sig_b_i[i == stan_dat$sy_id])
  sig_ei_r[i]  <-  mean(sig_ei_i[i == stan_dat$sy_id])
  sig_eb_r[i]  <-  mean(sig_eb_i[i == stan_dat$sy_id])
  srs_e_r[i]  <-  mean(srs_e_i[i == stan_dat$sy_id])
  srs_i_r[i]  <-  mean(srs_i_i[i == stan_dat$sy_id])
  srs_b_r[i]  <-  mean(srs_b_i[i == stan_dat$sy_id])
  srs_ei_r[i]  <-  mean(srs_ei_i[i == stan_dat$sy_id])
  srs_eb_r[i]  <-  mean(srs_eb_i[i == stan_dat$sy_id])  
  ex_e_r[i]  <-  mean(sig_e_i[i == stan_dat$sy_id] - srs_e_i[i == stan_dat$sy_id])
  ex_i_r[i]  <-  mean(sig_i_i[i == stan_dat$sy_id] - srs_i_i[i == stan_dat$sy_id])
  ex_b_r[i]  <-  mean(sig_b_i[i == stan_dat$sy_id] - srs_b_i[i == stan_dat$sy_id])
  ex_ei_r[i]  <-  mean(sig_ei_i[i == stan_dat$sy_id] - srs_ei_i[i == stan_dat$sy_id])
  ex_eb_r[i]  <-  mean(sig_eb_i[i == stan_dat$sy_id] - srs_eb_i[i == stan_dat$sy_id])  
}

# endorsement, incumbency & battleground bias
b_e_end <- mean(p_e_i_end - stan_dat$vote[sy]) %>% summary
b_e_end$par <- 'Endorsed'
b_e_end$model <- 'Endorsement'

b_i_inc <- mean(p_i_i_inc - stan_dat$vote[sy]) %>% summary
b_i_inc$par <- 'Incumbent'
b_i_inc$model <- 'Incumbency'

b_b_bat <- mean(p_b_i_bat - stan_dat$vote[sy]) %>% summary
b_b_bat$par <- 'Battleground'
b_b_bat$model <- 'Battleground'

b_ei_end <- mean(p_ei_end - stan_dat$vote[sy]) %>% summary
b_ei_end$par <- 'Endorsed & challenger'
b_ei_end$model <- 'Endorsement & incumbency'

b_ei_inc <- mean(p_ei_inc - stan_dat$vote[sy]) %>% summary
b_ei_inc$par <- 'Not endorsed & incumbent'
b_ei_inc$model <- 'Endorsement & incumbency'

b_ei_end_inc <- mean(p_ei_end_inc - stan_dat$vote[sy]) %>% summary
b_ei_end_inc$par <- 'Endorsed & incumbent'
b_ei_end_inc$model <- 'Endorsement & incumbency'

b_eb_end <- mean(p_eb_end - stan_dat$vote[sy]) %>% summary
b_eb_end$par <- 'Endorsed & safe state'
b_eb_end$model <- 'Endorsement & battleground'

b_eb_bat <- mean(p_eb_bat - stan_dat$vote[sy]) %>% summary
b_eb_bat$par <- 'Not endorsed & battleground'
b_eb_bat$model <- 'Endorsement & battleground'

b_eb_end_bat <- mean(p_ei_end_bat - stan_dat$vote[sy]) %>% summary
b_eb_end_bat$par <- 'Endorsed & battleground'
b_eb_end_bat$model <- 'Endorsement & battleground'

# mean abs. bias
mab_e <- mean(abs(b_e_r)) %>% summary
mab_i <- mean(abs(b_i_r)) %>% summary
mab_b <- mean(abs(b_b_r)) %>% summary
mab_ei <- mean(abs(b_ei_r)) %>% summary
mab_eb <- mean(abs(b_eb_r)) %>% summary

mab0_e <- mean(abs(b0_e_r)) %>% summary
mab0_i <- mean(abs(b0_i_r)) %>% summary
mab0_b <- mean(abs(b0_b_r)) %>% summary
mab0_ei <- mean(abs(b0_ei_r)) %>% summary
mab0_eb <- mean(abs(b0_eb_r)) %>% summary

msig_e <- mean(sig_e_r) %>% summary
msig_i <- mean(sig_i_r) %>% summary
msig_b <- mean(sig_b_r) %>% summary
msig_ei <- mean(sig_ei_r) %>% summary
msig_eb <- mean(sig_eb_r) %>% summary

mex_e <- mean(ex_e_r) %>% summary
mex_i <- mean(ex_i_r) %>% summary
mex_b <- mean(ex_b_r) %>% summary
mex_ei <- mean(ex_ei_r) %>% summary
mex_eb <- mean(ex_eb_r) %>% summary

# merge bias
b_end_inc_summary <- rbind(b_ei_end, b_ei_end_inc, b_ei_inc)
b_end_bat_summary <- rbind(b_eb_end, b_eb_end_bat, b_eb_bat)

# election day bias
b0_e_r_summary <- summary(b0_e_r)
b0_e_r_summary$state_year_int <- seq(1:length(unique(stan_dat$sy_id)))
b0_e_r_summary <- merge(b0_e_r_summary, vote_sy, by = 'state_year_int')
b0_e_r_summary <- b0_e_r_summary %>% 
  mutate(end_trump_factor = if_else(end_trump == 1, "Endorsed", "Not endorsed"))

b0_ei_r_summary <- summary(b0_ei_r)
b0_ei_r_summary$state_year_int <- seq(1:length(unique(stan_dat$sy_id)))
b0_ei_r_summary <- merge(b0_ei_r_summary, vote_sy, by = 'state_year_int')
b0_ei_r_summary <- b0_ei_r_summary %>% 
  mutate(end_inc = factor(end_inc,  levels = c("endorsed & challenger",
                                                  "not endorsed & incumbent", 
                                                  "endorsed & incumbent", 
                                                  "not endorsed & challenger"),
                             labels = c("Endorsed & challenger",
                                        "Not endorsed & incumbent", 
                                        "Endorsed & incumbent", 
                                        "Not endorsed & challenger")))
b0_eb_r_summary <- summary(b0_eb_r)
b0_eb_r_summary$state_year_int <- seq(1:length(unique(stan_dat$sy_id)))
b0_eb_r_summary <- merge(b0_eb_r_summary, vote_sy, by = 'state_year_int')
b0_eb_r_summary <- b0_eb_r_summary %>% 
  mutate(end_bat = factor(end_bat,  levels = c("endorsed & safe state",
                                               "not endorsed & battleground", 
                                               "endorsed & battleground", 
                                               "not endorsed & safe state"),
                          labels = c("Endorsed & safe state",
                                     "Not endorsed & battleground", 
                                     "Endorsed & battleground", 
                                     "Not endorsed & safe state")))

#-------------------------------------------------------------------------------

#### Plots #### 

## plot election day bias

# endorsement only
b0_end_plot <- ggplot(b0_e_r_summary, aes(x)) + 
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`, 
                   color = end_trump_factor)) + 
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`, 
                   color = end_trump_factor), size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(x = '', y = '') + 
  scale_y_discrete(limits = rev(levels(factor(b0_e_r_summary$state)))) + 
  scale_x_continuous(labels=percent_format(accuracy = 1)) + 
  theme_bw() + 
  theme(text = element_text(size = 16), axis.text = element_text(size = 12),
        legend.position="bottom") +
  facet_wrap(~election_year) +
  scale_color_manual(name = '', values = c("#E69F00", "#999999"))

ggsave(filename = 'election_day_bias_end.png', 
       plot = b0_end_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 7)

ggsave(filename = 'election_day_bias_end.eps', 
       plot = b0_end_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 7) # save as .eps 

# endorsement & incumbency
b0_end_inc_plot <- ggplot(b0_ei_r_summary, aes(x)) + 
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`, 
                   color = as.factor(end_inc))) + 
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`, 
                   color = as.factor(end_inc)), size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(x = '', y = '') + 
  scale_y_discrete(limits = rev(levels(factor(b0_ei_r_summary$state)))) + 
  scale_x_continuous(labels=percent_format(accuracy = 1)) + 
  theme_bw() + 
  theme(text = element_text(size = 16), axis.text = element_text(size = 12),
        legend.position="bottom") +
  facet_wrap(~election_year) +
  scale_color_manual(name = '', values = c("#E69F00", "#56B4E9",
                                           "#009E73", "#999999"))

ggsave(filename = 'election_day_bias_end_inc.png', 
       plot = b0_end_inc_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 7)

ggsave(filename = 'election_day_bias_end_inc.eps', 
       plot = b0_end_inc_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 7) # save as .eps 

# endorsement & battleground
b0_end_bat_plot <- ggplot(b0_eb_r_summary, aes(x)) + 
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`, 
                   color = as.factor(end_bat))) + 
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`, 
                   color = as.factor(end_bat)), size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(x = '', y = '') + 
  scale_y_discrete(limits = rev(levels(factor(b0_eb_r_summary$state)))) + 
  scale_x_continuous(labels=percent_format(accuracy = 1)) + 
  theme_bw() + 
  theme(text = element_text(size = 16), axis.text = element_text(size = 12),
        legend.position="bottom") +
  facet_wrap(~election_year) +
  scale_color_manual(name = '', values = c("#E69F00", "#56B4E9",
                                           "#009E73", "#999999"))

ggsave(filename = 'election_day_bias_end_bat.png', 
       plot = b0_end_bat_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 7)

ggsave(filename = 'election_day_bias_end_bat.eps', 
       plot = b0_end_bat_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 7) # save as .eps 


## plot endorsement, incumbency and battleground bias
b_end_all <- rbind(b_e_end, b_i_inc, b_b_bat, b_end_inc_summary,
                   b_end_bat_summary) %>% 
  mutate(par = factor(par, levels = c('Endorsed',
                                      'Incumbent',
                                      'Battleground',
                                      'Endorsed & incumbent',
                                      'Not endorsed & incumbent',
                                      'Endorsed & challenger',
                                      'Endorsed & battleground',
                                      'Not endorsed & battleground',
                                      'Endorsed & safe state')),
         model = factor(model, levels = c('Endorsement',
                                          'Incumbency',
                                          'Battleground',
                                          'Endorsement & incumbency',
                                          'Endorsement & battleground')))

end_inc_bat_bias <- 
  ggplot(b_end_all, aes(x, color = par)) + 
  geom_segment(aes(y = factor(par), yend = factor(par), x = `2.5%`, xend = `97.5%`)) + 
  geom_segment(aes(y = factor(par), yend = factor(par), x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  labs(x = '', y = '') + 
  scale_x_continuous(labels=percent_format(accuracy = 1), 
                     limits = c(-0.031, 0.031), breaks = seq(-0.03, 0.03, 0.01)) + 
  theme_bw() + 
  theme(text = element_text(size = 12), axis.text = element_text(size = 12)) + 
  facet_grid(model~., space = 'free', scales = 'free') +
  scale_color_manual(values = c("#E69F00", "#999999", "#999999",
                                "#E69F00", "#999999", "#E69F00", 
                                "#E69F00", "#999999", "#E69F00")) + 
  theme(legend.position = "none")

ggsave(filename = 'end_inc_bat_bias.png', 
       plot = end_inc_bat_bias, 
       path = '~/results_vis/plots',
       width = 12, height = 9)

ggsave(filename = 'end_inc_bat_bias.eps', 
       plot = end_inc_bat_bias, 
       path = '~/results_vis/plots',
       width = 12, height = 9) # save as .eps 

