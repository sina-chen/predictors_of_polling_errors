########################################################################################
# Senate poll accuracy, gender/race and incumbency: posterior vis
# Author: Sina Chen
#
############################################################################################

#### Libraries ####

library(rv)
library(gtools)
library(ggplot2)
library(tidybayes)
library(rstan)
library(dplyr)
library(ggsci) # color scales
library(scales)
library(xtable) # safe Latex tables
library(bayesplot)
library(arm) # inverse logit
library(readr)
library(shinystan)

#### Helper functions ####

ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x)
logit.rv <- function(x) rvmapply(FUN = logit, x)


#### Data ####

# polls

data_senate <- read_csv("~/Documents/Uni/PollingError/senate/data/senate_polls_merged.csv")

# senate results
senate_results <- readRDS("~/Documents/Uni/PollingError/senate/data/senate_results.RDS")

# merge data
data_senate <- merge(data_senate, senate_results, by = c('election_year', 'state'), all.x = T )

# compute days until election, gender and race dummy
data_senate <- data_senate %>%
  mutate(gender_rep_dummy = if_else(clarifai_gender_rep == 'feminine', 1, 0),
         gender_dem_dummy = if_else(clarifai_gender_dem == 'feminine', 1, 0),
         gender_dummy = if_else(clarifai_gender_rep == 'feminine' | 
                                  clarifai_gender_dem == 'feminine', 1, 0),
         race_rep_dummy = if_else(clarifai_race_rep != 'white', 1, 0),
         race_dem_dummy = if_else(clarifai_race_dem != 'white', 1, 0),
         race_dummy = if_else(clarifai_race_rep != 'white' | 
                                clarifai_race_dem != 'white', 1, 0),
         t = case_when(
           election_year == '1998' ~ difftime(as.Date('11/03/1998','%m/%d/%Y'), date),
           election_year == '2000' ~ difftime(as.Date('11/07/2000','%m/%d/%Y'), date),
           election_year == '2002' ~ difftime(as.Date('11/05/2002','%m/%d/%Y'), date),
           election_year == '2004' ~ difftime(as.Date('11/02/2004','%m/%d/%Y'), date),
           election_year == '2006' ~ difftime(as.Date('11/07/2006','%m/%d/%Y'), date),
           election_year == '2008' ~ difftime(as.Date('11/04/2008','%m/%d/%Y'), date),
           election_year == '2010' ~ difftime(as.Date('11/02/2010','%m/%d/%Y'), date),
           election_year == '2012' ~ difftime(as.Date('11/06/2012','%m/%d/%Y'), date),
           election_year == '2014' ~ difftime(as.Date('11/04/2014','%m/%d/%Y'), date),
           election_year == '2016' ~ difftime(as.Date('11/08/2016','%m/%d/%Y'), date),
           election_year == '2018' ~ difftime(as.Date('11/06/2018','%m/%d/%Y'), date)),
         t_sc = as.numeric(t)/max(as.numeric(t)),
         state_year = paste0(state, '_', election_year)
  )


# compute two-party Rep. poll share
data_senate$rep_poll2 <- with(data_senate, rep_result/(rep_result + dem_result))

# subset complete cases
data_complete <- data_senate %>% 
  subset(!is.na(gender_dummy) & 
           !is.na(race_dummy) &
           !is.na(rep_poll2) &
           !is.na(rep_result2) &
           !is.na(t) &
           t >= 0 &
           !is.na(n)) %>%
  mutate(inc_dummy = if_else(incumbency == 'TRUE', 1, 0))

data_complete <- data_complete  %>%
  group_by(state_year) %>%
  filter(n() > 3)

# election level information
vote_sy <- data_complete %>%
  group_by(state_year, election_year, state, rep_result2, gender_rep_dummy, 
           gender_dem_dummy, race_dem_dummy, race_rep_dummy, inc_dummy) %>%
  summarise(n = n()) 
vote_sy$state_year_int <- as.integer(as.factor(vote_sy$state_year))

# Stan data 

stan_dat <- list(
  N = nrow(data_complete),                               # number of polls
  SY = length(unique(data_complete$state_year)),         # number of elections (state x election year)
  S = length(unique(data_complete$state)),          # number of states 
  
  poll = data_complete$rep_poll2,                        # two-party poll share
  vote = vote_sy$rep_result2,                      # two-party vote share
  
  sample_size = as.numeric(data_complete$n) * (data_complete$rep_result + data_complete$dem_result), 
  # sample size adjusted for Rep. & Dem. poll share
  
  gender_rep = vote_sy$gender_rep_dummy,               # gender dummy Rep.: 1 = female Rep. candidate
  gender_dem = vote_sy$gender_dem_dummy,              # gender dummy Dem.: 1 = female Dem. candidate
  race_rep = vote_sy$race_rep_dummy,               # gender dummy Rep.: 1 = female Rep. candidate
  race_dem = vote_sy$race_dem_dummy,               # gender dummy Rep.: 1 = female Rep. candidate
  inc_dummy = vote_sy$inc_dummy,                   # incumbency dummy: 1 = incumbency participates
  t = data_complete$t_sc,                                # scaled days to election
  
  sy_id = as.integer(as.factor(data_complete$state_year)),# election(state-year) identifier
  state_id = as.integer(as.factor(data_complete$state))  # state identifier
)

# Stan fit objects

res_stan_race <- readRDS('res_stan_race.RDS') 
res_stan_gender <- readRDS('res_stan_gender.RDS') 

#### Preparation ####

postrv_race <- as.rv(res_stan_race$stand_fit) # random variable of estimated parametrs
postrv_gender <- as.rv(res_stan_gender$stand_fit) # random variable of estimated parametrs

sy <- stan_dat$sy # state-year identifier


#-------------------------------------------------------------------------------
#### Election level bias and poll variance ####

### Race ###

# Estimated poll mean
p_i_race <- ilogit.rv(
  logit(stan_dat$vote[sy]) +
    postrv_race$alpha[sy] + 
    postrv_race$beta1 * stan_dat$race_rep[sy] + 
    postrv_race$beta2 * stan_dat$race_dem[sy] +
    postrv_race$beta3[sy] * stan_dat$t 
    
)

# Estimated poll mean on election day 
p_i_a_race <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_race$alpha[sy] + 
    postrv_race$beta1 * stan_dat$race_rep[sy] + 
    postrv_race$beta2 * stan_dat$race_dem[sy] 
)

# Estimated poll mean on election day 
p_i_a_race <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_race$alpha[sy] + 
    postrv_race$beta1 * stan_dat$race_rep[sy] + 
    postrv_race$beta2 * stan_dat$race_dem[sy] 
)

# Estimated poll mean on election day without election level bias
p_i_race <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_race$beta1 * stan_dat$race_rep[sy] + 
    postrv_race$beta2 * stan_dat$race_dem[sy] 
)

# Estimated variance
sigma_sqr_i_race <- exp(log(p_i_race*(1-p_i_race)/stan_dat$sample_size) + 
                          postrv_race$tau[sy] +
                          postrv_race$gamma1 * stan_dat$inc_dummy[sy]
                     )

# Estimated excess variance
sigma_sqr_ex_i_race <- exp(log(p_i_race*(1 - p_i_race)/stan_dat$sample_size) + 
                        postrv_race$tau[sy] +
                        postrv_race$gamma1 * stan_dat$inc_dummy[sy]) - 
  (p_i_race*(1 - p_i_race)/stan_dat$sample_size)

# Calculating average absolute election level bias and varaince
b_r_race<- rv(length(unique(sy)))
b_r_a_race<- rv(length(unique(sy)))
b_r_race<- rv(length(unique(sy)))
var_r_race <- rv(length(unique(sy)))
ex_var_r_race <- rv(length(unique(sy)))

for(i in 1:length(unique(sy))){
  b_r_race[i] <- mean(p_i_race[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_a_race[i] <- mean(p_i_a_race[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_race[i] <- mean(p_i_race[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  var_r_race[i] <- mean(sigma_sqr_i_race[i == stan_dat$sy]) 
}
for(i in 1:length(unique(sy))){
  ex_var_r_race[i] <- mean(sigma_sqr_ex_i_race[i == stan_dat$sy]) 
}


mabs_b_r_race <- mean(abs(b_r_race))
mabs_b_r_a_race <- mean(abs(b_r_a_race))
mabs_b_r_race <- mean(abs(b_r_race))
mabs_var_r_race <- mean(abs(var_r_race))
mabs_ex_var_r_race <- mean(abs(ex_var_r_race))


### Gender ###

# Estimated poll mean
p_i_gender <- ilogit.rv(
  logit(stan_dat$vote[sy]) +
    postrv_gender$alpha[sy] + 
    postrv_gender$beta1 * stan_dat$gender_rep[sy] + 
    postrv_gender$beta2 * stan_dat$gender_dem[sy] +
    postrv_gender$beta3[sy] * stan_dat$t 
  
)

# Estimated poll mean on election day 
p_i_a_gender <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_gender$alpha[sy] + 
    postrv_gender$beta1 * stan_dat$gender_rep[sy] + 
    postrv_gender$beta2 * stan_dat$gender_dem[sy] 
)

# Estimated poll mean on election day without election-level bias
p_i_gender <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_gender$beta1 * stan_dat$gender_rep[sy] + 
    postrv_gender$beta2 * stan_dat$gender_dem[sy] 
)

# Estimated variance
sigma_sqr_i_gender <- exp(log(p_i_gender*(1-p_i_gender)/stan_dat$sample_size) + 
                          postrv_gender$tau[sy] +
                          postrv_gender$gamma1 * stan_dat$inc_dummy[sy]
)

# Estimated excess variance
sigma_sqr_ex_i_gender <- exp(log(p_i_gender*(1 - p_i_gender)/stan_dat$sample_size) + 
                        postrv_gender$tau[sy] +
                        postrv_gender$gamma1 * stan_dat$inc_dummy[sy]) - 
  (p_i_gender*(1 - p_i_gender)/stan_dat$sample_size)

# Calculating average absolute election level bias and varaince
b_r_gender<- rv(length(unique(sy)))
b_r_a_gender<- rv(length(unique(sy)))
b_r_gender<- rv(length(unique(sy)))
var_r_gender <- rv(length(unique(sy)))
ex_var_r_gender <- rv(length(unique(sy)))

for(i in 1:length(unique(sy))){
  b_r_gender[i] <- mean(p_i_gender[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_a_gender[i] <- mean(p_i_a_gender[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_gender[i] <- mean(p_i_gender[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  var_r_gender[i] <- mean(sigma_sqr_i_gender[i == stan_dat$sy]) 
}
for(i in 1:length(unique(sy))){
  ex_var_r_gender[i] <- mean(sigma_sqr_ex_i_gender[i == stan_dat$sy]) 
}

mabs_b_r_gender <- mean(abs(b_r_gender))
mabs_b_r_a_gender <- mean(abs(b_r_a_gender))
mabs_b_r_gender <- mean(abs(b_r_gender))
mabs_var_r_gender <- mean(abs(var_r_gender))
mabs_ex_var_r_gender <- mean(abs(ex_var_r_gender))

### Parameter estimates ###

### Race ###

# Alpha
alpha_race_summary <- summary(postrv_race$alpha)
alpha_race_summary$sy_id <-seq(1:239)
alpha_race_summary <- merge(alpha_race_summary, vote_sy, by.x = 'sy_id', 
                       by.y = 'state_year_int')

ggplot(alpha_race_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(alpha~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(alpha_race_summary$state)))) +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

# Beta1
beta1_race_summary <- summary(postrv_race$beta1)
beta1_race_summary$par <- 'Beta1'

# Beta2
beta2_race_summary <- summary(postrv_race$beta2)
beta2_race_summary$par <- 'Beta2'

#Beta1 & 2
beta1_2_race <- rbind(beta1_race_summary, beta2_race_summary)

ggplot(beta1_2_race, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = expression(beta[1/2]~race~credible~intervals), y = 'Parameter') +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  scale_y_discrete(limits = rev(levels(as.factor(beta1_2_race$par))),
                   labels = c('Beta1' = expression(beta[1]),
                              'Beta2'   = expression(beta[2])))

# Beta3
beta3_race_summary <- summary(postrv_race$beta3)
beta3_race_summary$sy_id <-seq(1:239)
beta3_race_summary <- merge(beta3_race_summary, vote_sy, by.x = 'sy_id', 
                            by.y = 'state_year_int')

ggplot(beta3_race_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(beta[3]~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(beta3_race_summary$state)))) +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

# Tau
tau_race_summary <- summary(postrv_race$tau)
tau_race_summary$sy_id <-seq(1:239)
tau_race_summary <- merge(tau_race_summary, vote_sy, by.x = 'sy_id', 
                     by.y = 'state_year_int')

ggplot(tau_race_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(tau~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(tau_race_summary$state)))) +
  theme_bw() + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

# Gamma1

gamma1_summary <- summary(postrv_race$gamma1)
gamma1_summary$par <- 'Gamma1'

ggplot(gamma1_summary, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = '', y = '') +
  scale_y_discrete(limits = rev(levels(as.factor(gamma1_summary$par))),
                   labels = c('Gamma1' = expression(gamma[1]))) +
  theme_bw() + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 18)) +
  labs(x = 'Credible intervals')



### Gender ###

# Alpha
alpha_gender_summary <- summary(postrv_gender$alpha)
alpha_gender_summary$sy_id <-seq(1:239)
alpha_gender_summary <- merge(alpha_gender_summary, vote_sy, by.x = 'sy_id', 
                            by.y = 'state_year_int')

ggplot(alpha_gender_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(alpha~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(alpha_gender_summary$state)))) +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

# Beta1
beta1_gender_summary <- summary(postrv_gender$beta1)
beta1_gender_summary$par <- 'Beta1'

# Beta2
beta2_gender_summary <- summary(postrv_gender$beta2)
beta2_gender_summary$par <- 'Beta2'

#Beta1 & 2
beta1_2_gender <- rbind(beta1_gender_summary, beta2_gender_summary)

ggplot(beta1_2_gender, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = expression(beta[1/2]~gender~credible~intervals), y = 'Parameter') +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  scale_y_discrete(limits = rev(levels(as.factor(beta1_2_gender$par))),
                   labels = c('Beta1' = expression(beta[1]),
                              'Beta2'   = expression(beta[2])))

# Beta3
beta3_gender_summary <- summary(postrv_gender$beta3)
beta3_gender_summary$sy_id <-seq(1:239)
beta3_gender_summary <- merge(beta3_gender_summary, vote_sy, by.x = 'sy_id', 
                            by.y = 'state_year_int')

ggplot(beta3_gender_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(beta[3]~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(beta3_gender_summary$state)))) +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

# Tau
tau_gender_summary <- summary(postrv_gender$tau)
tau_gender_summary$sy_id <-seq(1:239)
tau_gender_summary <- merge(tau_gender_summary, vote_sy, by.x = 'sy_id', 
                          by.y = 'state_year_int')

ggplot(tau_gender_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(tau~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(tau_gender_summary$state)))) +
  theme_bw() + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

# Gamma1

gamma1_summary <- summary(postrv_gender$gamma1)
gamma1_summary$par <- 'Gamma1'

ggplot(gamma1_summary, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = '', y = '') +
  scale_y_discrete(limits = rev(levels(as.factor(gamma1_summary$par))),
                   labels = c('Gamma1' = expression(gamma[1]))) +
  theme_bw() + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 18)) +
  labs(x = 'Credible intervals')
