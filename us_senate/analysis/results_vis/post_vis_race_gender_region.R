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

data_senate <-read_csv("~/Documents/Uni/PollingError/senate/data/senate_polls_merged.csv")

# senate results
senate_results <- readRDS("~/Documents/Uni/PollingError/senate/data/senate_results.RDS")

# merge data
data_senate <- merge(data_senate, senate_results, by = c('election_year', 'state'), all.x = T )

# compute days until election, gender and race dummy, incumbency dummy, two-party vote share
data_senate <- data_senate %>%
  mutate(gender_rep_dummy = if_else(clarifai_gender_rep == 'feminine', 1, 0),
         gender_dem_dummy = if_else(clarifai_gender_dem == 'feminine', 1, 0),
         race_rep_dummy = if_else(clarifai_race_rep != 'white', 1, 0),
         race_dem_dummy = if_else(clarifai_race_dem != 'white', 1, 0),
         state_year = paste0(election_year, '_', state),
         rep_poll2 = rep_vote/(rep_vote + dem_vote)
  )


# Subset complete cases
data_senate <- data_senate %>% 
  subset(!is.na(gender_rep_dummy) &
           !is.na(gender_dem_dummy) &
           !is.na(race_rep_dummy) &
           !is.na(race_dem_dummy) &
           !is.na(rep_poll2) &
           !is.na(rep_result2) &
           !is.na(n))

# Add groups (source: https://en.wikipedia.org/wiki/United_States_Census_Bureau)
data_senate <- data_senate %>%
  mutate(region_group4 = case_when(state == 'CT'|
                                     state == 'ME'|
                                     state == 'MA'|
                                     state == 'NH'|
                                     state == 'NJ'|
                                     state == 'NY'|
                                     state == 'PA'|
                                     state == 'RI'|
                                     state == 'VT' ~ 'Northeast',
                                   state == 'IL'|
                                     state == 'IN'|
                                     state == 'IA'|
                                     state == 'KS'|
                                     state == 'MI'|
                                     state == 'MN'|
                                     state == 'MO'|
                                     state == 'NE'|
                                     state == 'ND'|
                                     state == 'OH'|
                                     state == 'SD'|
                                     state == 'WI' ~ 'Midwest',
                                   state == 'AL'|
                                     state == 'AR'|
                                     state == 'DE'|
                                     state == 'FL'|
                                     state == 'GA'|
                                     state == 'KY'|
                                     state == 'LA'|
                                     state == 'MD'|
                                     state == 'MS'|
                                     state == 'NC'|
                                     state == 'OK'|
                                     state == 'SC'|
                                     state == 'TN'|
                                     state == 'TX'|
                                     state == 'VA'|
                                     state == 'WV' ~ 'South',
                                   state == 'AZ'|
                                     state == 'AK'|
                                     state == 'CA'|
                                     state == 'CO'|
                                     state == 'HI'|
                                     state == 'ID'|
                                     state == 'MT'|
                                     state == 'NV'|
                                     state == 'NM'|
                                     state == 'UT'|
                                     state == 'OR'|
                                     state == 'WA'|
                                     state == 'WY' ~ 'West'))



# Election-level data 
vote_sy <- data_senate %>%
  group_by(state_year, election_year, state, rep_result2, 
           race_rep_dummy, race_dem_dummy, gender_rep_dummy,gender_dem_dummy) %>%
  summarise()
vote_sy$state_year_int <- as.integer(as.factor(vote_sy$state_year))

# Stan data 
stan_dat <- list(
  N = nrow(data_senate),                            # number of polls
  SY = length(unique(data_senate$state_year)),      # number of elections (state x election year)
  RG = length(unique(data_senate$region_group4)),   # number of regions
  
  poll = data_senate$rep_poll2,                     # two-party poll share
  vote = vote_sy$rep_result2,                       # two-party vote share
  
  sample_size = as.numeric(data_senate$n) * (data_senate$rep_result + data_senate$dem_result), 
  # sample size adjusted for Rep. & Dem. poll share
  
  race_rep = vote_sy$race_rep_dummy,                # race dummy Rep.: 1 = non-white Rep. candidate
  race_dem = vote_sy$race_dem_dummy,                # race dummy Dem.: 1 = non-white Dem. candidate
  gender_rep = vote_sy$gender_rep_dummy,            # gender dummy Rep.: 1 = female Rep. candidate
  gender_dem = vote_sy$gender_dem_dummy,            # gender dummy Dem.: 1 = female Dem. candidate
  
  sy_id = as.integer(as.factor(data_senate$state_year)),   # election(state-year) identifier
  rg_id = as.integer(as.factor(data_senate$region_group4)) # region identifier
)

# Stan fit objects

res_stan_race <- readRDS('res_stan_race.RDS') 
res_stan_gender <- readRDS('res_stan_gender.RDS') 

#### Preparation ####

postrv_race <- as.rv(res_stan_race) # random variable of estimated parametrs
postrv_gender <- as.rv(res_stan_gender) # random variable of estimated parametrs

sy <- stan_dat$sy # state-year identifier
rg <- stan_dat$rg_id # state-year identifier


#-------------------------------------------------------------------------------
#### Election level bias and poll variance ####

### Race ###

# Estimated poll mean
p_i_race <- ilogit.rv(
  logit(stan_dat$vote[sy]) +
    postrv_race$alpha[sy] + 
    postrv_race$beta1[rg] * stan_dat$race_rep[sy] + 
    postrv_race$beta2[rg] * stan_dat$race_dem[sy] 
    
)

# Estimated poll mean without election-level bias
p_i_a_race <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_race$beta1[rg] * stan_dat$race_rep[sy] + 
    postrv_race$beta2[rg] * stan_dat$race_dem[sy] 
)

# Estimated variance
sigma_sqr_i_race <- exp(log(p_i_race*(1-p_i_race)/stan_dat$sample_size) + 
                          postrv_race$tau[sy] 
                     )

# Estimated excess variance
sigma_sqr_ex_i_race <- exp(log(p_i_race*(1 - p_i_race)/stan_dat$sample_size) + 
                        postrv_race$tau[sy]) - 
  (p_i_race*(1 - p_i_race)/stan_dat$sample_size)

# Calculating average absolute election level bias and varaince
b_r_race<- rv(length(unique(sy))) # election level bias
b_a_r_race<- rv(length(unique(sy))) # election level boas explained by race
var_r_race <- rv(length(unique(sy))) # election level variance
ex_var_r_race <- rv(length(unique(sy))) # election level excess variance

for(i in 1:length(unique(sy))){
  b_r_race[i] <- mean(p_i_race[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_a_race[i] <- mean(p_i_a_race[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  var_r_race[i] <- mean(sigma_sqr_i_race[i == stan_dat$sy]) 
}
for(i in 1:length(unique(sy))){
  ex_var_r_race[i] <- mean(sigma_sqr_ex_i_race[i == stan_dat$sy]) 
}


mabs_b_r_race <- mean(abs(b_r_race))
mabs_b_r_a_race <- mean(abs(b_r_a_race))
mabs_var_r_race <- mean(abs(var_r_race))
mabs_ex_var_r_race <- mean(abs(ex_var_r_race))


### Gender ###

# Estimated poll mean
p_i_gender <- ilogit.rv(
  logit(stan_dat$vote[sy]) +
    postrv_gender$alpha[sy] + 
    postrv_gender$beta1[rg] * stan_dat$gender_rep[sy] + 
    postrv_gender$beta2[rg] * stan_dat$gender_dem[sy] 
  
)

# Estimated poll mean without election-level bias
p_i_a_gender <- ilogit.rv(
  logit(stan_dat$vote[sy]) + 
    postrv_gender$beta1 * stan_dat$gender_rep[sy] + 
    postrv_gender$beta2 * stan_dat$gender_dem[sy] 
)

# Estimated variance
sigma_sqr_i_gender <- exp(log(p_i_gender*(1-p_i_gender)/stan_dat$sample_size) + 
                          postrv_gender$gamma1 * stan_dat$inc_dummy[sy]
)

# Estimated excess variance
sigma_sqr_ex_i_gender <- exp(log(p_i_gender*(1 - p_i_gender)/stan_dat$sample_size) + 
                        postrv_gender$tau[sy]) - 
  (p_i_gender*(1 - p_i_gender)/stan_dat$sample_size)

# Calculating average absolute election level bias and varaince
b_r_gender<- rv(length(unique(sy)))
b_r_a_gender<- rv(length(unique(sy)))
var_r_gender <- rv(length(unique(sy)))
ex_var_r_gender <- rv(length(unique(sy)))

for(i in 1:length(unique(sy))){
  b_r_gender[i] <- mean(p_i_gender[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_a_gender[i] <- mean(p_i_a_gender[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  var_r_gender[i] <- mean(sigma_sqr_i_gender[i == stan_dat$sy]) 
}
for(i in 1:length(unique(sy))){
  ex_var_r_gender[i] <- mean(sigma_sqr_ex_i_gender[i == stan_dat$sy]) 
}

mabs_b_r_gender <- mean(abs(b_r_gender))
mabs_b_r_a_gender <- mean(abs(b_r_a_gender))
mabs_var_r_gender <- mean(abs(var_r_gender))
mabs_ex_var_r_gender <- mean(abs(ex_var_r_gender))

### Parameter estimates ###

### Race ###

# Alpha
alpha_race_summary <- summary(postrv_race$alpha)
alpha_race_summary$sy_id <-seq(1:length(unique(data_senate$state_year)))
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

ggplot(beta1_race_summary, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = expression(beta[1]~race~credible~intervals), y = '') +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12)) 

# Beta2
beta2_race_summary <- summary(postrv_race$beta2)
beta2_race_summary$par <- 'Beta2'

ggplot(beta2_race_summary, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = expression(beta[2]~race~credible~intervals), y = '') +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12)) 

# Tau
tau_race_summary <- summary(postrv_race$tau)
tau_race_summary$sy_id <-seq(1:length(unique(data_senate$state_year)))
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


### Gender ###

# Alpha
alpha_gender_summary <- summary(postrv_gender$alpha)
alpha_gender_summary$sy_id <-seq(1:length(unique(data_senate$state_year)))
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

ggplot(beta1_gender_summary, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = expression(beta[1]~race~credible~intervals), y = '') +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12)) 

# Beta2
beta2_gender_summary <- summary(postrv_gender$beta2)
beta2_gender_summary$par <- 'Beta2'

ggplot(beta2_gender_summary, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = expression(beta[2]~race~credible~intervals), y = '') +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12)) 


# Tau
tau_gender_summary <- summary(postrv_gender$tau)
tau_gender_summary$sy_id <-seq(1:length(unique(data_senate$state_year)))
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

