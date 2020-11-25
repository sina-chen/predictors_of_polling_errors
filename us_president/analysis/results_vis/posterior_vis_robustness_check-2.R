################################################################################
# Posterior Calculation/Visualization & Robustness analysis
# Author: Sina Chen
# Notes: 
#
################################################################################

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


#### Helper functions ####

ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x)
logit.rv <- function(x) rvmapply(FUN = logit, x)


#### Data ####

# Poll level data
data <- readRDS('data.RDS')
data <- data %>% 
  mutate(swy = paste0(election_year,'_', swing),
         swy_int = as.integer(as.factor(swy)))

# Election level data
vote_sy <- data %>% 
  group_by(state_year, state_year_int, state, election_year, rep_result2,
           swing, ev_sc, swy, swy_int) %>% 
  summarise(n_poll = n())

# Swing state - year level data
data_swy <- data %>%
  group_by(election_year, swing, swy, swy_int) %>%
  summarise(n = n())

# Stan input data
stan_dat <- list(
  N = nrow(data),
  SY = length(unique(data$state_year)),
  SWY = length(unique(data$swy_int)),
  
  poll = data$rep_poll2,
  vote = vote_sy$rep_result2,
  
  sample_size = as.numeric(data$n) * (data$rep_result + data$dem_result),
  
  c_int = data$exp_week_sc,
  c_bal = data$diff_exp_sc,
  ev = vote_sy$ev_sc,
  after_con = data$after_con,
  dte = data$dte_sc,
  
  sy_id = data$state_year_int,
  swy_id = data$swy_int
)

# Stan fit objects
res_stan <- readRDS('camp_swy_mv.RDS') 
res_stan <- res_stan$stan_fit
res_stanSRGG <- readRDS('srgg.RDS') 
res_stanSRGG <- res_stanSRGG$stan_fit
res_stan25 <- readRDS('camp_swy_mv25.RDS') 
res_stan25 <- res_stan25$stan_fit
res_stan01 <- readRDS('camp_swy_mv01.RDS') 
res_stan01 <- res_stan01$stan_fit

#### Preparation ####

postrv <- as.rv(res_stan) # random variable of estimated parametrs
postrvSRGG <- as.rv(res_stanSRGG) # random variable of estimated parametrs
postrv25 <- as.rv(res_stan25) # random variable of estimated parametrs
postrv01 <- as.rv(res_stan01) # random variable of estimated parametrs

sy <- stan_dat$sy # state-year identifier
year_id <- data %>%
  group_by(state_year,election_year) %>%
  summarise(n_poll = n())
swy <- vote_sy$swy_int
swy_poll <- data$swy_int


#-------------------------------------------------------------------------------
#### Election level bias and poll variance ####

# Estimated poll mean
p_i <- ilogit.rv(
  logit(stan_dat$vote[sy]) +
  postrv$alpha[sy] + 
  postrv$beta1[swy_poll] * stan_dat$c_bal + 
  postrv$beta2[sy] * stan_dat$dte 
)

# Estimated poll mean on election day 
p_i_a <- ilogit.rv(
  logit(stan_dat$vote[sy]) + postrv$alpha[sy]  + 
    postrv$beta1[swy_poll] * stan_dat$c_bal
)

# Estimated variance
sigma_sqr_i <- exp(log(p_i*(1-p_i)/stan_dat$sample_size) + 
                     postrv$tau_sq[sy] +
                     postrv$gamma1[swy_poll] * stan_dat$c_int +
                     postrv$gamma2 * stan_dat$after_con +
                     postrv$gamma3 * vote_sy$ev_sc[sy])

# Estimated excess variance
sigma_sqr_ex_i <- exp(log(p_i*(1-p_i)/stan_dat$sample_size) + 
                        postrv$tau_sq[sy] +
                        postrv$gamma1[swy_poll] * stan_dat$c_int +
                        postrv$gamma2 * stan_dat$after_con +
                        postrv$gamma3 * vote_sy$ev_sc[sy]) - 
                  (p_i*(1-p_i)/stan_dat$sample_size)



# Calculating average absolute election level bias
b_r<- rv(length(unique(sy)))
b_r_a<- rv(length(unique(sy)))
var_r <- rv(length(unique(sy)))
ex_var_r <- rv(length(unique(sy)))

for(i in 1:length(unique(sy))){
  b_r[i] <- mean(p_i[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_a[i] <- mean(p_i_a[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  var_r[i] <- mean(sigma_sqr_i[i == stan_dat$sy]) 
}
for(i in 1:length(unique(sy))){
  ex_var_r[i] <- mean(sigma_sqr_ex_i[i == stan_dat$sy]) 
}
mabs_b_r <- mean(abs(b_r))
mabs_b_r_a <- mean(abs(b_r_a))
mabs_var_r <- mean(abs(var_r))
mabs_ex_var_r <- mean(abs(ex_var_r))

mabs_main <- rbind(mabs_b_r, mabs_b_r_a, mabs_var_r, mabs_ex_var_r)


### Parameter estimates ###

# Alpha
alpha_summary <- summary(postrv$alpha)
alpha_summary$sy_id <-seq(1:131)
alpha_summary <- merge(alpha_summary, vote_sy, by.x = 'sy_id', 
                       by.y = 'state_year_int')

ggplot(alpha_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_grid(~ election_year) +
  labs(x = expression(alpha~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(alpha_summary$state)))) +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

alpha_mean_summary <- mean(alpha_summary$mean)
alpha_sd_summary <- mean(alpha_summary$sd)
alpha_var_summary <- var(alpha_summary$mean)

alpha_mean_year <- aggregate(alpha_summary$mean, 
                             by = list(year = alpha_summary$election_year), 
                             mean)
alpha_var_year <- aggregate(alpha_summary$mean, 
                            by = list(year = alpha_summary$election_year), var)
alpha_sd_year <- aggregate(alpha_summary$sd, 
                           by = list(year = alpha_summary$election_year), mean)

alpha_table <- alpha_summary[,c('mean', '2.5%', '97.5%','state', 
                                'election_year')]
names(alpha_table) <- c('Mean', '2.5%', '97.5%','State', 'Year')

print(xtable(subset(alpha_table, Year == 2004), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(alpha_table, Year == 2008), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(alpha_table, Year == 2012), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(alpha_table, Year == 2016), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)

# Beta1
beta1_summary <- summary(postrv$beta1)
beta1_summary$swy_int <-seq(1:8)
beta1_summary <- merge(beta1_summary, data_swy, by = 'swy_int')

ggplot(beta1_summary, aes(x, y = as.factor(swing))) +
  geom_segment(aes( x = `2.5%`, xend = `97.5%`, yend = as.factor(swing))) +
  geom_segment(aes( x = `25%`, xend = `75%`, yend = as.factor(swing)), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_grid(~ election_year) +
  labs(x = expression(beta[1]~credible~intervals), y = '') +
  scale_y_discrete(labels = c ('Safe state', 'Swing state')) +
  theme_bw()  + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

beta1_mean_summary <- mean(beta1_summary$mean)
beta1_sd_summary <- mean(beta1_summary$sd)
beta1_var_summary <- var(beta1_summary$mean)

beta1_table <- beta1_summary[,c('mean', '2.5%', '97.5%','swing', 
                                'election_year')]
names(beta1_table) <- c('Mean', '2.5%', '97.5%','Swing state dummy', 'Year')

print(xtable(beta1_table, digits = c(0,3,3,3,0,0)), include.rownames = F, 
      sanitize.text.function = force)

# Beta2
beta2_summary <- summary(postrv$beta2)
beta2_summary$sy_id <-seq(1:131)
beta2_summary <- merge(beta2_summary, vote_sy, by.x = 'sy_id', 
                       by.y = 'state_year_int')

ggplot(beta2_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_grid(~ election_year) +
  labs(x = expression(beta[2]~credible~intervals), y = 'State')  +
  scale_y_discrete(limits = rev(levels(as.factor(beta2_summary$state)))) +
  theme_bw() + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

beta2_mean_summary <- mean(beta2_summary$mean)
beta2_sd_summary <- mean(beta2_summary$sd)
beta2_var_summary <- var(beta2_summary$mean)

beta2_table <- beta2_summary[,c('mean', '2.5%', '97.5%','state', 'election_year')]
names(beta2_table) <- c('Mean', '2.5%', '97.5%','State', 'Year')

print(xtable(subset(beta2_table, Year == 2004), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(beta2_table, Year == 2008), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(beta2_table, Year == 2012), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(beta2_table, Year == 2016), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)

# Tau
tau_summary <- summary(postrv$tau)
tau_summary$sy_id <-seq(1:131)
tau_summary <- merge(tau_summary, vote_sy, by.x = 'sy_id', 
                     by.y = 'state_year_int')

ggplot(tau_summary, aes(x)) +
  geom_segment(aes(y = state, yend = state, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = state, yend = state, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_grid(~ election_year) +
  labs(x = expression(tau~credible~intervals), y = 'State') +
  scale_y_discrete(limits = rev(levels(as.factor(tau_summary$state)))) +
  theme_bw() + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

tau_mean_summary <- mean(tau_summary$mean)
tau_sd_summary <- mean(tau_summary$sd)
tau_var_summary <- var(tau_summary$mean)

tau_table <- tau_summary[,c('mean', '2.5%', '97.5%','state', 'election_year')]
names(tau_table) <- c('Mean', '2.5%', '97.5%','State', 'Year')

print(xtable(subset(tau_table, Year == 2004), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(tau_table, Year == 2008), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(tau_table, Year == 2012), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)
print(xtable(subset(tau_table, Year == 2016), digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)

# Gamma1
gamma1_summary <- summary(postrv$gamma1)
gamma1_summary$swy_int <-seq(1:8)
gamma1_summary <- merge(gamma1_summary, data_swy, by = 'swy_int')

ggplot(gamma1_summary, aes(x, y = as.factor(swing))) +
  geom_segment(aes( x = `2.5%`, xend = `97.5%`, yend = as.factor(swing))) +
  geom_segment(aes( x = `25%`, xend = `75%`, yend = as.factor(swing)), size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_grid(~ election_year) +
  labs(x = expression(gamma[1]~credible~intervals), y = '') +
  scale_y_discrete(labels = c ('Safe state', 'Swing state')) +
  theme_bw() + 
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))

gamma1_mean_summary <- mean(gamma1_summary$mean)
gamma1_sd_summary <- mean(gamma1_summary$sd)
gamma1_var_summary <- var(gamma1_summary$mean)

gamma1_table <- gamma1_summary[,c('mean', '2.5%', '97.5%','swing', 
                                  'election_year')]
names(gamma1_table) <- c('Mean', '2.5%', '97.5%','Swing state dummy', 'Year')

print(xtable(gamma1_table, digits = c(0,3,3,3,0,0)), 
      include.rownames = F, sanitize.text.function = force)

# Gamma2 & 3
gamma2_summary <- summary(postrv$gamma2)
gamma2_summary$par <- 'Gamma2'

gamma3_summary <- summary(postrv$gamma3)
gamma3_summary$par <- 'Gamma3'

gamma2_3 <- rbind(gamma2_summary, gamma3_summary)

ggplot(gamma2_3, aes(x)) +
  geom_segment(aes(y = par, yend = par, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = par, yend = par, x = `25%`, xend = `75%`), size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = '', y = '') +
  scale_y_discrete(limits = rev(levels(as.factor(gamma2_3$par))),
                   labels = c('Gamma2' = expression(gamma[2]),
                              'Gamma3'   = expression(gamma[3]))) +
  theme_bw() + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 18)) +
  labs(x = 'Credible intervals')


### Plot bias vs. balance ###

p_summary <- summary(p_i)
p_summary <- cbind(p_summary, data[,c('state', 'election_year', 
                                      'swing', 'state_year_int', 
                                      'diff_exp_sc','rep_result2')])
p_summary$bias <- p_summary$mean - p_summary$rep_result2

p_summary_grouped <- p_summary %>%
  group_by(election_year, state, swing, diff_exp_sc,state_year_int) %>%
  summarise(mean_bias = mean(bias))

ggplot(p_summary_grouped, aes(x = diff_exp_sc, y = mean_bias,
                              group = as.factor(state_year_int),
                              color = as.factor(swing))) +
  geom_line(alpha = 0.2) +
  facet_grid(~election_year) + 
  scale_color_manual(values = c('#8A9045FF','#C16622FF'), name = '', 
                     labels = c('Safe state', 'Swing state')) + 
  labs(x = 'Scaled balance', y = 'Total bias') +
  geom_smooth(data = p_summary_grouped, 
              aes(x = diff_exp_sc, y = mean_bias, 
                  group = as.factor(swing),
                  color = as.factor(swing)), se = T) +
  theme_bw() +
  theme(legend.position="bottom",
        text = element_text(size = 18),
        axis.text = element_text(size = 12))


### Plot variance vs. intensity ###

# Campaign expenditures
sigma_sqr_summary <- summary(sigma_sqr_i)
sigma_sqr_summary <- cbind(sigma_sqr_summary, 
                           data[,c('state', 
                                   'election_year', 
                                   'swing', 
                                   'state_year_int', 
                                   'exp_week_sc', 
                                   'ev_sc')])

sigma_sqr_summary_sy <- sigma_sqr_summary %>%
  group_by(election_year, state, swing, exp_week_sc,state_year_int, ev_sc) %>%
  summarise(mean_var = mean(mean))

ggplot(sigma_sqr_summary_sy, aes(x = exp_week_sc, y = mean_var,
                                 group = as.factor(state_year_int),
                                 color = as.factor(swing))) +
  geom_line(alpha = 0.2) +
  facet_grid(~election_year) + 
  scale_color_manual(values = c('#8A9045FF','#C16622FF'), name = '', 
                     labels = c('Safe state', 'Swing state')) + 
  labs(x = 'Scaled expenditure', y = 'Total variance') +
  geom_smooth(data = sigma_sqr_summary_sy, 
              aes(x = exp_week_sc, 
                  y = mean_var, 
                  group = as.factor(swing),
                  color = as.factor(swing)), se = T) +
  theme_bw() +
  theme(legend.position="bottom",
        text = element_text(size = 18),
        axis.text = element_text(size = 12))

# Electoral votes
ggplot(sigma_sqr_summary_sy, aes(x = ev_sc, y = mean_var)) +
  geom_point(alpha = 0.2) +
  labs(x = 'Scaled electoral college votes', y = 'Total variance') +
  geom_smooth(color = 'darkgrey') +
  theme_bw() +
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 12))


#-------------------------------------------------------------------------------
#### Comparison with SRGG ####

# Alpha
alphaSRGG_summary <- summary(postrvSRGG$alpha)
alphaSRGG_summary$sy_id <-seq(1:131)
alphaSRGG_summary <- merge(alphaSRGG_summary, vote_sy, by.x = 'sy_id', 
                           by.y = 'state_year_int')

alphaSRGG_mean_summary <- mean(alphaSRGG_summary$mean)
alphaSRGG_sd_summary <- mean(alphaSRGG_summary$sd)
alphaSRGG_var_summary <- var(alphaSRGG_summary$mean)

# Beta1
beta1RSGG_summary <- summary(postrvSRGG$beta)
beta1RSGG_summary$sy_id <-seq(1:131)
beta1RSGG_summary <- merge(beta1RSGG_summary, vote_sy, by.x = 'sy_id', 
                           by.y = 'state_year_int')

beta1SRGG_mean_summary <- mean(beta1RSGG_summary$mean)
beta1SRGG_sd_summary <- mean(beta1RSGG_summary$sd)
beta1SRGG_var_summary <- var(beta1RSGG_summary$mean)

# Tau
tauSRGG_summary <- summary(postrvSRGG$tao_sqr)
tauSRGG_summary$sy_id <-seq(1:131)
tauSRGG_summary <- merge(tauSRGG_summary, vote_sy, by.x = 'sy_id', 
                         by.y = 'state_year_int')

tauSRGG_mean_summary <- mean(tauSRGG_summary$mean)
tauSRGG_sd_summary <- mean(tauSRGG_summary$sd)
tauSRGG_var_summary <- var(tauSRGG_summary$mean)

#-------------------------------------------------------------------------------
#### Model fit #####
# R hat
color_scheme_set("darkgray")


rhats <- rhat(res_stan)
mcmc_rhat_hist(rhats) + theme(text = element_text(size = 20),
                              legend.text = element_text(size = 20),
                              legend.position = 'bottom')

# Effective sampple size
neffs <- neff_ratio(res_stan)
mcmc_neff_hist(neffs) + theme(text = element_text(size = 20),
                              legend.text = element_text(size = 20),
                              legend.position = 'bottom')
which(neffs < 0.5)


#-------------------------------------------------------------------------------
#### Posterior predictive check ####

sim_fit <- extract(res_stan)

# Simulate data
n_sims <- length(sim_fit$lp__) 
y_rep <- array(NA, c(n_sims, nrow(data))) 
p_i <- array(NA, c(n_sims, nrow(data))) 
logit_p <- array(NA, c(n_sims, nrow(data))) 


# Calculate y_rep based on estimated parameter
for (s in 1:n_sims){
  print(s)
  logit_p[s,] <- logit(data$rep_result2) + 
    sim_fit$alpha[s,sy] + 
    sim_fit$beta1[s,swy_poll] * data$diff_exp_sc + 
    sim_fit$beta2[s,sy] * data$dte_sc
  
  p_i[s,] <- invlogit(logit_p[s,])
  sigma <- sqrt(exp(log(p_i[s,]*(1-p_i[s,])/
                          (as.numeric(data$n) * 
                             (data$rep_result + data$dem_result))) + 
                      sim_fit$tau[s,sy] +
                      sim_fit$gamma1[s,swy_poll] * data$exp_week_sc +
                      sim_fit$gamma2[s] * data$after_con +
                      sim_fit$gamma3[s] * data$ev_sc))
  y_rep[s,] <- rnorm(nrow(data), p_i[s,], sigma)
}

rm(logit_p, sigma, s)

# Plot
y <- data$rep_poll2
y_rep1000 <- y_rep[sample(1:30000, 1000),]


ppc_dens_overlay(y, y_rep1000) +
  xlim(-0.1,1.1) + 
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20))


#-------------------------------------------------------------------------------
#### Sensetivity Analysis ####

### 25% ###

# Alpha
alpha25_summary <- summary(postrv25$alpha)
alpha25_summary$sy_id <-seq(1:131)
alpha25_summary <- merge(alpha25_summary, vote_sy, by.x = 'sy_id', 
                         by.y = 'state_year_int')

alpha25_mean_summary <- mean(alpha25_summary$mean)
alpha25_sd_summary <- mean(alpha25_summary$sd)
alpha25_var_summary <- var(alpha25_summary$mean)

# Beta1
beta1_25_summary <- summary(postrv25$beta1)
beta1_25_summary$swy_int <-seq(1:8)
beta1_25_summary <- merge(beta1_25_summary, data_swy, by = 'swy_int')

beta1_25_mean_summary <- mean(beta1_25_summary$mean)
beta1_25_sd_summary <- mean(beta1_25_summary$sd)
beta1_25_var_summary <- var(beta1_25_summary$mean)

# Beta2
beta2_25_summary <- summary(postrv25$beta2)
beta2_25_summary$sy_id <-seq(1:131)
beta2_25_summary <- merge(beta2_25_summary, vote_sy, by.x = 'sy_id', 
                          by.y = 'state_year_int')

beta2_25_mean_summary <- mean(beta2_25_summary$mean)
beta2_25_sd_summary <- mean(beta2_25_summary$sd)
beta2_25_var_summary <- var(beta2_25_summary$mean)

# Tau
tau25_summary <- summary(postrv25$tau)
tau25_summary$sy_id <-seq(1:131)
tau25_summary <- merge(tau25_summary, vote_sy, by.x = 'sy_id', 
                       by.y = 'state_year_int')

tau25_mean_summary <- mean(tau25_summary$mean)
tau25_sd_summary <- mean(tau25_summary$sd)
tau25_var_summary <- var(tau25_summary$mean)

# Gamma1
gamma1_25_summary <- summary(postrv25$gamma1)
gamma1_25_summary$swy_int <-seq(1:8)
gamma1_25_summary <- merge(gamma1_25_summary, data_swy, by = 'swy_int')

gamma1_25_mean_summary <- mean(gamma1_25_summary$mean)
gamma1_25_sd_summary <- mean(gamma1_25_summary$sd)
gamma1_25_var_summary <- var(gamma1_25_summary$mean)

# Gamma2 & 3
gamma2_25_summary <- summary(postrv25$gamma2)
gamma3_25_summary <- summary(postrv25$gamma3)


### Election level bias and poll variance ###

# Estimated poll mean 
p_i25 <- ilogit.rv(
  logit(stan_dat$vote[sy]) +
    postrv25$alpha[sy] + 
    postrv25$beta1[swy_poll] * stan_dat$c_bal + 
    postrv25$beta2[sy] * stan_dat$dte 
)

# Estimated poll mean on election day
p_i_a25 <- ilogit.rv(
  logit(stan_dat$vote[sy]) + postrv25$alpha[sy]  + 
    postrv25$beta1[swy_poll] * stan_dat$c_bal
)

# Estimated variance
sigma_sqr25_i <- exp(log(p_i25*(1-p_i25)/stan_dat$sample_size) + 
                       postrv25$tau[sy] +
                       postrv25$gamma1[swy_poll] * stan_dat$c_int +
                       postrv25$gamma2 * stan_dat$after_con +
                       postrv25$gamma3 * vote_sy$ev_sc[sy])

# Estimated excess variance
sigma_sqr_ex25_i <- exp(log(p_i25*(1-p_i25)/stan_dat$sample_size) + 
                          postrv25$tau[sy] +
                          postrv25$gamma1[swy_poll] * stan_dat$c_int +
                          postrv25$gamma2 * stan_dat$after_con +
                          postrv25$gamma3 * vote_sy$ev_sc[sy]) -
  (p_i25*(1-p_i25)/stan_dat$sample_size)


# Calculating average absolute election level bias
b_r25<- rv(length(unique(sy)))
b_r_a25<- rv(length(unique(sy)))
var25_r <- rv(length(unique(sy)))
ex_var25_r <- rv(length(unique(sy)))

for(i in 1:length(unique(sy))){
  b_r25[i] <- mean(p_i25[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_a25[i] <- mean(p_i_a25[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  var25_r[i] <- mean(sigma_sqr25_i[i == stan_dat$sy]) 
}
for(i in 1:length(unique(sy))){
  ex_var25_r[i] <- mean(sigma_sqr_ex25_i[i == stan_dat$sy]) 
}

mabs_b_r25 <- mean(abs(b_r25))
mabs_b_r_a25 <- mean(abs(b_r_a25))
mabs_var25_r <- mean(abs(var25_r))
mabs_ex_var25_r <- mean(abs(ex_var25_r))

mabs_25 <- rbind(mabs_b_r25, mabs_b_r_a25, mabs_var25_r, mabs_ex_var25_r)


### 01% ###

# Alpha
alpha01_summary <- summary(postrv01$alpha)
alpha01_summary$sy_id <-seq(1:131)
alpha01_summary <- merge(alpha01_summary, vote_sy, by.x = 'sy_id', 
                         by.y = 'state_year_int')

alpha01_mean_summary <- mean(alpha01_summary$mean)
alpha01_sd_summary <- mean(alpha01_summary$sd)
alpha01_var_summary <- var(alpha01_summary$mean)

# Beta1
beta1_01_summary <- summary(postrv01$beta1)
beta1_01_summary$swy_int <-seq(1:8)
beta1_01_summary <- merge(beta1_01_summary, data_swy, by = 'swy_int')

beta1_01_mean_summary <- mean(beta1_01_summary$mean)
beta1_01_sd_summary <- mean(beta1_01_summary$sd)
beta1_01_var_summary <- var(beta1_01_summary$mean)

# Beta2
beta2_01_summary <- summary(postrv01$beta2)
beta2_01_summary$sy_id <-seq(1:131)
beta2_01_summary <- merge(beta2_01_summary, vote_sy, by.x = 'sy_id', 
                          by.y = 'state_year_int')

beta2_01_mean_summary <- mean(beta2_01_summary$mean)
beta2_01_sd_summary <- mean(beta2_01_summary$sd)
beta2_01_var_summary <- var(beta2_01_summary$mean)

# Tau
tau01_summary <- summary(postrv01$tau)
tau01_summary$sy_id <-seq(1:131)
tau01_summary <- merge(tau01_summary, vote_sy, by.x = 'sy_id', 
                       by.y = 'state_year_int')

tau01_mean_summary <- mean(tau01_summary$mean)
tau01_sd_summary <- mean(tau01_summary$sd)
tau01_var_summary <- var(tau01_summary$mean)

# Gamma1
gamma1_01_summary <- summary(postrv01$gamma1)
gamma1_01_summary$swy_int <-seq(1:8)
gamma1_01_summary <- merge(gamma1_01_summary, data_swy, by = 'swy_int')

gamma1_01_mean_summary <- mean(gamma1_01_summary$mean)
gamma1_01_sd_summary <- mean(gamma1_01_summary$sd)
gamma1_01_var_summary <- var(gamma1_01_summary$mean)

# Gamma2 & 3
gamma2_01_summary <- summary(postrv01$gamma2)
gamma3_01_summary <- summary(postrv01$gamma3)


### Election level bias and poll variance ###

# Estimated poll mean
p_i01 <- ilogit.rv(
  logit(stan_dat$vote[sy]) +
    postrv01$alpha[sy] + 
    postrv01$beta1[swy_poll] * stan_dat$c_bal + 
    postrv01$beta2[sy] * stan_dat$dte 
)

# Estimated poll mean on election day
p_i_a01 <- ilogit.rv(
  logit(stan_dat$vote[sy]) + postrv01$alpha[sy]  + 
    postrv01$beta1[swy_poll] * stan_dat$c_bal
)

# Estimated variance
sigma_sqr01_i <- exp(log(p_i01*(1-p_i01)/stan_dat$sample_size) + 
                       postrv01$tau[sy] +
                       postrv01$gamma1[swy_poll] * stan_dat$c_int +
                       postrv01$gamma2 * stan_dat$after_con +
                       postrv01$gamma3 * vote_sy$ev_sc[sy])

# Estimated excess variance
sigma_sqr_ex01_i <- exp(log(p_i01*(1-p_i01)/stan_dat$sample_size) + 
                          postrv01$tau[sy] +
                          postrv01$gamma1[swy_poll] * stan_dat$c_int +
                          postrv01$gamma2 * stan_dat$after_con +
                          postrv01$gamma3 * vote_sy$ev_sc[sy]) -
  (p_i01*(1-p_i01)/stan_dat$sample_size) 

# Calculating average absolute election level bias
b_r01<- rv(length(unique(sy)))
b_r_a01<- rv(length(unique(sy)))
var01_r <- rv(length(unique(sy)))
ex_var01_r <- rv(length(unique(sy)))

for(i in 1:length(unique(sy))){
  b_r01[i] <- mean(p_i01[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  b_r_a01[i] <- mean(p_i_a01[i == stan_dat$sy]) - stan_dat$vote[i]
}
for(i in 1:length(unique(sy))){
  var01_r[i] <- mean(sigma_sqr01_i[i == stan_dat$sy]) 
}
for(i in 1:length(unique(sy))){
  ex_var01_r[i] <- mean(sigma_sqr_ex01_i[i == stan_dat$sy]) 
}

mabs_b_r01 <- mean(abs(b_r01))
mabs_b_r_a01 <- mean(abs(b_r_a01))
mabs_var01_r <- mean(abs(var01_r))
mabs_ex_var01_r <- mean(abs(ex_var01_r))

mabs_01 <- rbind(mabs_b_r01, mabs_b_r_a01, mabs_var01_r, mabs_ex_var01_r)




