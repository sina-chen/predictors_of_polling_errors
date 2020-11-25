# --------------------------------------------------------------------------- #
# analysis of Stan objects
# run fit_stan_race_gender_region.R bevore
# --------------------------------------------------------------------------- #

res_stan_race <- resStan_race
res_stan_gender <- resStan_gender

# View(stan_dat)

# --------------------------------------------------------------------------- #

vote_sy <- data_senate %>%
  group_by(state_year, region_group4, election_year, state, rep_result2, 
           race_rep_dummy, race_dem_dummy, gender_rep_dummy,gender_dem_dummy) %>%
  summarise()

vote_sy$state_year_int <- as.integer(as.factor(vote_sy$state_year))
vote_sy$region_int <- as.integer(as.factor(vote_sy$region_group4))


postrv_race <- as.rv(res_stan_race) # random variable of estimated parametrs
postrv_gender <- as.rv(res_stan_gender) # random variable of estimated parametrs

sy <- stan_dat$sy # state-year identifier
rg <- stan_dat$rg_id # region identifier

# --------------------------------------------------------------------------- #

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

# --------------------------------------------------------------------------- #

### Parameter estimates ###

### Race ###

# Beta1
beta1_race_summary <- summary(postrv_race$beta1)
beta1_race_summary$rg_id <-seq(1:length(unique(data_senate$region_group4)))
beta1_race_summary <- merge(beta1_race_summary, vote_sy, by.x = 'rg_id', 
                            by.y = 'region_int')
beta1_race_summary$par <- 'Beta1'

ggplot(beta1_race_summary, aes(x)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(beta[1]~race~credible~intervals), y = 'Region') +
  scale_y_discrete(limits = rev(levels(as.factor(beta1_race_summary$region_group4)))) +
  theme_bw()  + 
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 10))

# Beta2
beta2_race_summary <- summary(postrv_race$beta2)
beta2_race_summary$rg_id <-seq(1:length(unique(data_senate$region_group4)))
beta2_race_summary <- merge(beta2_race_summary, vote_sy, by.x = 'rg_id', 
                            by.y = 'region_int')
beta1_race_summary$par <- 'Beta2'

ggplot(beta2_race_summary, aes(x)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(beta[2]~race~credible~intervals), y = 'Region') +
  scale_y_discrete(limits = rev(levels(as.factor(beta2_race_summary$region_group4)))) +
  theme_bw()  + 
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 10))


### Gender ###

# Beta1
beta1_gender_summary <- summary(postrv_race$beta1)
beta1_gender_summary$rg_id <-seq(1:length(unique(data_senate$region_group4)))
beta1_gender_summary <- merge(beta1_gender_summary, vote_sy, by.x = 'rg_id', 
                            by.y = 'region_int')
beta1_gender_summary$par <- 'Beta1'

ggplot(beta1_gender_summary, aes(x)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(beta[1]~gender~credible~intervals), y = 'Region') +
  scale_y_discrete(limits = rev(levels(as.factor(beta1_gender_summary$region_group4)))) +
  theme_bw()  + 
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 10))

# Beta2
beta2_gender_summary <- summary(postrv_race$beta2)
beta2_gender_summary$rg_id <-seq(1:length(unique(data_senate$region_group4)))
beta2_gender_summary <- merge(beta2_gender_summary, vote_sy, by.x = 'rg_id', 
                            by.y = 'region_int')
beta2_gender_summary$par <- 'Beta2'

ggplot(beta2_gender_summary, aes(x)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `2.5%`, xend = `97.5%`)) +
  geom_segment(aes(y = region_group4, yend = region_group4, x = `25%`, xend = `75%`), 
               size = 1.5) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ election_year, ncol = 6) +
  labs(x = expression(beta[2]~gender~credible~intervals), y = 'Region') +
  scale_y_discrete(limits = rev(levels(as.factor(beta2_gender_summary$region_group4)))) +
  theme_bw()  + 
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 10))

