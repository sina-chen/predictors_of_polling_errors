#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Posterior visualization Bundestag election polls 1994 - 2021
# 
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(rv) 
library(ggplot2) 
library(rstan) 
library(scales)
library(xtable)
library(dplyr)
library(splines)


# Data --------------------------------------------------------------------

# poll data
polls <- readRDS('~/data/polls1990_2021.RDS')

# stan fit obj.
resStan <- readRDS("~/fit_stan/resStan_btw_splines.RDS")



# Functions ---------------------------------------------------------------

source("~/results_vis/helper_res_vis_btw_splines.R")


#-------------------------------------------------------------------------------


# Preparation -------------------------------------------------------------

# number of simulations for rv
setnsims(XXX)

# subset polls
polls <- polls %>% 
  group_by(poll_id) %>% 
  mutate(n_party = n()) %>% 
  ungroup() %>% 
  subset(!is.na(support) & 
           !is.na(date) & 
           !election %in% c(1990,2025) &
           n_party == 6 #&
         #days_to_election <= 365
  ) %>% 
  droplevels() %>% 
  mutate(poll_id_int = as.integer(as.factor(poll_id)))

# order by poll id and party
polls <- polls[order(polls$poll_id_int, polls$party),]

# specify levels for election-party id
order_kr <-paste0(rep(c(1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021), 
                      each = 6),
                  rep(c("cdu", "fdp", "gru", "lin","spd", "oth"), 8)) 

# compute election-party id
polls <- polls %>% 
  mutate(kr = factor(paste0(election, party), levels = order_kr),
         kr_id = as.integer(kr),
         rl = paste0(election, institute),
         rl_id = as.integer(as.factor(rl)),
         t_sc = as.numeric(days_to_election)/max(as.numeric(days_to_election)))  %>% 
  group_by(poll_id_int, election, institute, t_sc, sample_size) %>% 
  mutate(r_id = case_when(election == 1994 ~ 1,
                          election == 1998 ~ 2,
                          election == 2002 ~ 3,
                          election == 2005 ~ 4,
                          election == 2009 ~ 5,
                          election == 2013 ~ 6,
                          election == 2017 ~ 7,
                          election == 2021 ~ 8),
         l_id = as.integer(as.factor(institute)),
         k_id = as.integer(as.factor(party)),
         kl = factor(paste0(institute, party)),
         kl_id = as.integer(as.factor(kl))) %>% 
  ungroup() 

# poll level data
poll_level <- polls %>% 
  group_by(poll_id_int, election, institute, t_sc, sample_size, r_id, l_id) %>% 
  summarise() %>% 
  ungroup() 

# election-party level data
election_party_level <- polls %>%  
  group_by(election, party, voteshare, kr, kr_id, k_id,r_id) %>%  
  summarise()

# institute-party level data
institute_party_level <- polls %>%  
  group_by(institute, party, l_id, kl, kl_id) %>%  
  summarise()

# create b-splines design matrix
B <- t(bs(poll_level$t_sc, knots = quantile(poll_level$t_sc, 
                                            probs= seq(0.1,0.9,0.1)), 
          degree = 3,
          Boundary.knots = c(0,1)))


# generate random variable of estimated parameters 
postrv <- as.rv(resStan)  


# Posterior estimation ----------------------------------------------------

# election day bias
bias0_i <- estimate_bias0(polls = polls,
                        postrv = postrv)

# party election level election day bias
b0_kr <- rv(length(unique(polls$kr)))

for (i in 1:length(unique(polls$kr))) {
  idx <- which(polls$kr_id == i)
  b0_kr[i] <- mean(bias0_i[idx])
}

rm(bias0_i)

# institute level bias by party
bias_l_i <- estimate_institute_bias(polls = polls,
                                    postrv = postrv)

b_kl <- rv(length(unique(polls$kl)))

for (i in 1:length(unique(polls$kl))) {
  b_kl[i] <- mean(bias_l_i[i == polls$kl_id])
}

rm(bias_l_i)

# mean p_i
splines <- get_splines(par = postrv$beta, b_splines = B, polls = polls)
p_i <- generate_predictions(polls = polls,
                            postrv = postrv,
                            splines = splines)

# mean p_i without institute effect
p_i_without_inst <- generate_predictions(polls = polls,
                            postrv = postrv,
                            splines = splines,
                            inc_inst = F)
rm(splines)

# mean estimate summary
p_summary <- summary(p_i_without_inst)
p_summary <- cbind(p_i_without_inst, polls[,c("election", "party", "support", 
                                       "days_to_election", "voteshare")])

# srs variance
srs_var_i <- rv(nrow(polls))
for(i in 1:nrow(polls)){
  srs_var_i[[i]] <- p_i[[i]] * (1 - p_i[[i]])/polls$sample_size[i]
  print(i)
}
rm(p_i)

# election level srs variance
srs_var_kr <- rv(length(unique(polls$kr)))
for (i in 1:length(unique(election_party_level$kr_id))) {
  idx <- which(polls$kr_id == i)
  srs_var_kr[i] <- mean(srs_var_i[idx])
}

# election level srs se
srs_se_kr <- rv(length(unique(polls$kr)))
for (i in 1:length(unique(election_party_level$kr_id))) {
  idx <- which(polls$kr_id == i)
  srs_se_kr[i] <- mean(sqrt(srs_var_i[idx]))
}

log_srs_var <- rvmapply(srs_var_i, FUN = log)
rm(srs_var_i)

# total variance
log_var_i <- rv(nrow(polls))
for(i in 1:nrow(polls)){
  log_var_i[[i]] <- log_srs_var[[i]] +
    postrv$phi[polls$r_id[i], polls$k_id[i]]
  print(i)
}

# election level total variance
var_kr <- rv(length(unique(polls$kr)))
for (i in 1:nrow(election_party_level)) {
  var_kr[i] <- exp(log(srs_var_kr[i]) + 
    postrv$phi[election_party_level$r_id[i], election_party_level$k_id[i]]) 
}

# election level total se
se_kr <- rvmapply(var_kr, FUN = sqrt)

# design effect
deff_kr <- var_kr/srs_var_kr


# saveRDS(p_summary, "~/results_vis/p_summary_btw94_21.RDS")
# saveRDS(b0_kr, "~/results_vis/b0_kr_btw94_21.RDS")
# saveRDS(b_kl, "~/results_vis/b_kl_btw94_21.RDS")
# saveRDS(srs_var_kr, "~/results_vis/srs_var_kr_btw94_21.RDS")
# saveRDS(srs_se_kr, "~/results_vis/srs_se_kr_btw94_21.RDS")
# saveRDS(var_kr, "~/results_vis/var_kr_btw94_21.RDS")
# saveRDS(se_kr, "~/results_vis/se_kr_btw94_21.RDS")
# saveRDS(deff_kr, "~/results_vis/deff_kr_btw94_21.RDS")

p_summary <- readRDS( "~/results_vis/p_summary_btw94_21.RDS")
b_kr <- readRDS( "~/results_vis/b_kr_btw94_21.RDS")
b0_kr <- readRDS( "~/results_vis/b0_kr_btw94_21.RDS")
b_kl <- readRDS( "~/results_vis/b_kl_btw94_21.RDS")
srs_var_kr <- readRDS( "~/results_vis/srs_var_kr_btw94_21.RDS")
srs_se_kr <- readRDS( "~/results_vis/srs_se_kr_btw94_21.RDS")
var_kr <- readRDS( "~/results_vis/var_kr_btw94_21.RDS")
se_kr <- readRDS( "~/results_vis/se_kr_btw94_21.RDS")
deff_kr <- readRDS( "~/results_vis/deff_kr_btw94_21.RDS")




# Bias and variance summaries ---------------------------------------------

# election day bias
b0_kr_summary <- summary(b0_kr)
b0_kr_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
b0_kr_summary <- merge(b0_kr_summary, election_party_level, by = 'kr_id') %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 
                                   'others')))

# party-institute bias
b_kl_summary <- summary(b_kl)
b_kl_summary$kl_id <- seq(1:length(unique(polls$kl_id)))
b_kl_summary <- merge(b_kl_summary, institute_party_level, by = 'kl_id') %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 
                                   'others')),
         institute = factor(institute, 
                            levels = c('allensbach', 'emnid', 'forsa', 
                                       'politbarometer', 'gms', 'dimap', 'insa',
                                       'yougov', "civey"),
                            labels = c('IfD Allensbach', 'TNS Emnid/Kantar', 
                                       'Forsa', 'Forschungsgruppe Wahlen', 
                                       'GMS', 'Infratest dimap', 'INSA', 
                                       'YouGov', "Civey")))

# design effect
deff_summary <- summary(deff_kr)
deff_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
deff_summary$var_type <- 'Design effect'
deff_summary <- merge(deff_summary, election_party_level, by = 'kr_id') %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 
                                   'others')))

# SRS var
srs_var_summary <- summary(srs_var_kr)
srs_var_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
srs_var_summary$var_type <- 'SRS variance'
srs_var_summary <- merge(srs_var_summary, election_party_level, by = 'kr_id') %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 
                                   'others')))

# SRS se
srs_se_summary <- summary(srs_se_kr)
srs_se_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
srs_se_summary$se_type <- 'SRS standard error'
srs_se_summary <- merge(srs_se_summary, election_party_level, by = 'kr_id') %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 
                                   'others')))
# total se
se_summary <- summary(se_kr)
se_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
se_summary$se_type <- 'Total standard error'
se_summary <- merge(se_summary, election_party_level, by = 'kr_id') %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 
                                   'others')))



# Tables ------------------------------------------------------------------

# average absolute election level bias
mean(abs(b_kr))
mean(abs(b0_kr))


# average abs. bias on election day by party
mab0_k <- b0_kr_summary %>%
  group_by(party) %>% 
  summarise(error = format(round(mean(abs(mean)*100),3), nsmall = 2),
            sd = format(round(mean(sd)*100,3), nsmall = 2),
            mu_b0 = paste0(error,' (', sd,')')) %>% 
  select(party, mu_b0) %>% 
  t() %>% 
  data.frame()

colnames(mab0_k) <- mab0_k[1,]
mab0_k <- mab0_k[-1,]

# average election level total standard error by party
msrs_k <- srs_var_summary %>%
  group_by(party) %>% 
  summarise(error = format(round(mean(mean*100),3), nsmall = 2),
            sd = format(round(mean(sd)*100,3), nsmall = 2),
            mu_b0 = paste0(error,' (', sd,')')) %>% 
  select(party, mu_b0) %>% 
  t() %>% 
  data.frame()

colnames(msrs_k) <- msrs_k[1,]
msrs_k <- msrs_k[-1,]

# average election level excess standard deviation by party
mdeff_k <- deff_summary %>%
  group_by(party) %>% 
  summarise(error = format(round(mean(mean),3), nsmall = 2),
            sd = format(round(mean(sd),3), nsmall = 2),
            mu_b0 = paste0(error,' (', sd,')')) %>% 
  select(party, mu_b0) %>% 
  t() %>% 
  data.frame()

colnames(mdeff_k) <- mdeff_k[1,]
mdeff_k <- mdeff_k[-1,]

# Merge errors
error <- rbind(mab0_k, msrs_k, mdeff_k)
rownames(error) <- c('Absolute election day bias',
                     'SRS variance',
                     'Design effect')

xtable(error)



# Plots -------------------------------------------------------------------

# election level bias on election day
b0_kr_plot <- ggplot(b0_kr_summary, aes(x)) + 
  geom_segment(aes(y = party, yend = party, x = `2.5%`, xend = `97.5%`)) + 
  geom_segment(aes(y = party, yend = party, x = `25%`, xend = `75%`), 
               size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = '\n Election day bias', y = 'Party') + 
  scale_y_discrete(limits = rev(levels(as.factor(b0_kr_summary$party)))) + 
  theme_bw()  + 
  theme(text = element_text(size = 14), axis.text.x = element_text(size = 14),
        axis.title=element_text(size=14), 
        axis.text.y = element_text(size = 14),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm")) +
  facet_wrap(~ election, ncol = 4)  +
  scale_x_continuous(labels=label_percent(accuracy = 1), 
                     breaks = seq(-0.1, 0.15, 0.05))

ggsave(filename = 'btw94_21_election_day_bias_old.png', 
       plot = b0_kr_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 6)

# party-institute level bias
b_kl_plot <- ggplot(b_kl_summary, aes(x)) + 
  geom_segment(aes(y = party, yend = party, x = `2.5%`, xend = `97.5%`)) + 
  geom_segment(aes(y = party, yend = party, x = `25%`, xend = `75%`), 
               size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = '\n Party-institute bias', y = 'Party') + 
  scale_y_discrete(limits = rev(levels(as.factor(b_kl_summary$party)))) + 
  theme_bw()  + 
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14)) +
  facet_wrap(~ institute, ncol = 3) +
  scale_x_continuous(labels=label_percent(accuracy = 1), 
                     breaks = seq(-0.02, 0.02, 0.01))

ggsave(filename = 'btw94_21_institute_bias.png', 
       plot = b_kl_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 6)


# total vs. SRS sd 
tot_srs_se <- rbind(srs_se_summary, se_summary)

tot_srs_plot <- ggplot(tot_srs_se, aes(x = party, y = mean, 
                                        color = se_type, fill = se_type,
                                        linetype = se_type)) +
  geom_bar(stat = 'identity', width = 0.8, 
           position = 'identity', alpha = 0.5) +
  facet_wrap(~election, ncol = 4) +
  theme_bw()+
  scale_y_continuous(labels=label_percent(accuracy = 0.1)) + 
  theme(legend.position = 'bottom', 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14),
        text = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 14),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14)) +
  labs(x = 'Party', y = '') +
  scale_color_manual(name = '', values = c('black', NA)) +
  scale_fill_manual(name = '', values = c(NA, "darkgrey")) +
  scale_linetype_manual(name = '',values = c("dashed", "blank"))

ggsave(filename = 'btw94_21_tot_srs_se_old.png', 
       plot = tot_srs_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 6)

# mean estimates 
btw94_21_estimate <- ggplot(p_summary, aes(x=as.numeric(days_to_election), 
                            y = mean, 
                            color = party)) +
  geom_line() +
  facet_wrap(~ election, ncol = 4) +
  scale_x_reverse() +
  theme_bw() +
  geom_point(aes(y = support/100, color = party), size = 0.7, alpha = 0.2) +
  geom_hline(aes(yintercept = voteshare/100, 
                 color = party), linetype = "dashed") +
  scale_color_manual(values = c('cdu' = '#000000', 
                                'spd' = '#E3000F', 
                                'gru' = '#46962b', 
                                'fdp' = '#FBEE31', 
                                'lin' = '#B1003A', 
                                'oth' = 'darkgrey'),
                     labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 
                                'others'),
                     name = 'Party') +
  labs(x = "Scaled days to election", y = "Average prediction")  + 
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size = 14),
        strip.text = element_text(size = 14)) 

ggsave(filename = "btw94_21_estimate_institute.png", 
       plot = btw94_21_estimate, 
       path = '~/results_vis/plots',
       width = 12, height = 8) 
