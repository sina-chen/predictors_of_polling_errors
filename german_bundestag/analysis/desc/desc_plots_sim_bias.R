#-------------------------------------------------------------------------------
# Disentangling Bias and Variance in Multi-party Election Polls 
# 
# Descriptive plots
# Author: Peter Selb, John Körtner, Philipp Bosch, Sina Chen
#
# Data source: https://github.com/zweitstimme/btw-2017
#-------------------------------------------------------------------------------

#### Libraries ####

library(tidyverse)
library(reshape2)
library(data.table)
library(scales)
library(doBy)


#### Data ####

# polls
polls94_21 <- readRDS("data/polls1990_2021.RDS")
polls13_21 <- readRDS("data/polls2013_2021.RDS")


#-------------------------------------------------------------------------------

#### Pre-processing ####

polls94_21 <- polls94_21 %>% 
  group_by(poll_id) %>% 
  mutate(n_party = n()) %>% 
  ungroup() %>% 
  subset(!is.na(support) & 
           !is.na(date) & 
           !election %in% c(1990,2025) &
           n_party == 6 
  ) %>% 
  droplevels() %>% 
  mutate(poll_id_int = as.integer(as.factor(poll_id)))

id_afd <- polls13_21[which(polls13_21$party == "afd" & 
                             !is.na(polls13_21$support)),]$poll_id 

polls13_21 <- polls13_21 %>% 
  subset(poll_id %in% id_afd &
           !is.na(support) & 
           !is.na(date) & 
           election < 2025) %>% 
  group_by(poll_id) %>% 
  mutate(n_party = n()) %>% 
  ungroup() %>% 
  subset(n_party == 7 
  ) %>% 
  droplevels() %>% 
  mutate(poll_id_int = as.integer(as.factor(poll_id)),
         support = support,
         voteshare = voteshare)

# reshape to wide
polls94_21_wide <- data.table::dcast(setDT(polls94_21), 
                                     poll_id + sample_size ~ party,
                                     value.var = c('support', 'voteshare')) 

polls13_21_wide <- data.table::dcast(setDT(polls13_21), 
                                     poll_id + sample_size ~ party,
                                     value.var = c('support', 'voteshare')) 


# create sample of poll ids
id94_21 <- sample(1:nrow(polls94_21_wide), 100000, replace = T)
id13_21 <- sample(1:nrow(polls13_21_wide), 100000, replace = T)


# create srs from multinominal distribution
srs94_21 <- lapply(id94_21, 
                   function(x) rmultinom(n = 1, 
                                         size = round(polls94_21_wide$sample_size[x]),
                                         c(polls94_21_wide$voteshare_cdu[x],
                                           polls94_21_wide$voteshare_fdp[x],
                                           polls94_21_wide$voteshare_gru[x],
                                           polls94_21_wide$voteshare_lin[x],
                                           polls94_21_wide$voteshare_spd[x],
                                           polls94_21_wide$voteshare_oth[x]))/round(polls94_21_wide$sample_size[x]))


srs94_21 <- matrix(unlist(srs94_21), nrow = length(srs94_21), byrow = T) %>%  
  as.data.frame()
names(srs94_21) <- c('cdu', 'fdp', 'gru', 'lin', 'spd', 'oth')

srs13_21 <- lapply(id13_21,
                   function(x) rmultinom(n = 1,
                                         size = round(polls13_21_wide$sample_size[x]),
                                         c(polls13_21_wide$voteshare_afd[x],
                                           polls13_21_wide$voteshare_cdu[x],
                                           polls13_21_wide$voteshare_spd[x],
                                           polls13_21_wide$voteshare_gru[x],
                                           polls13_21_wide$voteshare_fdp[x],
                                           polls13_21_wide$voteshare_lin[x],
                                           polls13_21_wide$voteshare_oth[x]))/round(polls13_21_wide$sample_size[x]))

srs13_21 <- matrix(unlist(srs13_21), nrow = length(srs13_21), byrow = T) %>%
  as.data.frame()
names(srs13_21) <- c('afd', 'cdu', 'spd', 'gru', 'fdp', 'lin', 'oth')

# reshape srs sample to long
srs94_21_long <- reshape2::melt(srs94_21, variable.name = 'party', value.name = 'srs')
srs13_21_long <- reshape2::melt(srs13_21, variable.name = 'party', value.name = 'srs')

# vector with election results
voteshare94_21 <- c(polls94_21_wide$voteshare_cdu[id94_21],
                    polls94_21_wide$voteshare_fdp[id94_21],
                    polls94_21_wide$voteshare_gru[id94_21],
                    polls94_21_wide$voteshare_lin[id94_21],
                    polls94_21_wide$voteshare_spd[id94_21],
                    polls94_21_wide$voteshare_oth[id94_21])/100

voteshare13_21 <- c(polls13_21_wide$voteshare_afd[id13_21],
                    polls13_21_wide$voteshare_cdu[id13_21],
                    polls13_21_wide$voteshare_spd[id13_21],
                    polls13_21_wide$voteshare_gru[id13_21],
                    polls13_21_wide$voteshare_fdp[id13_21],
                    polls13_21_wide$voteshare_lin[id13_21],
                    polls13_21_wide$voteshare_oth[id13_21])/100

# compute bias from srs
srs94_21_long$bias_srs <- srs94_21_long$srs - voteshare94_21
srs13_21_long$bias_srs <- srs13_21_long$srs - voteshare13_21

# compute bias 
polls94_21$bias <- polls94_21$support - polls94_21$voteshare
polls13_21$bias <- polls13_21$support - polls13_21$voteshare

# adjust party names
polls94_21 <- polls94_21 %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 'others')))

srs94_21_long <- srs94_21_long  %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 'others')))

polls13_21 <- polls13_21 %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'afd', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 'AfD', 'others')))

srs13_21_long <- srs13_21_long  %>% 
  mutate(party = factor(party, 
                        levels = c('cdu', 'spd', 'gru', 'fdp', 'lin', 'afd', 'oth'),
                        labels = c('CDU/CSU', 'SPD', 'GRUENE', 'FDP', 'LINKE', 'AfD', 'others')))

#-------------------------------------------------------------------------------

#### Plots ####

tse_dist_bt94_21 <- ggplot(polls94_21, aes(x = bias/100, y=..density..)) +
  geom_histogram(color = 'grey', fill = 'grey') +
  facet_wrap(~ party) +
  theme_bw() +
  geom_density(data = srs94_21_long, aes(x = bias_srs), linetype = 'dashed') + 
  scale_x_continuous('', breaks=seq(-.20,0.20,0.1), limits=c(-.22, .22), 
                     labels=percent_format()) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave(filename = 'bt94_21_tse_dist.png', 
       plot = tse_dist_bt94_21, 
       path = '~/Desktop/plots/german_bundestag',
       width = 12, height = 6)

tse_dist_bt13_21 <- ggplot(polls13_21, aes(x = bias/100, y=..density..)) +
  geom_histogram(color = 'grey', fill = 'grey') +
  facet_wrap(~party) +
  theme_bw()  +
  geom_density(data = srs13_21_long, aes(x = bias_srs), linetype = 'dashed') + 
  scale_x_continuous('', breaks=seq(-.2,0.2,0.05), limits=c(-.2, .2), 
                     labels=percent_format()) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))


ggsave(filename = 'bt13_21_tse_dist.png', 
       plot = tse_dist_bt13_21, 
       path = '~/Desktop/plots/german_bundestag',
       width = 12, height = 6)

#### Table ####

# Mean & median bias by party
mean_bias <- summary_by(polls94_21, bias ~ party, FUN = mean) %>% 
  t() %>%  as.data.frame() 
colnames(mean_bias) <- mean_bias[1,]
mean_bias <- mean_bias[-1,]
median_bias <- summary_by(polls94_21, bias ~ party, FUN = median)  %>% 
  t() %>%  as.data.frame() 
colnames(median_bias) <- median_bias[1,]
median_bias <- median_bias[-1,]

bias_by_party <- rbind(mean_bias, median_bias)
