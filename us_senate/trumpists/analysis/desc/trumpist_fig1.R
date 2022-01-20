# ---------------------------------------------------------------------------- #
# Trumpists were not underestimated in US Senate election polls
# Authors: Sina Chen, John Körtner
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(ggrepel)

# Data

senate1998_2020 <- readRDS("~/data/senate_polls1998_2020_score.RDS")

# ---------------------------------------------------------------------------- #

# Add Endorsement and Incumbency Dummies

senate1998_2020 <- senate1998_2020 %>% 
  mutate(end_trump = if_else(state == 'WV' & election_year == 2020 |
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
         inc_rep = if_else(rep_candidate == senator, 1, 0))

# Subset Senate 2018, 2020

senate2018_2020 <- subset(senate1998_2020, election_year >= 2018) %>% 
  mutate(end_trump_factor = if_else(end_trump == 1, "Endorsed", "Not endorsed"))

# Add Error

senate2018_2020$error <- senate2018_2020$rep_poll2 - senate2018_2020$rep_result2

# Group Polls by Election and State

senate2018_2020_grouped <- senate2018_2020 %>% 
  group_by(election_year, state, end_trump, end_trump_factor, inc_rep, rep_result2,
           cf_score_rep) %>% 
  summarise(mean_error = mean(error),
            mean_poll = mean(rep_poll2)) 

# ---------------------------------------------------------------------------- #

# Plot

# Figure 1
fig1 <- ggplot(senate2018_2020_grouped, aes(x = mean_poll, 
                                            y = rep_result2)) +
  geom_abline(linetype = 'dashed', color = 'black') +
  geom_hline(linetype = 'dashed', color = 'black', yintercept = 0.5) +
  geom_vline(linetype = 'dashed', color = 'black', xintercept = 0.5) +
  geom_point(aes(color = end_trump_factor), 
             size = 2.5) +
  xlim(0.2, 0.75) + ylim(0.2, 0.75) +
  scale_color_manual(name = '', values = c("#E69F00", "#999999"),
                     labels = c('Endorsed', 'Not endorsed')) +
  geom_text_repel(aes(label=state, color = end_trump_factor, fontface = 1), 
                  size = 4, 
                  show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~election_year) +
  labs(x = 'Rep. average 2-party poll support', y = 'Rep. 2-party vote share') +
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14),
        legend.position = 'bottom', strip.text = element_text(size=16),
        legend.text = element_text(size = 14)) 


ggsave(filename = '~/results_vis/plots/fig1.png', 
       plot = fig1, 
       width = 12, height = 6.5)

ggsave(filename = '~/results_vis/plots/fig1.eps', 
       plot = fig1, 
       width = 12, height = 6.5)

