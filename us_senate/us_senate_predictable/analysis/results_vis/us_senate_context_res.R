#-------------------------------------------------------------------------------
#
# US Senate poll accuracy 1990 - 2020: 
#   empty model bias and variance estimates by context factors
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(rv)
  library(gtools)
  library(ggplot2)
  library(rstan)
  library(dplyr)
  library(scales)
  library(shinystan)
  library(RColorBrewer)
  library(ggridges)
  library(cowplot)
}

# setnsims(10000)


# Data --------------------------------------------------------------------

# polls
polls <- readRDS("~/data/us/senate/us_senate_polls_context.RDS")


# # simulation results
# resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context_empty.RDS')


# Functions ---------------------------------------------------------------

# # extra rv functins
# ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
# logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)




# Preparation -------------------------------------------------------------

# compute election groups and ids for election year and state
polls <- polls %>%
  mutate(state_year = paste0(state, cycle),
         state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(t)/max(as.numeric(t)),
         cycle = as.integer(cycle)) 

# # Election-level data
# election_data <- polls %>%
#   group_by(state_year, state_year_int, cycle, state, state_abb, vote2_rep,
#            minority, minority4,  gender, gender4, inc, front3, front5,
#            cf_score_rep, cf_score_dem, margin2_rep_prev, winner3_rep_prev,
#            turnout_vep, state_control, democracy_mcmc) %>%
#   summarise() %>%
#   ungroup()

# # convert simulations to random variable (rv) obj.
# postrv <- as.rv(resStan)


#### Election level election day bias ####

# # election day estimate
# p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
#                     postrv$alpha)
# 
# # election day bias
# b0 <- p0_r - election_data$vote2_rep
# 
# # election day bias summary
# b0_summary <- summary(b0)
# b0_summary$state_year_int <- seq(1:length(unique(polls$state_year)))
# b0_summary <- merge(b0_summary, election_data, by = "state_year_int")
# b0_summary <- b0_summary %>%
#   mutate(cycle_factor = as.factor(cycle),
#          cf_dist = cf_score_rep - cf_score_dem)
# rm(b0, p0_r)

# saveRDS(b0_summary, "~/results_vis/us_senate_predictable/bias_senate_empty.RDS")

b0_summary <- readRDS("~/results_vis/us_senate_predictable/bias_senate_empty.RDS")

b0_summary <- b0_summary %>%
  mutate(inc = factor(inc, levels = c("Dem. incumbent", "Rep. incumbent",
                                      "Open seat")),
         front3 = factor(front3, levels = c("Dem. front runner", 
                                            "Rep. front runner",
                                            "No front runner")),
         front5 = factor(front5, levels = c("Dem. front runner", 
                                            "Rep. front runner",
                                            "No front runner")),
         winner3_rep_prev = factor(winner3_rep_prev, 
                                   levels = c("swing_state", "solid_dem", 
                                              "solid_rep"),
                                   labels = c("Mixed winner 3 prev. elections", 
                                              "Dem. winner 3 prev. elections", 
                                              "Rep. winner 3 prev. elections")),
         state_control = factor(state_control, levels = c("none", "dem", "rep"),
                                labels = c("Mixed state control", "Dem. state control", 
                                           "Rep. state control")))


#### Election level variance ####

# # poll estimates
# p <- ilogit.rv(logit.rv(polls$vote2_rep) +
#                  postrv$alpha[polls$state_year_int] +
#                  postrv$beta1[polls$state_year_int]*polls$t_sc)
# 
# # total variance
# v <- (p*(1-p))/polls$sample_size + postrv$phi2[polls$state_year_int]
# v_summary <- summary(v)
# 
# # additional variance above srs
# v_add_summary <- summary(postrv$phi2)
# 
# v_add_summary$state_year_int <- seq(1:length(unique(polls$state_year)))
# v_add_summary <- merge(v_add_summary, election_data, by = "state_year_int")
# v_add_summary <- v_add_summary %>%
#   mutate(cycle_factor = as.factor(cycle),
#          cf_dist = cf_score_rep - cf_score_dem)
# rm(v, p)

# saveRDS(v_add_summary, "~/results_vis/us_senate_predictable/excess_var_senate_empty.RDS")
# saveRDS(v_summary, "~/results_vis/us_senate_predictable/var_senate_empty.RDS")

v_add_summary <- readRDS("~/results_vis/us_senate_predictable/excess_var_senate_empty.RDS")

v_add_summary <- v_add_summary %>% 
  mutate(winner3_rep_prev = factor(winner3_rep_prev, 
                                   levels = c("swing_state", "solid_dem", 
                                              "solid_rep"),
                                   labels = c("Mixed winner 3 prev. elections", 
                                              "Dem. winner 3 prev. elections", 
                                              "Rep. winner 3 prev. elections")),
         state_control = factor(state_control, levels = c("none", "dem", "rep"),
                                labels = c("Mixed state control", "Dem. state control", 
                                           "Rep. state control")))

v_summary <- readRDS("~/results_vis/us_senate_predictable/var_senate_empty.RDS")
v_summary <- bind_cols(v_summary, polls[,c("state_abb", "cycle", "state_year",
                                           "state_year_int")])


# Plots -------------------------------------------------------------------

# colors 
col <- brewer.pal(n = 11, name = "RdYlBu")
# display.brewer.pal(n = 11, name = "RdYlBu")

#### Observed polling errors ####

election_density_cycle_plot <- ggplot() +
  geom_density_ridges(data = polls, aes(x = pct2_rep - vote2_rep,
                                        y = as.factor(cycle), color = state),
                      scale = 0.75, alpha = 0.7) +
  theme_bw() +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits=rev) +
  labs(y = "Cycle", x = "Rep.2 party poll support - Rep. 2 party vote share") +
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.margin = unit(c(1,0.5,0,0.5),"cm")) +
  scale_color_manual(guide = "none", values = rep("black", 50)) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/us_senate_cycle_density.png', 
       plot = election_density_cycle_plot, 
       width = 18, height = 12)    


#### Female & minority candidates ####

# bias over cycles by gender
b0_r_gender <- ggplot(b0_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = 'Election level bias', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), fun.data = mean_cl_boot,
               geom = "ribbon",
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  facet_wrap(~gender4, ncol = 2)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_gender.png', 
       plot = b0_r_gender, 
       width = 18, height = 10, bg='#ffffff')   

# bias over cycles by race 
b0_r_race <- ggplot(b0_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = 'Election level bias', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), fun.data = mean_cl_boot,
               geom = "ribbon",
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  facet_wrap(~minority4, ncol = 2)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_race.png', 
       plot = b0_r_race, 
       width = 18, height = 10, bg='#ffffff')    


#### Mobilization & turnout ####

## Turnout

# turnout vs estimated election level bias (global)
b0_r_turnout <- ggplot(b0_summary) +
  geom_segment(aes(x = turnout_vep, xend = turnout_vep, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = turnout_vep, y = mean)) +
  labs(y = '', x = '') +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  geom_smooth(aes(x = turnout_vep, y = mean), se = F, method = "loess",
              color = "darkgrey") 

# turnout vs estimated election level var by cycle (cross-section)
b0_r_cycle_turnout <- ggplot(b0_summary) +
  geom_segment(aes(x = turnout_vep, xend = turnout_vep, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = turnout_vep, y = mean),
             size = 1.5) +
  labs(y = '', x = 'Turnout') +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  facet_wrap(~cycle,  strip.position = "top")  +
  geom_smooth(aes(x = turnout_vep, y = mean), se = F, method = "loess",
              color = "darkgrey") 

b0_r_turnout_all <- plot_grid(b0_r_turnout, b0_r_cycle_turnout, ncol = 1, 
                              rel_heights = c(0.7, 1))  +
  draw_label("Election level bias", x=0, y=0.5, vjust=1.1, angle=90,
             size = 16)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_turnout.png', 
       plot = b0_r_turnout_all, 
       width = 18, height = 12) 

# turnout vs estimated election level var (global)
v_add_r_turnout <- ggplot(v_add_summary) +
  geom_segment(aes(x = turnout_vep, xend = turnout_vep, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = turnout_vep, y = mean)) +
  labs(y = '', x = '') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  geom_smooth(aes(x = turnout_vep, y = mean), se = F, method = "loess",
              color = "darkgrey") 

# turnout vs estimated election level var by cycle (cross-section)
v_add_r_cycle_turnout <- ggplot(v_add_summary) +
  geom_segment(aes(x = turnout_vep, xend = turnout_vep, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = turnout_vep, y = mean),
             size = 1.5) +
  labs(y = '', x = 'Turnout') +
  theme_bw() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  facet_wrap(~cycle,  strip.position = "top")  +
  geom_smooth(aes(x = turnout_vep, y = mean), se = F, method = "loess",
              color = "darkgrey") 

v_add_r_turnout_all <- plot_grid(v_add_r_turnout, v_add_r_cycle_turnout, ncol = 1, 
                                 rel_heights = c(0.7, 1))  +
  draw_label("Election level excess variance", x=0, y=0.5, vjust=1.1, angle=90,
             size = 16)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/v_add_r_turnout.png', 
       plot = v_add_r_turnout_all, 
       width = 18, height = 12) 

## Front runner 

# based on thirst 3 polls
b0_r_front3 <- ggplot(b0_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = 'Election level bias', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), fun.data  = mean_cl_boot, geom = "ribbon",
               fun.max = max,
               fun.min = min,
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  facet_wrap(~front3, ncol = 2) + 
  labs(caption = "Based on three first polls")

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_front3.png', 
       plot = b0_r_front3, 
       width = 18, height = 10, bg='#ffffff')  

# based on thirst 5 polls
b0_r_front5 <- ggplot(b0_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = 'Election level bias', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), fun.data  = mean_cl_boot, geom = "ribbon",
               fun.max = max,
               fun.min = min,
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  facet_wrap(~front5, ncol = 2) + 
  labs(caption = "Based on five first polls")

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_front5.png', 
       plot = b0_r_front5, 
       width = 18, height = 10, bg='#ffffff')  

## Incumbency
b0_r_inc <- ggplot(b0_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = 'Election level bias', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), fun.data  = mean_cl_boot, geom = "ribbon",
               fun.max = max,
               fun.min = min,
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  facet_wrap(~inc, ncol = 2)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_inc.png', 
       plot = b0_r_inc, 
       width = 18, height = 10, bg='#ffffff')   

## Swing state

# bias over cycles by swing state
b0_r_swing <- ggplot(b0_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = 'Election level bias', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), 
               fun.data  = mean_cl_boot, geom = "ribbon",
               fun.max = max,
               fun.min = min, 
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  facet_wrap(~winner3_rep_prev, ncol = 2)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_swing.png', 
       plot = b0_r_swing, 
       width = 18, height = 10, bg='#ffffff') 

# variance over cycles by swing state
v_add_r_swing <- ggplot(v_add_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  labs(y = 'Election level additional variance', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), 
               fun.data  = mean_cl_boot, geom = "ribbon",
               fun.max = max,
               fun.min = min, 
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2))  +
  facet_wrap(~winner3_rep_prev, ncol = 2)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/v_add_r_swing.png', 
       plot = v_add_r_swing, 
       width = 18, height = 12, bg="#ffffff") 

# winning margin prev. election vs estimated election level bias (global)
b0_r_margin <- ggplot(b0_summary)  +
  geom_smooth(aes(x = margin2_rep_prev, y = mean), se = T, method = "loess",
              color = "darkgrey")+
  geom_segment(aes(x = margin2_rep_prev, xend = margin2_rep_prev, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = margin2_rep_prev, y = mean)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = '', x = '') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12))  

?geom_smooth

# winning margin prev. election vs. estimated election level var by cycle (cross-section)
b0_r_cycle_margin <- ggplot(b0_summary) +
  geom_smooth(aes(x = margin2_rep_prev, y = mean), se = F, method = "loess",
              color = "darkgrey") +
  geom_segment(aes(x = margin2_rep_prev, xend = margin2_rep_prev, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = margin2_rep_prev, y = mean),
             size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = '', x = 'Winning margin prev. election') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  facet_wrap(~cycle,  strip.position = "top") 

b0_r_margin_all <- plot_grid(b0_r_margin, b0_r_cycle_margin, ncol = 1, 
                             rel_heights = c(0.7, 1))  +
  draw_label("Election level bias", x=0, y=0.5, vjust=1.1, angle=90, size = 16)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_margin.png', 
       plot = b0_r_margin_all, 
       width = 18, height = 12, bg='#ffffff') 

# 3 previous winning margin vs estimated election level excess variance (global)
v_add_r_margin <- ggplot(v_add_summary) +
  geom_smooth(aes(x = margin2_rep_prev, y = mean), se = T, method = "loess",
              color = "darkgrey") +
  geom_segment(aes(x = margin2_rep_prev, xend = margin2_rep_prev, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = margin2_rep_prev, y = mean)) +
  labs(y = '', x = '') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12))

# 3 previous winning margin vs estimated election level var by cycle (cross-section)
v_add_r_cycle_margin <- ggplot(v_add_summary) +
  geom_segment(aes(x = margin2_rep_prev, xend = margin2_rep_prev, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = margin2_rep_prev, y = mean),
             size = 1.5) +
  labs(y = '', x = 'Winning margin prev. election') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  facet_wrap(~cycle,  strip.position = "top")  +
  geom_smooth(aes(x = margin2_rep_prev, y = mean), se = F, method = "loess",
              color = "darkgrey") 

v_add_r_margin_all <- plot_grid(v_add_r_margin, v_add_r_cycle_margin, ncol = 1, 
                                rel_heights = c(0.7, 1))  +
  draw_label("Election level additional variance", x=0, y=0.5, vjust=1.1, 
             angle=90, size = 16)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/v_add_r_margin.png', 
       plot = v_add_r_margin_all, 
       width = 18, height = 12, bg='#ffffff') 


#### Polarization & etremist candidates ####

## Polarization

# polarization vs estimated election level bias (global)
b0_r_pol <- ggplot(b0_summary) +
  geom_segment(aes(x = cf_dist, xend = cf_dist, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = cf_dist, y = mean)) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
  labs(y = '', x = '') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12))  +
  geom_smooth(aes(x = cf_dist, y = mean), se = F, method = "loess",
              color = "darkgrey") 

# polarization vs estimated election level bias by cycle (cross-section)
b0_r_cycle_pol <- ggplot(b0_summary) +
  geom_segment(aes(x = cf_dist, xend = cf_dist, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = cf_dist, y = mean),
             size = 1.5) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
  labs(y = '', x = 'CF Score distance') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  facet_wrap(~cycle,  strip.position = "top", drop = T) +
  geom_smooth(aes(x = cf_dist, y = mean), se = F, method = "loess",
              color = "darkgrey")  

b0_r_pol_all <- plot_grid(b0_r_pol, b0_r_cycle_pol, 
                          ncol = 1, rel_heights = c(1, 1))  +
  draw_label("Estimated election level bias", x=0, y=0.5, vjust=1.1, angle=90,
             size = 16)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_pol.png', 
       plot = b0_r_pol_all, 
       width = 18, height = 12, bg='#ffffff')

## Ideology

b0_r_cf <- ggplot(b0_summary) +
  geom_segment(aes(x = cf_score_rep, xend = cf_score_rep, y = `2.5%`, 
                   yend = `97.5%`, color = "Rep.")) +
  geom_point(aes(x = cf_score_rep, y = mean, color = "Rep.")) +
  geom_segment(aes(x = cf_score_dem, xend = cf_score_dem, y = `2.5%`, 
                   yend = `97.5%`, color = "Dem.")) +
  geom_point(aes(x = cf_score_dem, y = mean, color = "Dem.")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "", x = '') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "none") +
  scale_color_manual(name = "CF Score", values = c(col[11], col[1])) +
  geom_smooth(aes(x = cf_score_rep, y = mean), se = F, 
              method = "loess", color = col[2])  +
  geom_smooth(aes(x = cf_score_dem, y = mean), se = F, 
              method = "loess", color = col[10])

b0_r_cycle_cf <- ggplot(b0_summary) +
  geom_segment(aes(x = cf_score_rep, xend = cf_score_rep, y = `2.5%`, 
                   yend = `97.5%`, color = "Rep.")) +
  geom_point(aes(x = cf_score_rep, y = mean, color = "Rep.")) +
  geom_segment(aes(x = cf_score_dem, xend = cf_score_dem, y = `2.5%`, 
                   yend = `97.5%`, color = "Dem.")) +
  geom_point(aes(x = cf_score_dem, y = mean, color = "Dem."))  +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = '', x = "CF Score") +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12), legend.position = "bottom") +
  facet_wrap(~cycle) +
  scale_color_manual(name = "", values = c(col[11], col[1])) +
  geom_smooth(aes(x = cf_score_rep, y = mean), se = F, 
              method = "loess", color = col[2])  +
  geom_smooth(aes(x = cf_score_dem, y = mean), se = F, 
              method = "loess", color = col[10])

b0_r_cf_all <- plot_grid(b0_r_cf, b0_r_cycle_cf, ncol = 1, 
                         rel_heights = c(0.7, 1))  +
  draw_label("Estimated election level bias", x=0, y=0.5, vjust=1.1, angle=90,
             size = 16)

ggsave(filename = "~/results_vis/us_senate_predictable/plots/b0_r_cf.png", 
       plot = b0_r_cf_all, 
       width = 18, height = 12, bg="#ffffff") 

# polarization vs estimated election level variance (global)
v_add_r_pol <- ggplot(v_add_summary) +
  geom_segment(aes(x = cf_dist, xend = cf_dist, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = cf_dist, y = mean)) +
  labs(y = '', x = '') +
  theme_bw() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12))  +
  geom_smooth(aes(x = cf_dist, y = mean), se = F, method = "loess",
              color = "darkgrey") 

# polarization vs estimated election level variance by cycle (cross-section)
v_add_r_cycle_pol <- ggplot(v_add_summary) +
  geom_segment(aes(x = cf_dist, xend = cf_dist, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = cf_dist, y = mean),
             size = 1.5) +
  labs(y = '', x = 'CF Score distance') +
  theme_bw() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  facet_wrap(~cycle,  strip.position = "top", drop = T) +
  geom_smooth(aes(x = cf_dist, y = mean), se = F, method = "loess",
              color = "darkgrey") 

v_add_r_pol_all <- plot_grid(v_add_r_pol, v_add_r_cycle_pol, 
                             ncol = 1, rel_heights = c(1, 1))  +
  draw_label("Estimated election level additional variance", x=0, y=0.5, 
             vjust=1.1, angle=90, size = 16)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/v_add_r_pol.png', 
       plot = v_add_r_pol_all, 
       width = 18, height = 12, bg='#ffffff')


#### Electoral conduct vote buying and vote suppression

## vote suppression

# grumbach vs estimated election level var (global)
b0_r_grumbach <- ggplot(b0_summary) +
  geom_segment(aes(x = democracy_mcmc, xend = democracy_mcmc, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = democracy_mcmc, y = mean)) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
  labs(y = '', x = '') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  geom_smooth(aes(x = democracy_mcmc, y = mean), se = F, method = "loess",
              color = "darkgrey") 

# grumbach score vs estimated election level var by cycle (cross-section)
b0_r_cycle_grumbach <- ggplot(b0_summary %>% 
                                filter(cycle %in% seq(2000, 2018,2))) +
  geom_segment(aes(x = democracy_mcmc, xend = democracy_mcmc, y = `2.5%`, yend = `97.5%`)) +
  geom_point(aes(x = democracy_mcmc, y = mean),
             size = 1.5) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
  labs(y = '', x = 'Democracy score') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12)) +
  facet_wrap(~cycle,  strip.position = "top", drop = T) +
  geom_smooth(aes(x = democracy_mcmc, y = mean), se = F, method = "loess",
              color = "darkgrey") 

b0_r_grumbach_all <- plot_grid(b0_r_grumbach, b0_r_cycle_grumbach, 
                               ncol = 1, rel_heights = c(1, 1))  +
  draw_label("Estimated election level bias", x=0, y=0.5, vjust=1.1, angle=90,
             size = 16)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_grumbach.png', 
       plot = b0_r_grumbach_all, 
       width = 18, height = 12, bg='#ffffff')

# bias over cycles by state control
b0_r_control <- ggplot(b0_summary) +
  geom_segment(aes(x = cycle, xend = cycle, y = `2.5%`, yend = `97.5%`),
               position = position_jitter(seed = 123)) +
  geom_point(aes(x = cycle, y = mean),
             position = position_jitter(seed = 123)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = 'Election level bias', x = 'Cycle') +
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  stat_summary(aes(y = mean, x = cycle), fun.data  = mean_cl_boot, geom = "ribbon",
               fun.max = max,
               fun.min = min, 
               alpha = 0.2) +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  facet_wrap(~state_control, ncol = 2)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_r_control.png', 
       plot = b0_r_control, 
       width = 18, height = 10, bg='#ffffff') 



