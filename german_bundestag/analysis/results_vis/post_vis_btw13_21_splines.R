#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Posterior visualization Bundestag election polls 2013 - 2021
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
library(ggrepel)


# Data --------------------------------------------------------------------

# poll data
polls <- readRDS("~/data/germany/bundestag/polls2013_2021.RDS")

# # stan fit obj.
# resStan <- readRDS("~/fit_stan/resStan_btw13_21_redundant_splines_final.RDS")


# Functions ---------------------------------------------------------------

# source("~/results_vis/germany/helper_res_vis_btw_splines.R")


#-------------------------------------------------------------------------------

# Preparation -------------------------------------------------------------

# number of simulations for rv
setnsims(7500)

# subset polls
poll_id_afd <- polls[which(polls$party == "afd" & 
                             !is.na(polls$support)),]$poll_id 

polls <- polls %>% 
  subset(poll_id %in% poll_id_afd &
           !is.na(support) & 
           !is.na(date) & 
           election < 2025) %>% 
  group_by(poll_id) %>% 
  mutate(n_party = n()) %>% 
  ungroup() %>% 
  subset(n_party == 7 
  ) %>% 
  droplevels() %>% 
  mutate(poll_id_int = as.integer(as.factor(poll_id)))

rm(poll_id_afd)

# order by poll id and party
polls <- polls[order(polls$poll_id_int, polls$party),]

# specify levels for election-party id
order_kr <-paste0(rep(c( 2013, 2017, 2021), 
                      each = 7),
                  rep(c("cdu", "fdp", "gru", "lin","spd", "afd", "oth"), 3)) 

# specify levels for institute-party id
order_kl <-paste0(rep(unique(polls$institute), 
                      each = 7),
                  rep(c("cdu", "fdp", "gru", "lin","spd", "afd", "oth"), 
                      length(unique(polls$institute)))) 

# compute election-party id
polls <- polls %>% 
  mutate(kr = factor(paste0(election, party), levels = order_kr),
         kr_id = as.integer(kr),
         l_id = as.integer(as.factor(institute)),
         rl = paste0(election, institute),
         rl_id = as.integer(as.factor(rl)),
         kl = factor(paste0(institute, party), levels = order_kl),
         kl_id = as.integer(kl),
         t_sc = as.numeric(days_to_election)/max(as.numeric(days_to_election)),
         r_id = case_when(election == 2013 ~ 1,
                          election == 2017 ~ 2,
                          election == 2021 ~ 3),
         k_id = as.integer(factor(party, levels = c("cdu", "fdp", "gru", "lin",
                                                    "spd", "afd", "oth"))))

# poll level data
poll_level <- polls %>% 
  group_by(poll_id_int, election, institute, t_sc, sample_size, r_id, l_id) %>% 
  summarise() %>% 
  ungroup() 

# election-party level data
election_party_level <- polls %>%  
  group_by(election, party, voteshare, kr, kr_id, k_id, r_id) %>%  
  summarise()

election_party_level <- election_party_level[order(election_party_level$kr_id),]

# institute-party level data
institute_party_level <- polls %>%  
  group_by(institute, party, l_id, kl, kl_id) %>%  
  summarise()

# # create b-splines design matrix
# B <- t(bs(poll_level$t_sc, knots = quantile(poll_level$t_sc,
#                                             probs= seq(0.1,0.9,0.1)),
#           degree = 3,
#           Boundary.knots = c(0,1)))
# 
# # generate random variable of estimated parameters
# postrv <- as.rv(resStan)


# # Posterior estimation ----------------------------------------------------
# 
# # election day bias
# bias0_i <- estimate_bias0(polls = polls,
#                         postrv = postrv)
# 
# # party election level election day bias
# b0_kr <- rv(length(unique(polls$kr)))
# 
# for (i in 1:length(unique(polls$kr))) {
#   b0_kr[i] <- mean(bias0_i[i == polls$kr_id])
# }
# 
# rm(bias0_i)
# 
# # splines
# splines <- get_splines(par = postrv$beta, b_splines = B, polls = polls)
# 
# # institute level bias by party
# bias_l_i <- estimate_institute_bias(polls = polls,
#                                     postrv = postrv,
#                                     splines = splines)
# 
# b_kl <- rv(length(unique(polls$kl)))
# 
# for (i in 1:length(unique(polls$kl))) {
#   b_kl[i] <- mean(bias_l_i[i == polls$kl_id])
# }
# 
# rm(bias_l_i)

# # mean p_i without institute effect
# p_i_without_inst <- generate_predictions(polls = polls,
#                             postrv = postrv,
#                             splines = splines,
#                             inc_inst = F)
# 
# # mean estimate summary
# p_summary <- summary(p_i_without_inst)
# p_summary <- cbind(p_summary, polls[,c("election", "party", "support",
#                                        "days_to_election", "voteshare")])
# rm(p_i_without_inst)
# 
# # mean p_i
# p_i <- generate_predictions(polls = polls,
#                             postrv = postrv,
#                             splines = splines)
# rm(splines)
# 
# # srs variance
# srs_var_i <- rv(nrow(polls))
# for(i in 1:nrow(polls)){
#   srs_var_i[[i]] <- p_i[[i]] * (1 - p_i[[i]])/polls$sample_size[i]
#   print(i)
# }
# rm(p_i)
# 
# # election level srs variance
# srs_var_kr <- rv(length(unique(polls$kr)))
# for (i in 1:length(unique(election_party_level$kr_id))) {
#   idx <- which(polls$kr_id == i)
#   srs_var_kr[i] <- mean(srs_var_i[idx])
# }
# 
# # election level srs se
# srs_se_kr <- rvmapply(srs_var_kr, FUN = sqrt)
# 
# rm(srs_var_i)
# 
# # election level total variance
# var_kr <- rv(length(unique(polls$kr)))
# for (i in 1:nrow(election_party_level)) {
#   var_kr[i] <- exp(log(srs_var_kr[i]) +
#     postrv$phi[i])
# }
# 
# # election level total se
# se_kr <- rvmapply(var_kr, FUN = sqrt)
# 
# # design effect
# deff_kr <- var_kr/srs_var_kr
# 
# saveRDS(b0_kr, "~/results_vis/b0_kr_btw13_21.RDS")
# saveRDS(b_kl, "~/results_vis/b_kl_btw13_21.RDS")
# saveRDS(p_summary, "~/results_vis/p_summary_btw13_21.RDS")
# saveRDS(srs_var_kr, "~/results_vis/srs_var_kr_btw13_21.RDS")
# saveRDS(srs_se_kr, "~/results_vis/srs_se_kr_btw13_21.RDS")
# saveRDS(var_kr, "~/results_vis/var_kr_btw13_21.RDS")
# saveRDS(se_kr, "~/results_vis/se_kr_btw13_21.RDS")
# saveRDS(deff_kr, "~/results_vis/deff_kr_btw13_21.RDS")


# Bias and variance summaries ---------------------------------------------

# load quantities of interest
p_summary <- readRDS( "~/results_vis/germany/p_summary_btw13_21.RDS")
b0_kr <- readRDS( "~/results_vis/b0_kr_btw13_21.RDS")
b_kl <- readRDS( "~/results_vis/b_kl_btw13_21.RDS")
srs_var_kr <- readRDS( "~/results_vis/srs_var_kr_btw13_21.RDS")
srs_se_kr <- readRDS( "~/results_vis/srs_se_kr_btw13_21.RDS")
var_kr <- readRDS( "~/results_vis/var_kr_btw13_21.RDS")
se_kr <- readRDS( "~/results_vis/se_kr_btw13_21.RDS")
deff_kr <- readRDS( "~/results_vis/deff_kr_btw13_21.RDS")

# adjust party names 
p_summary <- p_summary  %>% 
  mutate(party = factor(recode(party, 
                               cdu = "CDU/CSU", 
                               spd = "SPD", 
                               gru = "GRUENE", 
                               fdp = "FDP", 
                               lin = "LINKE", 
                               afd = "AfD", 
                               oth = "others"), 
                        levels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")))


# election day bias
b0_kr_summary <- summary(b0_kr)
b0_kr_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
b0_kr_summary <- merge(b0_kr_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "afd",
                                   "oth"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")))

# party-institute bias
b_kl_summary <- summary(b_kl)
b_kl_summary$kl_id <- seq(1:length(unique(polls$kl_id)))
b_kl_summary <- merge(b_kl_summary, institute_party_level, by = "kl_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "afd", 
                        "oth"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")),
         institute = factor(institute, 
                            levels = c("allensbach", "emnid", "forsa", 
                                       "politbarometer", "gms", "dimap", "insa",
                                       "yougov", "civey"),
                            labels = c("IfD Allensbach", "TNS Emnid/Kantar", 
                                       "Forsa", "Forschungsgruppe Wahlen", 
                                       "GMS", "Infratest dimap", "INSA", 
                                       "YouGov", "Civey")))

# design effect
deff_summary <- summary(deff_kr)
deff_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
deff_summary$var_type <- "Design effect"
deff_summary <- merge(deff_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "afd",
                                   "oth"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")))

# SRS var
srs_var_summary <- summary(srs_var_kr)
srs_var_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
srs_var_summary$var_type <- "SRS variance"
srs_var_summary <- merge(srs_var_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "afd",
                                   "oth"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")))

# Total var
var_summary <- summary(var_kr)
var_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
var_summary$var_type <- "Total variance"
var_summary <- merge(var_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "afd",
                                   "oth"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")))

# SRS se
srs_se_summary <- summary(srs_se_kr)
srs_se_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
srs_se_summary$se_type <- "SRS standard error"
srs_se_summary <- merge(srs_se_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "afd",
                                   "oth"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")))
# total se
se_summary <- summary(se_kr)
se_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
se_summary$se_type <- "Total standard error"
se_summary <- merge(se_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "afd",
                                   "oth"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "AfD", "others")))



# Tables ------------------------------------------------------------------

# average absolute election level bias
mean(abs(b0_kr))
mean(deff_kr)

# average abs. bias on election day by party
mab0_k <- b0_kr_summary %>%
  group_by(party) %>% 
  summarise(error = format(round(mean(abs(mean)*100),3), nsmall = 2),
            sd = format(round(mean(sd)*100,3), nsmall = 2),
            mu_b0 = paste0(error," (", sd,")")) %>% 
  select(party, mu_b0) %>% 
  t() %>% 
  data.frame()

colnames(mab0_k) <- mab0_k[1,]
mab0_k <- mab0_k[-1,]

# average rel. abs. bias on election day by party
mrab0_k <- b0_kr_summary %>%
  group_by(party) %>% 
  summarise(error = format(round(mean(abs(mean)*100/voteshare),3), nsmall = 2),
            sd = format(round(mean(sd*100/voteshare),3), nsmall = 2),
            mu_b0 = paste0(error,' (', sd,')')) %>% 
  select(party, mu_b0) %>% 
  t() %>% 
  data.frame()

colnames(mrab0_k) <- mrab0_k[1,]
mrab0_k <- mrab0_k[-1,]

# average election level total variance by party
mvar_k <- var_summary %>%
  group_by(party) %>% 
  summarise(error = format(round(mean(mean*100),3), nsmall = 2),
            sd = format(round(mean(sd)*100,3), nsmall = 2),
            mu_b0 = paste0(error," (", sd,")")) %>% 
  select(party, mu_b0) %>% 
  t() %>% 
  data.frame()

colnames(mvar_k) <- mvar_k[1,]
mvar_k <- mvar_k[-1,]

# average election level SRS variance by party
msrs_k <- srs_var_summary %>%
  group_by(party) %>% 
  summarise(error = format(round(mean(mean*100),3), nsmall = 2),
            sd = format(round(mean(sd)*100,3), nsmall = 2),
            mu_b0 = paste0(error," (", sd,")")) %>% 
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
            mu_b0 = paste0(error," (", sd,")")) %>% 
  select(party, mu_b0) %>% 
  t() %>% 
  data.frame()

colnames(mdeff_k) <- mdeff_k[1,]
mdeff_k <- mdeff_k[-1,]

# Merge errors
error <- rbind(mab0_k, mrab0_k,  msrs_k, mvar_k, mdeff_k)
rownames(error) <- c('Absolute election day bias',
                     'Absolute election day relbias',
                     'SRS variance',
                     'Total variance',
                     'Variance ratio')


xtable(error)


# Plots -------------------------------------------------------------------

# election level bias on election day
b0_kr_plot <- ggplot(b0_kr_summary, aes(x)) + 
  geom_segment(aes(y = party, yend = party, x = `2.5%`, xend = `97.5%`),
               size= 0.3) + 
  geom_segment(aes(y = party, yend = party, x = `25%`, xend = `75%`), 
               size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", size = 0.2) +
  labs(x = "\n Election-day bias", y = "Party") + 
  scale_y_discrete(limits = rev(levels(as.factor(b0_kr_summary$party)))) + 
  theme_bw()  + 
  theme(text = element_text(size = 6), axis.text.x = element_text(size = 6),
        axis.title=element_text(size=6), 
        axis.text.y = element_text(size = 6),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.05, "cm"),
        panel.border = element_rect(size = 0.2),
        strip.text = element_text(size = 6),
        strip.background = element_rect(size = 0.2),
        panel.grid.major = element_line(size = 0.2),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "cm"))) +
  facet_wrap(~ election, ncol = 4)  +
  scale_x_continuous(labels=label_percent(accuracy = 1), 
                     breaks = seq(-0.04, 0.04, 0.02),
                     limits = c(-0.045, 0.045))

ggsave(filename = "btw13_21_redundant_splines_election_day_bias.png", 
       plot = b0_kr_plot, 
       path = "~/results_vis/plots",
       width = 11, height = 4, units = "cm", dpi = 600)

# party-institute level bias
b_kl_plot <- ggplot(b_kl_summary, aes(x)) + 
  geom_segment(aes(y = party, yend = party, x = `2.5%`, xend = `97.5%`),
               size = 0.3) + 
  geom_segment(aes(y = party, yend = party, x = `25%`, xend = `75%`), 
               size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", size = 0.2) +
  labs(x = "\n Party-institute effect", y = "Party") + 
  scale_y_discrete(limits = rev(levels(as.factor(b_kl_summary$party)))) + 
  theme_bw()  + 
  theme(text = element_text(size = 6), 
        axis.text = element_text(size = 6),
        axis.title=element_text(size=6),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.05, "cm"),
        panel.border = element_rect(size = 0.2),
        strip.text = element_text(size = 6),
        strip.background = element_rect(size = 0.2),
        panel.grid.major = element_line(size = 0.2),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "cm"))) +
  facet_wrap(~ institute, ncol = 3) +
  scale_x_continuous(labels=label_percent(accuracy = 1), 
                     breaks = seq(-0.04, 0.04, 0.02),
                     limits = c(-0.045, 0.045))

ggsave(filename = "btw13_21_redudant_splines_institute_bias.png", 
       plot = b_kl_plot, 
       path = "~/results_vis/plots",
       width = 11, height = 7.5, units = "cm", dpi = 600)


# total vs. SRS sd 
tot_srs_se <- rbind(srs_se_summary, se_summary)

tot_srs_plot <- ggplot(tot_srs_se, aes(x = party, y = mean, 
                                        color = se_type, fill = se_type,
                                        linetype = se_type)) +
  geom_bar(stat = "identity", width = 0.8, 
           position = "identity", alpha = 0.5, size = 0.2) +
  facet_wrap(~election, ncol = 4) +
  theme_bw()+
  scale_y_continuous(labels=label_percent(accuracy = 0.1)) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size=6), 
        legend.title = element_text(size=6),
        legend.key.size = unit(0.5,"line"),
        legend.margin=margin(t=-8),
        text = element_text(size = 6), 
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 6),
        axis.title=element_text(size=6),
        strip.text = element_text(size = 6),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.05, "cm"),
        panel.border = element_rect(size = 0.2),
        strip.background = element_rect(size = 0.2),
        panel.grid.major = element_line(size = 0.2),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "cm"))) +
  labs(x = "Party", y = "") +
  scale_color_manual(name = "", values = c("black", NA)) +
  scale_fill_manual(name = "", values = c("white", "darkgrey")) +
  scale_linetype_manual(name = "",values = c("dashed", "blank"))

ggsave(filename = "btw13_21_redundant_splines_tot_srs_se.png", 
       plot = tot_srs_plot, 
       path = "~/results_vis/plots",
       width = 11, height = 4.5, units = "cm", dpi = 600)

# mean estimates 
btw13_21_estimate <- ggplot(p_summary, aes(x=as.numeric(days_to_election), 
                            y = mean, 
                            color = party)) +
  geom_line(size = 0.3) +
  facet_wrap(~ election, ncol = 4) +
  scale_x_reverse() +
  theme_bw() +
  geom_point(aes(y = support/100, color = party), size = 0.4, alpha = 0.2,
             stroke = 0) +
  geom_hline(aes(yintercept = voteshare/100, 
                 color = party), linetype = "dashed", size = 0.3) +
  scale_color_manual(values = c("CDU/CSU" = "#000000", 
                                "SPD" = "#E3000F", 
                                "GRUENE" = "#46962b", 
                                "FDP" = "#FBEE31", 
                                "LINKE" = "#B1003A", 
                                "AfD" = "#0033CC",
                                "others" = "darkgrey"),
                     name = "Party") +
  labs(x = "Days to election", y = "Mean prediction")  + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6),
        legend.margin=margin(t=-8),
        legend.key.height = unit(0.3, "cm"),
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 6),
        axis.title=element_text(size = 6),
        strip.text = element_text(size = 6),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.05, "cm"),
        panel.border = element_rect(size = 0.2),
        strip.background = element_rect(size = 0.2),
        panel.grid.major = element_line(size = 0.2),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "cm"))) 

ggsave(filename = "btw13_21_redundant_splines_estimate.png", 
       plot = btw13_21_estimate, 
       path = "~/results_vis/plots",
       width = 11, height = 4.5, dpi = 600, units = "cm") 

# mean estimate: gray scale 
label_gray <- p_summary %>% 
  group_by(election, party) %>% 
  slice(which.min(days_to_election))

btw13_21_estimate_gray <- ggplot(p_summary, aes(x=as.numeric(days_to_election), 
                                           y = mean, 
                                           color = party)) +
  geom_line(size = 0.3) +
  facet_wrap(~ election, ncol = 4) +
  scale_x_reverse(limits = c(1500,-450), breaks = c(1500, 1000, 500, 0), 
                  labels = c("1500", "1000", "500", "0")) +
  theme_bw() +
  geom_point(aes(y = support/100, color = party), size = 0.4, alpha = 0.2,
             stroke = 0)+
  geom_segment(aes(y = voteshare/100, yend = voteshare/100, 
                   x = 1500, xend = 0,
                   color = party), linetype = "dashed", size = 0.2)  +
  labs(x = "Days to election", y = "Mean prediction")  + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 6),
        axis.title=element_text(size = 6),
        strip.text = element_text(size = 6),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.05, "cm"),
        panel.border = element_rect(size = 0.2),
        strip.background = element_rect(size = 0.2),
        panel.grid.major = element_line(size = 0.2),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "cm")))  +
  ylim(0, 0.5) +
  scale_color_grey(name = "Party") +
  geom_text_repel(data = label_gray, aes(label =  gsub("^.*$", " ", party)), 
                  segment.curvature = -0.2,
                  segment.square = TRUE,
                  segment.color = 'grey',
                  segment.linetype = 1,
                  box.padding = 0.05,
                  nudge_x = 300,
                  nudge_y = 0.01,
                  direction="y",
                  xlim =c(1500, -150),
                  max.overlaps = 20,
                  ylim =c(0, 0.5),
                  min.segment.length = unit(0, 'lines'),
                  hjust = 0,
                  segment.size = 0.2,
                  size = 1,
                  show.legend = F
  ) +
  geom_text_repel(data = label_gray,
                  aes(label = paste0("", party)),
                  segment.alpha = 0, 
                  segment.curvature = -0.2,
                  segment.square = TRUE,
                  box.padding = 0.05,
                  nudge_x = 300,
                  nudge_y = 0.01,
                  direction="y",
                  na.rm = TRUE,
                  xlim = c(1500, -150),
                  ylim =c(0, 0.5),
                  min.segment.length = unit(0, 'lines'),
                  hjust = 0, 
                  size = 1,
                  show.legend = F)

ggsave(filename = "btw13_21_redundant_splines_estimate_gray.png", 
       plot = btw13_21_estimate_gray, 
       path = "~/results_vis/plots",
       width = 16, height = 5, dpi = 600, units = "cm") 
