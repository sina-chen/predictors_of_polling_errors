#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Posterior visualization Landtag election polls 1994 - 2021
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
polls <- readRDS("~/data/polls_lt_1994_2021.RDS")

# stan fit obj.
resStan <- readRDS("~/fit_stan/resStan_ltw_splines_final.RDS")


# Functions ---------------------------------------------------------------

source("~/results_vis/helper_res_vis_ltw_splines.R")


#-------------------------------------------------------------------------------


# Preparation -------------------------------------------------------------

# number of simulations for rv
setnsims(7500)

# subset polls
polls <- polls %>% 
  subset(!is.na(support) & 
           !is.na(date) &
           support != 0) %>% 
  group_by(poll_id) %>% 
  mutate(n_party = n()) %>% 
  ungroup() %>% 
  subset(n_party == 6) %>% 
  droplevels() %>% 
  mutate(poll_id_int = as.integer(as.factor(poll_id)),
         party = factor(party, levels = c("cdu", "fdp", "gru", "lin","spd",
                                          "oth_afd")),
         k_id = as.integer(party),
         t_sc = days_to_election/max(days_to_election),
         r_short = recode(r,`mecklenburg-vorpommern:1994` = "MV:1994",
                          `mecklenburg-vorpommern:1998`= "MV:1998",
                          `brandenburg:1999` = "BB:1999",
                          `sachsen:1999` = "SN:1999",
                          `berlin:2001` = "BE:2001",
                          `mecklenburg-vorpommern:2002` = "MV:2002",
                          `sachsen-anhalt:2002` = "ST:2002",
                          `bremen:2003` = "HB:2003",
                          `niedersachsen:2003` = "NI:2003",
                          `brandenburg:2004` = "BB:2004",
                          `saarland:2004` = "SL:2004",
                          `sachsen:2004` = "SN:2004",
                          `thueringen:2004` = "TH:2004",
                          `nrw:2005` = "NW:2005",
                          `schleswig-holstein:2005` = "SH:2005",
                          `baden-wuerttemberg:2006` = "BW:2006",
                          `berlin:2006` = "BE:2006",
                          `mecklenburg-vorpommern:2006` = "MV:2006",
                          `rheinland-pfalz:2006` = "RP:2006",
                          `sachsen-anhalt:2006` = "ST:2006",
                          `bremen:2007` = "HB:2007",                
                          `bayern:2008` = "BY:2008", 
                          `hamburg:2008` = "HH:2008",
                          `hessen:2008` = "HE:2008",
                          `niedersachsen:2008` = "NI:2008",
                          `brandenburg:2009` = "BB:2009",
                          `hessen:2009` = "HE:2009",
                          `saarland:2009` = "SL:2009",
                          `sachsen:2009` = "SN:2009",
                          `schleswig-holstein:2009` = "SH:2009",    
                          `thueringen:2009` = "TH:2009",
                          `nrw:2010` = "NW:2010",
                          `baden-wuerttemberg:2011` = "BW:2011",
                          `berlin:2011` = "BE:2011",
                          `bremen:2011` = "HB:2011",
                          `hamburg:2011` = "HH:2011",
                          `mecklenburg-vorpommern:2011` = "MV:2011",
                          `rheinland-pfalz:2011` = "RP:2011",
                          `sachsen-anhalt:2011` = "ST:2011",        
                          `nrw:2012` = "NW:2012",
                          `saarland:2012` = "SL:2012",
                          `schleswig-holstein:2012` = "SH:2012",
                          `bayern:2013` = "BY:2013",
                          `hessen:2013` = "HE:2013",
                          `niedersachsen:2013` = "NI:2013",         
                          `brandenburg:2014` = "BB:2014",
                          `sachsen:2014` = "SN:2014",
                          `thueringen:2014` = "TH:2014",
                          `bremen:2015` = " HB:2015",
                          `hamburg:2015`= "HH:2015",
                          `baden-wuerttemberg:2016` = "BW:2016",
                          `berlin:2016` = "BE:2016",
                          `mecklenburg-vorpommern:2016` = "MV:2016",
                          `rheinland-pfalz:2016` = "RP:2016",       
                          `sachsen-anhalt:2016` = "ST:2016",
                          `niedersachsen:2017` = "NI:2017",
                          `nrw:2017` = "NW:2017",                   
                          `saarland:2017` = "SL:2017",
                          `schleswig-holstein:2017` = "SH:2017",
                          `bayern:2018` = "BY:2018",                
                          `hessen:2018` = "HE:2018",
                          `brandenburg:2019` = "BB:2019",
                          `bremen:2019` = "HB:2019",                
                          `sachsen:2019` = "SN:2019",
                          `thueringen:2019` = "TH:2019",
                          `hamburg:2020` = "HH:2020",               
                          `baden-wuerttemberg:2021` = "BW:2021",
                          `rheinland-pfalz:2021` = "RP:2021",
                          `berlin:2021` = "BE:2021",
                          `mecklenburg-vorpommern:2021` = "MV:2021",       
                          `sachsen-anhalt:2021` = "ST:2021")) %>% 
  rename(r_id = "r_int")

# order by poll id and party
polls <- polls[order(polls$poll_id_int, polls$party),]

# specify levels for election-party id
order_kr <-paste0(rep(unique(polls$r),
                      each = 6),
                  rep(c("cdu", "fdp", "gru", "lin","spd", "oth_afd"), 71))

# compute election-party id
polls <- polls %>%
  mutate(kr = factor(paste0(r, party), levels = order_kr),
         kr_id = as.integer(kr),
         t_sc = as.numeric(days_to_election)/max(as.numeric(days_to_election))) 

# poll level data
poll_level <- polls %>% 
  group_by(poll_id_int, election_year, r, r_id, t_sc, sample_size) %>% 
  summarise() %>% 
  ungroup()

# election-party level data
election_party_level <- polls %>%  
  group_by(r, party, voteshare, kr, kr_id, k_id, r_id, r_short) %>%  
  summarise()

# # create b-splines design matrix
# B <- t(bs(poll_level$t_sc, knots = quantile(poll_level$t_sc, 
#                                             probs= seq(0.1,0.9,0.1)), 
#           degree = 3,
#           Boundary.knots = c(0,1)))
# 
# postrv <- as.rv(resStan)
# 
# 
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
#   idx <- which(polls$kr_id == i)
#   b0_kr[i] <- mean(bias0_i[idx])
# }
# 
# rm(bias0_i)
# 
# # splines
# splines <- get_splines(par = postrv$beta, b_splines = B, polls = polls)
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
# 
# saveRDS(b0_kr, "~/results_vis/b0_kr_ltw94_21.RDS")
# saveRDS(srs_var_kr, "~/results_vis/srs_var_kr_ltw94_21.RDS")
# saveRDS(srs_se_kr, "~/results_vis/srs_se_kr_ltw94_21.RDS")
# saveRDS(var_kr, "~/results_vis/var_kr_ltw94_21.RDS")
# saveRDS(se_kr, "~/results_vis/se_kr_ltw94_21.RDS")
# saveRDS(deff_kr, "~/results_vis/deff_kr_ltw94_21.RDS")
# 

# Bias and variance summaries ---------------------------------------------

# leoad quantities of interest
b0_kr <- readRDS( "~/results_vis/b0_kr_ltw94_21.RDS")
srs_var_kr <- readRDS( "~/results_vis/srs_var_kr_ltw94_21.RDS")
srs_se_kr <- readRDS( "~/results_vis/srs_se_kr_ltw94_21.RDS")
var_kr <- readRDS( "~/results_vis/var_kr_ltw94_21.RDS")
se_kr <- readRDS( "~/results_vis/se_kr_ltw94_21.RDS")
deff_kr <- readRDS( "~/results_vis/deff_kr_ltw94_21.RDS")

# election day bias
b0_kr_summary <- summary(b0_kr)
b0_kr_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
b0_kr_summary <- merge(b0_kr_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "oth_afd"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "others")))

# design effect
deff_summary <- summary(deff_kr)
deff_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
deff_summary$var_type <- "Design effect"
deff_summary <- merge(deff_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "oth_afd"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "others")))

# SRS var
srs_var_summary <- summary(srs_var_kr)
srs_var_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
srs_var_summary$var_type <- "SRS variance"
srs_var_summary <- merge(srs_var_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "oth_afd"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "others")))

# Total var
var_summary <- summary(var_kr)
var_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
var_summary$var_type <- "Total variance"
var_summary <- merge(var_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "oth_afd"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "others")))

# SRS se
srs_se_summary <- summary(srs_se_kr)
srs_se_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
srs_se_summary$se_type <- "SRS standard error"
srs_se_summary <- merge(srs_se_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "oth_afd"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "others")))
# total se
se_summary <- summary(se_kr)
se_summary$kr_id <- seq(1:length(unique(election_party_level$kr_id)))
se_summary$se_type <- "Total standard error"
se_summary <- merge(se_summary, election_party_level, by = "kr_id") %>% 
  mutate(party = factor(party, 
                        levels = c("cdu", "spd", "gru", "fdp", "lin", "oth_afd"),
                        labels = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", 
                                   "others")))



# Tables ------------------------------------------------------------------

# average absolute election level bias
mean(abs(b0_kr))

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

# average election level srs variance by party
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
error <- rbind(mab0_k, msrs_k, mvar_k, mdeff_k)
rownames(error) <- c("Absolute election day bias",
                     "SRS variance",
                     "Total variance",
                     "Design effect")

xtable(error)


# Plots -------------------------------------------------------------------

# election level bias on election day
b0_kr_plot <- ggplot(b0_kr_summary, aes(x)) + 
  geom_segment(aes(y = party, yend = party, x = `2.5%`, xend = `97.5%`)) + 
  geom_segment(aes(y = party, yend = party, x = `25%`, xend = `75%`), 
               size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Election-day bias", y = "Party") + 
  scale_y_discrete(limits = rev(levels(as.factor(b0_kr_summary$party)))) + 
  theme_bw()  + 
  theme(text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, size = 12, hjust = 0.8),
        axis.title=element_text(size=14), 
        axis.text.y = element_text(size = 14),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm")) +
  facet_wrap(~ r_short, ncol = 8)  +
  scale_x_continuous(labels=label_percent(accuracy = 1), 
                     breaks = seq(-0.4, 0.4, 0.2))

ggsave(filename = "ltw94_21_redundant_splines_election_day_bias.png", 
       plot = b0_kr_plot, 
       path = "~/results_vis/plots",
       width = 12, height = 15)

# total vs. SRS sd 
tot_srs_se <- rbind(srs_se_summary, se_summary)

tot_srs_plot <- ggplot(tot_srs_se, aes(x = party, y = mean, 
                                        color = se_type, fill = se_type,
                                        linetype = se_type)) +
  geom_bar(stat = "identity", width = 0.8, 
           position = "identity", alpha = 0.5) +
  facet_wrap(~r_short, ncol = 8) +
  theme_bw()+
  scale_y_continuous(labels=label_percent(accuracy = 0.1)) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14),
        text = element_text(size = 14), 
        axis.text.x = element_text(size = 10, angle = 90,  hjust = 1), 
        axis.text.y = element_text(size = 14),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14)) +
  labs(x = "Party", y = "") +
  scale_color_manual(name = "", values = c("black", NA)) +
  scale_fill_manual(name = "", values = c("white", "darkgrey")) +
  scale_linetype_manual(name = "",values = c("dashed", "blank"))

ggsave(filename = "ltw94_21_redundant_splines_tot_srs_se.png", 
       plot = tot_srs_plot, 
       path = "~/results_vis/plots",
       width = 12, height = 15)

