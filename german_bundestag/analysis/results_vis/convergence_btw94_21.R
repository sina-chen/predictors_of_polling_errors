#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Convergence diagnostics German Bundestag election polls 1994-2021 
#
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(bayesplot)
library(rstan)

color_scheme_set(scheme = "darkgray")


# Data --------------------------------------------------------------------

# stan fit obj.
resStan <- readRDS("~/fit_stan/resStan_btw_redundant_splines_final.RDS")


# Diagnostics -------------------------------------------------------------

# rhat
rhat_btw <- rhat(resStan)
rhat_btw_plot <- mcmc_rhat_hist(rhat_btw)  +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 28),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 28))

ggsave(filename = "btw94_21_rhat.png", 
       plot = rhat_btw_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 6) 

# neff
neff_btw <- neff_ratio(resStan)
neff_btw_plot <- mcmc_neff_hist(neff_btw) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 28),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 28))

ggsave(filename = "btw94_21_neff.png", 
       plot = neff_btw_plot, 
       path = '~/results_vis/plots',
       width = 12, height = 6) 


