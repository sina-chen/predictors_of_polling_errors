#-------------------------------------------------------------------------------
#
# US Senate poll accuracy 1990 - 2020: 
#   empty model election day bias and variance plot
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
  library(ggpubr)
  library(ggstance)
}

setnsims(10000)


# Data --------------------------------------------------------------------

# # polls
# polls <- readRDS("~/data/us/senate/us_senate_polls_context.RDS")


# # simulation results
# resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context_empty.RDS')


# Functions ---------------------------------------------------------------

# # extra rv functins
# ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
# logit.rv <- function(x) rvmapply(FUN = logit, x) 

# generate plot by cycle 
plot_cycle <- function(year, margin = unit(c(-1.1, 0.5, 0.5, 0.5), "cm"), 
                       xaxis_labels = F) {
  if(xaxis_labels == F){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      mapply(function(mean, sd) {
        stat_function(fun = dnorm, args = list(mean = mean, sd = sd),
                      alpha = 0.5)
      }, 
      mean = cycle_data$mean_est,
      sd = sqrt(cycle_data$var_est)
      ) +
      xlim(-0.3, 0.25) +
      theme_minimal() +
      labs(y = year, x = "") +
      theme(axis.text = element_blank(),
            axis.title.y = element_text(angle = 0, vjust = 0.5, size = 16),
            plot.margin = margin) +
      geom_boxploth(data = cycle_data, inherit.aes = F, width = 2,
                    aes(y = -2, x = mean_est)) +
      # geom_segment(data = cycle_data, color = "darkgrey",
      #              aes(x = mean_est, xend = mean_est, y = 0,
      #                  yend = (1/sqrt((2*3.14*var_est)))*2.72^((-(mean_est-mean(mean_est))^2)/2*var_est))) +
      geom_vline(xintercept = 0, linetype = "dashed") 
    
  } else if (xaxis_labels == T){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      mapply(function(mean, sd) {
        stat_function(fun = dnorm, args = list(mean = mean, sd = sd),
                      alpha = 0.5)
      }, 
      mean = cycle_data$mean_est,
      sd = sqrt(cycle_data$var_est)
      ) +
      xlim(-0.3, 0.25) +
      theme_minimal() +
      labs(y = year, x = "") +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_text(angle = 0, vjust = 0.5, size = 16),
            plot.margin = margin,
            axis.text.x = element_text(size = 12)) +
      geom_boxploth(data = cycle_data, inherit.aes = F, width = 2,
                    aes(y = -2, x = mean_est)) +
      # geom_segment(data = cycle_data, color = "darkgrey", 
      #              aes(x = mean_est, xend = mean_est, y = 0,
      #                  yend = (1/sqrt((2*3.14*var_est)))*2.72^((-(mean_est-mean(mean_est))^2)/2*var_est))) + # https://davidmlane.com/hyperstat/A25726.html
      geom_vline(xintercept = 0, linetype = "dashed") 
  }
  
  
  return(plot)
}



# Preparation -------------------------------------------------------------

# # compute election groups and ids for election year and state
# polls <- polls %>%
#   mutate(state_year = paste0(state, cycle),
#          state_year_int = as.integer(as.factor(state_year)),
#          cycle = as.integer(cycle)) 

# # Election-level data
# election_data <- polls %>%
#   group_by(state_year, state_year_int, cycle, state, state_abb, vote2_rep) %>%
#   summarise(n_avg = mean(sample_size)) %>%
#   ungroup()
# 
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
#   mutate(cycle_factor = as.factor(cycle))
# rm(b0, resStan)
# 
# saveRDS(b0_summary, "~/results_vis/us_senate_predictable/bias_senate_empty.RDS")

b0_summary <- readRDS("~/results_vis/us_senate_predictable/bias_senate_empty.RDS")


#### Election level election day variance ####

# # election day variance
# v0_i <- (p0_r*(1-p0_r))[polls$state_year_int]/polls$sample_size + postrv$phi2[polls$state_year_int]
# 
# # avg. election level election day variance
# v0_r <- rv(length(unique(polls$state_year)))
# 
# for(i in 1:length(unique(polls$state_year))){
#   v0_r[i] <- mean(v0_i[i == polls$state_year_int])
# }
# 
# v0_r_summary <- summary(v0_r)
# rm(v0_i, v0_r, postrv, p0_r)
# 
# saveRDS(v0_r_summary, "~/results_vis/us_senate_predictable/v0_senate_empty.RDS")

v0_r_summary <- readRDS("~/results_vis/us_senate_predictable/v0_senate_empty.RDS")

#### Data for plot ####

plot_data <- data.frame(mean_est = b0_summary$mean,
                        var_est = v0_r_summary$mean,
                        cycle = b0_summary$cycle,
                        state = b0_summary$state,
                        state_abb = b0_summary$state_abb,
                        state_year = b0_summary$state_year,
                        state_year_int = b0_summary$state_year_int)

rm(b0_summary, v0_r_summary)


# Plot --------------------------------------------------------------------

# generate plots for each cycle
plot1990 <- plot_cycle(1990, margin =  unit(c(0 , 0.5, 0.5, 0.5), "cm"))
plot1992_2018 <- lapply(seq(1992, 2018, 2), plot_cycle) 
plot2020 <- plot_cycle(2020, xaxis_labels = T)

# append plots in list
plots <- append(list(plot1990), plot1992_2018, after = 1)
plots <- append(plots, list(plot2020), after = 15)

# plot
density_estimates_senate <- ggarrange(plotlist = plots, ncol = 1,
                                      heights = c(1.5,rep(1,14),1.2)) %>% 
  annotate_figure(bottom = text_grob("Estimated election day bias and variance", 
                                     size = 18),
                  left = text_grob("Cycle", rot = 90,
                                     size = 18))

# save plot
ggsave(filename = '~/results_vis/us_senate_predictable/plots/density_estimates_senate_mean.png', 
       plot = density_estimates_senate, 
       width = 16, height = 18, bg='#ffffff')   
