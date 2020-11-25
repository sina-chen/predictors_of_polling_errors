########################################################################################
# Exploratory data analysis
# Author: Sina Chen
# Notes: 
#
########################################################################################

#### Libraries ####

library(ggplot2)
library(usmap) # US map
library(dplyr)
library(datasets) # US states
library(Metrics) # RMSE
library(ggsci) # color scales
show_col(pal_uchicago("default")(9)) # color schema
library(scales)
library(xtable) # safe Latex tables
library(reshape2)
library(stringr)

#### Data ####

data <- readRDS('data.RDS')

# Create swing/Safe state year groups
data <- data %>% 
  mutate(swy = paste0(election_year,'_', swing),
         swy_int = as.integer(as.factor(swy)))
states <- data.frame(state = rep(state.abb, 4), 
                     election_year = rep(c(2004,2008,2012,2016),each = 50))


#-------------------------------------------------------------------------------
#### Visualisation ####

### Error by state and election year ###

data_error_r <- data %>%
  group_by(election_year, state) %>%
  summarise(mean_error = mean(rep_poll2 - rep_result2),
            rms_error = rmse(rep_poll2, rep_result2))

data_error_r <- merge(data_error_r, states, all = T, 
                      by =c('election_year', 'state'))

# Plot ME by state
plot_usmap(data = data_error_r, exclude = 'DC', value = 'mean_error') +
  facet_wrap(~election_year)  + 
  theme(legend.position = c(1,0.4),
        legend.text = element_text(size=20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20)) +
  scale_fill_continuous(name = "Mean error", 
                        low = '#FFA319FF',
                        high = '#800000FF') 

# Plot RMSE by state
plot_usmap(data = data_error_r, exclude = 'DC', value = 'rms_error') +
  facet_wrap(~election_year)  + 
  theme(legend.position = c(1,0.4),
        legend.text = element_text(size=20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20)) +
  scale_fill_continuous(name = "RMSE", 
                        low = '#FFA319FF', 
                        high = '#800000FF') 


### Table number of polls by state and election year ###

xtable(table(data$state, data$election_year), 
       caption  ='Number of polls by state and election year',
       label = 'ap:n_poll')


### Independnet expenditires over time ###

# Total independent expenditures
ggplot(data, aes(x = dte, y = exp_week/1000000, 
                 color = as.factor(election_year))) +
  geom_line(size = 1.5) +
  scale_x_reverse() +
  scale_color_manual(values = c('#8A9045FF', 
                                '#155F83FF', 
                                '#C16622FF',
                                '#350E20FF'), 
                     name = 'Year') +
  labs(x = 'Days to election', 
       y = 'Total independent expenditures of previous 7 days (in M)') + 
  theme_bw()  +
  theme(axis.title=element_text(size=rel(2.5)),
        legend.text=element_text(size=rel(2.5)),
        legend.title=element_text(size=rel(2.5)),
        axis.text=element_text(size=rel(2)))

# Difference in independent expenditures
ggplot(data, aes(x = dte, y = diff_exp_week/1000000, 
                 color = as.factor(election_year))) +
  geom_line(size = 1.5) +
  scale_x_reverse() +
  scale_color_manual(values = c('#8A9045FF', 
                                '#155F83FF', 
                                '#C16622FF',
                                '#350E20FF'), 
                     name = 'Year') +
  labs(x = 'Days to election', 
       y = 'Difference independent expenditures of previous 7 days (in M)') +
  theme_bw()  +
  theme(axis.title=element_text(size=rel(2.5)),
        legend.text=element_text(size=rel(2.5)),
        legend.title=element_text(size=rel(2.5)),
        axis.text=element_text(size=rel(2)))


### Table swing state by state and election year ###

data_swing <- data %>% 
  group_by(state, election_year, swing) %>%
  summarise()  %>% 
  dcast(state ~ election_year, value.var = 'swing')
data_swing[is.na(data_swing)] <- '-'

print(xtable(data_swing, 
             caption = 'Swig state (1), safe state (0) by state and election year',
             label = 'ap:swing_state_year', digits = 0), 
      include.rownames = F)


### Number of polls before and after both conventions ###

xtable(table(data$after_con, data$election_year), 
       caption = 'Number of polls conducted before and after the conventions by election year', 
       label = 'ap:con_n')


### Table scales electoral votes by state and election year ###

data_ev <- data %>% 
  group_by(election_year, state, ev_sc) %>%
  summarise %>% 
  dcast(state ~ election_year, value.var = 'ev_sc')

print(xtable(data_ev, 
             caption = 'Scaled electoral votes by state and election year',
             label = 'ap:ev_sc', digits = 3), 
      include.rownames = F)


### Table swing/safe state - election year group ###

xtable(table(data$swing, data$election_year), 
       label = 'ap:n_swing_year', 
       caption = 'Number of swing states by election year')

