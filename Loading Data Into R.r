# Code Below Checks for New Versions of R and Downloads it ----------------

# installr::updateR()


# Installing Packages -----------------------------------------------------

install.packages(c('haven',
                   'readr',
                   'broom', 
                   'ggplot2', 
                   'ggthemes', 
                   'gt', 
                   'gtsummary', 
                   'Hmisc', 
                   'knitr', 
                   'labeling',
                   'lattice',
                   'lmtest',
                   'magrittr',
                   'naniar',
                   'paletteer',
                   'png',
                   'prismatic',
                   'purrr',
                   'RColorBrewer',
                   'renv',
                   'rio',
                   'readxl',
                   'skimr',
                   'sf',
                   'stargazer',
                   'tidyverse',
                   'viridis',
                   'mitml',
                   'readxl'))

# renv::init()


# Loading Packages --------------------------------------------------------


library(readr)
library(tidyverse)
library(haven)
library(mitml)
library(skimr)
library(ggplot2)
library(RColorBrewer)
library(readxl)

renv::snapshot()

# Using Merged Dataset ----------------------------------------------------

  # Only One Imputed Dataset (Dropped others using Stata) #
db <- read_stata('Single Dataset (1).dta')


  # Fixing Race Variable
db <- db %>% 
  mutate(race_eth5 = as.numeric(race_eth5)) %>% 
  filter(race_eth5 <= 3)
db %>% count(race_eth5)


  # Summarizing Disadvantage Variables #
db %>% skim(matdisad_f    
           ,socdisad_f 
           ,matdisad_s
           ,socdisad_s
           ,matdisad_n
           ,socdisad_n)

  # Plotting Material Disadvantage Measures
matplot <- db %>% group_by(race_eth5) %>% 
  ggplot(mapping = aes(x = matdisad_n
                      ,y = matdisad_s
                      ,size = matdisad_f
                      ,color = as_factor(race_eth5)
                      ,shape = as_factor(race_eth5)
                      ))


    # Race-Specific Plots
matplot + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .3) +
  scale_y_log10()


    # Race-Specific Plots
matplot + 
  geom_point(mapping = aes(group = race_eth5)
            ,alpha = .3) +
  scale_y_log10() +
  facet_wrap(. ~ race_eth5
             ,nrow = 1)


# Plotting Social Disadvantage Measures
socplot <- db %>% group_by(race_eth5) %>% 
  ggplot(mapping = aes(x = socdisad_f
                      ,y = socdisad_s
                      ,size = socdisad_n
                      ,color = as_factor(race_eth5)
                      ,shape = as_factor(race_eth5)))


# Race-Specific Plots
socplot + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .1) +
   scale_y_log10()


# Race-Specific Plots
socplot + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .1) +
  scale_y_log10() +
  facet_wrap(. ~ race_eth5
             ,nrow = 1)



# Plotting Total Disadvantage Measures
totplot <- db %>% group_by(race_eth5) %>% 
  ggplot(mapping = aes(x = matdisad_tot
                      ,y = socdisad_tot
                      ,size = 1
                      ,color = as_factor(race_eth5)
                      ,shape = as_factor(race_eth5)))


# Race-Specific Plots
totplot + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .1)


# Race-Specific Plots
totplot + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .1) +
  facet_wrap(. ~ race_eth5
             ,nrow = 1)

# W1 HH Income & Material Disadvantage
incplot_m <- db %>% 
  filter(par_inc <= 200) %>% 
  group_by(race_eth5) %>% 
  ggplot(mapping = aes(x = par_inc
                      ,y = matdisad_tot
                      ,size = 1
                      ,color = as_factor(race_eth5)
                      ,shape = as_factor(race_eth5)))

  # Race-Specific Plots
incplot_m + 
  geom_point(mapping = aes(group = race_eth5)
            ,alpha = .2) +
  scale_x_reverse()


  # Race-Specific Plots
incplot_m + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .2) +
  facet_wrap(. ~ race_eth5
             ,ncol = 1) +
  scale_x_reverse()


# W1 HH Income & Social Disadvantage
  incplot_s <- db %>% 
    filter(par_inc <= 200) %>% 
    group_by(race_eth5) %>% 
    ggplot(mapping = aes(x = par_inc
                         ,y = socdisad_tot
                         ,size = 1
                         ,color = as_factor(race_eth5)
                         ,shape = as_factor(race_eth5)))
  
# Race-Specific Plots
incplot_s + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .2) 
scale_x_reverse()
  
  
# Race-Specific Plots
incplot_s + 
  geom_point(mapping = aes(group = race_eth5)
             ,alpha = .2) +
  facet_wrap(. ~ race_eth5
             ,ncol = 1) +
scale_x_reverse()
  
  
# W1 HH Income & Material Disadvantage
incplot <- db %>% 
  filter(par_inc <= 200) %>% 
  group_by(race_eth5) %>% 
  ggplot(mapping = aes(x = par_inc
                      ,y = disad_tot
                      ,size = 1
                      ,color = as_factor(race_eth5)
                      ,shape = as_factor(race_eth5)))
  # Race-Specific Plots
  incplot + 
    geom_point(mapping = aes(group = race_eth5)
               ,alpha = .2) +
  scale_x_reverse()
  
  
  # Race-Specific Plots
  incplot + 
    geom_point(mapping = aes(group = race_eth5)
               ,alpha = .2) +
    facet_wrap(. ~ race_eth5
               ,ncol = 1) +
  scale_x_reverse()
  
  

# Plotting Margins Calculated in Stata ------------------------------------

  # Importing Data (1 File, 4 sheets)
margins_proj1 <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx' 
                           ,sheet = 1)

margins_mat <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx' 
                         ,sheet = 3)

margins_soc <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx' 
                         ,sheet = 4)

margins_tot <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx'
                         ,sheet = 2)
  
race.labs <- c('1' = 'NH White', '2' = 'NH Black', '3' = 'Hispanic/Latinx')



# Plotting Racial/Ethnic Variations in Resilience Health-Benefits

# Plotting Margins
proj1_plot <- margins_proj1 %>% group_by(race_eth) %>% 
  ggplot(mapping = aes(x = resilience
                       ,y = srh5
                       ,ymin = lower_ci
                       ,ymax = upper_ci
                       ,fill = race_eth
                       ,shape = race_eth))


# Race-Specific Plots (Material Disadvantage)
proj1_plot + 
  geom_ribbon(alpha = .3, 
              aes(color = NULL)) +
  geom_line(mapping = aes(group = race_eth
                          ,color = sig_slope)
            ,size = 1.5) +
  scale_color_manual(values = c("p < 0.05" = "black"
                                ,"NS" = "red")) +
  scale_y_continuous(breaks = seq(2.0, 5.0, 0.5),
                     minor_breaks = seq(2.75, 4.75, .5)) +
  labs(title="Resilience Health-Effects Vary by Race/Ethnicity (80% CIs)"
       ,colour = "Sig:") +
  xlab("Resilience (Standardized)") +
  ylab("Wave 5 Self-Rated Health") +
  guides(fill = guide_legend(title = "Race/Ethnicity:")
         ,colour = guide_legend(override.aes = list(fill = "white"))) +
  theme(legend.position = "bottom"
        ,legend.text = element_text(size = 12)
        ,plot.title =  element_text(size  = 16
                                    ,hjust = 0.5)
        ,panel.background = element_rect(fill = 'white')
        ,strip.text = element_text(size = 12, face = "bold")
        ,axis.text = element_text(size = 12)
        ,axis.ticks.length.x = unit(1.5, 'mm'))

ggsave('disad_proj1.png'
       ,dpi = 1080
       ,height = 6
       ,width = 10.5
       ,units = 'in') 


# Plotting Resilience-Health Slopes by Race/Ethnicity and Total Disadvantage

  # Plotting Margins
  totplot <- margins_tot %>% group_by(disad) %>% 
    ggplot(mapping = aes(x = resilience
                         ,y = srh5
                         ,ymin = lower_ci
                         ,ymax = upper_ci
                         ,fill = disad
                         ,shape = disad))
  
  
  # Race-Specific Plots (Total Disadvantage)
  totplot + 
    geom_ribbon(alpha = .3, 
                aes(color = NULL)) +
    geom_line(mapping = aes(group = disad
                           ,color = sig_slope)
             ,size = 1.5) +
    scale_color_manual(values = c("p < 0.05" = "black"
                                 ,"NS" = "red")) +
    scale_y_continuous(breaks = seq(2.0, 5.0, 0.5),
                       minor_breaks = seq(2.75, 4.75, .5)) +
    facet_wrap(. ~ race_eth_q
               ,ncol = 3
               ,labeller = labeller(race_eth_q = race.labs)) +
    labs(title="Early Life Disadvantage Moderates Resilience Health-Benefits"
        ,colour = "Sig:") +
    xlab("Resilience (Standardized)") +
    ylab("Wave 5 Self-Rated Health") +
    guides(fill = guide_legend(title = "Early Life Disadvantage:")
          ,colour = guide_legend(override.aes = list(fill = "white"))) +
    theme(legend.position = "bottom"
          ,legend.text = element_text(size = 12)
          ,plot.title =  element_text(size  = 16
                                     ,hjust = 0.5)
          ,panel.background = element_rect(fill = 'white')
          ,strip.text = element_text(size = 12, face = "bold")
          ,axis.title = element_text(size = 16)
          ,axis.text = element_text(size = 12)
          ,axis.ticks.length.x = unit(1.5, 'mm'))

  ggsave('disad_tot.png'
         ,dpi = 1080
         ,height = 6
         ,width = 10.5
         ,units = 'in')
  
  

# Plotting Resilience-Health Slopes by Race/Ethnicity and Material Disadvantage
  
  # Plotting Margins
  matplot <- margins_mat %>% group_by(disad) %>% 
    ggplot(mapping = aes(x = resilience
                         ,y = srh5
                         ,ymin = lower_ci
                         ,ymax = upper_ci
                         ,fill = disad
                         ,shape = disad))
  
  
  # Race-Specific Plots (Material Disadvantage)
  matplot + 
    geom_ribbon(alpha = .3, 
                aes(color = NULL)) +
    geom_line(mapping = aes(group = disad
                            ,color = sig_slope)
              ,size = 1.5) +
    scale_color_manual(values = c("p < 0.05" = "black"
                                  ,"NS" = "red")) +
    scale_y_continuous(breaks = seq(2.0, 5.0, 0.5),
                       minor_breaks = seq(2.75, 4.75, .5)) +
    facet_wrap(. ~ race_eth_q
               ,ncol = 3
               ,labeller = labeller(race_eth_q = race.labs)) +
    labs(title="Resilience Health-Effects Vary by Disadvantage (80% CIs)"
         ,colour = "Sig:") +
    xlab("Resilience (Standardized)") +
    ylab("Wave 5 Self-Rated Health") +
    guides(fill = guide_legend(title = "Early Life Material Disadvantage:")
           ,colour = guide_legend(override.aes = list(fill = "white"))) +
    theme(legend.position = "bottom"
          ,legend.text = element_text(size = 12)
          ,plot.title =  element_text(size  = 16
                                      ,hjust = 0.5)
          ,panel.background = element_rect(fill = 'white')
          ,strip.text = element_text(size = 12, face = "bold")
          ,axis.title = element_text(size = 16)
          ,axis.text = element_text(size = 12)
          ,axis.ticks.length.x = unit(1.5, 'mm'))
  
  ggsave('disad_mat.png'
         ,dpi = 1080
         ,height = 6
         ,width = 10.5
         ,units = 'in')
  

  
# Plotting Resilience-Health Slopes by Race/Ethnicity and Social Disadvantage
  
  # Plotting Margins
  socplot <- margins_soc %>% group_by(disad) %>% 
    ggplot(mapping = aes(x = resilience
                         ,y = srh5
                         ,ymin = lower_ci
                         ,ymax = upper_ci
                         ,fill = disad
                         ,shape = disad))
  
  
  # Race-Specific Plots (Material Disadvantage)
  socplot + 
    geom_ribbon(alpha = .3, 
                aes(color = NULL)) +
    geom_line(mapping = aes(group = disad
                            ,color = sig_slope)
              ,size = 1.5) +
    scale_color_manual(values = c("p < 0.05" = "black"
                                  ,"NS" = "red")) +
    scale_y_continuous(breaks = seq(2.0, 5.0, 0.5),
                       minor_breaks = seq(2.75, 4.75, .5)) +
    facet_wrap(. ~ race_eth_q
               ,ncol = 3
               ,labeller = labeller(race_eth_q = race.labs)) +
    labs(title="Resilience Health-Effects Vary by Disadvantage (80% CIs)"
         ,colour = "Sig:") +
    xlab("Resilience (Standardized)") +
    ylab("Wave 5 Self-Rated Health") +
    guides(fill = guide_legend(title = "Early Life Social Disadvantage:")
           ,colour = guide_legend(override.aes = list(fill = "white"))) +
    theme(legend.position = "bottom"
          ,legend.text = element_text(size = 12)
          ,plot.title =  element_text(size  = 16
                                      ,hjust = 0.5)
          ,panel.background = element_rect(fill = 'white')
          ,strip.text = element_text(size = 12, face = "bold")
          ,axis.text = element_text(size = 12)
          ,axis.ticks.length.x = unit(1.5, 'mm'))
  
  ggsave('disad_soc.png'
         ,dpi = 1080
         ,height = 6
         ,width = 10.5
         ,units = 'in')



  