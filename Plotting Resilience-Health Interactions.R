
# Plotting Racial/Ethnic Variations in Resilience Health-Benefits ---------



# Margins of Race-specific slopes (Resilience and Health)
proj1_plot <- margins_proj1 %>% group_by(race_eth) %>% 
  ggplot(mapping = aes(x = resilience
                       ,y = srh5
                       ,ymin = lower_ci
                       ,ymax = upper_ci
                       ,fill = race_eth
                       ,shape = race_eth))


# Plotting Interaction
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

  # Plotting Margins (Race-Specific: Resilience-Health by Total Disadvantage)
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
  
  

# Plotting Margins (Race-Specific: Resilience-Health by Material Disadvantage)
  
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
  

  
# Plotting Margins (Race-Specific: Resilience-Health by Social Disadvantage)
  
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



  