

# Descriptive Visualizations of Early Life Disadvantage -------------------

  # Summarizing Disadvantage Variables #
db %>% skim(matdisad_f    
           ,socdisad_f 
           ,matdisad_s_sum
           ,socdisad_s_sum
           ,matdisad_n_sum
           ,socdisad_n
           ,
           ,matdisad_tot
           ,socdosad_tot)

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



  