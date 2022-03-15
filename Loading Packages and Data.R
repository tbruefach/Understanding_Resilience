# Code Below Checks for New Versions of R and Downloads it ----------------

# installr::updateR()


# Installing/Loading Packages ---------------------------------------------

# renv should use the downloaded lockfile to load required packages and the
# versions used at the time analyses were conducted.
install.packages('renv')
renv::restore()

# If renv is not functioning, try "renv::repair()" to fix broken sym-links.
# Otherwise, you can manually download the following packages:

#  install.packages(c('tidyverse'
#                     ,'readr'
#                     ,'haven'
#                     ,'skimr'
#                     ,'ggplot2'
#                     ,'RColorBrewer'
#                     ,'readxl'))

# Loading Packages
library(readr)
library(tidyverse)
library(haven)
library(skimr)
library(ggplot2)
library(RColorBrewer)
library(readxl)



# Importing Data Sources --------------------------------------------------

# Subset used for Descriptive Statistics

  # Only One Imputed Dataset (Dropped others using Stata) #
db <- read_stata('Single Dataset (1).dta')

  # Fixing Race Variable
db <- db %>% 
  mutate(race_eth5 = as.numeric(race_eth5)) %>% 
  filter(race_eth5 <= 3)
db %>% count(race_eth5)


# Importing Margins Calculated in Stata (1 File, 4 sheets)
margins_proj1 <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx' 
                            ,sheet = 1)

margins_mat <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx' 
                          ,sheet = 3)

margins_soc <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx' 
                          ,sheet = 4)

margins_tot <- read_excel(path = 'Margins (Diss Projects 1-2).xlsx'
                          ,sheet = 2)

# Race/Ethnicity Labels
race.labs <- c('1' = 'NH White', '2' = 'NH Black', '3' = 'Hispanic/Latinx')

