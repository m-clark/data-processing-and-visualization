# Install libraries -------------------------------------------------------

install.packages(
  c(
    'tidyverse',
    'haven',       # not required for workshop but potentially useful
    'data.table'
  )
)


# to run interactive exercises (not required):

install.packages('learnr')



# Load libraries ----------------------------------------------------------


library(tidyverse)      # loads dplyr, ggplot2, etc.
# library(haven)          # for importing from other statistical packages
library(data.table)     # for alternative framework for data processing