# Install libraries -------------------------------------------------------

# If you already have some of these (e.g. tidyverse), there is no need to install them again

install.packages(
  c(
    'tidyverse',
    'haven',       # not required for workshop but potentially useful
    'data.table',
    'nycflights13'
  )
)


# to run interactive exercises (not required!):

install.packages('learnr')



# Load libraries ----------------------------------------------------------


library(tidyverse)      # loads dplyr, ggplot2, purrr, etc.

# load others as needed