# Install libraries -------------------------------------------------------

# If you already have some of these (e.g. tidyverse), there is no need to
# install them again, so just comment out those lines

install.packages(
  c(
    'tidyverse',
    'patchwork',
    'gganimate',
    'scico'
  )
)


# to run interactive exercises (not required!):

install.packages('learnr')


# Load libraries ----------------------------------------------------------

library(tidyverse)      # loads dplyr, ggplot2, purrr, etc.

# load others as needed

library(patchwork)