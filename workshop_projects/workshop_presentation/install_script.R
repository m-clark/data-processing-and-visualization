# Install libraries -------------------------------------------------------

# If you already have some of these (e.g. tidyverse), there is no need to install them again

install.packages(
  c(
    'tidyverse',
    'tidymodels',
    'emmeans',
    'ggeffects',
    'tune'
  )
)


# to run interactive exercises (not required!):

install.packages('learnr')



# Load libraries ----------------------------------------------------------


library(tidyverse)      # loads dplyr, ggplot2, purrr, etc.
library(tidymodels)     # loads parsnip, etc.

# load others as needed