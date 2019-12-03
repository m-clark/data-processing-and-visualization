# Install libraries -------------------------------------------------------

install.packages(
  c(
    'tidyverse',
    'plotly',
    'patchwork',
    'reticulate'
  )
)


# for scientific color scales

if (!requireNamespace('devtools')) {
  install.packages('devtools')
}

devtools::install_github('thomasp85/scico')


# to run interactive exercises:

install.packages('learnr')

# misc demonstrated

install.packages(
  c(
    'highcharter',
    'DT',
    'visNetwork',
    'sigmajs',
    'leaflet'
  )
)


# Load libraries ----------------------------------------------------------


library(tidyverse)  # loads dplyr, ggplot2, etc.
library(plotly)     # for interactive demonstration