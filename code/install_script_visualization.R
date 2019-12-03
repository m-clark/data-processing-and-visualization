install.packages(
  c(
    'tidyverse',
    'plotly',
    'patchwork',
    'reticulate'
  )
)

if (!requireNamespace('devtools')) {
  install.packages('devtools')
}

devtools::install_github('thomasp85/scico')



