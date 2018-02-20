# purl code

library(stringr)
files_from = dir()[str_detect(dir(), pattern='[0-9]+(.)*Rmd')]
files_from
files_to = paste0('code/', str_sub(files_from, end=-4), 'R')

library(knitr); library(purrr)

files_from %>% map2(.y=files_to, ~purl(input=.x, output=.y))
