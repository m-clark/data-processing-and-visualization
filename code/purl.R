# purl code

library(stringr)
files_from = dir()[str_detect(dir(), pattern = '(.)*Rmd')]
files_from = files_from[!str_detect(files_from, 'summary|index.Rmd|intro|references')]
files_to = paste0('code/', str_sub(files_from, end = -4), 'R')

library(knitr); library(purrr)

map2(.x = files_from, .y = files_to, ~ purl(input = .x, output = .y))
