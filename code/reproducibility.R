## ----rmdsetup, include=FALSE, eval=TRUE, cache=FALSE---------------------------
knitr::opts_chunk$set(eval=T, echo=T)


## ----lm_demo, echo=FALSE-------------------------------------------------------
lm(mpg ~ wt, mtcars) %>% 
  summary() %>% 
  pander::pander(round = 2)


## ----lm_demo_show, eval=FALSE--------------------------------------------------
## lm(mpg ~ wt, mtcars) %>%
##   summary() %>%
##   pander(round = 2)


## ----arxiv, echo=FALSE---------------------------------------------------------
library(rvest)

test0 = read_html('https://arxiv.org/abs/1507.02646')

N = test0 %>% 
  html_nodes('.submission-history') %>% 
  html_nodes('a') %>% 
  grepl(pattern = 'v[0-9]+') %>% 
  sum()
N = N + 1

