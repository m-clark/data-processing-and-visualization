## ----pipessetup, include=FALSE, eval=TRUE, cache=FALSE-------------------
knitr::opts_chunk$set(echo = T)

## ----pipes---------------------------------------------------------------
mydf %>% 
  select(var1, var2) %>% 
  filter(var1 == 'Yes') %>% 
  summary

## ----varfly--------------------------------------------------------------
mydf %>% 
  mutate(newvar1 = var1 + var2,
         newvar2 = newvar1/var3) %>% 
  summarise(newvar2avg = mean(newvar2))

## ----pipeviz-------------------------------------------------------------
basegraph %>% 
  points %>%
  lines %>%
  layout

## ----pipeerror-----------------------------------------------------------
mydf %>% 
  lm(y ~ x)  # error

## ----pipedot-------------------------------------------------------------
mydf %>% 
  lm(y ~ x, data=.)  # . == mydf

## ----ridiculous, eval=T, echo=-1-----------------------------------------
library(magrittr)
c('Ceci', "n'est", 'pas', 'une', 'pipe!') %>%
{
  .. <-  . %>%
    if (length(.) == 1)  .
    else paste(.[1], '%>%', ..(.[-1]))
  ..(.)
} 

