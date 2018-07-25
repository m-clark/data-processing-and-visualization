## ----progsetup, include=FALSE, eval=TRUE, cache=FALSE--------------------
knitr::opts_chunk$set(eval=T, echo=T)

## ----view, eval=FALSE----------------------------------------------------
## View(diamonds)

## ----str-----------------------------------------------------------------
str(diamonds)
glimpse(diamonds)

## ----lm_object-----------------------------------------------------------
lm_mod = lm(mpg ~ ., data=mtcars)
str(lm_mod, 0)
str(lm_mod, 1)

## ----lm_object2----------------------------------------------------------
lm_mod$coefficients
lm_mod$model %>% 
  head()

## ----lm_mod_summary------------------------------------------------------
summary(lm_mod)

## ----lm_mod_summary_obj--------------------------------------------------
lm_mod_summary = summary(lm_mod)
str(lm_mod_summary)

## ----lm_mod_summary_table------------------------------------------------
lm_mod_summary$coefficients %>%
  kableExtra::kable(digits = 2)

## ----summary-------------------------------------------------------------
summary(diamonds)
summary(diamonds$clarity)
summary(lm_mod)
summary(lm_mod_summary)

## ----methods_summary-----------------------------------------------------
methods('summary')

## ----methods_brms--------------------------------------------------------
library(brms)
methods(class = 'brmsfit')

## ----s4_example----------------------------------------------------------
car_matrix = mtcars %>% 
  as.matrix() %>%       # convert from df to matrix
  Matrix::Matrix()      # convert to Matrix class (S4)

typeof(car_matrix)
str(car_matrix)

## ----lm_func-------------------------------------------------------------
str(lm)

## ----lm_func2, eval=2, echo=1--------------------------------------------
lm
head(lm, 20)

## ----tibble_inspect------------------------------------------------------
library(dplyr)
?starwars

## ----help, eval=FALSE----------------------------------------------------
## sample(?)

## ----help2, error=TRUE---------------------------------------------------
nums = 1:5
sample(nums, 10)

