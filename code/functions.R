## ----func_ex, eval=FALSE-------------------------------------------------
## mean(myvar)
## sd(myvar)
## sum(is.na(myvar))

## ----func_ex2------------------------------------------------------------
my_summary <- function(myvar) {
  data.frame(
    mean = mean(myvar, na.rm = TRUE),
    sd = sd(myvar, na.rm = TRUE),
    N_missing = sum(is.na(myvar))
    )
}

## ----func_ex3, echo=-1---------------------------------------------------
mtcars = datasets::mtcars  # to undo previous factors
my_summary(mtcars$mpg)

mtcars2 = mtcars
mtcars2$wt[c(3,10)] = NA
my_summary(mtcars2$wt)

## ----func_ex4------------------------------------------------------------
my_summary <- function(myvar) {
  # create an arbitrarily named object with the summary information
  summary_data = data.frame(
    mean = mean(myvar, na.rm = TRUE),
    sd = sd(myvar, na.rm = TRUE),
    N_total = length(myvar),
    N_missing = sum(is.na(myvar))
  )
  
  # return the result!
  summary_data       
}

## ----func_ex5------------------------------------------------------------
my_summary(mtcars2$wt)

## ----func_ex6------------------------------------------------------------
mtcars2 %>% 
  map_dfr(my_summary, .id = 'variable')

## ----nothing_function----------------------------------------------------
two <- function() {
  2
}

two()

## ----source_func, eval=FALSE---------------------------------------------
## source('my_functions/awesome_func.R')

## ----dry, eval=FALSE-----------------------------------------------------
## good_mileage_displ_low_cyl_4  = if_else(cyl == 4 & displ < mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_low_cyl_6  = if_else(cyl == 6 & displ < mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_low_cyl_8  = if_else(cyl == 8 & displ < mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_high_cyl_4 = if_else(cyl == 4 & displ > mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_high_cyl_6 = if_else(cyl == 6 & displ > mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_high_cyl_8 = if_else(cyl == 8 & displ > mean(displ) & hwy > 30, 'yes', 'no')

## ----mpgfunc-------------------------------------------------------------
good_mileage <- function(cylinder = 4,
                         mpg_cutoff = 30,
                         displ_fun = mean,
                         displ_low = TRUE,
                         cls = 'compact') {
  
  if (displ_low == TRUE) {              # condition to check, if it holds,
    result <- mpg %>%                   # filter data given the arguments
      filter(cyl == cylinder,
             displ <= displ_fun(displ),
             hwy >= mpg_cutoff,
             class == cls)
  }
  else {                                # if the condition doesn't hold, filter 
    result <- mpg %>%                   # the data this way instead
      filter(cyl == cylinder,
             displ >= displ_fun(displ),
             hwy >= mpg_cutoff,
             class == cls)
  }
  
  result                                # return the object
}

## if (Math.random() < 0.5) {

## if x == 2:

## ----mpgfunc_demo--------------------------------------------------------
good_mileage(mpg_cutoff = 40)

good_mileage(cylinder = 8, mpg_cutoff = 15, displ_low = F, cls='suv')

## ----mpgfunc_extend, echo=1:6--------------------------------------------
good_mileage <- function(cylinder = 4,
                         mpg_cutoff = 30,
                         displ_fun = mean,
                         displ_low = TRUE,
                         cls = 'compact',
                         yr = 2008) {
  
  if (displ_low) {
    result = mpg %>%
    filter(cyl == cylinder,
           displ <= displ_fun(displ),
           hwy >= mpg_cutoff,
           class == cls,
           year == yr)
  }
  else {
    result = mpg %>%
    filter(cyl == cylinder,
           displ >= displ_fun(displ),
           hwy >= mpg_cutoff,
           class == cls,
           year == yr)
  }
  
  result
}

## ----mpgfunc_extend_demo-------------------------------------------------
good_mileage(cylinder = 8, 
             mpg_cutoff = 19, 
             displ_low = F, 
             cls='suv', 
             yr = 2008)

## ----lambda, eval=FALSE--------------------------------------------------
## apply(mtcars, 2, sd)
## apply(mtcars, 2, function(x) sd(x))

## ----lambda_ex-----------------------------------------------------------
# some variables have a mad = 0, and so return Inf, NaN
# apply(mtcars, 2, function(x) (x - median(x))/mad(x)) %>% 
#   head()

mtcars %>%
  map_df(function(x) (x - median(x))/mad(x))

## ----wf_ex1, eval=FALSE--------------------------------------------------
## log_sum <- function(a, b) {
##   ?
## }

## ----wf_ex1b, eval=FALSE-------------------------------------------------
## log_sum <- function(a, b) {
##   #
##   #
##   #
## 
##   if (?) {
##     stop('Your message here.')
##   }
##   else {
##     ?
##     return(your_log_sum_object)
##   }
## }

## ----wf_ex2, eval=FALSE--------------------------------------------------
## set.seed(123)  # so you get the exact same 'random' result
## x <- rnorm(10)
## if_else(x < 0, "negative", "positive")

## ----wf_ex2b, eval=FALSE-------------------------------------------------
## pos_neg <- function(?) {
##   ?
## }

