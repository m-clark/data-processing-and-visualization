## ----func_ex, eval=FALSE----------------------------------------------------------------
## mean(myvar)
## sd(myvar)
## sum(is.na(myvar))


## ----func_ex2---------------------------------------------------------------------------
my_summary <- function(x) {
  data.frame(
    mean = mean(x),
    sd = sd(x),
    N_missing = sum(is.na(x))
  )
}


## ----func_ex3, echo=-1------------------------------------------------------------------
mtcars = datasets::mtcars  # to undo previous factors
my_summary(mtcars$mpg)


## ----func_ex_missing--------------------------------------------------------------------
load('data/gapminder.RData')
my_summary(gapminder_2019$lifeExp)


## ----func_ex3ext------------------------------------------------------------------------
my_summary <- function(x) {
  data.frame(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    N_missing = sum(is.na(x))
  )
}


my_summary_na <- function(x, remove_na = TRUE) {
  data.frame(
    mean = mean(x, na.rm = remove_na),
    sd = sd(x, na.rm = remove_na),
    N_missing = sum(is.na(x))
  )
}


my_summary(gapminder_2019$lifeExp)

my_summary_na(gapminder_2019$lifeExp, remove_na = FALSE)


## ----func_ex4---------------------------------------------------------------------------
my_summary <- function(x) {
  # create an arbitrarily named object with the summary information
  summary_data = data.frame(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    N_total = length(x),
    N_missing = sum(is.na(x))
  )
  
  # return the result!
  summary_data       
}


## ----func_ex5---------------------------------------------------------------------------
my_summary(gapminder_2019$lifeExp)


## ----func_ex6---------------------------------------------------------------------------
gapminder_2019 %>% 
  select_if(is.numeric) %>% 
  map_dfr(my_summary, .id = 'variable')


## ----func_ex_redux, eval=FALSE----------------------------------------------------------
## mean(myvar)
## sd(myvar)
## sum(is.na(myvar))


## ----func_ex_redux_redux, eval=FALSE----------------------------------------------------
## test_fun <- function(myvar) {
##   mean(myvar)
##   sd(myvar)
##   sum(is.na(myvar))
## }


## ----nothing_function-------------------------------------------------------------------
two <- function() {
  2
}

two()


## ----function_function------------------------------------------------------------------
center <- function(type) {
  if (type == 'mean') {
    mean
  } else {
    median
  }
}

center(type = 'mean')

myfun = center(type = 'mean')

myfun(1:3)


## ----default_arg------------------------------------------------------------------------
hi <- function(name = 'Beyoncé') {
  paste0('Hi ', name, '!')
}

hi()
hi(name = 'Jay-Z')


## ----source_func, eval=FALSE------------------------------------------------------------
## source('my_functions/awesome_func.R')


## ----dry, eval=FALSE--------------------------------------------------------------------
## good_mileage_displ_low_cyl_4  = if_else(cyl == 4 & displ < mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_low_cyl_6  = if_else(cyl == 6 & displ < mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_low_cyl_8  = if_else(cyl == 8 & displ < mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_high_cyl_4 = if_else(cyl == 4 & displ > mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_high_cyl_6 = if_else(cyl == 6 & displ > mean(displ) & hwy > 30, 'yes', 'no')
## good_mileage_displ_high_cyl_8 = if_else(cyl == 8 & displ > mean(displ) & hwy > 30, 'yes', 'no')


## ----mpgfunc----------------------------------------------------------------------------

good_mileage <- function(
  cylinder = 4,
  mpg_cutoff = 30,
  displ_fun = mean,
  displ_low = TRUE,
  cls = 'compact'
) {
  
  if (displ_low == TRUE) {              # condition to check, if it holds,
    result <- mpg %>%                   # filter data given the arguments
      filter(cyl == cylinder,
             displ <= displ_fun(displ),
             hwy >= mpg_cutoff,
             class == cls)
  } 
  else {                                # if the condition doesn't hold, filter 
    result <- mpg %>%                   # the data this way instead
      filter(
        cyl == cylinder,
        displ >= displ_fun(displ),      # the only change is here
        hwy >= mpg_cutoff,
        class == cls
      )
  }
  
  result                                # return the object
}


## if (Math.random() < 0.5) {

## console.log("You got Heads!");

## } else {

## console.log("You got Tails!");

## }


## if x == 2:

##   print(x)

## else:

##   print(x*x)


## ----mpgfunc_demo-----------------------------------------------------------------------
good_mileage(mpg_cutoff = 40)

good_mileage(
  cylinder = 8,
  mpg_cutoff = 15,
  displ_low = F,
  cls = 'suv'
)


## ----mpgfunc_extend, echo=1:6-----------------------------------------------------------
good_mileage <- function(
  cylinder = 4,
  mpg_cutoff = 30,
  displ_fun = mean,
  displ_low = TRUE,
  cls = 'compact',
  yr = 2008
) {
  
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


## ----mpgfunc_extend_demo----------------------------------------------------------------
good_mileage(
  cylinder = 8,
  mpg_cutoff = 19,
  displ_low = F,
  cls = 'suv',
  yr = 2008
)


## ----lambda, eval=FALSE-----------------------------------------------------------------
## apply(mtcars, 2, sd)
## apply(mtcars, 2, function(x) x/2)


## ----lambda_ex--------------------------------------------------------------------------
# some variables have a mad = 0, and so return Inf (x/0) or NaN (0/0)
# apply(mtcars, 2, function(x) (x - median(x))/mad(x)) %>% 
#   head()

mtcars %>%
  map_df(function(x) (x - median(x))/mad(x))


## ----wf_ex1, eval=FALSE-----------------------------------------------------------------
## log_sum <- function(a, b) {
##   ?
## }


## ----wf_ex1b, eval=FALSE----------------------------------------------------------------
## log_sum <- function(a, b) {
##   #
##   #
##   #
## 
##   if (?) {
##     stop('Your message here.')
##   } else {
##     ?
##     return(your_log_sum_results)
##   }
## }


## ----wf_ex2, eval=FALSE-----------------------------------------------------------------
## set.seed(123)  # so you get the exact same 'random' result
## x <- rnorm(10)
## if_else(x < 0, "negative", "positive")


## ----wf_ex2b, eval=FALSE----------------------------------------------------------------
## pos_neg <- function(?) {
##   ?
## }

