## ----space, eval=FALSE---------------------------------------------------
## x=rnorm(10, mean=0,sd=1)
## 
## x = rnorm(10, mean = 0, sd = 1)

## ----vasetup, include=FALSE, eval=TRUE, cache=FALSE----------------------
knitr::opts_chunk$set(eval=FALSE)

## ----boolindex, eval=T---------------------------------------------------
x = c(-1, 2, 10, -5)
idx = x > 2
idx
x[idx]

## ----flexindex-----------------------------------------------------------
x[x > 2]
x[x != 3]
x[ifelse(x > 2 & x !=10, TRUE, FALSE)]
x[{y = idx; y}]
x[resid(lm(y ~ x)) > 0]

## ----loop----------------------------------------------------------------
for (i in 1:nrow(mydf)) {
  check = mydf$x[i] > 2
  if (check==TRUE) {
    mydf$y[i] = 'Yes'
  } 
  else {
    mydf$y[i] = 'No'
  }
}

## ----boolnoloop----------------------------------------------------------
mydf$y = 'No'
mydf$y[mydf$x > 2] = 'Yes'

## ----vecmatrixop---------------------------------------------------------
mymatrix_log = log(mymatrix)

## ----loopvsvec, eval=T---------------------------------------------------
mymatrix = matrix(runif(100), 10, 10)
identical(apply(mymatrix, 2, log), log(mymatrix))

library(microbenchmark)
microbenchmark(apply(mymatrix, 2, log),
               log(mymatrix))

## ----timings, echo=T-----------------------------------------------------
mymat = matrix(rnorm(100000), ncol=1000)

stdize <- function(x) {
  (x-mean(x)) / sd(x)
}

doubleloop = function() {
  for (i in 1:ncol(mymat)) {
    x = mymat[, i]
    for (j in 1:length(x)) {
      x[j] = (x[j] - mean(x)) / sd(x)
    }
  }
}


singleloop = function() {
  for (i in 1:ncol(mymat)) {
    x = mymat[, i]
    x = (x - mean(x)) / sd(x)
  }
}


library(parallel)
cl = makeCluster(8)
clusterExport(cl, c('stdize', 'mymat'))
doParallel::registerDoParallel(cl)

test = microbenchmark::microbenchmark(doubleloop=doubleloop(),
                                      singleloop=singleloop(), 
                                      apply=apply(mymat, 2, stdize), 
                                      parApply=parApply(cl, mymat, 2, stdize),
                                      vectorized=scale(mymat), times=25)
stopCluster(cl)
test

## ----vectorization_timings, echo=FALSE, eval=TRUE, cache=FALSE-----------
load('data/vectorization.RData'); library(microbenchmark); test

## ----regex_intro_ex------------------------------------------------------
string = c('r is the shiny', 'r is the shiny1', 'r shines brightly')
grepl(string, pattern='^r.*shiny[0-9]$')

## ----quick_regex_exercise, eval=FALSE------------------------------------
## grepl(c('apple', 'pear', 'banana'), pattern='a')
## grepl(c('apple', 'pear', 'banana'), pattern='^a')
## grepl(c('apple', 'pear', 'banana'), pattern='^a|a$')

## ----name_ex, eval=FALSE-------------------------------------------------
## lm(hwy ~ cyl, data=mpg)                 # hwy mileage predicted by number of cylinders
## summary(lm(hwy ~ cyl, data=mpg))        # the summary of that
## lm(hwy ~ cyl + displ + year, data=mpg)  # an extension of that

## ----vector_ex1, eval=FALSE----------------------------------------------
## ?

## ----vector_ex2, eval=FALSE----------------------------------------------
## x = matrix(rpois(100000, lambda = 5), ncol = 100)
## colSums(x)
## apply(x, 2, sum)
## 
## microbenchmark::microbenchmark(
##   cs = colSums(x),
##   app = apply(x, 2, sum)
## )

## ----regex_ex, eval=FALSE------------------------------------------------
## library(stringr)
## str_replace(state.name, pattern = ?, replacement = ?)

