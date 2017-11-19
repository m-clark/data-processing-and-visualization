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

## ----loopvsapply1, eval=FALSE--------------------------------------------
## for (i in 1:ncol(mydf)){
##   x = mydf[,i]
##   for (j in 1:length(x)){
##     x[j] = (x[j] - mean(x))/sd(x)
##   }
## }

## ----loopvsapply2, eval=F------------------------------------------------
## stdize <- function(x) {
##   (x-mean(x)) / sd(x)
## }
## 
## apply(mydf, 2, stdize)   # 1 for rows, 2 for columnwise application

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

## ----vectorapp_ex1, echo=1-----------------------------------------------
x = matrix(1:9, 3, 3)
apply()

## ----vectorapp_ex2, echo=1-----------------------------------------------
x = list(1:3, 4:6, 7:9)
lapply()
sapply()

