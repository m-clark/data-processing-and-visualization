## ----read.csv, eval=FALSE--------------------------------------------------------------------------
## mydata = read.csv('data/myfile.csv')


## ----write.csv, eval=FALSE-------------------------------------------------------------------------
## write.csv(mydata, file = 'data/newfile.csv')


## ----double_colon, eval=FALSE----------------------------------------------------------------------
## readr::read_csv('fileloc/filename.csv')


## ----rsave, eval = FALSE---------------------------------------------------------------------------
## save(object1, object2, file = 'data/myfile.RData')


## ----rload, eval = FALSE---------------------------------------------------------------------------
## load('data/myfile.RData')


## ----rdatasets-------------------------------------------------------------------------------------
head(iris)


## ----data-load-------------------------------------------------------------------------------------
data(mcycle, package = 'MASS') # loads data without loading package
head(mcycle)

