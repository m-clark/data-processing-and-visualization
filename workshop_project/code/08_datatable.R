## ----dterror, eval=TRUE--------------------------------------------------
library(data.table)
dt = data.table(x=sample(1:10, 6), g=1:3, y=runif(6))
class(dt)

## ----dtgeneric, eval=FALSE-----------------------------------------------
## x[i, j, by, keyby, with = TRUE, ...]

## ----dtrows, results='hold'----------------------------------------------
dt[2]
dt[2,]

## ----dtcols, results='hold'----------------------------------------------
dt[,x]
dt[,z := x+y]  # dt now has a new column
dt[,z]
dt[g>1, mean(z), by=g]
dt

## ----dtdropcol, eval=1:2, results='hold'---------------------------------
dt[,-y]             # creates negative values of y
dt[,-'y', with=F]   # drops y, but now needs quotes
dt[,y:=NULL]        # drops y, but this is just a base R approach
dt$y = NULL

## ----dtmemory------------------------------------------------------------
DT = data.table(A=5:1,B=letters[5:1])
DT2 = DT

## ----dtcopy--------------------------------------------------------------
DT2 = copy(DT)     

## ----dtgroup-------------------------------------------------------------
dt1 = dt2 = dt
dt[,sum(x,y), by=g]                  # sum of all x and y values
dt1[,newvar := sum(x), by=g]         # add new variable to the original data 
dt1

## ----dtgroup2, eval=1----------------------------------------------------
dt2[, list(mean_x = mean(x), sum_x = sum(x)), by = g==1]
dt2 %>% group_by(g==1) %>% summarise(mean_x=mean(x), sum_x=sum(x))

## ----dtjoin, eval=FALSE--------------------------------------------------
## dt1 = setkey(dt1, x)
## dt1[dt2]

## ----dtjoin_timing, cache=FALSE, echo=FALSE------------------------------
load('data/timing.RData')
timing_join %>% pander::pander(justify='lr')
# timing_join =
#   microbenchmark::microbenchmark(dt_join = dt1[dt2],
#                                  dplyr_join = left_join(dt1, dt2, by='x')) %>%
#   summary() %>%
#   select(expr, mean) %>%
#   mutate(mean=round(mean, 2)) %>%
#   rename(func=expr, `mean (microseconds)`=mean)

## ----dtsetkey, echo=1:6,eval=1:8-----------------------------------------
test_dt0 = data.table(x=rnorm(10000000), g = sample(letters, 10000000, replace=T))
test_dt1 = copy(test_dt0)
test_dt2 = setkey(test_dt1, g)

identical(test_dt0, test_dt1)
identical(test_dt1, test_dt2)

timing_group_by_1 %>% pander::pander(justify='lr')


# timing_group_by_1 = 
#   microbenchmark::microbenchmark(test_dt0 = test_dt0[,mean:=mean(x),by=g],
#                                  test_dt1 = test_dt1[,mean:=mean(x),by=g],
#                                  test_dt2 = test_dt2[,mean:=mean(x),by=g]) %>% 
#   summary() %>% 
#   select(expr, mean) %>% 
#   mutate(mean=round(mean, 2)) %>% 
#   rename(func=expr, `mean (milliseconds)`=mean)



## ----dtcharacters, echo=2:3, eval=2:4------------------------------------
set.seed(1234)
lets_1 = sample(letters[1:14], 1000, replace=T)
lets_1 %chin% letters[13:26] %>% head(10)
timing_chmatch %>% pander::pander(justify='lr')

# library(stringr); library(stringi)
# timing_chmatch =
#   microbenchmark::microbenchmark(
#   dt = lets_1 %chin% letters[13:26],
#   stringi= stri_detect_regex(lets_1, paste(letters[13:26], collapse='|')),
#   stringr= str_detect(lets_1, paste(letters[13:26], collapse='|'))
# ) %>%
#   summary() %>%
#   select(expr, mean) %>%
#   mutate(mean=round(mean, 2)) %>%
#   rename(func=expr, `mean (microseconds)`=mean)



## ----dtfread, eval=FALSE-------------------------------------------------
## fread('data/cars.csv')

## ----dtfread_timing, echo=FALSE------------------------------------------
# library(readr)
# timing_read =
#   microbenchmark::microbenchmark(
#   dt = fread('data/cars.csv'),
#   readr = read_csv('data/cars.csv')
# ) %>%
#   summary() %>%
#   select(expr, mean) %>%
#   mutate(mean=round(mean, 2)) %>%
#   rename(func=expr, `mean (microseconds)`=mean)

timing_read %>% pander::pander(justify='lr')

## ----dttimings, eval=TRUE, echo=FALSE------------------------------------
as.data.table(sapply(timing_group_by_big, `[[`, "elapsed"), keep.rownames = TRUE)[,.(fun = V1, elapsed = V2)][order(-elapsed)]

## ----dtpipebad, eval=F---------------------------------------------------
## mydt[,newvar:=mean(x),][,newvar2:=sum(newvar), by=group][,-'y', with=FALSE]
## mydt[,newvar:=mean(x),
##   ][,newvar2:=sum(newvar), by=group
##   ][,-'y', with=FALSE
##   ]

## ----dtpipebad2, eval=F--------------------------------------------------
## mydt[,newvar:=mean(x),] %>%
##   .[,newvar2:=sum(newvar), by=group] %>%
##   .[,-'y', with=FALSE]

## ----dtplyr, echo=FALSE, eval=TRUE, cache=F------------------------------
# something weird with n() function in chunk; saved out
# detach(package:data.table); library(dplyr); library(dtplyr)
# set.seed(123)
# n = 5e7  possible conflict with n()?
# k = 5e5
# x = runif(5e7)
# grp = sample(k, 5e7, TRUE)
# 
# dtplyrtest = system.time({
#     dt = tbl_dt(data.frame(x, grp))
#     r.dplyr = summarise(group_by(dt, grp), sum(x), n())
# })
# timingsdtplyr = append(timing[6:7], list(dtplyr=dtplyrtest))
# save(timingsdtplyr, 'data/timingsdtplyr.RData')
load('data/timingsdtplyr.RData')
data_frame(package=names(timingsdtplyr)) %>% 
  mutate(timing = sapply(timingsdtplyr, `[[`, "elapsed")) %>% 
  arrange(desc(timing)) %>% 
  pander::pander()

## ---- echo=FALSE, eval=FALSE---------------------------------------------
## mydt_a = mydt[y=='a',]

## ---- echo=FALSE, eval=FALSE---------------------------------------------
## mydt_grpsum = mydt[,sum(z), by=x]
## mydt_grpsum = mydt[,list(sumz=sum(z)), by=x]

