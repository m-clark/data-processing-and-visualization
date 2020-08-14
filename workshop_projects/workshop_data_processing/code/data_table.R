## ----dterror, eval=TRUE-----------------------------------------------------------------------
library(data.table)
dt = data.table(x = sample(1:10, 6),
                g = 1:3,
                y = runif(6))
class(dt)


## ----dtgeneric, eval=FALSE--------------------------------------------------------------------
## x[i, j, by, keyby, with = TRUE, ...]


## ----dtrows, results='hold'-------------------------------------------------------------------
dt[2]    # rows! not columns as with standard data.frame
dt[2,]


## ----dtcols, results='hold'-------------------------------------------------------------------
dt[,x]
dt[,z := x+y]  # dt now has a new column
dt[,z]
dt[g > 1, mean(z), by = g]
dt


## ----dtdropcol, eval=1:2, results='hold'------------------------------------------------------
dt[, -y]             # creates negative values of y
dt[, -'y', with = F] # drops y, but now needs quotes
dt[, y := NULL]      # drops y, but this is just a base R approach
dt$y = NULL


## ----dtmemory---------------------------------------------------------------------------------
DT = data.table(A = 5:1, B = letters[5:1])
DT2 = DT
DT3 = copy(DT)  


## ----dtnocopy---------------------------------------------------------------------------------
DT2[,q:=1]
DT


## ----dtcopy-----------------------------------------------------------------------------------
DT3 


## ----dtgroup----------------------------------------------------------------------------------
dt1 = dt2 = dt
dt[, sum(x, y), by = g]                # sum of all x and y values
dt1[, mysum := sum(x), by = g]         # add new variable to the original data
dt1


## ----dtgroup2, eval=1-------------------------------------------------------------------------
dt2[, list(mean_x = mean(x), sum_x = sum(x)), by = g == 1]
dt2 %>% 
  group_by(g == 1) %>% 
  summarise(mean_x = mean(x), sum_x = sum(x))


## ----dtjoin, eval=FALSE-----------------------------------------------------------------------
## dt1 = setkey(dt1, x)
## dt1[dt2]
## 
## dt1_df = dt2_df = as.data.frame(dt1)
## left_join(dt1_df, dt2_df, by = 'x')


## ----dtjoin_timing, echo=FALSE----------------------------------------------------------------
# load('data/timing.RData')
# timing_join %>% 
#   kable_df(justify='lr')
dt1 = setkey(dt1, x)
dt1_df = dt2_df = as.data.frame(dt1)
# dt1[dt2]
# timing_join =
  microbenchmark::microbenchmark(dt_join = dt1[dt2],
                                 dplyr_join = left_join(dt1_df, dt2_df, by='x')) %>%
  summary() %>%
  select(expr, mean) %>%
  mutate(mean = round(mean, 2)) %>%
  rename(func = expr, `mean (microseconds)` = mean) %>%
  kable_df(justify = 'lr')


## ----dtsetkey, echo=1:10----------------------------------------------------------------------
test_dt0 = data.table(x = rnorm(10000000),
                      g = sample(letters, 10000000, replace = T))
test_dt1 = copy(test_dt0)
test_dt2 = setkey(test_dt1, g)

identical(test_dt0, test_dt1)
identical(test_dt1, test_dt2)

test_dt0 = test_dt0[, mean := mean(x), by = g]
test_dt1 = test_dt1[, mean := mean(x), by = g]
test_dt2 = test_dt2[, mean := mean(x), by = g]

# timing_group_by_1 %>% kable_df(justify='lr')


timing_group_by_1 =
  microbenchmark::microbenchmark(
    test_dt0 = test_dt0[, mean := mean(x), by = g],
    test_dt1 = test_dt1[, mean := mean(x), by = g],
    test_dt2 = test_dt2[, mean := mean(x), by = g],
    times = 25
  ) %>%
  summary() %>%
  select(expr, mean) %>%
  mutate(mean = round(mean, 2)) %>%
  rename(func = expr, `mean (milliseconds)` = mean)

timing_group_by_1 %>% 
  kable_df(justify = 'lr')


## ----dtcharacters, echo=2:6, eval=2:8---------------------------------------------------------
set.seed(1234)
lets_1 = sample(letters[1:14], 1000, replace=T)

lets_1 %chin% letters[13:26] %>% head(10)

# stri_detect_regex(lets_1, paste(letters[13:26], collapse='|'))
# str_detect(lets_1, paste(letters[13:26], collapse='|'))

# timing_chmatch %>% kable_df(justify='lr')
# 
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




## ----dtfread, eval=FALSE----------------------------------------------------------------------
## fread('data/cars.csv')


## ----dtfread_timing, echo=FALSE---------------------------------------------------------------
library(readr)
timing_read =
  microbenchmark::microbenchmark(
  dt = fread('data/cars.csv'),
  readr = read_csv('data/cars.csv')
) %>%
  summary() %>%
  select(expr, mean) %>%
  mutate(mean=round(mean, 2)) %>%
  rename(func=expr, `mean (microseconds)`=mean)

timing_read %>% kable_df(justify='lr')


## ----dttimings_big, eval=FALSE, echo=FALSE----------------------------------------------------
## library(dplyr)
## library(data.table)
## set.seed(123)
## n = 5e7
## k = 5e5
## x = runif(n)
## grp = sample(k, n, TRUE)
## 
## timing_group_by_big = list()
## 
## # sapply
## timing_group_by_big[["sapply"]] = system.time({
##     lt = split(x, grp)
##     r.sapply = sapply(lt, function(x) list(sum(x), length(x)), simplify = FALSE)
## })
## 
## # lapply
## timing_group_by_big[["lapply"]] = system.time({
##     lt = split(x, grp)
##     r.lapply = lapply(lt, function(x) list(sum(x), length(x)))
## })
## 
## # tapply
## timing_group_by_big[["tapply"]] = system.time(
##     r.tapply <- tapply(x, list(grp), function(x) list(sum(x), length(x)))
## )
## 
## # by
## timing_group_by_big[["by"]] = system.time(
##     r.by <- by(x, list(grp), function(x) list(sum(x), length(x)), simplify = FALSE)
## )
## 
## # aggregate
## timing_group_by_big[["aggregate"]] = system.time(
##     r.aggregate <- aggregate(x, list(grp), function(x) list(sum(x), length(x)), simplify = FALSE)
## )
## 
## # dplyr
## timing_group_by_big[["dplyr"]] = system.time({
##     df = data_frame(x, grp)
##     r.dplyr = summarise(group_by(df, grp), sum(x), n())
## })
## 
## # data.table
## timing_group_by_big[["data.table"]] = system.time({
##     dt = setnames(setDT(list(x, grp)), c("x","grp"))
##     r.data.table = dt[, .(sum(x), .N), grp]
## })
## 
## save(timing_group_by_big, file = 'data/timing_group_by_big.RData')


## ----dttimings, eval=TRUE, echo=FALSE---------------------------------------------------------
load('data/timing_group_by_big.RData')
as.data.table(sapply(timing_group_by_big, `[[`, "elapsed"), keep.rownames = TRUE)[, .(fun = V1, elapsed = V2)][order(-elapsed)] %>% 
  kable_df()


## ----dtpipebad, eval=F------------------------------------------------------------------------
## mydt[, newvar := mean(x), ][, newvar2 := sum(newvar), by = group][, -'y', with = FALSE]
## mydt[, newvar := mean(x),
##      ][, newvar2 := sum(newvar), by = group
##        ][,-'y', with=FALSE]


## ----dtpipebad2, eval=F-----------------------------------------------------------------------
## mydt[, newvar := mean(x), ] %>%
##   .[, newvar2 := sum(newvar), by = group] %>%
##   .[, -'y', with = FALSE]


## ----dtplyr, echo=FALSE, eval=TRUE------------------------------------------------------------
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


# data(flights, 'nycflights13')
# head(flights)
# 
# library(tidyverse)
# library(dtplyr)
# library(data.table)
# library(tidyfast)
# 
# 
# flights_dtp = lazy_dt(flights)
# 
# flights_dt = data.table(flights)
# 
# library(microbenchmark)
# 
# test = microbenchmark::microbenchmark(
#   dplyr      = count(flights, arr_time),
#   dtplyr     = as_tibble(count(flights_dtp, arr_time)),
#   tidyfast   = dt_count(flights_dt, arr_time),
#   data.table = flights_dt[, .(n = .N), by = arr_time],
#   times = 500
# )
# 
# test_table = as.data.frame(print(test))
# colnames(test_table)[c(1, 5)] = c('package', 'timing')  # knitr was having issues with the names as they expr and median are functions
# test_table = test_table[c('package', 'timing')]
# save(test, test_table, file = 'data/timingsdtplyr.RData')


load('data/timingsdtplyr.RData')

test_table %>%
  arrange(desc(timing)) %>%
  kable_df() %>% 
  kableExtra::add_footnote('Median time in milliseconds to do a count of arr_time on nycflights::flights')

ggplot(test, aes(expr, time, color = expr, fill = expr)) +
  geom_violin(alpha = .6)  +
  # ggbeeswarm::geom_beeswarm(dodge.width = 5, alpha = .6)  +
  visibly::theme_clean()


## import pandas as pd

## 
## flights = r.flights

## 
## flights.set_index(["arr_time", 'year']).count(level="arr_time")

## 
## def test():

##   flights.set_index(["arr_time", 'year']).count(level="arr_time")

## 
## test()

## import timeit

## 
## timeit.Timer.timeit() # see documentation

## 
## test_result = timeit.timeit(stmt="test()", setup="from __main__ import test", number=100)

## 
## # result is in seconds for the total number of runs

## test_result/100  # per run

## test_result/100*1000  # in milliseconds


## ----dt-ex0, eval=FALSE-----------------------------------------------------------------------
## mydt = data.table(
##   expand.grid(x = 1:3,
##               y = c('a', 'b', 'c')),
##   z = sample(1:20, 9)
## )


## ---- echo=FALSE, eval=FALSE------------------------------------------------------------------
## mydt_a = mydt[y == 'a', ]


## ---- echo=FALSE, eval=FALSE------------------------------------------------------------------
## mydt_grpsum = mydt[, sum(z), by = x]
## mydt_grpsum = mydt[, list(sumz = sum(z)), by = x]

