## ----iter_1, eval=FALSE--------------------------------------------------
## means1 = mean(df$x)
## means2 = mean(df$y)
## means3 = mean(df$z)
## means4 = mean(df$q)

## ----iter_loop_demo, eval=FALSE------------------------------------------
## for (column in c('x','y','z','q')) {
##   mean(df[[column]])
## }

## ----nyc_flights---------------------------------------------------------
weather = nycflights13::weather

for (column in c('temp','humid','wind_speed','precip')) {
  print(mean(weather[[column]], na.rm = TRUE))
}

## ----nyc_flights2--------------------------------------------------------
columns = c('temp', 'humid', 'wind_speed', 'precip')
nyc_means = rep(NA, length(columns))

for (i in seq_along(columns)) {
  column = columns[i]
  nyc_means[i] = mean(weather[[column]], na.rm = TRUE)
  
  # alternative without the initial first step
  # nyc_means[i] = mean(weather[[columns[i]]], na.rm = TRUE)  
}

nyc_means

## ----nyc_flights3--------------------------------------------------------
columns = c('temp', 'humid', 'wind_speed', 'visib', 'pressure')
nyc_means = rep(NA, length(columns))

for (i in seq_along(columns)) {
  nyc_means[i] = mean(weather[[columns[i]]], na.rm = TRUE)
}

nyc_means %>% round(2)

## ----nyc_flights4--------------------------------------------------------
columns = c('temp','humid','wind_speed', 'visib', 'pressure')
nyc_means = numeric()

for (i in seq_along(columns)) {
  nyc_means[i] = mean(weather[[columns[i]]], na.rm = TRUE)
}

nyc_means %>% round(2)

## ----loop_timing, eval=FALSE, echo=FALSE---------------------------------
## X = matrix(rnorm(1000000), ncol=10000)
## means1 = rep(NA, ncol(X))
## means2 = c()
## 
## loop1 = function(X) {
##   for (i in 1:ncol(X)) {
##     means1[i] = mean(X[,i])
##   }
## }
## 
## loop2 = function(X) {
##   for (i in 1:ncol(X)) {
##     means2[i] = mean(X[,i])
##   }
## }
## 
## microbenchmark::microbenchmark(loop1(X), loop2(X))

## ----nyc_flights_while---------------------------------------------------
columns = c('temp','humid','wind_speed', 'visib', 'pressure')
nyc_means = c()

i = 1
while (i <= length(columns)) {
  nyc_means[i] = mean(weather[ ,columns[i]], na.rm = TRUE)
  i = i + 1
}

nyc_means %>% round(2)

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

## ----lapply_example------------------------------------------------------
x = list('aba', 'abb', 'abc', 'abd', 'abe')

lapply(x, str_remove, pattern = 'ab')

## ----sapply_example------------------------------------------------------
sapply(x, str_remove, pattern = 'ab')

## ----purr_example, eval=TRUE, echo=-(1:2)--------------------------------
detach(package:maps)
map = purrr::map  # could not remove conflict with something, nor able to debug
x = list(1:3, 4:6, 7:9)
map(x, sum)

## ----purr_example2, eval=-1----------------------------------------------
mtcars %>% 
  map(scale)    # returns a list, not shown

mtcars %>% 
  map_df(scale) # returns a df

mtcars %>% 
  map_dbl(sum)  # returns a numeric (double) vector of column sums

diamonds %>% 
  map_at(c('carat', 'depth', 'price'), 
         function(x) as.integer(x > median(x))) %>% 
  as_data_frame()

## ----purr_models---------------------------------------------------------
library(mgcv) # for gam
mtcars$cyl = factor(mtcars$cyl)
mod_lm = lm(mpg ~ wt, data=mtcars)
mod_poly = lm(mpg ~ poly(wt, 2), data=mtcars)
mod_inter = lm(mpg ~ wt*cyl, data=mtcars)
mod_gam = gam(mpg ~ s(wt), data=mtcars)
mod_gam_inter = gam(mpg ~ cyl + s(wt, by=cyl), data=mtcars)

model_list = list(mod_lm = mod_lm,
                  mod_poly = mod_poly,
                  mod_inter = mod_inter,
                  mod_gam = mod_gam,
                  mod_gam_inter = mod_gam_inter)

# lowest wins
model_list %>% 
  map_dbl(AIC) %>% 
  sort()

# highest wins
model_list %>% 
  map_dbl(function(x) if_else(inherits(x, 'gam'),
                              summary(x)$r.sq,
                              summary(x)$adj)
          ) %>% 
  sort(decreasing = TRUE)

## ----purr_models2--------------------------------------------------------
model_list %>% 
  map_df(function(x) if_else(inherits(x, 'gam'),
                             summary(x)$r.sq,
                             summary(x)$adj)
  ) %>% 
  gather(key = 'model', value = `Adj. Rsq`) %>% 
  arrange(desc(`Adj. Rsq`)) %>% 
  mutate(model = factor(model, levels = model)) %>% # sigh
  ggplot(aes(x=model, y=`Adj. Rsq`)) + 
  geom_point(aes(color=model), size=10, show.legend = F)

## ----purr_models3--------------------------------------------------------
mod_rsq = 
  model_list %>% 
  map_df(function(x) if_else(inherits(x, 'gam'),
                             summary(x)$r.sq,
                             summary(x)$adj)
  ) %>% 
  gather(key = 'model', value='Rsq')

mod_aic =
  map_df(model_list, AIC) %>% 
  gather(key='model', value='AIC')

left_join(mod_rsq, mod_aic) %>% 
  arrange(AIC) %>% 
  mutate(model = factor(model, levels = model)) %>%
  gather(key = 'measure', value='value', -model) %>% 
  ggplot(aes(x=model, y=value)) + 
  geom_point(aes(color=model), size=10, show.legend = F) +
  facet_wrap(~measure, scales = 'free')

## ----list_data_frame-----------------------------------------------------
mtcars2 = as.matrix(mtcars)
mtcars2[sample(1:length(mtcars2),50)] = NA   # add some missing data
mtcars2 = data.frame(mtcars2) %>%
  rownames_to_column(var = 'observation') %>% 
  as_data_frame()

head(mtcars2)

mtcars2 = mtcars2%>% 
  mutate(newvar = 
           pmap(., ~ data.frame(
             N = sum(!is.na(c(...))),
             Missing = sum(is.na(c(...)))
           ))
         )

## ----list_data_frame2----------------------------------------------------
mtcars2

mtcars2$newvar %>% head(3)

## ----list_data_frame3----------------------------------------------------
mtcars2 %>% 
  unnest(newvar)

## ----app_ex1, echo=1, eval=FALSE-----------------------------------------
## x = matrix(1:9, 3, 3)
## apply()

## ----app_ex2, echo=1, eval=FALSE-----------------------------------------
## x = list(1:3, 4:10, 11:100)
## lapply()
## sapply()

## ----purrr_ex3, echo=1:5, eval=FALSE-------------------------------------
## d = data_frame(
##   x = rnorm(100),
##   y = rnorm(100, 10, 2),
##   z = rnorm(100, 50, 10),
## )
## 
## d %>% map_df(mean)

