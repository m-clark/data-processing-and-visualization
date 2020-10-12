## ----ml-criticism-setup, include=FALSE, eval=TRUE, cache=FALSE-------------------------------------



## ----loss, out.width='50%', echo=FALSE-------------------------------------------------------------
knitr::include_graphics('img/learningcurve.svg')


## ----bias-var, out.width='50%', echo=FALSE---------------------------------------------------------
knitr::include_graphics('img/biasvar2.svg')


## ----cross-validation, out.width='50%', echo=FALSE-------------------------------------------------
knitr::include_graphics('img/kfold.svg')


## ----kfoldcv---------------------------------------------------------------------------------------
# install.packages(tidymodels)  # if needed
library(tidymodels)  

load('data/world_happiness.RData')

set.seed(1212)

# specify the model
happy_base_spec = linear_reg() %>%
  set_engine(engine = "lm")

# by default 10-folds
happy_folds = vfold_cv(happy)

library(tune)

happy_base_results = fit_resamples(
  happy_base_spec,
  happiness_score ~ democratic_quality + generosity + log_gdp_per_capita,
  happy_folds,
  control = control_resamples(save_pred = TRUE)
)

cv_res = happy_base_results %>%
  collect_metrics()


## ----kfoldres-print, echo = FALSE------------------------------------------------------------------
cv_res %>% 
  kable_df()


## ----optim-vis, echo=FALSE, out.width='50%'--------------------------------------------------------
knitr::include_graphics('img/opt_vis_rad.gif')


## ----regLM-----------------------------------------------------------------------------------------
library(tidymodels)

happy_prepped = happy %>% 
  select(-country, -gini_index_world_bank_estimate, -dystopia_residual) %>% 
  recipe(happiness_score ~ .) %>% 
  step_scale(everything()) %>% 
  step_naomit(happiness_score) %>%
  prep() %>% 
  bake(happy) 

happy_folds = happy_prepped %>% 
  drop_na() %>%
  vfold_cv() 

library(tune)

happy_regLM_spec = linear_reg(penalty = 1e-3, mixture = .5) %>%
  set_engine(engine = "glmnet")

happy_regLM_results = fit_resamples(
  happy_regLM_spec,
  happiness_score ~ .,
  happy_folds,
  control = control_resamples(save_pred = TRUE)
)

cv_regLM_res = happy_regLM_results %>%
  collect_metrics()


## ----regLM-display, echo=FALSE---------------------------------------------------------------------
kable_df(cv_regLM_res)


## ----regLM-tune-data-split-------------------------------------------------------------------------
# removing some variables with lots of missing values
happy_split = happy %>% 
  select(-country, -gini_index_world_bank_estimate, -dystopia_residual) %>% 
  initial_split(prop = 0.75)

happy_train = training(happy_split)
happy_test  = testing(happy_split)


## ----regLM-tune-prep-------------------------------------------------------------------------------
happy_prepped = happy_train %>% 
  recipe(happiness_score ~ .) %>% 
  step_knnimpute(everything()) %>%    # impute missing values
  step_center(everything()) %>%       # standardize
  step_scale(everything()) %>%        # standardize
  prep()                              # prepare for other uses

happy_test_normalized <- bake(happy_prepped, new_data = happy_test, everything())

happy_folds = happy_prepped %>% 
  bake(happy) %>% 
  vfold_cv() 

# now we are indicating we don't know the value to place
happy_regLM_spec = linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine(engine = "glmnet")


## ----regLM-tune-model------------------------------------------------------------------------------
grid_search = expand_grid(
  penalty = exp(seq(-4, -.25, length.out = 10)),
  mixture = seq(0, 1, length.out = 10)
)

regLM_tune = tune_grid(
  happy_prepped,
  model = happy_regLM_spec,
  resamples = happy_folds,
  grid = grid_search
)

autoplot(regLM_tune, metric = "rmse") + geom_smooth(se = FALSE)

best = show_best(regLM_tune, metric = "rmse", maximize = FALSE, n = 1) # we want to minimize rmse

best


## ----regLM-tune-model-vis, echo=FALSE, eval=FALSE--------------------------------------------------
## regLM_tune$.metrics %>%
##   bind_rows() %>%
##   filter(.estimate < .5) %>%
##   ggplot(aes(penalty, mixture)) +
##   geom_point(aes(size=.estimate), color = '#990024', alpha = .15, position = position_jitter()) +
##   # geom_smooth(color = '#ff550080', se = F) +
##   scale_size_continuous(range = c(1,10), breaks = c(.5,1,2,5,10)) +
##   theme_clean()


## ----regLM-tune-refit-test-------------------------------------------------------------------------
# for technical reasons, only mixture is passed to the model; see https://github.com/tidymodels/parsnip/issues/215
tuned_model = linear_reg(penalty = best$penalty, mixture = best$mixture) %>%
  set_engine(engine = "glmnet") %>% 
  fit(happiness_score ~ ., data = juice(happy_prepped))

test_predictions = predict(tuned_model, new_data = happy_test_normalized)

rmse = yardstick::rmse(
  data = bind_cols(test_predictions, happy_test_normalized),
  truth = happiness_score,
  estimate = .pred
)

rsq = yardstick::rsq(
  data = bind_cols(test_predictions, happy_test_normalized),
  truth = happiness_score,
  estimate = .pred
)


## ----regLM-tune-results, echo=FALSE----------------------------------------------------------------
bind_rows(rmse, rsq) %>% 
  kable_df()


## ----decision-tree, out.width='25%', echo=FALSE----------------------------------------------------
knitr::include_graphics('img/tree1.png')


## ----rf-demo---------------------------------------------------------------------------------------
happy_rf_spec = rand_forest(mode = 'regression', mtry = 6) %>%
  set_engine(engine = "ranger")

happy_rf_results = fit_resamples(
  happy_rf_spec,
  happiness_score ~ .,
  happy_folds
)

cv_rf_res = happy_rf_results %>%
  collect_metrics()


## ----rf-display, echo=FALSE------------------------------------------------------------------------
kable_df(cv_rf_res)


## ----rf-tune---------------------------------------------------------------------------------------
grid_search = expand.grid(
  mtry = c(3, 5, ncol(happy_train)-1),  # up to total number of predictors 
  min_n = c(1, 5, 10)
)

happy_rf_spec = rand_forest(mode = 'regression',
                            mtry = tune(),
                            min_n = tune()) %>% 
  set_engine(engine = "ranger")

rf_tune = tune_grid(
  happy_prepped,
  model = happy_rf_spec,
  resamples = happy_folds,
  grid = grid_search
)

autoplot(rf_tune, metric = "rmse")

best = show_best(rf_tune, metric = "rmse", maximize = FALSE, n = 1) # we want to minimize rmse

best


## ----rf-tune-refit-test----------------------------------------------------------------------------
tuned_model = rand_forest(mode = 'regression', mtry = best$mtry, min_n = best$min_n) %>%
  set_engine(engine = "ranger") %>% 
  fit(happiness_score ~ ., data = juice(happy_prepped))

test_predictions = predict(tuned_model, new_data = happy_test_normalized)

rmse = yardstick::rmse(
  data = bind_cols(test_predictions, happy_test_normalized),
  truth = happiness_score,
  estimate = .pred
)

rsq = yardstick::rsq(
  data = bind_cols(test_predictions, happy_test_normalized),
  truth = happiness_score,
  estimate = .pred
)


## ----rf-tune-result, echo=FALSE--------------------------------------------------------------------
bind_rows(rmse, rsq) %>% 
  kable_df()


## ----nn, out.width='50%', echo=FALSE---------------------------------------------------------------
knitr::include_graphics('img/nnet.png')


## ----nn-demo, cache=TRUE, echo = -1, eval=FALSE----------------------------------------------------
## # bookdown consistently fails here with uninformative error message, so just loading results separately.
## happy_nn_spec = mlp(
##   mode = 'regression',
##   hidden_units = 1000,
##   epochs = 500,
##   activation = 'linear'
## ) %>%
##   set_engine(engine = "keras")
## 
## happy_nn_results = fit_resamples(
##   happy_nn_spec,
##   happiness_score ~ .,
##   happy_folds,
##   control = control_resamples(save_pred = TRUE,
##                               verbose   = FALSE,
##                               allow_par = TRUE)
## )


## ----nn-display, echo=FALSE------------------------------------------------------------------------
load('data/other/happy_nn_results.RData')
cv_nn_res = happy_nn_results %>%
  collect_metrics()

cv_nn_res %>% 
  kable_df()


## ----resnet, out.width='50%', echo=FALSE-----------------------------------------------------------
knitr::include_graphics('img/resnet.png')


## ----ex1-ml, eval=FALSE----------------------------------------------------------------------------
## # run these if needed to load data and install the package
## # load('data/google_apps.RData')
## # install.packages('ranger')
## 
## google_for_mod = google_apps %>%
##   select(avg_sentiment_polarity, rating, type,installs, reviews, size_in_MB, category) %>%
##   drop_na()
## 
## google_split = google_for_mod %>%
##   initial_split(prop = 0.75)
## 
## google_train = training(google_split)
## google_test  = testing(google_split)
## 
## ga_rf_results = rand_forest(mode = 'regression', mtry = 2, trees = 1000) %>%
##   set_engine(engine = "ranger") %>%
##   fit(
##     rating ~ ?,
##     google_train
##   )
## 
## test_predictions = predict(ga_rf_results, new_data = google_test)
## 
## rmse = yardstick::rmse(
##   data = bind_cols(test_predictions, google_test),
##   truth = rating,
##   estimate = .pred
## )
## 
## rsq = yardstick::rsq(
##   data = bind_cols(test_predictions, google_test),
##   truth = rating,
##   estimate = .pred
## )
## 
## bind_rows(
##   rmse,
##   rsq
## )


## ----ml-ex-2, eval=FALSE---------------------------------------------------------------------------
## grid_search = expand.grid(
##   hidden_units = c(25, 50),
##   penalty = exp(seq(-4, -.25, length.out = 5))
## )
## 
## happy_nn_spec = mlp(mode = 'regression',
##                     penalty = tune(),
##                     hidden_units = tune()) %>%
##   set_engine(engine = "nnet")
## 
## nn_tune = tune_grid(
##   happy_prepped,                  # from previous examples, see tuning for regularized regression
##   model = happy_nn_spec,
##   resamples = happy_folds,        # from previous examples, see tuning for regularized regression
##   grid = grid_search
## )
## 
## show_best(nn_tune, metric = "rmse", maximize = FALSE, n = 1)

