## ----model-criticism-setup, include=FALSE, eval=TRUE, cache=FALSE----------------------------------



## ----model-summary-rev-----------------------------------------------------------------------------
happy_model_base_sum


## ----happy-anova-----------------------------------------------------------------------------------
anova(happy_model_base)


## ----f-stat----------------------------------------------------------------------------------------
happy_model_base_sum$fstatistic
pf(309.6, 3, 407, lower.tail = FALSE)


## ----f-stat-redux----------------------------------------------------------------------------------
f_test = lm(happiness_score ~ generosity, happy)

summary(f_test)

pf(8.78, 1, 533, lower.tail = FALSE)


## ----null-vs-realistic-----------------------------------------------------------------------------
happy_model_null = lm(happiness_score ~ 1, data = model.frame(happy_model_base)) 
anova(happy_model_null, happy_model_base)


## ----happy-anova-rev-------------------------------------------------------------------------------
((527.27 - 160.65)/3) / (160.65/407)


## ----happy-r2--------------------------------------------------------------------------------------
happy_model_base_sum


## ----r2-calc---------------------------------------------------------------------------------------
366.62 / 527.27


## ----r2-as-cor-------------------------------------------------------------------------------------
predictions = predict(happy_model_base)
target = happy_model_base$model$happiness_score
rho = cor(predictions, target)
rho
rho^2


## ----confusionMatrix, echo=FALSE-------------------------------------------------------------------
t1 = tibble(
  ` ` = c('Predicted = 1', 'Predicted = 0'),
  `Observed = 1` = c(41, 16),
  `Observed = 0` = c(21, 13)
)

t2 = tibble(
  ` ` = c('Predicted = 1', 'Predicted = 0'),
  `Observed = 1` = c('A', 'C'),
  `Observed = 0` = c('B', 'D')
)

kable_df(list(t1, t2), align='cc') %>% 
  kable_styling(full_width = T, bootstrap_options = 'basic')


## ----confmat_values, echo=FALSE--------------------------------------------------------------------
A = 41
B = 21
C = 16
D = 13
# truepos_total = A + C
# pospred_total = A + B
# trueneg_total = B + D
# negpred_total = C + D
Total = A + B + C + D


## ----diag-plot-code, eval=F------------------------------------------------------------------------
## library(ggfortify)
## 
## autoplot(happy_model_base)


## ----diag-plot-show, echo=FALSE, fig.height=4------------------------------------------------------
library(ggfortify)

autoplot(
  happy_model_base,
  which = 1:2,
  colour = '#ff550040',
  smooth.colour = '#00aaff80',
  label.colour = 'gray50'
) +
  theme_clean()


## ----bad_fit, echo=FALSE---------------------------------------------------------------------------
set.seed(1234)
N = 500
x = rnorm(N)
mu = 2 + .25*x - .5*x^2 + rnorm(sd=abs(x), N)
y = mu

bad_fit = lm(y ~ x)

autoplot(
  bad_fit,
  which = 1:2,
  colour = '#ff550040',
  smooth.colour = '#00aaff80',
  label.colour = 'gray50'
) +
  theme_clean()


## ----pp_check_base, echo=F, fig.width=8------------------------------------------------------------
init = data.frame(
  predictions = predict(happy_model_base),
  target = happy_model_base$model$happiness_score
) 

p_dense = init %>% 
  pivot_longer(everything(), names_to = 'result') %>% 
  ggplot() +
  geom_density(aes(x = value, color = result, fill = result), alpha = .25) +
  scico::scale_color_scico_d(end = .5, aesthetics = c('color', 'fill')) +
  guides(fill = guide_legend(title = ""), color = 'none') +
  theme_clean() +
  theme(
    legend.title = element_text(size = 4),
    legend.key.size = unit(2, 'mm')
  )

p_scatter = init %>% 
  ggplot() +
  geom_point(aes(x = predictions, y = target), alpha = .25) +
  lims(x = c(2, 8), y = c(2, 8)) +
  theme_clean()

# still an issue with lack 
p_scatter +
  p_dense +
  plot_layout(widths = c(3, 1)) * theme_clean()
# gridExtra::grid.arrange(p_scatter, p_dense, layout_matrix = matrix(c(1,1,1,2,2), nrow = 1 ))


## ----train-test------------------------------------------------------------------------------------
library(tidymodels)

set.seed(12)

happy_split = initial_split(happy, prop = 0.75)

happy_train = training(happy_split)
happy_test  = testing(happy_split) %>% drop_na()

happy_model_train = lm(
  happiness_score ~ democratic_quality + generosity + log_gdp_per_capita,
  data = happy_train
)

predictions = predict(happy_model_train, newdata = happy_test)


## ----train-test-loss, echo=FALSE-------------------------------------------------------------------
pita = data.frame(
  observed_train = happy_model_train$model$happiness_score,
  predict_train  = happy_model_train$fitted.values
)

pita2 = data.frame(
  observed_test = happy_test$happiness_score,
  predict_test  = predictions
)


tibble(
  RMSE_train = yardstick::rmse(pita, truth = observed_train, estimate = predict_train)$.estimate,
  RMSE_test = yardstick::rmse(pita2, truth = observed_test, estimate = predict_test)$.estimate,
  `% increase` = rnd(100*(RMSE_test/RMSE_train - 1), 1)
  ) %>% 
  kable_df()


## ----happy-data-proc-------------------------------------------------------------------------------
happy_recipe = happy %>% 
  select(
    year,
    happiness_score,
    democratic_quality,
    generosity,
    healthy_life_expectancy_at_birth,
    log_gdp_per_capita
  ) %>% 
  recipe(happiness_score ~ . ) %>% 
  step_center(all_numeric(), -log_gdp_per_capita, -year) %>% 
  step_scale(all_numeric(), -log_gdp_per_capita, -year) %>% 
  step_knnimpute(all_numeric()) %>% 
  step_naomit(everything()) %>% 
  step_center(year, means = 2005) %>% 
  prep()
  
happy_processed = happy_recipe %>% bake(happy)


## ----happy-model-base------------------------------------------------------------------------------
happy_model_base = lm(
  happiness_score ~ democratic_quality + generosity + log_gdp_per_capita, 
  data = happy_processed
)

summary(happy_model_base)


## ----happy_model_more------------------------------------------------------------------------------
happy_model_more = lm(
  happiness_score ~ democratic_quality + generosity + log_gdp_per_capita + healthy_life_expectancy_at_birth + year,
  data = happy_processed
)

summary(happy_model_more)


## ----anova-comparison------------------------------------------------------------------------------
anova(happy_model_base, happy_model_more, test = 'Chi')


## ----AIC-------------------------------------------------------------------------------------------
AIC(happy_model_base)


## ----AIC-compare-----------------------------------------------------------------------------------
AIC(happy_model_base, happy_model_more)


## ----boot-$R^2$, echo=FALSE------------------------------------------------------------------------
library(boot)

f_base = happiness_score ~ democratic_quality + generosity + log_gdp_per_capita
f_more = happiness_score ~ democratic_quality + generosity + log_gdp_per_capita + healthy_life_expectancy_at_birth + year

r2_base <- boot(
  happy_processed,
  function(data, indices)
    summary(lm(f_base, data[indices, ])
    )$adj.r.squared, 
  R = 1000
)

r2_more <- boot(
  happy_processed,
  function(data, indices)
    summary(lm(f_more, data[indices, ])
    )$adj.r.squared, 
  R = 1000
)

ci = rbind(
  quantile(r2_base$t, c(0.025,0.975)),
  quantile(r2_more$t, c(0.025,0.975))
)

tibble(
  model = c('base', 'more'), 
  r2 = c(r2_base$t0, r2_more$t0),
) %>% 
  bind_cols(as_tibble(ci)) %>% 
  kable_df()


## ----boot-$R^2$-diff, echo=FALSE-------------------------------------------------------------------
diff_in_r2 = data.matrix(quantile(r2_more$t0 - r2_base$t, c(0.025,0.975)))

colnames(diff_in_r2) = 'Difference in $R^2$'

diff_in_r2 %>% 
  t() %>% 
  as_tibble(rownames = NA) %>% 
  kable_df()


## ----boot-$AIC$, echo=FALSE------------------------------------------------------------------------
aic_base <- boot(
  happy_processed,
  function(data, indices)
    AIC(lm(f_base, data[indices, ])
    ), 
  R = 1000
)

aic_more <- boot(
  happy_processed,
  function(data, indices)
    AIC(lm(f_more, data[indices, ])
    ), 
  R = 1000
)

ci = rbind(
  quantile(aic_base$t, c(0.025,0.975)),
  quantile(aic_more$t, c(0.025,0.975))
)

tibble(
  model = c('base', 'more'), 
  aic = c(aic_base$t0, aic_more$t0),
) %>% 
  bind_cols(as_tibble(ci)) %>% 
  kable_df()


## ----boot-$ARIC$-diff, echo=FALSE------------------------------------------------------------------
diff_in_aic = data.matrix(quantile(aic_more$t0 - aic_base$t, c(0.025,0.975)))

colnames(diff_in_aic) = 'Difference in AIC'

diff_in_aic %>% 
  t() %>% 
  as_tibble(rownames = NA) %>% 
  kable_df()


## ----happy-interact--------------------------------------------------------------------------------
happy_model_interact = lm(
  happiness_score ~ democratic_quality + generosity + log_gdp_per_capita + 
    healthy_life_expectancy_at_birth +
    democratic_quality:healthy_life_expectancy_at_birth,
  data = happy_processed
  )

summary(happy_model_interact)


## ----vis-interact----------------------------------------------------------------------------------
library(ggeffects)

plot(
  ggpredict(
    happy_model_interact,
    terms = c('democratic_quality', 'healthy_life_expectancy_at_birth[-1, 0, 1]')
  )
)


## ----contour-plot, echo=FALSE, fig.retina=5--------------------------------------------------------
# library(mgcv)
# 
# happy_model_interact_ = gam(
#   happiness_score ~ democratic_quality + generosity + log_gdp_per_capita + 
#     healthy_life_expectancy_at_birth +
#     democratic_quality:healthy_life_expectancy_at_birth,
#   data = happy_processed
#   )
# 
# plot_gam_2d(
#   happy_model_interact_,
#   democratic_quality,
#   healthy_life_expectancy_at_birth,
#   n = 500,
#   palette = 'acton'
# ) +
#   guides(fill = guide_legend(title = "Expected Happiness")) +
#   theme(
#     axis.title.x = element_text(hjust = .5),
#     axis.title.y = element_text(vjust = .5),
#     legend.title = element_text(size = 8)
#   )
# 
# ggsave('img/interaction_2d.png') # svg was 55mb

# plot_gam_3d(happy_model_interact_, democratic_quality, healthy_life_expectancy_at_birth)

knitr::include_graphics('img/interaction_2d.png')


## ----anova-3---------------------------------------------------------------------------------------
AIC(happy_model_base, happy_model_more, happy_model_interact)


## ----happy-gam-------------------------------------------------------------------------------------
library(mgcv)

happy_model_gam = gam(
  happiness_score ~ s(democratic_quality) + s(generosity) + s(log_gdp_per_capita) + 
    s(healthy_life_expectancy_at_birth),
  data = happy_processed
)

summary(happy_model_gam)


## ----happy-gam-vis, eval=FALSE---------------------------------------------------------------------
## library(mgcViz)
## 
## plot.gamViz(happy_model_gam, allTerms = T)


## ----happy-gam-vis-show, echo=FALSE----------------------------------------------------------------
visibly::plot_gam(happy_model_gam)


## ----gam-interact, echo=FALSE----------------------------------------------------------------------
gam(
  happiness_score ~ s(democratic_quality, healthy_life_expectancy_at_birth, k = 10) + s(generosity) + s(log_gdp_per_capita),
  data = happy_processed
) %>% 
  visibly::plot_gam_3d(democratic_quality, healthy_life_expectancy_at_birth, palette = 'vik') %>% 
  plotly::hide_colorbar()


## ----AIC-all, cache=FALSE--------------------------------------------------------------------------
AIC(
  happy_model_null,
  happy_model_base,
  happy_model_more,
  happy_model_interact,
  happy_model_gam
)


## ----aic-average, echo=FALSE-----------------------------------------------------------------------
model_list = list(
  happy_model_base,
  happy_model_more,
  happy_model_interact,
  happy_model_gam
)

aic_weights = AIC(
  happy_model_base,
  happy_model_more,
  happy_model_interact,
  happy_model_gam
) %>% 
  as_tibble(rownames = 'model') %>% 
  mutate(
    AICc = map_dbl(model_list, AICcmodavg::AICc),
    deltaAICc = AICc - min(AICc),
    `Rel. Like.` = exp(-.5*deltaAICc),
    weight = `Rel. Like.` / sum(`Rel. Like.`)
  )

aic_weights %>% 
  kable_df(digits = 3)
# 
# model_avg = list(
#   base = happy_model_base,
#   more = happy_model_more,
#   interact = happy_model_interact,
#   gam = happy_model_gam
# ) %>% 
#   map_df(fitted) %>%
#   mutate(avg = rowSums(. * aic_weights$weight))

# model_avg %>% 
#   head() %>% 
#   kable_df()


## ----avgr2, echo=FALSE-----------------------------------------------------------------------------
# avg_r2 = cor(model_avg$avg, happy_model_base$model$happiness_score)^2


## ----ex0-goog, eval=FALSE--------------------------------------------------------------------------
## load('data/google_apps.RData')


## ----ex1-model-assess, eval=FALSE------------------------------------------------------------------
## summary(model)
## plot(model)


## ----ex2-model-compare, eval=FALSE-----------------------------------------------------------------
## anova(model1, model2)
## AIC(model1, model2)

