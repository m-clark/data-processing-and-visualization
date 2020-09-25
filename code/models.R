## ----model-setup, include=FALSE, eval=TRUE, cache=FALSE------------------------
knitr::opts_chunk$set(eval=T, echo=T)


## ----model-names, echo=FALSE---------------------------------------------------
y = c('Dependent variable', 'Endogenous', 'Response', 'Outcome', 'Output', 'Y', 'Regressand','Left hand side (LHS)')

x = c('Independent variable', 'Exogenous', 'Explanatory Variable' , 'Covariate', 'Input', 'X', 'Regressor', 'Right hand side (RHS)')

tibble(
  Type = rep(c('Target', 'Predictor'), each = length(y)),
  Names = c(y, x)
) %>% 
  kable_df() %>% 
  collapse_rows()


## ----linear-model, eval=FALSE--------------------------------------------------
## lm(y ~ x + z)


## ----linear-quad-inter, eval=FALSE---------------------------------------------
## lm(y ~ x + z + x:z)
## lm(y ~ x + x_squared)     # a better way: lm(y ~ poly(x, degree = 2))


## ----ls-loss-------------------------------------------------------------------
ls_loss <- function(X, y, beta) {
  
  # initialize the objects
  loss  = rep(0, nrow(X))
  y_hat = rep(0, nrow(X))
  
  # for each row, calculate y_hat and square the difference with y
  for (n in 1:nrow(X)) {
    y_hat[n] = sum(X[n, ] * beta)
    loss[n]  = (y[n] - y_hat[n]) ^ 2
  }
  
  sum(loss)  
}


## ----ls-loss-data-gen----------------------------------------------------------
set.seed(123)           # for reproducibility
N = 100
X = cbind(1, rnorm(N))  # a model matrix; first column represents the intercept
y = 5 * X[, 1] + .5 * X[, 2] + rnorm(N)  # a target with some noise; truth is y = 5 +.5*x

df = data.frame(y = y, x = X[, 2])


## ----ls-loss-guess-------------------------------------------------------------
ls_loss(X, y, beta = c(0, 1))    # guess 1
ls_loss(X, y, beta = c(1, 2))    # guess 2
ls_loss(X, y, beta = c(4, .25))  # guess 3


## ----ls-loss-lm----------------------------------------------------------------
model = lm(y ~ x, df)            # fit the model and obtain parameter estimates using OLS
coef(model)                      # best guess given the data 
sum(residuals(model)^2)          # least squares loss


## ----normal-eq-----------------------------------------------------------------
solve(crossprod(X)) %*% crossprod(X, y)  # 'normal equations'

coef(model)


## ----maximum-likelihood, echo=FALSE--------------------------------------------
set.seed(1234)
y = rpois(100000, lambda = 5)
mus = seq(3, 8, l = 100)
L = sapply(mus, function(mu) sum(dpois(y, lambda = mu, log = T)))
plot_dat = data.frame(mus, L, Max = ifelse(L == max(L), 'Maximum', ''))

# ggplot() + 
#   geom_bar(aes(x = y)) +
#   labs(x = '', y = '', subtitle = 'Observed Data') +
#   theme_clean(center_axis_labels = TRUE) +
#   theme(
#     # axis.ticks.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.title.y = element_blank(),
#     axis.title.x = element_text(size = 12, vjust = -1.5),
#     axis.text.x = element_text( size = 14),
#     plot.background = element_rect(fill = "transparent", colour = NA)
#   )

ggplot(plot_dat) + 
  geom_vline(aes(xintercept = 5), alpha = .25, lty = 3) +
  geom_hline(aes(yintercept = max(L)), alpha = .25) +
  geom_path(aes(x = mus, y = L), color = '#ff5500', alpha=.75, lwd = 2) +
  geom_point(aes(x = 5, y = max(L)), color = '#ff5500',  size = 3) +
  # labs(x = expression(theta), y = 'Likelihood') +
  labs(x = 'Potential Estimates', y = 'More Probable ↑') +
  scale_y_continuous(breaks = L, labels = plot_dat$Max) +
  theme_clean(center_axis_labels = TRUE) +
  theme(
    # axis.ticks.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, vjust = -1.5),
    axis.text.x = element_text( size = 14),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )


## ----max-like------------------------------------------------------------------
max_like <- function(X, y, beta, sigma = 1) {
  
  likelihood = rep(0, nrow(X))
  y_hat = rep(0, nrow(X))
  
  for (n in 1:nrow(X)) {
    y_hat[n] <- sum(X[n, ] * beta)
    likelihood[n] = dnorm(y[n],  mean = y_hat[n], sd = sigma, log = TRUE)
  }
  
  sum(likelihood)  
}


## ----maximum-likelihood-estimate-----------------------------------------------
max_like(X, y, beta = c(0, 1))    # guess 1
max_like(X, y, beta = c(1, 2))    # guess 2
max_like(X, y, beta = c(4, .25))  # guess 3

logLik(model)


## ----models-graph, caption = 'Models', eval=TRUE, echo=FALSE-------------------
htmltools::tags$div(
  style = "width:500px; margin:0 auto; font-family:Roboto; ",
  DiagrammeR::DiagrammeR(
    "scripts/models.gv",
    type = 'grViz',
    width = 500,
    height = 250
  )
)


## ----estimation-graph, caption = 'Estimation Procedures', eval=TRUE, echo=FALSE----
htmltools::tags$div(
  style = "width:500px; margin:0 auto; font-family:Roboto; ",
  DiagrammeR::DiagrammeR(
    "scripts/estimation.gv",
    type = 'grViz',
    width = 500,
    height = 250
  )
)


## ----abb, echo=FALSE-----------------------------------------------------------
data.frame(
  Label = c('LM', 'GLM', 'GLMM', 'GAMM', 'OLS', 'WLS', 'GLS', 'GEE', 'GMM'),
  Name = c(
    'Linear Model',
    'Generalized Linear Model',
    'Generalized Linear Mixed Model',
    'Generalized Linear Mixed Model',
    'Ordinary Least Squares',
    'Weighted Least Squares',
    'Generalized Least Squares',
    'Generalized Estimating Equations',
    'Generalized Method of Moments'
  )
) %>%
  kable_df() %>% 
  kable_styling(font_size = 10)


## ----model-syntax, eval=FALSE--------------------------------------------------
## lm(y ~ x + z)                                          # standard linear model/OLS
## glm(y ~ x + z, family = 'binomial')                    # logistic regression with binary response
## glm(y ~ x + z + offset(log(q)), family = 'poisson')    # count/rate model
## betareg::betareg(y ~ x + z)                            # beta regression for targets between 0 and 1
## pscl::hurdle(y ~ x + z, dist = "negbin")               # hurdle model with negative binomial response
## lme4::glmer(y ~ x + (1 | group), family = 'binomial')  # generalized linear mixed model
## mgcv::gam(y ~ s(x))                                    # generalized additive model
## survival::coxph(Surv(time = t, event = q) ~ x)         # Cox Proportional Hazards Regression
## 
## # Bayesian mixed model
## brms::brm(
##   y ~ x + (1 + x | group),
##   family = 'zero_one_inflated_beta',
##   prior = priors
## )


## ----model-prep, echo=1:5------------------------------------------------------
library(tidyverse)  # load if you haven't already

load('data/world_happiness.RData')

# glimpse(happy)

tidyext::describe_all_num(happy) %>% 
  kable_df()

happy_score = tidyext::num_summary(happy$happiness_score)


## ----model-fit-----------------------------------------------------------------
happy_model_base = lm(
  happiness_score ~ democratic_quality + generosity + log_gdp_per_capita,
  data = happy
)


## ----model-matrix--------------------------------------------------------------
X = model.matrix(
  happiness_score ~ democratic_quality + generosity + log_gdp_per_capita, 
  data = happy
)

head(X)


## ----na-omit-------------------------------------------------------------------
nrow(happy)
nrow(X)


## ----model-response------------------------------------------------------------
X_df = model.frame(
    happiness_score ~ democratic_quality + generosity + log_gdp_per_capita,
    data = happy
  )

y = model.response(X_df)


## ----model-fit-model-matrix----------------------------------------------------
happy_model_matrix = lm.fit(X, y)

summary(happy_model_matrix)  # only a standard list is returned


## ----coef-model-matrix---------------------------------------------------------
coef(happy_model_matrix)


## ----model-summarize, echo=1---------------------------------------------------
happy_model_base_sum = summary(happy_model_base)

happy_model_base_sum


## ----model-confint-------------------------------------------------------------
confint(happy_model_base)


## ----coefficient-interpretation, echo=FALSE------------------------------------
tibble(
  target = c('y', 'y', 'log(y)', 'log(y)', 'y', 'scale(y)', 'scale(y)'),
  predictor = c('x', 'log(x)', 'x', 'log(x)', 'scale(x)', 'x', 'scale(x)'), 
  interpretation = c(
    '$\\Delta y = \\beta\\Delta x$',
    '$\\Delta y \\approx (\\beta/100)\\%\\Delta x$',
    '$\\%\\Delta y \\approx 100\\cdot \\beta\\%\\Delta x$',
    '$\\%\\Delta y = \\beta\\%\\Delta x$',
    '$\\Delta y =  \\beta\\sigma\\Delta x$',
    '$\\sigma\\Delta y =  \\beta\\Delta x$',
    '$\\sigma\\Delta y =  \\beta\\sigma\\Delta x$'
  ), 
) %>%
  kable_df(booktabs = T, escape = FALSE)


## ----dummy-coding--------------------------------------------------------------
library(recipes)

nafta = happy %>% 
  filter(country %in% c('United States', 'Canada', 'Mexico'))

dummy = nafta  %>% 
  recipe(~ country + generosity) %>%        # formula approach for specifying variables
  step_dummy(country, one_hot = TRUE) %>%   # make variables for all factor levels
  step_center(generosity) %>%               # example of centering
  step_scale(generosity)                    # example of standardizing

prep(dummy) %>%      # estimates the necessary data to apply to this or other data sets
  bake(nafta) %>%    # apply the computations
  print(n = 20)


## ----dummy-reg-----------------------------------------------------------------
model_dummy = lm(happiness_score ~ country, data = nafta)

summary(model_dummy)


## ----contrast-coding, echo=FALSE-----------------------------------------------
tibble(
  group = c('Canada', 'Mexico', 'United States'),
  canada_vs_other = c(-2/3,1/3,1/3),
  mexico_vs_us = c(0,-.5,.5)
) %>% 
  kable_df() %>% 
  kableExtra::add_footnote(notation = 'none', 'weights sum to zero, but are arbitrary')


## ----contrast-coding-lm--------------------------------------------------------
nafta = nafta %>% 
  mutate(country_fac = factor(country))

contrasts(nafta$country_fac) = matrix(c(-2/3, 1/3, 1/3, 0, -.5, .5), 
                                      ncol = 2)

summary(lm(happiness_score ~ country_fac, data = nafta))

nafta %>% 
  group_by(country) %>% 
  summarise(happy = mean(happiness_score, na.rm = TRUE))


## ----model-methods-------------------------------------------------------------
predict(happy_model_base, newdata = happy %>% slice(1:5))
coef(happy_model_base)


## ----model-summary-------------------------------------------------------------
str(happy_model_base_sum, 1)


## ----extract-from-model--------------------------------------------------------
happy_model_base_sum$adj.r.squared
happy_model_base_sum[['sigma']]


## ----broom-demo----------------------------------------------------------------
library(broom)
tidy(happy_model_base)


## ----emmeans-------------------------------------------------------------------
happy_model_nafta = lm(happiness_score ~ country + year, data = nafta)

library(emmeans)
country_means = emmeans(happy_model_nafta, ~ country)
country_means

plot(country_means)


## ----emmeans-pairwise----------------------------------------------------------
pw_comparisons = contrast(country_means, method = 'pairwise', adjust = 'bonferroni')
pw_comparisons
plot(pw_comparisons)


## ----ggeffects-----------------------------------------------------------------
happy_model_nafta = lm(happiness_score ~ year*country, data = nafta)

library(ggeffects)
preds = ggpredict(happy_model_nafta, terms = c('year', 'country'))

plot(preds)


## ----poisson-------------------------------------------------------------------
set.seed(123)                     # for reproducibility
N = 1000                          # sample size
beta = c(2, 1)                    # the true coefficient values
x = rnorm(N)                      # a single predictor variable
mu = exp(beta[1] + beta[2]*x)     # the linear predictor
y = rpois(N, lambda = mu)         # the target variable lambda = mean

glm(y ~ x, family = poisson)


## ----binom---------------------------------------------------------------------
mu = plogis(beta[1] + beta[2]*x)
y = rbinom(N, size = 1, mu)

glm(y ~ x, family = binomial)

# extension to count/proportional model
# mu = plogis(beta[1] + beta[2]*x)
# total = rpois(N, lambda = 5)
# events = rbinom(N, size = total, mu)
# nonevents = total - events
# 
# glm(cbind(events, nonevents) ~ x, family = binomial)


## ----ex1-goog, eval=FALSE------------------------------------------------------
## load('data/google_apps.RData')
## 
## model = lm(? ~ reviews + type + size_in_MB, data = google_apps)
## 
## plot(emmeans::emmeans(model, ~type))


## ----ex2-goog, eval=FALSE------------------------------------------------------
## model = lm(? ~ reviews + type*?, data = google_apps)
## 
## plot(ggeffects::ggpredict(model, terms = c('size_in_MB', 'type')))


## ----ex3-fish, eval=FALSE------------------------------------------------------
## load('data/fish.RData')
## 
## model = glm(?, data = fish)

