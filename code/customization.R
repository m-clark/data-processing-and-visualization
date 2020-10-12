## ----plotly_text, echo=FALSE, out.width='50%'------------------------------------------------------
mgcv::gamSim(verbose = F) %>%
  modelr::add_predictions(., model = mgcv::gam(y ~ s(x2), data = .)) %>%
  plot_ly(x =  ~ x2, y =  ~ y) %>%
  add_lines(
    color = I(palettes$stan_red$complementary[2]),
    showlegend = F,
    opacity = .5
  ) %>%
  add_lines(
    y =  ~ pred,
    color = I(palettes$stan_red$stan_red),
    showlegend = F
  ) %>%
  theme_plotly()


## ----plotly_text2, echo=FALSE, out.width='50%'-----------------------------------------------------
mgcv::gamSim(verbose = F) %>%
  modelr::add_predictions(., model = mgcv::gam(y ~ s(x2), data = .)) %>%
  plot_ly(x =  ~ x2, y =  ~ y) %>%
  add_lines(
    color = I(palettes$stan_red$complementary[2]),
    showlegend = F,
    opacity = .5
  ) %>%
  add_lines(
    y =  ~ pred,
    color = I(palettes$stan_red$stan_red),
    showlegend = F
  ) %>%
  theme_plotly()


## ----include_graphics------------------------------------------------------------------------------
knitr::include_graphics('img/R.ico')


## # Header 1

## ## Header 2


## Yadda yadda `r object[1]` hey that's neat!

