
# Preliminaries -----------------------------------------------------------

library(tidyverse)



# Example -----------------------------------------------------------------

data("diamonds")

data('economics')

ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point(size = .5, color = 'peru')



# Geoms -------------------------------------------------------------------

ggplot(aes(x = date, y = unemploy), data = economics) +
  geom_line() +
  geom_text(
    aes(label = unemploy),
    vjust = -.5,
    data = filter(economics, date == '2009-10-01')
  )


ggplot(aes(x = date, y = unemploy), data = economics) +
  geom_line() +
  geom_text(
    aes(label = unemploy),
    vjust = -.5,
    data = filter(economics, date == '2009-10-01')
  )

ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point(aes(size = carat, color = clarity), alpha = .05) 



# Stats -------------------------------------------------------------------


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_quantile()

data(mcycle, package = 'MASS')

ggplot(aes(x = times, y = accel), data = mcycle) +
  geom_point() +
  geom_smooth(formula = y ~ s(x, bs = 'ad'), method = 'gam')


ggplot(mtcars, aes(cyl, mpg)) +
  geom_point() +
  stat_summary(
    fun.data = "mean_cl_boot",
    colour = "orange",
    alpha = .75,
    size = 1
  )



# Scales ------------------------------------------------------------------


ggplot(aes(x = times, y = accel), data = mcycle) +
  geom_smooth(se = FALSE) +
  labs(
    x     = 'milliseconds after impact', 
    y     = 'head acceleration', 
    title = 'Motorcycle Accident'
  )


ggplot(mpg, aes(x = displ, y = hwy, size = cyl)) +
  geom_point() +
  ylim(c(0, 60))


ggplot(mpg, aes(x = displ, y = hwy, size = cyl)) +
  geom_point() +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 12),
    minor_breaks = seq(6, 60, by = 6)
  )


ggplot(mpg, aes(x = displ, y = hwy, size = cyl)) +
  geom_point() +
  scale_size(range = c(1, 3))


ggplot(mpg, aes(x = displ, y = hwy, color = cyl)) +
  geom_point() +
  scale_color_gradient2()

ggplot(mpg, aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))



ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  scale_x_log10()



# Facets ------------------------------------------------------------------

ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  facet_grid(~ cyl)

ggplot(mpg, aes(displ, cty)) + 
  geom_point() +
  facet_grid(~ cyl, labeller = label_both)

ggplot(midwest, aes(popdensity, percbelowpoverty)) + 
  geom_point() +
  facet_grid(~ state, labeller = label_both)


ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  facet_wrap(vs ~ cyl, labeller = label_both, ncol=2)



# Multiple Plots ----------------------------------------------------------


library(patchwork)

g1 = ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point()

g2 = ggplot(mtcars, aes(wt)) + 
  geom_density()

g3 = ggplot(mtcars, aes(mpg)) + 
  geom_density()

g1 /                       # initial plot, place next part underneath
  (g2 | g3)                # groups g2 and g3 side by side


p1 = ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 = ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 = ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 = ggplot(mtcars) + geom_bar(aes(carb))
p5 = ggplot(mtcars) + geom_violin(aes(cyl, mpg, group = cyl))

p1 +
  p2 +
  (p3 / p4) * theme_void() +
  p5 +
  plot_layout(widths = c(2, 1))



# Extensions --------------------------------------------------------------

library(gganimate)

load('data/gapminder.RData')

gap_plot = gapminder_2019 %>% 
  filter(giniPercap != 40) 

gap_plot_filter = gap_plot %>% 
  filter(country %in% c('United States', 'Mexico', 'Canada'))

initial_plot = ggplot(gap_plot, aes(x = year, y = giniPercap, group = country)) +
  geom_line(alpha = .05) +
  geom_path(
    aes(color = country),
    lwd = 2,
    arrow = arrow(
      length = unit(0.25, "cm")
    ), 
    alpha = .5,
    data = gap_plot_filter,
    show.legend = FALSE
  ) +
  geom_text(
    aes(color = country, label = country),
    nudge_x = 5, 
    nudge_y = 2, 
    size = 2,
    data = gap_plot_filter,
    show.legend = FALSE
  ) +
  theme_minimal() +
  transition_reveal(year)

animate(initial_plot, end_pause = 50, nframes = 150, rewind = TRUE)
