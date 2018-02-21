## ----ggplot2setup, include=FALSE, eval=TRUE, cache=FALSE-----------------
knitr::opts_chunk$set(eval=T, echo=T)

## ----layer, eval=FALSE---------------------------------------------------
## # recall that starwars is in the dplyr package
## ggplot(aes(x=height, y=mass), data=starwars)

## ----layer2--------------------------------------------------------------
ggplot(aes(x=height, y=mass), data=starwars) +
  geom_point()

## ----layer3--------------------------------------------------------------
ggplot(aes(x=height, y=mass), data=starwars) +
  geom_point() +
  labs(x='Height in cm', y='Weight in kg') +
  theme_dark()

## ----pipeplus, eval=FALSE------------------------------------------------
## ggplot(aes(x=myvar, y=myvar2), data=mydata) +
##   geom_point()

## ----aes, eval=F---------------------------------------------------------
## aes(x=myvar, y=myvar2, color=myvar3, group=g)

## ----aes_vs_not1, eval=FALSE---------------------------------------------
## ... +
##   geom_point(..., size=4)

## ----aes_vs_not2, eval=FALSE---------------------------------------------
## ... +
##   geom_point(aes(size=myvar))

## ----ggscatter, dev='png'------------------------------------------------
library(ggplot2)
data("diamonds"); data('economics')
ggplot(aes(x=carat, y=price), data=diamonds) +
  geom_point(size=.5, color='peru')

## ----ggline--------------------------------------------------------------
ggplot(aes(x=date, y=unemploy), data=economics) +
  geom_line() +
  geom_text(aes(label=unemploy), 
            vjust=-.5, 
            data=filter(economics, date=='2009-10-01'))

## ----ggalpha, fig.width=6, fig.height=4, dev='png'-----------------------
ggplot(aes(x=carat, y=price), data=diamonds) +
  geom_point(aes(size=carat, color=clarity), alpha=.05) 

## ----ggquant-------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_quantile()

## ----ggsmooth------------------------------------------------------------
data(mcycle, package='MASS')
ggplot(aes(x=times, y=accel), data=mcycle) +
  geom_point() +
  geom_smooth(formula=y ~ s(x, bs='ad'), method='gam')

## ----ggstatsum-----------------------------------------------------------
ggplot(mtcars, aes(cyl, mpg)) + 
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", colour = "orange", alpha=.75, size = 1)

## ----scale_labs----------------------------------------------------------
ggplot(aes(x=times, y=accel), data=mcycle) +
  geom_smooth(se=F) +
  labs(x='milliseconds after impact', y='head acceleration', title='Motorcycle Accident')

## ----scale_lims----------------------------------------------------------
ggplot(mpg, aes(displ, hwy, size=cyl)) + 
  geom_point() + 
  ylim(c(0,60))

ggplot(mpg, aes(displ, hwy, size=cyl)) + 
  geom_point() + 
  scale_y_continuous(limits=c(0,60), 
                     breaks=seq(0,60,by=12), 
                     minor_breaks=seq(6,60,by=6))

## ----scale_size2---------------------------------------------------------
ggplot(mpg, aes(displ, hwy, size=cyl)) + 
  geom_point() +
  scale_size(range=c(1,3))

## ----scale_color---------------------------------------------------------
ggplot(mpg, aes(displ, hwy, color=cyl)) + 
  geom_point() +
  scale_color_gradient2()

ggplot(mpg, aes(displ, hwy, color=factor(cyl))) + 
  geom_point() +
  scale_color_manual(values=c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))

## ----scale_scale---------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  scale_x_log10()

## ----facetgrid, eval=1, echo=1-------------------------------------------
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_grid(~ cyl)
ggplot(mpg, aes(displ, cty)) + 
  geom_point() +
  facet_grid(~ cyl, labeller = label_both)
ggplot(midwest, aes(popdensity, percbelowpoverty)) + 
  geom_point() +
  facet_grid(~ state, labeller = label_both)

## ----facetgrid2----------------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_grid(vs ~ cyl, labeller = label_both)

## ----facetwrap-----------------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_wrap(vs ~ cyl, labeller = label_both, ncol=2)

## ----finecontrol, fig.height=6, fig.width=8, echo=-c(1:5, 7), dev='png'----
library(grid); library(caTools)
img = caTools::read.gif('img/lamb.gif')
lambosun = img$col[img$image+1]
dim(lambosun) = dim(img$image)

ggplot(aes(x=carat, y=price), data=diamonds) +
  annotation_custom(rasterGrob(lambosun, 
                               width=unit(1,"npc"), 
                               height=unit(1,"npc"), 
                               interpolate = FALSE), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(aes(color=clarity), alpha=.5) + 
  scale_y_log10(breaks=c(1000,5000,10000)) +
  xlim(0, 10) +
  scale_color_brewer(type='div') +
  facet_wrap(~cut, ncol=3) +
  theme_minimal() +
  theme(axis.ticks.x=element_line(color='darkred'),
        axis.text.x=element_text(angle=-45),
        axis.text.y=element_text(size=20),
        strip.text=element_text(color='forestgreen'),
        strip.background=element_blank(),
        panel.grid.minor=element_line(color='lightblue'),
        legend.key=element_rect(linetype=4),
        legend.position='bottom')

## ----mullerplot, echo=FALSE----------------------------------------------
library(ggmuller)
Muller_df <- get_Muller_df(example_edges, example_pop_df, cutoff = 0.005)

# generate pretty plot
Muller_plot(Muller_df, palette=rep(RColorBrewer::brewer.pal(12, 'Set3'), 4), xlab='', ylab='') +
  lazerhawk::theme_trueMinimal() +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

## ----ggplot_ex1, echo=FALSE----------------------------------------------
library(ggplot2)
ggplot(aes(x=waiting, y=eruptions), data=faithful) +
  geom_point()

## ----ggplot_ex2----------------------------------------------------------
library(maps)
mi = map_data("county", "michigan")
seats = mi %>% 
  group_by(subregion) %>% 
  summarise_at(vars(lat, long), function(x) median(range(x)))

# inspect the data
# head(mi)
# head(seats)

ggplot(mi, aes(long, lat)) +
  geom_polygon(aes(group = subregion), fill = NA, colour = "grey60") +
  geom_text(aes(label = subregion), data = seats, size = 1, angle = 45) +
  geom_point(x=-83.748333, y=42.281389, color='#1e90ff', size=3) +
  theme_minimal() +
  theme(panel.grid=element_blank())

