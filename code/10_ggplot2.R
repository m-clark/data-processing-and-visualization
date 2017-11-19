## ----ggplot2setup, include=FALSE, eval=TRUE------------------------------


## ----aes, eval=F---------------------------------------------------------
## aes(x=myvar, y=myvar2, color=myvar3, group=g)

## ----layer, eval=FALSE---------------------------------------------------
## ggplot(aes(x=myvar, y=myvar2), data=mydata)

## ----pipeplus, eval=FALSE------------------------------------------------
## ggplot(aes(x=myvar, y=myvar2), data=mydata) +
##   geom_point()

## ----ggscatter, eval=TRUE------------------------------------------------
library(ggplot2)
data("diamonds"); data('economics')
ggplot(aes(x=carat, y=price), data=diamonds) +
  geom_point()

## ----ggline, eval=TRUE---------------------------------------------------
ggplot(aes(x=date, y=unemploy), data=economics) +
  geom_line()

## ----ggalpha, fig.width=6, fig.height=4, eval=TRUE-----------------------
ggplot(aes(x=carat, y=price), data=diamonds) +
  geom_point(aes(size=carat, color=clarity), alpha=.25) 

## ----ggquant, eval=TRUE--------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_quantile()

## ----ggsmooth, eval=TRUE-------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth()

## ----ggstatsum, eval=TRUE------------------------------------------------
ggplot(mtcars, aes(cyl, mpg)) + 
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", colour = "orange", alpha=.75, size = 1)

## ----facetgrid, eval=TRUE------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_grid(vs ~ cyl, labeller = label_both)

## ----facetwrap, eval=TRUE------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_wrap(vs ~ cyl, labeller = label_both, ncol=2)

## ----finecontrol, fig.height=6, fig.width=6, echo=-c(1:5, 7), eval=TRUE, cache=TRUE, dev='png'----
library(grid); library(caTools)
img = caTools::read.gif('img/lamb.gif')
lambosun = img$col[img$image+1]
dim(lambosun) = dim(img$image)

ggplot(aes(x=carat, y=price), data=diamonds) +
  annotation_custom(rasterGrob(lambosun, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE), 
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

## ----mullerplot, echo=FALSE, eval=TRUE-----------------------------------
library(ggmuller)
Muller_df <- get_Muller_df(example_edges, example_pop_df, cutoff = 0.005)

# generate pretty plot
Muller_plot(Muller_df, palette=rep(RColorBrewer::brewer.pal(12, 'Set3'), 4)) +
  labs(x='', y='') +
  lazerhawk::theme_trueMinimal() +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

## ---- echo=FALSE---------------------------------------------------------
library(ggplot2)
ggplot(aes(x=waiting, y=eruptions), data=faithful) +
  geom_point()

## ------------------------------------------------------------------------
library(maps)
mi = map_data("county", "michigan")
seats = mi %>% 
  group_by(subregion) %>% 
  summarise_at(vars(lat, long), function(x) median(range(x)))

ggplot(mi, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = NA, colour = "grey60") +
  geom_text(aes(label = subregion), data = seats, size = 1, angle = 45) +
  geom_point(y=42.281389, x=-83.748333, color='#1e90ff', size=3) +
  theme_minimal() +
  theme(panel.grid=element_blank())

