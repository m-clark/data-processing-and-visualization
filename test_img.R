library(ggplot2)
library(grid)

img <- readJPEG("image.jpg")

df <- data.frame(x=sample(1:64, 1000, replace=T), 
                 y=sample(1:64, 1000, replace=T))

ggplot(df, aes(x,y)) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  stat_bin2d() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) 


img = caTools::read.gif('img/lamb.gif', frame = 1)

mat = img$col[img$image+1]
dim(mat) = dim(img$image)
qplot(1,1) + annotation_custom(rasterGrob(mat))
# g <- grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)
# 
# g_ct <- ggplot(data=df_ct) +
#   annotation_custom(g, -Inf, Inf, -Inf, Inf) +
  
  
  library(grid); library(caTools)
imglist = lapply(0:5, function(.) caTools::read.gif('img/lamb.gif', frame=.))
img = imglist[[0]]
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
