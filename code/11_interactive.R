## ----title, echo=F, eval=F-----------------------------------------------
## saveWidget(vn, file='vntitle.html')
## library(visNetwork)
## nodes = data.frame(id=1:2, label=c('Interactive', 'Visualization'), color='#1e90ff')
## edges = data.frame(from=1, to=2, color='transparent')
## library(visNetwork)
## vn = visNetwork(nodes, edges, height=400, width=600) %>%
##   visNodes(shape='text', font=list(color='#1e90ff', size=164)) %>%
##   visOptions(highlightNearest=F)

## ----plotly1, cache=FALSE, eval=TRUE-------------------------------------
library(plotly)
midwest %>% 
  filter(inmetro==T) %>% 
  plot_ly(x=~percbelowpoverty, y=~percollege, mode='markers') 

## ----plotly2, echo=TRUE, cache=FALSE, eval=TRUE--------------------------
library(mgcv); library(modelr)

mtcars %>% 
  mutate(amFactor = factor(am, labels=c('auto', 'manual')),
         hovertext = paste('weight:',wt, '<br>', 'mgp:', mpg, '<br>', amFactor)) %>% 
  add_predictions(gam(mpg~s(wt, am, bs='fs'), data=mtcars)) %>% 
  arrange(wt) %>% 
  plot_ly() %>%
  add_markers(x=~wt, y=~mpg, color=~amFactor, text=~hovertext, hoverinfo='text') %>% 
  add_lines(x=~wt, y=~pred, color=~amFactor, alpha=.5, name='gam prediction', showlegend=F)

## ----ggplotly, cache=FALSE, eval=TRUE------------------------------------
gp = mtcars %>% 
  mutate(amFactor = factor(am, labels=c('auto', 'manual')),
         hovertext = paste(wt, mpg, amFactor)) %>% 
  arrange(wt) %>% 
  ggplot(aes(x=wt, y=mpg, color=amFactor)) +
  geom_smooth(se=F) +
  geom_point(aes(color=amFactor))
ggplotly()

## ----dygraphdata, cache=FALSE, eval=F, echo=FALSE------------------------
## # library(dygraphs)
## # data(UKLungDeaths)
## # cbind(ldeaths, mdeaths, fdeaths) %>%
## #   dygraph() %>%
## #   dyOptions(stackedGraph = TRUE, colors=RColorBrewer::brewer.pal(3, name='Dark2')) %>%
## #   dyRangeSelector(height = 20)

## ----highcharts, eval=T--------------------------------------------------
library(highcharter); library(quantmod)

x = getSymbols("GOOG", auto.assign = FALSE)

hchart(x, width=1000)

## ----visNetworkinitial, echo=FALSE, cache=TRUE, eval=T-------------------
set.seed(1352)
nodes = data.frame(id = 0:5,
                   label = c('Bobby', 'Janie','Timmie', 'Mary', 'Johnny', 'Billy'),
                   group = c('friend', 'frenemy','frenemy', rep('friend', 3)),
                   value = sample(10:50, 6))
edges = data.frame(from = c(0,0,0,1,1,2,2,3,3,3,4,5,5),
                   to = sample(0:5, 13, replace = T),
                   value = sample(1:10, 13, replace = T)) %>% 
  filter(from!=to)

## ----visNetwork, eval=F, echo=-1-----------------------------------------
## saveWidget(vn, file='visnetwork.html')
## library(visNetwork)
## visNetwork(nodes, edges) %>% #, height=600, width=800
##   visNodes(shape='circle',
##            font=list(),
##            scaling=list(min=10, max=50, label=list(enable=T))) %>%
##   visLegend()

## ----datatable, eval=TRUE------------------------------------------------
library(DT)
ggplot2movies::movies %>% 
  select(1:6) %>% 
  filter(rating>8, !is.na(budget), votes > 1000) %>% 
  datatable()

## ----interactive_ex1, echo=FALSE-----------------------------------------
movies %>% 
  group_by(year) %>% 
  summarise(Avg_Rating=mean(rating)) %>% 
  plot_ly() %>% 
  add_markers(x=~year, y=~Avg_Rating)

## ----interactive_ex2, echo=FALSE-----------------------------------------
movies %>% 
  group_by(year, Drama) %>% 
  summarise(Avg_Rating=mean(rating),
            Avg_Votes = mean(votes)) %>% 
  plot_ly() %>% 
  add_markers(x=~year, y=~Avg_Rating, size=~Avg_Votes, color=~Drama, mode='markers')

