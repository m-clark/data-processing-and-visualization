## ----interactivesetup, include=FALSE, eval=TRUE, cache=FALSE-------------
knitr::opts_chunk$set(cache.rebuild=F)

## ----title, echo=F-------------------------------------------------------
# saveWidget(vn, file='vntitle.html')
library(visNetwork)
nodes = data.frame(id=1:2, label=c('Interactive', 'Visualization'), color='#1e90ff')
edges = data.frame(from=1, to=2, color='transparent')
visNetwork(nodes, edges, height=400, width='100%') %>% 
  visNodes(shape='text', font=list(color=palettes$tyrian_purple2$tyrian_purple, size=64)) %>% 
  visOptions(highlightNearest=F) 

## ----plotly1-------------------------------------------------------------
library(plotly)
midwest %>% 
  filter(inmetro==T) %>% 
  plot_ly(x=~percbelowpoverty, y=~percollege) %>% 
  add_markers()

## ----plotly2-------------------------------------------------------------
library(mgcv); library(modelr); library(glue)

mtcars %>% 
  mutate(amFactor = factor(am, labels=c('auto', 'manual')),
         hovertext = glue('weight: {wt} <br> mgp:  {mpg} <br> {amFactor}')) %>% 
  add_predictions(gam(mpg~s(wt, am, bs='fs'), data=mtcars)) %>% 
  plot_ly() %>%
  add_markers(x=~wt, 
              y=~mpg, 
              color=~amFactor, 
              opacity=.5, 
              text=~hovertext, 
              hoverinfo='text', 
              showlegend=F) %>% 
  add_lines(x=~wt, 
            y=~pred, 
            color=~amFactor, 
            name='gam prediction')

## ----plotly_1line--------------------------------------------------------
plot_ly(midwest, x = ~percollege, color = ~state, type = "box")

## ----ggplotly------------------------------------------------------------
gp = mtcars %>% 
  mutate(amFactor = factor(am, labels=c('auto', 'manual')),
         hovertext = paste(wt, mpg, amFactor)) %>% 
  arrange(wt) %>% 
  ggplot(aes(x=wt, y=mpg, color=amFactor)) +
  geom_smooth(se=F) +
  geom_point(aes(color=amFactor))
ggplotly()

## ----highcharts----------------------------------------------------------
library(highcharter); library(quantmod)

google_price = getSymbols("GOOG", auto.assign = FALSE)
hchart(google_price)

## ----visNetworkinitial---------------------------------------------------
set.seed(1352)
nodes = data.frame(id = 0:5,
                   label = c('Bobby', 'Janie','Timmie', 'Mary', 'Johnny', 'Billy'),
                   group = c('friend', 'frenemy','frenemy', rep('friend', 3)),
                   value = sample(10:50, 6))
edges = data.frame(from = c(0,0,0,1,1,2,2,3,3,3,4,5,5),
                   to = sample(0:5, 13, replace = T),
                   value = sample(1:10, 13, replace = T)) %>% 
  filter(from!=to)

library(visNetwork)
visNetwork(nodes, edges, height=300, width=800) %>%
  visNodes(shape='circle', 
           font=list(), 
           scaling=list(min=10, max=50, label=list(enable=T))) %>% 
  visLegend()

## ----visNetwork, eval=F, echo=F------------------------------------------
## # saveWidget(vn, file='visnetwork.html')
## # library(visNetwork)
## # visNetwork(nodes, edges) %>% #, height=600, width=800
## #   visNodes(shape='circle',
## #            font=list(),
## #            scaling=list(min=10, max=50, label=list(enable=T))) %>%
## #   visLegend()
## # <iframe src='../img/visnetwork.html', width=1000, height=600, scrolling="no", frameBorder="0"></iframe>
## 

## ----leaflet-------------------------------------------------------------
hovertext <- paste(sep = "<br/>",
  "<b><a href='http://umich.edu/'>University of Michigan</a></b>",
  "Ann Arbor, MI"
)


library(leaflet)
leaflet() %>% 
  addTiles() %>%
  addPopups(lng=-83.738222, lat=42.277030, popup=hovertext)

## ----datatable-----------------------------------------------------------
library(DT)
ggplot2movies::movies %>% 
  select(1:6) %>% 
  filter(rating>8, !is.na(budget), votes > 1000) %>% 
  datatable()

## ----datatable_options---------------------------------------------------
iris %>% 
  # arrange(desc(Petal.Length)) %>% 
  datatable(rownames=F,
            options=list(dom='firtp'), 
            class = 'row-border') %>% 
  formatStyle('Sepal.Length', 
              fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle(
    'Sepal.Width',
    color = styleInterval(c(3.4, 3.8), c('#7f7f7f', '#00aaff', '#ff5500')),
    backgroundColor = styleInterval(3.4, c('#ebebeb', 'aliceblue'))
  ) %>%
  formatStyle(
    'Petal.Length',
    # color = 'transparent',
    background = styleColorBar(iris$Petal.Length, '#5500ff'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'Species',
    color='white',
    transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    backgroundColor = styleEqual(
      unique(iris$Species), c('#1f65b7', '#66b71f', '#b71f66')
    )
  )

## ----shiny, eval=FALSE---------------------------------------------------
## library(shiny)
## 
## # Running a Shiny app object
## app <- shinyApp(
##   ui = bootstrapPage(
##     numericInput('n', 'Number of obs', 100),
##     plotOutput('plot')
##   ),
##   server = function(input, output) {
##     output$plot <- renderPlot({
##       hist(runif(input$n))
##       })
##   }
## )
## 
## runApp(app)

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
  add_markers(x=~year, 
              y=~Avg_Rating, 
              size=~Avg_Votes, 
              color=~Drama)

