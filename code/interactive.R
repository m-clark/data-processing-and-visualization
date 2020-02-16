## ----interactivesetup, include=FALSE, eval=TRUE, cache=FALSE----------------------------------
knitr::opts_chunk$set(cache.rebuild=T)


## ----title, echo=F----------------------------------------------------------------------------
# saveWidget(vn, file='vntitle.html')
library(visNetwork)
nodes = data.frame(id=1:2, label=c('Interactive', 'Visualization'))
edges = data.frame(from=1, to=2, color='transparent')
# visNetwork(nodes, edges, height=400, width='100%') %>% 
#   visNodes(shape='text', font=list(color=palettes$tyrian_purple2$tyrian_purple, size=64)) %>% 
#   visOptions(highlightNearest=F) 
visNetwork(nodes, edges, height=400, width='100%') %>% 
  visNodes(shape='circle', 
           color = list(background='#fffff8', 
                        # highlight=col2rgb(palettes$tyrian_purple$tetradic[2], .25)[,1],
                        highlight='rgba(2,102,44,.5)',
                        border=palettes$tyrian_purple2$tyrian_purple),
           borderWidth = 3,
           size=50, 
           label='text', 
           font=list(color=palettes$tyrian_purple2$tyrian_purple,
                     size=24,
                     highlight='#ffffff8'),
           shadow = T) %>% 
  visOptions(highlightNearest=F) 


## ----plotly1----------------------------------------------------------------------------------
library(plotly)

midwest %>%
  filter(inmetro == T) %>%
  plot_ly(x =  ~ percbelowpoverty, y =  ~ percollege) %>%
  add_markers()


## import pandas as pd

## 
## import plotly.express as px

## 
## midwest = pd.DataFrame(r.midwest)  # from previous chunk using reticulate

## 
## plt = px.scatter(midwest, x = 'percbelowpoverty', y = 'percollege')

## 
## plt.show() # opens in browser


## ----basic-plotly-show,  echo=FALSE-----------------------------------------------------------
knitr::include_graphics('img/plotly-basic-python.png')


## ----plotly2----------------------------------------------------------------------------------
library(mgcv)
library(modelr)
library(glue)

mtcars %>%
  mutate(
    amFactor = factor(am, labels = c('auto', 'manual')),
    hovertext = glue('weight: {wt} <br> mpg: {mpg} <br> {amFactor}')
  ) %>%
  add_predictions(gam(mpg ~ s(wt, am, bs = 'fs'), data = mtcars)) %>%
  arrange(am) %>% 
  plot_ly() %>%
  add_markers(
    x =  ~ wt,
    y =  ~ mpg,
    color =  ~ amFactor,
    opacity = .5,
    text =  ~ hovertext,
    hoverinfo = 'text',
    showlegend = F
  ) %>%
  add_lines(
    x =  ~ wt,
    y =  ~ pred,
    color =  ~ amFactor
  )


## ----plotly_1line-----------------------------------------------------------------------------
plot_ly(midwest, x = ~percollege, color = ~state, type = "box")


## plt = px.box(midwest, x = 'state', y = 'percollege', color = 'state', notched=True)

## 
## plt.show() # opens in browser

## 
## tips = px.data.tips()  # built-in dataset

## 
## px.violin(

##   tips,

##   y      = "tip",

##   x      = "smoker",

##   color  = "sex",

##   box    = True,

##   points = "all",

##   hover_data = tips.columns

## ).show()


## ----express-show, out.width='75%', echo=FALSE------------------------------------------------
knitr::include_graphics('img/plotly-box-python.png')
knitr::include_graphics('img/plotly-violin-python.png')


## ----ggplotly---------------------------------------------------------------------------------
gp = mtcars %>%
  mutate(amFactor = factor(am, labels = c('auto', 'manual')),
         hovertext = paste(wt, mpg, amFactor)) %>%
  arrange(wt) %>%
  ggplot(aes(x = wt, y = mpg, color = amFactor)) +
  geom_smooth(se = F) +
  geom_point(aes(color = amFactor))

ggplotly()


## ----highcharts-------------------------------------------------------------------------------
library(highcharter)
library(quantmod)

google_price = getSymbols("GOOG", auto.assign = FALSE)
hchart(google_price)


## ----visNetworkinitial, cache=FALSE-----------------------------------------------------------
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


## ----sigmajs, cache=FALSE---------------------------------------------------------------------
library(sigmajs)

nodes <- sg_make_nodes(30)
edges <- sg_make_edges(nodes)

# add transitions
n <- nrow(nodes)
nodes$to_x <- runif(n, 5, 10)
nodes$to_y <- runif(n, 5, 10)
nodes$to_size <- runif(n, 5, 10)
nodes$to_color <- sample(c("#ff5500", "#00aaff"), n, replace = TRUE)


sigmajs() %>%
  sg_nodes(nodes, id, label, size, color, to_x, to_y, to_size, to_color) %>%
  sg_edges(edges, id, source, target) %>%
  sg_animate(
    mapping = list(
      x = "to_x",
      y = "to_y",
      size = "to_size",
      color = "to_color"
    ),
    delay = 0
  ) %>%
  sg_settings(animationsTime = 3500) %>%
  sg_button("animate", # button label
            "animate", # event name
            class = "btn btn-warning")


## import plotly.graph_objects as go

## import networkx as nx

## 
## G = nx.random_geometric_graph(50, 0.125)

## 
## edge_x = []

## edge_y = []

## for edge in G.edges():

##     x0, y0 = G.nodes[edge[0]]['pos']

##     x1, y1 = G.nodes[edge[1]]['pos']

##     edge_x.append(x0)

##     edge_x.append(x1)

##     edge_x.append(None)

##     edge_y.append(y0)

##     edge_y.append(y1)

##     edge_y.append(None)

## 
## edge_trace = go.Scatter(

##     x=edge_x,

##     y=edge_y,

##     line=dict(width=0.5, color='#888'),

##     hoverinfo='none',

##     mode='lines')

## 
## node_x = []

## node_y = []

## for node in G.nodes():

##     x, y = G.nodes[node]['pos']

##     node_x.append(x)

##     node_y.append(y)

## 
## node_trace = go.Scatter(

##     x=node_x, y=node_y,

##     mode='markers',

##     hoverinfo='text',

##     marker=dict(

##         showscale=True,

##         colorscale='Blackbody',

##         reversescale=True,

##         color=[],

##         size=10,

##         colorbar=dict(

##             thickness=15,

##             title='Node Connections',

##             xanchor='left',

##             titleside='right'

##         ),

##         line_width=2))

## 
## node_adjacencies = []

## node_text = []

## for node, adjacencies in enumerate(G.adjacency()):

##     node_adjacencies.append(len(adjacencies[1]))

##     node_text.append('# of connections: '+str(len(adjacencies[1])))

## 
## node_trace.marker.color = node_adjacencies

## node_trace.text = node_text

## 
## fig = go.Figure(data=[edge_trace, node_trace],

##              layout=go.Layout(

##                 title='<br>Network graph made with Python',

##                 titlefont_size=16,

##                 showlegend=False,

##                 hovermode='closest',

##                 margin=dict(b=20,l=5,r=5,t=40),

##                 annotations=[ dict(

##                     text="Python code: <a href='https://plot.ly/ipython-notebooks/network-graphs/'> https://plot.ly/ipython-notebooks/network-graphs/</a>",

##                     showarrow=False,

##                     xref="paper", yref="paper",

##                     x=0.005, y=-0.002 ) ],

##                 xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),

##                 yaxis=dict(showgrid=False, zeroline=False, showticklabels=False))

##                 )

## fig.show()


## ----network-show, echo=FALSE-----------------------------------------------------------------
knitr::include_graphics('img/plotly-network-python.png')


## ----leaflet----------------------------------------------------------------------------------
hovertext <- paste(sep = "<br/>",
  "<b><a href='http://umich.edu/'>University of Michigan</a></b>",
  "Ann Arbor, MI"
)

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPopups(lng = -83.738222,
            lat = 42.277030,
            popup = hovertext)


## ----datatable--------------------------------------------------------------------------------
library(DT)

ggplot2movies::movies %>%
  select(1:6) %>%
  filter(rating > 8, !is.na(budget), votes > 1000) %>% 
  datatable()


## ----datatable_options------------------------------------------------------------------------
iris %>%
  # arrange(desc(Petal.Length)) %>%
  datatable(rownames = F,
            options = list(dom = 'firtp'),
            class = 'row-border') %>%
  formatStyle('Sepal.Length',
              fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle('Sepal.Width',
              color = styleInterval(c(3.4, 3.8), c('#7f7f7f', '#00aaff', '#ff5500')),
              backgroundColor = styleInterval(3.4, c('#ebebeb', 'aliceblue'))) %>%
  formatStyle(
    'Petal.Length',
    # color = 'transparent',
    background         = styleColorBar(iris$Petal.Length, '#5500ff'),
    backgroundSize     = '100% 90%',
    backgroundRepeat   = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'Species',
    color = 'white',
    transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    backgroundColor = styleEqual(unique(iris$Species), c('#1f65b7', '#66b71f', '#b71f66'))
  )


## ----shiny, eval=FALSE------------------------------------------------------------------------
## library(shiny)
## 
## # Running a Shiny app object
## app <- shinyApp(
## 
##   ui = bootstrapPage(
##     numericInput('n', 'Number of obs', 10),
##     plotOutput('plot')
##   ),
## 
##   server = function(input, output) {
##     output$plot <- renderPlot({
##       ggplot2::qplot(rnorm(input$n), xlab = 'Is this normal?!')
##     })
##   }
## 
## )
## 
## runApp(app)


## ----dash-example, eval=FALSE-----------------------------------------------------------------
## library(dash)
## library(dashCoreComponents)
## library(dashHtmlComponents)
## 
## app <- Dash$new()
## 
## df <- readr::read_csv(file = "data/gapminder_small.csv") %>%
##   drop_na()
## 
## continents <- unique(df$continent)
## 
## data_gdp_life <- with(df,
##   lapply(continents,
##          function(cont) {
##            list(
##              x = gdpPercap[continent == cont],
##              y = lifeExp[continent == cont],
##              opacity=0.7,
##              text = country[continent == cont],
##              mode = 'markers',
##              name = cont,
##              marker = list(size = 15,
##                            line = list(width = 0.5, color = 'white'))
##            )
##          }
##   )
## )
## 
## app$layout(
##   htmlDiv(
##     list(
##       dccGraph(
##         id = 'life-exp-vs-gdp',
##         figure = list(
##           data =  data_gdp_life,
##           layout = list(
##             xaxis  = list('type' = 'log', 'title' = 'GDP Per Capita'),
##             yaxis  = list('title' = 'Life Expectancy'),
##             margin = list('l' = 40, 'b' = 40, 't' = 10, 'r' = 10),
##             legend = list('x' = 0, 'y' = 1),
##             hovermode = 'closest'
##           )
##         )
##       )
##     )
##   )
## )
## 
## app$run_server()


## # -*- coding: utf-8 -*-

## import dash

## import dash_core_components as dcc

## import dash_html_components as html

## import pandas as pd

## 
## external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

## 
## app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

## 
## df = pd.read_csv('data/gapminder_small.csv')

## 
## 
## app.layout = html.Div([

##     dcc.Graph(

##         id='life-exp-vs-gdp',

##         figure={

##             'data': [

##                 dict(

##                     x=df[df['continent'] == i]['gdpPercap'],

##                     y=df[df['continent'] == i]['lifeExp'],

##                     text=df[df['continent'] == i]['country'],

##                     mode='markers',

##                     opacity=0.7,

##                     marker={

##                         'size': 15,

##                         'line': {'width': 0.5, 'color': 'white'}

##                     },

##                     name=i

##                 ) for i in df.continent.unique()

##             ],

##             'layout': dict(

##                 xaxis={'type': 'log', 'title': 'GDP Per Capita'},

##                 yaxis={'title': 'Life Expectancy'},

##                 margin={'l': 40, 'b': 40, 't': 10, 'r': 10},

##                 legend={'x': 0, 'y': 1},

##                 hovermode='closest'

##             )

##         }

##     )

## ])

## 
## if __name__ == '__main__':

##     app.run_server(debug=True)

## 

## 

## ----dash-python-show, echo=FALSE-------------------------------------------------------------
knitr::include_graphics('img/plotly-dash-python.png')


## ----interactive_ex1, eval=FALSE--------------------------------------------------------------
## library(ggplot2movies)
## 
## movies %>%
##   group_by(year) %>%
##   summarise(Avg_Rating = mean(rating))
## plot_ly() %>%
##   add_markers()


## ----interactive_ex1-hint, echo=FALSE---------------------------------------------------------
movies %>%
  group_by(year) %>%
  summarise(Avg_Rating = mean(rating)) %>%
  plot_ly() %>%
  add_markers(x =  ~ year, y =  ~ Avg_Rating)


## ----interactive_ex2, echo=FALSE--------------------------------------------------------------
movies %>%
  group_by(year, Drama) %>%
  summarise(Avg_Rating = mean(rating),
            Avg_Votes = mean(votes)) %>%
  plot_ly() %>%
  add_markers(
    x =  ~ year,
    y =  ~ Avg_Rating,
    size =  ~ Avg_Votes,
    color =  ~ Drama
  )

