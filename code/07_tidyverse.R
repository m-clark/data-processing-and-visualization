## ----tvsetup, include=FALSE, eval=TRUE, cache=FALSE----------------------
knitr::opts_chunk$set(echo = T, eval=T)

## ----tv_load, eval=FALSE-------------------------------------------------
## library(tidyverse)

## ----tv_startup_message, echo=FALSE, message=TRUE------------------------
"Loading tidyverse: ggplot2
Loading tidyverse: tibble
Loading tidyverse: tidyr
Loading tidyverse: readr
Loading tidyverse: purrr
Loading tidyverse: dplyr
Conflicts with tidy packages -------------------------
filter(): dplyr, stats
lag():    dplyr, stats" %>% 
  message()

## ----baseRexample1a, eval=FALSE------------------------------------------
## newData = oldData[,c(1,2,3,4, etc.)]

## ----baseRexample1b, eval=FALSE------------------------------------------
## newData = oldData[,c('ID','X1', 'X2', etc.)]

## ----baseRexample1c, eval=FALSE------------------------------------------
## cols = c('ID', paste0('X', 1:10), 'var1', 'var2', grep(colnames(oldData), '^XYZ', value=T))
## 
## newData = oldData[,cols]
## 
## # or via subset
## newData = subset(oldData, select = cols)

## ----baseRexample2, eval=FALSE-------------------------------------------
## # three operations and overwriting or creating new objects if we want clarity
## newData = newData[oldData$Z == 'Yes' & oldData$Q == 'No',]
## newData = newData[order(newData$var2, decreasing=T)[1:50],]
## newData = newData[order(newData$var1, decreasing=T),]

## ----pipeExample, eval=FALSE---------------------------------------------
## newData = oldData %>%
##   filter(Z == 'Yes', Q == 'No') %>%
##   select(num_range('X', 1:10), contains('var'), starts_with('XYZ')) %>%
##   top_n(n=50, var2) %>%
##   arrange(desc(var1))

## ----basketballDataScrape, eval=FALSE------------------------------------
## library(rvest)
## url = "http://www.basketball-reference.com/leagues/NBA_2017_totals.html"
## bball = read_html(url) %>%
##   html_nodes("#totals_stats") %>%
##   html_table %>%
##   data.frame
## save(bball, file='data/bball.RData')

## ----load_bball----------------------------------------------------------
load('data/bball.RData')
glimpse(bball[,1:5])

## ----select1-------------------------------------------------------------
bball %>% 
  select(Player, Tm, Pos) %>% 
  head

## ----select2-------------------------------------------------------------
bball %>%     
  select(-Player, -Tm, -Pos)  %>% 
  head

## ----select3-------------------------------------------------------------
bball %>% 
  select(Player, contains("3P"), ends_with("RB")) %>% 
  arrange(desc(TRB)) %>% 
  head

## ----filter0-------------------------------------------------------------
bball = bball %>% 
  filter(Rk != "Rk")

## ----filter1-------------------------------------------------------------
bball %>% 
  filter(Age > 35, Pos == "SF" | Pos == "PF") %>% 
  distinct(Player, Pos, Age)              

## ----filter2-------------------------------------------------------------
bball %>% 
  slice(1:10)

## ----uniteFilterArrange--------------------------------------------------
bball %>% 
  unite("posTeam", Pos, Tm) %>%         # create a new variable
  filter(posTeam == "PF_SAS") %>%       # use it for filtering
  select(Player, posTeam, Age) %>%      # use it for selection
  arrange(desc(Age))                    # order 

## ----mutateAt------------------------------------------------------------
bball = bball %>% 
  mutate_at(vars(-Player, -Pos, -Tm), funs(as.numeric))   

glimpse(bball[,1:7])

## ----mutate--------------------------------------------------------------
bball = bball %>% 
  mutate(trueShooting = PTS / (2 * (FGA + (.44 * FTA))),
         effectiveFG = (FG + (.5 * X3P)) / FGA, 
         shootingDif = trueShooting - FG.)

summary(select(bball, shootingDif))  # select and others don't have to be piped to use

## ----groupby-------------------------------------------------------------
bball %>%   
  mutate(trueShooting = PTS / (2 * (FGA + (.44 * FTA))),
         effectiveFG = (FG + (.5 * X3P)) / FGA, 
         shootingDif = trueShooting - FG.) %>%  
  select(Player, Tm, Pos, MP, trueShooting, effectiveFG, PTS) %>% 
  group_by(Pos) %>%                                                 
  summarize(meanTrueShooting = mean(trueShooting, na.rm = TRUE))    

## ----do------------------------------------------------------------------
bball %>% 
  mutate(Pos = if_else(Pos=='PF-C', 'C', Pos)) %>% 
  group_by(Pos) %>%     
  do(FgFt_Corr=cor(.$FG., .$FT., use='complete')) %>% 
  unnest(FgFt_Corr)

## ----do2-----------------------------------------------------------------
library(nycflights13)
carriers = group_by(flights, carrier)
group_size(carriers)

mods = do(carriers, model = lm(arr_delay ~ dep_time, data = .)) # reminder that data frames are lists
mods %>% 
  summarize(rsq = summary(model)$r.squared) %>% 
  head

## ----merge_demo, echo=-(1:2), message=TRUE-------------------------------
band_members = data_frame(Name = c('Seth', 'Francis', 'Bubba'),
                          Band = c('Com Truise', 'Pixies', 'The New Year'))
band_instruments = data_frame(Name = c('Seth', 'Francis', 'Bubba'),
                              Instrument = c('Synthesizer', 'Guitar', 'Guitar'))

band_members
band_instruments

left_join(band_members, band_instruments)

## ----gather_spread-------------------------------------------------------
library(tidyr)
stocks <- data.frame( time = as.Date('2009-01-01') + 0:9,
                      X = rnorm(10, 0, 1),
                      Y = rnorm(10, 0, 2),
                      Z = rnorm(10, 0, 4) )
stocks %>% head
stocks %>% 
  gather(stock, price, -time) %>% 
  head

## ----tidyrSpread---------------------------------------------------------
bball %>% 
  separate(Player, into=c('firstName', 'lastName'), sep=' ') %>% 
  select(1:5) %>% 
  head

## ----purr_example, eval=FALSE--------------------------------------------
## x = list(1:3, 4:6, 7:9)
## x %>%
##   map(sum)

## ----purr_example2, echo=FALSE-------------------------------------------
x = list(1:3, 4:6, 7:9)
x %>% 
  purrr::map(sum)

## ---- eval=FALSE---------------------------------------------------------
## install.packages('ggplot2movies')
## library(ggplot2movies)
## data('movies')

## ----ex1a_1, eval=FALSE--------------------------------------------------
## data %>%
##   mutate(new_var_name = '?')

## ----ex1a_2, echo=FALSE, eval=FALSE--------------------------------------
## movies %>%
##   mutate(ratingCen = rating - mean(rating))

## ----ex1b, echo=FALSE, eval=FALSE----------------------------------------
## movies %>%
##   filter(year >= 2000)

## ----ex1c, echo=FALSE, eval=FALSE----------------------------------------
## movies %>%
##   select(title, year, budget, length, rating, votes)
## movies %>%
##   select(1:6)
## movies %>%
##   select(-num_range('r',1:10), -mpaa, -starts_with('A'), -Comedy, -starts_with('D'), -Romance, -Short)

## ----ex2, echo=FALSE, eval=FALSE-----------------------------------------
## movies %>%
##   group_by(year) %>%
##   summarise(AvgBudget=mean(budget, na.rm=T)) %>%
##   tail

## ----ex3, echo=FALSE-----------------------------------------------------
movies %>%
  filter(year>=1990) %>% 
  select(title, year, budget, length, rating, votes, mpaa, Action, Drama) %>% 
  group_by(mpaa, Drama) %>% 
  summarise(AvgRating = mean(rating))

