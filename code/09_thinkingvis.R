## ----vissetup, include=FALSE, eval=TRUE, cache=FALSE---------------------
knitr::opts_chunk$set(eval=T, echo=F)

## ----problems------------------------------------------------------------
library(plotly)
sw_height = starwars %>% 
  filter(gender %in% c('male', 'female')) %>% 
  group_by(gender) %>% 
  summarise(Height=mean(height, na.rm=T),
            N= n(),
            se = sd(height, na.rm=T)/sqrt(N))
sw_height %>% 
  ggplot(aes(x=gender, y=Height)) +
  geom_bar(aes(), color='black', stat='identity', width=.5) +
  scale_fill_manual(values=c('red1', 'green')) +
  labs(title='Height of Some Star Wars Characters') +
  theme_bw()

## ----problems2-----------------------------------------------------------
sw_height %>% 
  ggplot(aes(x=gender, y=Height)) +
  geom_errorbar(aes(ymin=Height-2*se, ymax=Height+2*se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  geom_bar(aes(fill=gender), color='black', stat='identity', width=.5) +
  scale_fill_manual(values=c('red1', 'green')) +
  coord_cartesian(ylim=c(140,200)) +
  labs(title='Height of Some Star Wars Characters') +
  theme_bw()

## ----ugly----------------------------------------------------------------
starwars %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(x=mass, y=height)) +
  geom_point(aes(color=gender), size=6) + 
  geom_text(aes(label=name), size=2) + 
  geom_smooth(method='lm', se=F, color='red', lwd=2) + 
  scale_color_manual(values=c('red1', 'green')) +
  theme_linedraw() +
  theme(plot.background=element_rect(color='black'),
        panel.background=element_rect(fill='gray92'),
        panel.grid.major.x=element_line(size=2)
        )

## ----badbw---------------------------------------------------------------
starwars %>% 
  filter(gender %in% c('male', 'female')) %>% 
  ggplot(aes(x=mass, y=height)) +
  geom_point(aes(color=gender), size=6) + 
  geom_text(aes(label=name), size=2) + 
  geom_smooth(method='lm', se=F, color='black', lwd=2) + 
  scale_color_grey(start=.6, end=.4) +
  theme_linedraw() +
  theme(plot.background=element_rect(color='black'),
        panel.background=element_rect(fill='gray92'),
        panel.grid.major.x=element_line(size=2)
        )

## ----better--------------------------------------------------------------
sw2 = starwars %>% 
  filter(gender %in% c('male', 'female')) %>% 
  mutate(homeworld2 = ifelse(homeworld=='Tatooine', 'Tatooine', 'Naboo'),
         homeworld2 = ifelse(homeworld!='Tatooine'& homeworld!='Naboo', 'Other', homeworld2),
         homeworld2 = ifelse(is.na(homeworld2), 'Other', homeworld2)) %>% 
  select(name, mass, height, birth_year, gender, homeworld, homeworld2) %>% ungroup


# adding label=name will add name to plotly tooltip; ggplot will ignore
g = sw2 %>% 
  select(-homeworld) %>% 
  rename(homeworld=homeworld2) %>% 
  ggplot(aes(x=mass, y=height)) +
  geom_smooth(color=lazerhawk::palettes$orange$tetradic[4], se=F, size=.5, alpha=.15) +
  geom_point(aes(label=name, color=gender, shape=homeworld, size=birth_year), alpha=.5, show.legend=F) +
  scale_color_manual(values=lazerhawk::palettes$orange$complementary) +
  # geom_text(aes(label=name)) +
  scale_size_continuous(range=c(2,10)) +
  lazerhawk::theme_trueMinimal()+ 
  theme(legend.position="none")  # only way to keep plotly from putting a legend

ggplotly()%>% 
  # add_lines(x=~mass, y=~height)
  config(displayModeBar = F) %>% 
  layout(title='Star Wars Characters',
         font=list(family='Roboto'),
         xaxis=list(title='Mass'),
         yaxis=list(title='Height'))

# 
# sw2 %>% #drop_na() %>% 
#   # group_by(gender) %>% 
#   plot_ly(split=~gender) %>% 
#   add_markers(x=~mass, y=~height, color=~homeworld2,# colors=c("#1f77b4", "#ff7f0e"),
#               text=~name, size=~birth_year, opacity=.85,# symbol=~gender,
#               sizes=c(50,100)) %>% 
#   lazerhawk::theme_plotly()

## ----scale_size, out.width='50%'-----------------------------------------
sw2 %>% 
  mutate(bmi = mass/((height/100)^2)) %>% 
  ggplot(aes(x=mass, y=height)) +
  geom_point(aes(label=name,  size=bmi), color=palettes$orange$orange, alpha=.25, show.legend=F) +
  scale_size_continuous(range=c(2,10)) +
  lazerhawk::theme_trueMinimal()+ 
  theme(legend.position="none")  # only way to keep plotly from putting a legend
sw2 %>%
  mutate(bmi = mass/((height/100)^2)) %>% 
  ggplot(aes(x=mass, y=height)) +
  geom_point(aes(label=name, size=bmi), color=palettes$orange$orange, alpha=.25, show.legend=F) +
  scale_radius() + 
  lazerhawk::theme_trueMinimal()+ 
  theme(legend.position="none")  # only way to keep plotly from putting a legend

## ----transp--------------------------------------------------------------
N = 100
obs = 10

g = rep(1:N, e=obs)
x = rep(1:obs, N)
f = rep(c(-.5, .5), e=N/2)[g]
# sig = createCorr(c(-.75,-.25,.25))
# re = mvtnorm::rmvnorm(N, sigma=sig)
re = mvtnorm::rmvnorm(N, sigma=diag(c(.5,.05,.025)))
y = re[,1][g] + (.25+re[,2][g])*x + (re[,3][g]*f)*(x^2) + rnorm(N*obs, sd=.2)
library(lme4)
xsq = x^2
lmer(y ~ x + xsq + (1+x+xsq |g))
# y = scale(y)[,1]

# lm(y~poly(x,2)) %>% summary()

# 
# gg = data_frame(g, x, y) %>% 
#   mutate(y = scale(y)) %>% 
#   ggplot(aes(x,y)) + 
#   geom_smooth(aes(group=g), se=F, color=alpha(palettes$orange$orange, .2), lwd=.5)
# ggplotly() %>% 
#   theme_plotly()
data_frame(g, f=factor(f, labels=c('g1','g2')), x, y) %>% 
  group_by(g) %>% 
  mutate(updown = if_else(last(y) > first(y), 'up', 'down')) %>% 
  # ungroup() %>%
  plot_ly() %>% 
  add_lines(x=~x, y=~y,  color=~f, opacity=.25, showlegend=F) %>%
  add_markers(x=~x, y=~y, color=~f, opacity=.75) %>% 
  theme_plotly()

## ----transp2-------------------------------------------------------------
data_frame(g, x, y) %>% 
  group_by(g) %>% 
  mutate(updown = if_else(last(y) > first(y), 'up', 'down')) %>% 
  # ungroup() %>%
  plot_ly() %>% 
  add_lines(x=~x, y=~y,  color=~updown, showlegend=F) %>%
  add_markers(x=~x, y=~y, color=~updown) %>% 
  theme_plotly()

## ----transp_tornado, eval=F----------------------------------------------
## # data(flights, package='threejs')
## # flights %>%
## #   plot_ly(x=~origin_long, y=~origin_lat) %>%
## #   add_markers(opacity=.20, size=I(3), color=I('#ff5500')) %>%
## #   theme_plotly()
## 
## load('../../Other/Tornado/data/tornados.RData')
## g <- list(
##   scope = 'usa',
##   projection = list(type = 'albers usa'),
##   showland = TRUE,
##   landcolor = toRGB("gray95"),
##   subunitcolor = toRGB("gray85"),
##   countrycolor = toRGB("gray85"),
##   countrywidth = 0.5,
##   subunitwidth = 0.5
## )
## 
## tornados %>%
##   filter(Year>=2010) %>%
##   plot_geo(lat = ~StartLat, lon = ~StartLon) %>%
##   add_markers(size = I(3.5), color=I('#ff5500'), opacity=.25) %>%
##   layout(title = 'U.S. Tornados 1990-2015', geo = g) %>%
##   theme_plotly()
## 

## ----thinkingvis_ex1, echo=T, eval=FALSE, cache=FALSE--------------------
## library(ggplot2); library(viridis)
## ggplot(aes(x=carat, y=price), data=diamonds) +
##   geom_point(aes(color=price)) +
##   ????

## ----thinkingvis_ex1b, echo=F, out.width='50%'---------------------------
library(ggplot2); library(viridis)
ggplot(aes(x=carat, y=price), data=diamonds) +
  geom_point(aes(color=price)) +
  scale_color_viridis()

## ----thinkingvis_ex2, out.width='50%'------------------------------------
ggplot(aes(x=carat, y=price), data=diamonds) +
  geom_point(aes(color=cut)) +
  scale_color_viridis(discrete=T)

