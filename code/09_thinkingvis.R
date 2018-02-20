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
  theme_trueMinimal()+ 
  theme(legend.position="none")  # only way to keep plotly from putting a legend
sw2 %>%
  mutate(bmi = mass/((height/100)^2)) %>% 
  ggplot(aes(x=mass, y=height)) +
  geom_point(aes(label=name, size=bmi), color=palettes$orange$orange, alpha=.25, show.legend=F) +
  scale_radius(range=c(2,10)) + 
  theme_trueMinimal()+ 
  theme(legend.position="none")  # only way to keep plotly from putting a legend

## ----transp_old, eval=FALSE----------------------------------------------
## N = 100
## obs = 10
## 
## g = rep(1:N, e=obs)
## x = rep(1:obs, N)
## f = rep(c(-.5, .5), e=N/2)[g]
## # sig = createCorr(c(-.75,-.25,.25))
## # re = mvtnorm::rmvnorm(N, sigma=sig)
## re = mvtnorm::rmvnorm(N, sigma=diag(c(.5,.05,.025)))
## y = re[,1][g] + (.25+re[,2][g])*x + (re[,3][g]*f)*(x^2) + rnorm(N*obs, sd=.2)
## library(lme4)
## xsq = x^2
## lmer(y ~ x + xsq + (1+x+xsq |g))
## # y = scale(y)[,1]
## 
## # lm(y~poly(x,2)) %>% summary()
## 
## #
## # gg = data_frame(g, x, y) %>%
## #   mutate(y = scale(y)) %>%
## #   ggplot(aes(x,y)) +
## #   geom_smooth(aes(group=g), se=F, color=alpha(palettes$orange$orange, .2), lwd=.5)
## # ggplotly() %>%
## #   theme_plotly()
## data_frame(g, f=factor(f, labels=c('g1','g2')), x, y) %>%
##   group_by(g) %>%
##   mutate(updown = if_else(last(y) > first(y), 'up', 'down')) %>%
##   # ungroup() %>%
##   plot_ly() %>%
##   add_lines(x=~x, y=~y,  color=~f, opacity=.25, showlegend=F) %>%
##   add_markers(x=~x, y=~y, color=~f, opacity=.75) %>%
##   theme_plotly()

## ----transp, out.width='75%', fig.asp=.5---------------------------------
########################################################################################
### 'Noisy' gaussian process demo.  The matrix labeling is in keeping with Murphy    ###
### 2012 and Rasmussen and Williams 2006.  See those sources for more detail.        ###
### Murphy's matlab code can be found here: https://code.google.com/p/pmtk3/, though ###
### the relevant files are housed alongside this code.                               ###
###                                                                                  ###
### The goal of this code is to plot samples from the prior and posterior predictive ###
### of a gaussian process in which y = sin(x) + noise. It will reproduce an example  ###
### akin to figure 15.3 in Murphy 2012.                                              ###
########################################################################################


#################
### Functions ###
#################

# the mean function; in this case mean=0
muFn = function(x){
  x = sapply(x, function(x) x=0)
  x
}

# The covariance function; here it is the squared exponential kernel.
# l is the horizontal scale, sigmaf is the vertical scale, sigman the noise.
# See ?covSEiso in the gpr package for example, which is also based on Rasmussen and
# Williams Matlab code (gpml Matlab library)

Kfn = function(x, y=NULL, l=1, sigmaf=1, sigman=.5){
  if(!is.null(y)){
    sigmaf * exp( -(1/(2*l^2)) * as.matrix(dist(x, upper=T, diag=T)^2) ) + sigman*diag(length(x))    
  }
  else{
    sigmaf * exp( -(1/(2*l^2)) * as.matrix(dist(x, upper=T, diag=T)^2) )
  }  
}

#####################
### Preliminaries ###
#####################
set.seed(1234)
l = 1           # for l, sigmaf, sigman, see note at covariance function
sigmaf = 1      
sigman = .25 
keps = 1e-8     # see note at Kstarstar
nprior = 50      # number of prior draws
npostpred = 10   # number of posterior predictive draws

##################
### Prior plot ###
##################

### data setup
xg1 = seq(-5, 5, .2)
yg1 = mvtnorm::rmvnorm(nprior, 
                       mean=muFn(xg1), 
                       sigma=Kfn(xg1, l=l, sigmaf=sigmaf, sigman=sigman)) 

### plot
# reshape data for plotting
palviridis = viridis::plasma(4)
gdat1 = gather(data.frame(x=xg1, 
                          y=t(yg1), 
                          sd=apply(yg1, 2, sd)),
               key=variable,
               value=value,
               -x, -sd)


g1 = ggplot(aes(x=x, y=value), data=gdat1) + 
  geom_line(aes(group=variable), color='#FF5500', alpha=.15) +
  labs(title='Prior') +
  theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# g1 # show plot

####################################
### generate noisy training data ###
####################################

Xtrain = 15*(runif(20)-.5)  
nTrain = length(Xtrain)
ytrain = sin(Xtrain) + rnorm(n=nTrain, sd=.1)  # kept sine function for comparison to noise free result

Xtest = seq(-7.5, 7.5, length=200)
nTest = length(Xtest)

#####################################
### generate posterior predictive ###
#####################################

### Create Ky, K*, and K** matrices as defined in the texts
Ky = Kfn(x=Xtrain, y=ytrain, l=l, sigmaf=sigmaf, sigman=sigman)
K_ = Kfn(c(Xtrain, Xtest), l=l, sigmaf=sigmaf, sigman=sigman)                    # initial matrix
Kstar = K_[1:nTrain, (nTrain+1):ncol(K_)]                                        # dim = N x N*
tKstar = t(Kstar)                                                                # dim = N* x N
Kstarstar = K_[(nTrain+1):nrow(K_), (nTrain+1):ncol(K_)] + keps*diag(nTest)      # dim = N* x N*; the keps part is for positive definiteness
Kyinv = solve(Ky)

# calculate posterior mean and covariance
postMu = muFn(Xtest) + tKstar %*% Kyinv %*% (ytrain-muFn(Xtrain))
postCov = Kstarstar - tKstar %*% Kyinv %*% Kstar
s2 = diag(postCov)
# R = chol(postCov)  
# L = t(R)      # L is used in alternative formulation below based on gaussSample.m

# generate draws from posterior predictive
y2 = data.frame(t(mvtnorm::rmvnorm(npostpred, mean=postMu, sigma=postCov)))
# y2 = data.frame(replicate(npostpred, postMu + L %*% rnorm(postMu))) # alternative

#################################
### Posterior predictive plot ###
#################################

# reshape data for plotting
gdat2 = gather(data.frame(x=Xtest, 
                         y=y2, 
                         fmean=postMu, 
                         selower=postMu-2*sqrt(s2), 
                         seupper=postMu+2*sqrt(s2)),
              key=variable,
              value=value, 
              -x, -fmean, -selower, -seupper)


# g2 = ggplot(aes(x=x, y=value), data=gdat) + 
#   geom_ribbon(aes(ymin=selower, ymax=seupper,group=variable), fill='#00aaff', alpha=.01) +
#   geom_line(aes(group=variable), color='#FF5500', alpha=.5) +
#   geom_line(aes(group=variable, y=fmean), color='#5500ff', size=1, alpha=.5) +
#   geom_point(aes(x=Xtrain, y=ytrain), color='#aaff00', alpha=.5, data=data.frame(Xtrain, ytrain)) +
#   labs(title='Posterior Predictive') +
#   theme_trueMinimal()

g2 = ggplot(aes(x=x, y=value), data=gdat2) + 
  geom_ribbon(aes(ymin=selower, ymax=seupper,group=variable), fill=palviridis[1], alpha=.01) +
  geom_line(aes(group=variable), color=palviridis[2], alpha=.25) +
  geom_line(aes(group=variable, y=fmean), color=palviridis[3], size=1, alpha=.5) +
  geom_point(aes(x=Xtrain, y=ytrain), color=palviridis[4], alpha=1, size=2, data=data.frame(Xtrain, ytrain)) +
  labs(title='Posterior Predictive') +
  theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# g2

####################################################
### Plot prior and posterior predictive together ###
####################################################

# there is no way with knitr to control the size of this grid object in a
# logical fashion. The bookdown notes on figures did not work even remotely as
# described. The only success was found with out.width + fig.asp, so that was
# set as a default knitr setting
library(gridExtra)
grid.arrange(g1, g2, ncol=2)

## ----transp2_old, eval=FALSE---------------------------------------------
## data_frame(g, x, y) %>%
##   group_by(g) %>%
##   mutate(updown = if_else(last(y) > first(y), 'up', 'down')) %>%
##   # ungroup() %>%
##   plot_ly() %>%
##   add_lines(x=~x, y=~y,  color=~updown, showlegend=F) %>%
##   add_markers(x=~x, y=~y, color=~updown) %>%
##   theme_plotly()

## ----transp2, out.width='75%', fig.asp=.5--------------------------------
g1 = ggplot(aes(x=x, y=value), data=gdat1) + 
  geom_line(aes(group=variable), color='#FF5500', alpha=1) +
  labs(title='Prior') +
  theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
g2 = ggplot(aes(x=x, y=value), data=gdat2) + 
  geom_ribbon(aes(ymin=selower, ymax=seupper,group=variable), fill=palviridis[1], alpha=1) +
  geom_line(aes(group=variable), color=palviridis[2], alpha=1) +
  geom_line(aes(group=variable, y=fmean), color=palviridis[3], size=1, alpha=1) +
  geom_point(aes(x=Xtrain, y=ytrain), color=palviridis[4], alpha=1, size=2, data=data.frame(Xtrain, ytrain)) +
  labs(title='Posterior Predictive') +
  theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
grid.arrange(g1, g2, ncol=2)

## ----transp_tornado, eval=T----------------------------------------------
# depending on the plot, plotly may confuse opacity with some other quality of the color
# data(flights, package='threejs') 
# flights %>% 
#   plot_ly(x=~origin_long, y=~origin_lat) %>% 
#   add_markers(opacity=.20, size=I(3), color=I('#ff5500')) %>% 
#   theme_plotly()
# 
# load('../../Other/Tornado/data/tornados.RData')
# g <- list(
#   scope = 'usa',
#   projection = list(type = 'albers usa'),
#   showland = TRUE,
#   landcolor = toRGB("gray95"),
#   subunitcolor = toRGB("gray85"),
#   countrycolor = toRGB("gray85"),
#   countrywidth = 0.5,
#   subunitwidth = 0.5
# )
# 
# tornados %>% 
#   filter(Year>=2010) %>% 
#   plot_geo(lat = ~StartLat, lon = ~StartLon) %>%
#   add_markers(marker=list(size = 5), color=I('#ff5500'), opacity=.25) %>%
#   layout(title = 'U.S. Tornados 1990-2015', geo = g) %>% 
#   theme_plotly()

library(mclust)
sim_faithful = Mclust(faithful, G=2) %>% summary()

nsim = 500
g1 = mvtnorm::rmvnorm(nsim, sim_faithful$mean[,1], sigma=sim_faithful$variance[,,1])
g2 = mvtnorm::rmvnorm(nsim, sim_faithful$mean[,2], sigma=sim_faithful$variance[,,2])
gdat = data.frame(rbind(g1, g2), group=rep(0:1, e=nsim)) 

gdat %>%
  ggplot(aes(x=waiting, y=eruptions)) +
  geom_point(size = 3, alpha=.25, color='#ff5500') +
  theme_trueMinimal()

## ----transp_density------------------------------------------------------
detach(package:mclust)
set.seed(123)
gdat = 1:4 %>% 
  map(~rnorm(20, mean=.x, sd=.x/2)) %>% 
  data.frame() %>% 
  rename_all(function(x) paste0('x', 1:4)) %>% 
  gather(key=g) %>% 
  mutate(g = fct_relevel(g, 'x1', after=2))

gdat %>% 
  ggplot(aes(x=value, group=g)) +
  geom_density(aes(color=g, fill=g), alpha=.2, show.legend=F) +
  xlim(-3,10) +
  theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

## ----transp_density2-----------------------------------------------------
gdat %>% 
  ggplot(aes(x=value, group=g)) +
  geom_density(aes(color=g, fill=g), show.legend=F) +
  xlim(-3,10) +
  theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

## ----thinkingvis_ex1, echo=T, eval=FALSE, cache=FALSE--------------------
## library(ggplot2); library(viridis)
## ggplot(aes(x=carat, y=price), data=diamonds) +
##   geom_point(aes(color=price)) +
##   ????

## ----thinkingvis_ex1b, eval=F, out.width='50%'---------------------------
## library(ggplot2); library(viridis)
## ggplot(aes(x=carat, y=price), data=diamonds) +
##   geom_point(aes(color=price)) +
##   scale_color_viridis()

## ----thinkingvis_ex2, eval=FALSE, out.width='50%'------------------------
## ggplot(aes(x=carat, y=price), data=diamonds) +
##   geom_point(aes(color=cut)) +
##   scale_color_viridis(discrete=T)

