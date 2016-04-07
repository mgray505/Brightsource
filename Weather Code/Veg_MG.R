library(plyr)
library(dplyr)
library(tidyr)
setwd('E:/Projects/BrightSource_Ivanpah/data/Databases/Veg_thruSept2015')

spring2012 <- read.csv('spring2012_pointIntercept.csv') %>% 
  rename(PLOT=Plot,SPECIES=Species) %>% 
  select(PLOT,SPECIES) %>% 
  mutate(DATE='Spring2012')
spring2013 <- read.csv('EMPVegDatabaseSpring2013.csv', header=T) %>% 
  select(PLOT,SPECIES) %>% 
  mutate(DATE='Spring2013')
fall2013 <- read.csv('EMPVegDatabaseFall2013.csv', header=T) %>% 
  select(PLOT,SPECIES) %>% 
  mutate(DATE='Fall2013')
spring2014files <- list.files(pattern='2014 Spring*',full.names=T)
spring2014 <- adply(spring2014files, 1, read.csv) %>% 
  select(PLOT,SPECIES) %>% 
  mutate(DATE='Spring2014')
fall2014files <- list.files(pattern='2014 Fall*',full.names=T)
fall2014 <- adply(fall2014files, 1, read.csv) %>% 
  select(PLOT,SPECIES) %>% 
  mutate(DATE='Fall2014')
spring2015files <- list.files(pattern='2015 Spring*',full.names=T)
spring2015 <- adply(spring2015files, 1, read.csv) %>% 
  select(PLOT,SPECIES) %>% 
  mutate(DATE='Spring2015') 
fall2015files <- list.files(pattern='2015 Fall*',full.names=T)
fall2015 <- adply(fall2015files, 1, read.csv) %>% 
  select(PLOT,SPECIES) %>% 
  mutate(DATE='Fall2015')


allVeg = na.omit(rbind(spring2012,spring2013,fall2013,spring2014,fall2014,spring2015,fall2015)) %>% 
  filter(!(SPECIES %in% c('BARE GROUND','LITTER','ROCK','Bare Ground','Litter Ground','Litter Standing','Rock',0))) 
  


plots = unique(allVeg$PLOT)

for (i in 1:length(plots)){
  iplot = allVeg[allVeg$PLOT==plots[i],]
  byPlot = iplot %>% group_by(DATE,Life.cycle) %>% summarise(count=n_distinct(SPECIES))
  byPlot$Plot <- plots[i]
  if (i==1){ byPlotDiversity=byPlot} else
    byPlotDiversity = rbind(byPlot,byPlotDiversity)} 

byPlotDiversity = byPlotDiversity %>% 
  group_by(DATE) %>% 
  summarise(mean=mean(count),sd=sd(count))




