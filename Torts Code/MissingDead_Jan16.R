library(rgdal)

library(dplyr)
 
library(ggplot2)
library(scales)
library(lubridate)
setwd("E:/Projects/BrightSource_Ivanpah/data/Databases/CSVs")
source('E:/Projects/BrightSource_Ivanpah/code/Filter Encounters.R')
detach(package:plyr) 

Totals.postShort <- Enc.postShort %>% 
  group_by(TortType2) %>% 
  summarise(total=n_distinct(TortoiseID))

Totals.postLong <-  Enc.postLong %>% 
  group_by(TortType2) %>% 
  summarise(total=n_distinct(TortoiseID))

which.missing <- filter(Enc.postShort,TortoiseStatus %in% c('Tracked (Missing)')) %>% 
  join(Totals.postShort,by='TortType2') %>% 
  group_by(TortType2) %>%
  summarise(count=n_distinct(TortoiseID),total=first(total)) %>% 
  mutate(proportion = count/total)

zeros<-data.frame(c(2012,2013,2015,2012), c(0,0,0,0),c(17,17,17,35)) %>% 
  mutate_each(funs(as.numeric)) %>% 
  cbind(c('Translocatee (Long 2012)','Translocatee (Long 2012)','Translocatee (Long 2012)','Control East'))
names(zeros)<-c('year','count','total','TortType2') 

which.dead <- Mort.postShort %>% 
  full_join(Totals.postShort,by='TortType2') %>% 
  group_by(TortType2,year) %>% 
  summarise(count=n_distinct(TortoiseID),total=first(total)) %>%
  rbind(zeros) %>% 
  mutate(proportion=count/total) %>% 
  filter(TortType2 != 'Translocatee (Long 2012)')

ggplot(which.dead, aes(x=TortType2, y=proportion, fill=TortType2)) + geom_boxplot()+
  ylab("Annual proportion dead (2012-2015)")+xlab('Type')+theme(
    axis.text = element_text(size=14),
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size=22),
    axis.title.y = element_text(size=22, vjust=1),
    title = element_text(size=24))+
  guides(fill=FALSE)
  
                      
