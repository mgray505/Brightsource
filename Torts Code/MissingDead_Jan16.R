library(rgdal)

library(dplyr)
 
library(ggplot2)
library(scales)
library(lubridate)
setwd("E:/Projects/BrightSource_Ivanpah/data/Databases/CSVs")
source('E:/Projects/BrightSource_Ivanpah/code/GitHub/Brightsource-master/Torts Code/Filter.Encounters.R')
detach(package:plyr) 

Totals.postShort <- Enc.postShort %>% 
  group_by(TortType2) %>% 
  dplyr::summarise(total=n_distinct(TortoiseID))

Totals.postLong <-  Enc.postLong %>% 
  group_by(TortType2) %>% 
  dplyr::summarise(total=n_distinct(TortoiseID))

which.missing <- filter(Enc.postLong,TortoiseStatus %in% c('Tracked (Missing)')) %>% 
  left_join(Totals.postLong,by='TortType2') %>% 
  group_by(TortType2) %>%
  dplyr::summarise(count=n_distinct(TortoiseID),total=first(total)) %>% 
  dplyr::mutate(proportion = count/total)

zeros.postShort<-data.frame(c(2012,2013,2015,2012), c(0,0,0,0),c(17,17,17,34)) %>% 
  mutate_each(funs(as.numeric)) %>% 
  cbind(c('Translocatee (2012 Long)','Translocatee (2012 Long)','Translocatee (2012 Long)','Control East'))
names(zeros.postShort)<-c('year','count','total','TortType2')

zeros.postLong<-data.frame(c(2012,2013,2015,2012,2012), c(0,0,0,0,0),c(17,17,17,34,106)) %>% 
  mutate_each(funs(as.numeric)) %>% 
  cbind(c('Translocatee (2012 Long)','Translocatee (2012 Long)','Translocatee (2012 Long)','Control East','Control West'))
names(zeros.postLong)<-c('year','count','total','TortType2') 

which.dead.postLong <- Mort.postLong %>% 
  full_join(Totals.postLong,by='TortType2') %>% 
  group_by(TortType2,year) %>% 
  dplyr::summarise(count=n_distinct(TortoiseID),total=first(total)) %>%
  rbind.data.frame(zeros.postLong) %>% 
  mutate(proportion=count/total) %>% 
  filter(TortType2 != 'Translocatee (2012 Short)')

which.dead.postShort <- Mort.postShort %>% 
  full_join(Totals.postShort,by='TortType2') %>% 
  group_by(TortType2,year) %>% 
  dplyr::summarise(count=n_distinct(TortoiseID),total=first(total)) %>%
  rbind.data.frame(zeros.postShort) %>% 
  mutate(proportion=count/total) %>% 
  filter(TortType2 != 'Translocatee (2012 Long)')

ggplot(which.dead.postShort, aes(x=TortType2, y=proportion, fill=TortType2)) + geom_boxplot()+
  ylab("Annual proportion dead (2012-2016)")+xlab('Type')+theme(
    axis.text = element_text(size=14),
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size=22),
    axis.title.y = element_text(size=22, vjust=1),
    title = element_text(size=24))+
  guides(fill=FALSE)
  
                      
