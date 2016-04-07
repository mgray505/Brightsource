library(rgdal)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
setwd("E:/Projects/BrightSource_Ivanpah/data/Databases/CSVs")


##Encounters
encounters <- read.csv('Encounters_Jan2016.csv')
encounters.dead <- read.csv('Mortality_plus_Encounters_Jan2016.csv')
names(encounters) <- c("ID","Type","Status","Sex","Date","Time", "Site", "Location","LocationDetail","Activity","Purpose",
                       "Notes","Easting","Northing")
names(encounters.dead) <- c("ID","Date","Location","Activity","Purpose","Comments", "Type",
                            "Status","Sex","DeadEnc","DaysSinceDeath","Suspect")
encounters$Date <- as.Date(encounters$Date, format="%m/%d/%Y")
encounters.dead$Date<- as.Date(encounters.dead$Date, format="%m/%d/%Y")
encounters.dead$DeadEnc<- as.Date(encounters.dead$DeadEnc, format="%m/%d/%Y")
encounters <- encounters %>% 
  group_by(ID) %>% 
  arrange(Date)
encounters.dead <- encounters.dead %>% 
  group_by(ID) %>% 
  arrange(Date)


## Create data for survival analysis
diffs = encounters %>% 
  mutate(MootYear= as.Date(paste(month(Date),"-",day(Date),sep=""),format="%m-%d"), 
         Year=year(Date),diffApril=as.Date('2016-04-15')-MootYear,
         diffAugust=as.Date('2016-08-01')-MootYear,
         diffOctober=as.Date('2016-10-15')-MootYear
         ) %>% 
  filter(Year>=2012)

April = filter(diffs,diffApril>=-7 & diffApril<=7) %>% 
  ddply(.(ID,Year),function (x) x[which.min(x$diffApril),])

August = filter(diffs,diffAugust>=-7 & diffAugust<=7) %>% 
  ddply(.(ID,Year),function (x) x[which.min(x$diffAugust),])

October = filter(diffs,diffOctober>=-7 & diffOctober<=7) %>% 
  ddply(.(ID,Year),function (x) x[which.min(x$diffOctober),])

SurvivalEncounters=rbind(April,August,October) %>% 
  mutate(Label = ifelse(Activity=='Unknown', 'O', ifelse(Activity%in%c('Other - SEE COMMENTS',""),'UK',
                                                         ifelse(Activity%in%c('Dead','On Carapace'),'D','A')))) %>% 
  group_by(Status,Type,Sex,ID) %>%
  arrange(Date) %>% 
  mutate(Int = paste('x',seq(1:length(ID)),sep=''))%>% 
  spread(Int,Label) %>% 
  summarise(Apr12=ifelse(is.na(first(x1)),'O',first(x1)),Aug12=ifelse(is.na(nth(x2,2)),'O',nth(x2,2)),Oct12=ifelse(is.na(nth(x3,3)),'O',nth(x3,3)),
            Apr13=ifelse(is.na(nth(x4,4)),'O',nth(x4,4)),Aug13=ifelse(is.na(nth(x5,5)),'O',nth(x5,5)),Oct13=ifelse(is.na(nth(x6,6)),'O',nth(x6,6)),
            Apr14=ifelse(is.na(nth(7,7)),'O',nth(x7,7)),Aug14=ifelse(is.na(nth(x8,8)),'O',nth(x8,8)),Oct14=ifelse(is.na(nth(x9,9)),'O',nth(x9,9)),
            Apr15=ifelse(is.na(nth(x10,10)),'O',nth(x10,10)),Aug15=ifelse(is.na(nth(x11,11)),'O',nth(x11,11)),Oct15=ifelse(is.na(nth(x12,12)),'O',nth(x12,12))) %>% 
  filter(Status %in% c('Tracked (Missing)','Tracked'))

##join(rbind(April,August,October),by="ID") %>% 

write.csv(SurvivalEncounters,'SurvivalEncounters_Jan2016.csv')

diffs.dead = encounters.dead %>% 
  mutate(MootYear= as.Date(paste(month(Date),"-",day(Date),sep=""),format="%m-%d"), 
         Year=year(Date),diffApril=as.Date('2016-04-15')-MootYear,
         diffAugust=as.Date('2016-08-01')-MootYear,
         diffOctober=as.Date('2016-10-15')-MootYear
  ) %>% 
  filter(Year>=2012)

April.dead = filter(diffs.dead,diffApril>=-7 & diffApril<=7) %>% 
  ddply(.(ID,Year),function (x) x[which.min(x$diffApril),]) %>% 
  mutate(Interval=paste(month(diffApril+MootYear,label=T,abbr=T),year(Date),sep=''))

August.dead = filter(diffs.dead,diffAugust>=-7 & diffAugust<=7) %>% 
  ddply(.(ID,Year),function (x) x[which.min(x$diffAugust),]) %>% 
  mutate(Interval=paste(month(diffAugust+MootYear,label=T,abbr=T),year(Date),sep=''))

October.dead = filter(diffs.dead,diffOctober>=-7 & diffOctober<=7) %>% 
  ddply(.(ID,Year),function (x) x[which.min(x$diffOctober),]) %>% 
  mutate(Interval=paste(month(diffOctober+MootYear,label=T,abbr=T),year(Date),sep=''))

SurvivalEncounters.dead = rbind(April.dead,August.dead,October.dead) %>% 
  mutate(Label = ifelse(Activity=='Unknown', 'O', ifelse(Activity%in%c('Other - SEE COMMENTS',""),'UK',
                                                 ifelse(Activity=='Dead','D','A')))) %>% 
  group_by(Status,Type,Sex,ID) %>%
  arrange(Date) %>% 
  #mutate(Int = paste('x',seq(1:length(ID)),sep=''))%>% 
  spread(Interval,Label) %>% 
  summarise(Apr12=toString(na.omit(Apr2012)),Aug12=toString(na.omit(Aug2012)),Oct12=toString(na.omit(Oct2012)),
            Apr13=toString(na.omit(Apr2013)),Aug13=toString(na.omit(Aug2013)),Oct13=toString(na.omit(Oct2013)),
            Apr14=toString(na.omit(Apr2014)),Aug14=toString(na.omit(Aug2014)),Oct14=toString(na.omit(Oct2014)),
            Apr15=toString(na.omit(Apr2015)),Aug15=toString(na.omit(Aug2015))) %>% 
  join(rbind(April.dead,August.dead,October.dead),by='ID')


write.csv(SurvivalEncounters.dead,'SurvivalEncountersDead_Jan2016.csv')



##Demographics
Health <- read.csv('Tortoise Health USFWS.csv')
Demog <-read.csv('Tortoise Demographics.csv')
Joined <- left_join(Health,Demog,by='TortoiseID')
Joined$TimeStamp <-as.Date(Joined$TimeStamp,format="%m/%e/%Y")
Joined<-filter(Joined,TimeStamp>"2012-09-20")

MCL_df <- na.omit(Health[,c('TimeStamp','TortoiseID','MCL')])
MCL_avg <- MCL_df %>% group_by(TortoiseID) %>% 
  mutate(AvgMCL=mean(MCL),MaxMCL=max(MCL),MinMCL=min(MCL),sdMCL=sd(MCL)) %>%
  mutate(growth = MaxMCL-MinMCL)
write.csv(MCL_avg,"allTortsMCL.csv")

Change <- Joined %>% group_by(TortoiseType, TortoiseID,TortoiseStatus) %>% mutate(Change=MCL_Recent-MCL_Initial) 
single <- Change %>% group_by(TortoiseType, TortoiseID,TortoiseStatus) %>% summarise(SingleChange=first(Change))
AvgChange <- single %>% group_by(TortoiseType,TortoiseStatus) %>% summarise(avgChange = mean(SingleChange))

Joined$Year<-year(Joined$TimeStamp)
Joined<-na.omit(Joined)
BCS <- Joined %>% group_by(Year,TortoiseType,TortoiseID)%>%mutate(meanBCS=mean(BCS.x))
AvgBCS <-BCS %>% group_by(Year,TortoiseType)%>%summarise(AvgBCS=mean(meanBCS,na.rm=T))
