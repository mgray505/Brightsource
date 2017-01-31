setwd('E:/Projects/BrightSource_Ivanpah/code/GitHub/Brightsource-master/Torts Datasets') # Specify file path



library(dplyr)
library(plyr)
library(lubridate)


Enc<-read.csv('Encounters_Dec2016.csv')
Mort<-read.csv('Mortality_Dec2016.csv')


Enc<-Enc[which(Enc$TortoiseID != ""),]
Enc$EncounterDate <- as.Date(as.character(Enc$EncounterDate), format="%m/%d/%Y")
Mort$EncounterDate <- as.Date(as.character(Mort$EncounterDate), format="%m/%d/%Y")


group.lookup <- data.frame(TortoiseType=c('Control East','Control West','Resident','Translocatee (2011 Short)','Translocatee (2012 Short)','Translocatee (2012 Long)'),
                           TortType2=c('Control East','Control West','Resident','Resident','Translocatee (2012 Short)','Translocatee (2012 Long)'),
                           stringsAsFactors=FALSE)

Enc= join(group.lookup,Enc,by='TortoiseType') %>% 
  mutate(year=year(EncounterDate)) %>% 
  arrange(EncounterDate)

Mort=join(group.lookup,Mort,by='TortoiseType') %>% 
  mutate(year=year(EncounterDate)) %>% 
  arrange(EncounterDate)


# Limit to active seasons and post-translocations
Enc.12.TL <- filter(Enc, TortType2=='Translocatee (2012 Long)'& EncounterDate > "2012-09-23" &  EncounterDate < "2012-10-31")
Enc.12.TS <- filter(Enc, TortType2=='Translocatee (2012 Short)'& EncounterDate > "2012-04-24"  &  EncounterDate < "2012-10-31")
Enc.12.Else <- filter(Enc, TortType2!='Translocatee (2012 Short)'& TortType2 !='Translocatee (2012 Long)'& EncounterDate > "2012-03-31" &  EncounterDate < "2012-10-31")
Enc.12 <- rbind(Enc.12.TL,Enc.12.TS,Enc.12.Else)

Enc.filtered <- filter(Enc, EncounterDate > "2013-03-31" & Enc$EncounterDate < "2013-10-31" |
                         EncounterDate > "2014-03-31"  & Enc$EncounterDate < "2014-10-31" |
                         EncounterDate > "2015-03-31" & Enc$EncounterDate < "2015-10-31"|
                         EncounterDate > "2016-03-31" & Enc$EncounterDate < "2016-10-31") %>%
  rbind(Enc.12) %>% dplyr::mutate(year=as.factor(year(EncounterDate)))

# Limit to post-translocations

Enc.postShort <- filter(Enc, EncounterDate>"2012-04-24") 
Enc.postLong<- filter(Enc,EncounterDate>"2012-09-23")

Mort.postShort <- filter(Mort, EncounterDate>"2012-04-24") 
Mort.postLong <- filter(Mort,EncounterDate>"2012-09-23")


