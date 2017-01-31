setwd('E:/Projects/BrightSource_Ivanpah/code/GitHub/Brightsource-master/Torts Datasets')


library(ggplot2)

library(dplyr)
library(gridExtra)
library(lubridate)
library(rgdal)
demographics <- read.csv('Demographics_Dec2016.csv')
Health <- read.csv('Health_Dec2016.csv')
Elisa <- read.csv('ELISA_Dec2016.csv')
iButton<-read.csv('iButton_Dec2016.csv')
Health$EncounterDate <- as.Date(as.character(Health$EncounterDate), format="%m/%d/%Y")
Elisa$Sample_Date <- as.Date(as.character(Elisa$Sample_Date), format="%m/%d/%Y")
iButton$EncounterDate <- as.Date(as.character(iButton$EncounterDate), format="%m/%d/%Y")
SpringHealth<-Health%>%filter(EncounterDate>"2016-05-14" & EncounterDate<"2016-06-10")
SpringElisa <- Elisa%>%dplyr::rename(TortoiseID=Tortoise_ID)%>%left_join(demographics,by='TortoiseID')%>%filter(Sample_Date>"2016-05-14" & Sample_Date<"2016-06-10")%>%
  filter(Mag_Result=='Positive')%>%dplyr::select(TortoiseID,TortoiseType,Sex)%>%summarise(count=n_distinct(TortoiseID))
FallElisa <- Elisa%>%dplyr::rename(TortoiseID=Tortoise_ID)%>%filter(Sample_Date>"2016-06-10")%>%
  filter(Mtest_Result=='Positive')%>%summarise(count=n_distinct(TortoiseID))
FallHealth<-Health%>%filter(EncounterDate>"2016-06-10")
iButton2016<- iButton%>%filter(year(EncounterDate)==2016)
Unique_Health <- SpringHealth%>%summarise(count=n_distinct(TortoiseID))
Unique_Elisa<-SpringElisa%>%summarise(count=n_distinct(TortoiseID))
anti<-anti_join(SpringHealth,SpringElisa,by='TortoiseID')
AllElisa<-Elisa%>%dplyr::rename(TortoiseID=Tortoise_ID)%>%filter(Sample_Date>"2012-04-01")%>%
  filter(Mag_Result=='Positive')%>%summarise(count=n_distinct(TortoiseID))


fw1<- FallHealth%>%select(Fieldworker1)%>%rename(name=Fieldworker1)
fw2<-FallHealth%>%select(Fieldworker2)%>%rename(name=Fieldworker2)
fw3<-FallHealth%>%select(Fieldworker3)%>%rename(name=Fieldworker3)
fieldworkers<-rbind(fw1,fw2,fw3)%>%summarise(count=n_distinct(name))
