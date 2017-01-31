setwd('E:/Projects/BrightSource_Ivanpah/code/GitHub/Brightsource-master/Torts Datasets')


library(ggplot2)

library(dplyr)
library(gridExtra)
library(lubridate)
library(rgdal)

source('E:/Projects/BrightSource_Ivanpah/code/GitHub/Brightsource-master/Torts Code/Filter.Encounters.R')
demographics <- read.csv('E:/Projects/BrightSource_Ivanpah/code/GitHub/Brightsource-master/Torts Datasets/Demographics_Dec2016.csv')
demographics$TortoiseType[demographics$TortoiseType %in% c("Resident", 
                                      "Translocatee (2011 Short)")] <- "Resident"


# Limit to TortoiseIDs with n>25 for creation of UDs

Enc.filtered.gt25<-Enc.filtered %>% 
  group_by(year,TortoiseType,TortoiseID) %>% 
  summarise(count=n_distinct(EncounterDate)) %>% 
  filter(count>25 & year==2016)
Enc.filtered.gt25.2016 = filter(Enc.filtered,TortoiseID %in% Enc.filtered.gt25$TortoiseID & year==2016)

Enc.CE <- Enc.filtered%>%filter(TortoiseType=='Control East' & year==2016)

write.csv(Enc.filtered.gt25.2016, 'E:/Projects/BrightSource_Ivanpah/data/working/HomeRange/2016/Encounters_gt25.csv')


## Extract Unique IDs to create UDs in the Geospatial Modeling Environment
ControlWestIds <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Control West"]
ControlEastIds <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Control East"]
ResidentIds <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Resident"|
                                              Enc.filtered.gt25$TortoiseType=="Translocatee (2011 Short)"]
TransShort2012Ids <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Translocatee (2012 Short)"]
TransLong2012Ids <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Translocatee (2012 Long)"]


## Pull in the UDs
UDs_2014<-readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2014\\isopleths_polygon","UDs_merge")
UDs_2012<-readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2012","UDs_2012")
UDs_2013<- readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2013","UDs_2013")
UDs_2015<-readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2015\\isopleths_polygon","UDs_merge")
UDs_2016<-readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2016\\isopleths_polygon","UDs_merge")


UDs_2012_df<-as.data.frame(UDs_2012)
UDs_2012_df$TortoiseTy[UDs_2012_df$TortoiseTy=="Translocatee (Short 2011)"]   <-"Resident"
UDs_2012_df %>% group_by(TortoiseTy) %>% summarise(area=mean(UDhectares))
UDs_2013_df<-as.data.frame(UDs_2013)
UDs_2013_df$TortoiseTy[UDs_2013_df$TortoiseTy=="Translocatee (Short 2011)"]   <-"Resident"
UDs_2013_df %>% group_by(TortoiseTy) %>% summarise(area=mean(UDhectares))


UDs_2014_df<-as.data.frame(UDs_2014) %>% 
  group_by(Type) %>% 
  summarise(area=mean(area))
UDs_2015_df<-as.data.frame(UDs_2015) %>% 
  group_by(Type) %>% 
  summarise(count=n_distinct(TortoiseID),area=mean(area))
UDs_2016_df<-as.data.frame(UDs_2016) %>% 
  dplyr::left_join(demographics,by='TortoiseID')%>%
  group_by(TortoiseType) %>% 
  dplyr::summarise(count=n_distinct(TortoiseID),area=mean(area))





