setwd('E:/Projects/BrightSource_Ivanpah/data/Databases/')


library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(lubridate)
library(rgdal)

source('E:/Projects/BrightSource_Ivanpah/code/Filter Encounters.R')


# Limit to TortoiseIDs with n>25 for creation of UDs
Enc.filtered.gt25<-Enc.filtered %>% 
  group_by(year,TortoiseType,TortoiseID) %>% 
  summarise(count=n_distinct(EncounterDate)) %>% 
  filter(count>25 & year==2015)
Enc.filtered.gt25.2015 = filter(Enc.filtered,TortoiseID %in% Enc.filtered.gt25$TortoiseID & year==2015)

##write.csv(Enc.filtered.gt25.2015, 'E:/Projects/BrightSource_Ivanpah/data/working/HomeRange/2015/Encounters_gt25.csv')


## Extract Unique IDs to create UDs in the Geospatial Modeling Environment
ControlWestIds <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Control West"]
ControlEastIds <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Control East"]
ResidentIds <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Resident"|
                                              Enc.filtered.gt25$TortoiseType=="Translocatee (Short 2011)"]
TransShort2012Ids <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Translocatee (Short 2012)"]
TransLong2012Ids <- Enc.filtered.gt25$TortoiseID[Enc.filtered.gt25$TortoiseType=="Translocatee (Long 2012)"]


## Pull in the UDs
UDs_2014<-readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2014\\isopleths_polygon","UDs_merge")
UDs_2012<-readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2012","UDs_2012")
UDs_2013<- readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2013","UDs_2013")
UDs_2015<-readOGR("E:\\Projects\\BrightSource_Ivanpah\\data\\working\\HomeRange\\2015\\isopleths_polygon","UDs_merge")

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






