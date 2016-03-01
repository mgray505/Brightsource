setwd('E:/Projects/BrightSource_Ivanpah/data/Databases/')

library(plyr)
library(dplyr)
library(lubridate)


Enc<-read.csv('EncounterPts_Jan2016.csv')
Enc<-Enc[which(Enc$TortoiseID != ""),]
Enc$EncounterDate <- as.Date(as.character(Enc$EncounterDate), format="%m/%d/%Y")
Enc$TortType2[Enc$TortoiseType=="Control West"] <- "ControlWest"
Enc$TortType2[Enc$TortoiseType=="Control East"] <- "ControlEast"
Enc$TortType2[Enc$TortoiseType %in% c("Resident", 
                                      "Translocatee (Short 2011)")] <- "Resident"
Enc$TortType2[Enc$TortoiseType=="Translocatee (Short 2012)"] <- "Translocated Short 2012"
Enc$TortType2[Enc$TortoiseType=="Translocatee (Long 2012)"] <- "I-15 Pen"


# Limit to active seasons and post-translocations
Enc.12.TL <- filter(Enc, TortType2=='I-15 Pen'& EncounterDate > "2012-09-23" &  EncounterDate < "2012-10-31")
Enc.12.TS <- filter(Enc, TortType2=='Translocated Short 2012'& EncounterDate > "2012-04-24" &  EncounterDate < "2012-10-31")
Enc.12.Else <- filter(Enc, TortType2!='Translocated Short 2012'& TortType2 !='I-15 Pen'& EncounterDate > "2012-03-31" &  EncounterDate < "2012-10-31")
Enc.12 <- rbind(Enc.12.TL,Enc.12.TS,Enc.12.Else)
Enc.filtered <- filter(Enc, EncounterDate > "2013-03-31" & Enc$EncounterDate < "2013-10-31" |
                         EncounterDate > "2014-03-31" & Enc$EncounterDate < "2014-10-31" |
                         EncounterDate > "2015-03-31" & Enc$EncounterDate < "2015-10-31") %>% 
  rbind(Enc.12) %>% mutate(year=year(EncounterDate))