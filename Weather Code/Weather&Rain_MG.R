if(!'plyr' %in% rownames(installed.packages())) install.packages('plyr')
if(!'dplyr' %in% rownames(installed.packages())) install.packages('dplyr')
if(!'lubridate' %in% rownames(installed.packages())) install.packages('lubridate')
if(!'tidyr' %in% rownames(installed.packages())) install.packages('tidyr')
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
#===========================
#Read in and rename headers
#============================

#all excel files saved as csv and then deleted

baseDir = "E:/Projects/BrightSource_Ivanpah/code/GitHub/Brightsource-master/Weather Datasets"  ## Specify the path
mynames=c("File","Date","Time","TempOut","HiTemp","LowTemp","OutHum","DewPt","WindSpeed","WindDir","WindRun","HiSpeed",
          "HiDir","WindChill","HeatIndx", "THWIndx","THSWIndx","Bar","Rain","RainRate","SolarRad","SolarEngy","HiSolarRad",
          "UVIndex","UVDose","HiUV","HeatDD","CoolDD","InTemp","InHum","InDew","InHeat","InEMC","InAirDensity","ET",
          "WindSamp","WindTx","ISSRecept","ArcInt")

### Block of code to bring in new data and rbind to old data (1-12-2016)
ws5 <- adply(list.files('C:/Users/meg/Downloads/Station 2',full.names=T),1,read.delim,skip=1,na.strings='---')
names(ws5)<-mynames
ws5$Date<-as.POSIXct(ws5$Date,format="%m/%d/%y")
ws5<- ws5%>%dplyr::select(-File)%>%dplyr::mutate(File=5)%>%filter(Date>'2016-01-12')


ws1Files <- file.path(baseDir,'st 1 veg 10 all 1-12-2016.csv')
ws2Files <- file.path(baseDir,'st 2 veg 115 all 1-12-2016.csv')
ws3Files <- file.path(baseDir,'st 3 veg 4 all 1-12-2016.csv')
ws4Files <- file.path(baseDir,'st 4 veg 101 all 1-12-2016.csv')
ws5Files <- file.path(baseDir,'st 5 veg 107 all 1-12-2016.csv')
ws6Files <- list.files(baseDir,pattern=c('station6+','.csv+'),full.names=T,ignore.case=T)

ws6<-adply(ws6Files,1,read.csv,skip=1,na.strings="---")
ws6_unite<-unite(ws6,Date_Time,c(Date,Time))
ws6_unique<-ws6_unite[!duplicated(ws6_unite$Date_Time),]


wsFiles = c(ws1Files,ws2Files,ws3Files,ws4Files,ws5Files) ##Leave out ws6 for now
ws <- adply(wsFiles, 1, read.csv, skip=1,na.strings="---")
names(ws)=mynames
ws$Date=as.Date(ws$Date, format="%m/%d/%Y")
ws<-ws %>% 
  arrange(Date) %>% 
  filter(Date>'2013-05-28')%>%rbind(ws1,ws2,ws3,ws4,ws5)
ws$Station[ws$File==1]<-10
ws$Station[ws$File==2]<-115
ws$Station[ws$File==3]<-4
ws$Station[ws$File==4]<-101
ws$Station[ws$File==5]<-107
write.csv(ws,file.path(baseDir,'weatherStations1thru5.csv'))

##Testing 
ws6$Date<- as.Date(ws6$Date, format="%m/%d/%Y")
ws6summer<-filter(ws6, month(Date)==06|month(Date)==07|month(Date)==08)%>% filter(year(Date)==2014)
ws5<-read.csv(ws5Files,skip=1,na.strings="---")
ws5$Date<- as.Date(ws5$Date, format="%m/%d/%Y")
ws5summer<-filter(ws5, month(Date)==06|month(Date)==07|month(Date)==08) %>% filter(year(Date)==2014)
#create single weather station dataset 
#==============================================================

summer <- filter(ws, month(Date)>=06) %>% 
  filter(month(Date)<10)
summer$season <- "Summer"
summer$year <- year(summer$Date)

winter<-filter(ws, month(Date)>=10 | month(Date)<06)
winter$season <- "Winter/Spring"
winter$year<-ifelse(month(winter$Date)>=10,paste(year(winter$Date),'-',year(winter$Date)+1),
                    paste(year(winter$Date)-1,'-',year(winter$Date)))

ws<-rbind(summer,winter)


##Summarise by day for each station

DailyBySt <- ws %>% 
  group_by(File,season,year,Date) %>% 
  summarise(DailyMaxTemp = max(na.omit(TempOut)),
            DailyMinTemp = min(na.omit(TempOut)),
            DailyRain = sum(Rain))
##Summarise across stations

DailyAvg <- DailyBySt %>% 
  group_by(Date,season,year) %>% 
  summarise(avgDailyMaxTemp = mean(DailyMaxTemp),
            avgDailyMinTemp = mean(DailyMinTemp),
            avgDailyRain = mean(DailyRain),
            TotalRain = sum(DailyRain))

##Summarise across seasons                                                            

Seasonalws<- DailyAvg  %>% 
  group_by(season,year)%>% 
  summarise(avgMaxTemp = mean(avgDailyMaxTemp),
            avgMinTemp = mean(avgDailyMinTemp),
            avgRain = mean(avgDailyRain),## avg daily rain across stations,in season
            TotalRain =sum(TotalRain),## all rain all stations, in season
            avgTotalRain=sum(avgDailyRain))## sum of avg daily rain across stations, in season
                                                                  

write.csv(Seasonalws,'SeasonalWSData_0923.csv')



##-----------------------
### Rain Gauge Data
##-----------------------
rain <- read.csv(file.path(baseDir,'RainGauge_2016-03-11.csv'))
rain <- na.omit(rain)
rain=rain[c(1,2,5)]
names(rain)<-c('ID','Date','Level')
rain$Date<-as.Date(rain$Date,format="%e-%b-%Y")
write.csv(rain,'RainGauges.csv')

summerRain <- filter(rain, month(Date)>=06) %>% filter(month(Date)<10)
summerRain$season <- "Summer"
summerRain$year <- year(summerRain$Date)

winterRain<-filter(rain, month(Date)>=10 | month(Date)<06) 
winterRain$season <- "Winter/Spring"
winterRain$year<-ifelse(month(winterRain$Date)>=10,paste(year(winterRain$Date),'-',year(winterRain$Date)+1),
                        paste(year(winterRain$Date)-1,'-',year(winterRain$Date)))
rain<-rbind(summerRain,winterRain)

SeasonalRainbyID <- rain %>% group_by(year,season,ID) %>% summarise(avgWeeklySeasonalRain = mean(Level),
                                                                totalSeasonalRain = sum(Level))
                                                              
SeasonalRain <-SeasonalRainbyID %>% group_by(year,season) %>% summarise(avgweeklyRain = mean(avgWeeklySeasonalRain), ##across gauges
                                                                        avgTotalRain = mean(totalSeasonalRain)) %>% arrange(season)

write.csv(SeasonalRain,'rainGaugeSummaries_0923.csv')

