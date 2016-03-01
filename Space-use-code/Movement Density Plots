setwd('E:/Projects/BrightSource_Ivanpah/data/Databases/')
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(lubridate)

source('E:/Projects/BrightSource_Ivanpah/code/Filter Encounters.R')

# order encounters by date for each tort within each type
Enc.filtered <- ddply(Enc.filtered, .(TortType2, TortoiseID), function(x){
  x[order(x$EncounterDate),]
})


# Create Dataframe of movement distances
Enc.movement <- ddply(Enc.filtered,.(year,TortType2,TortoiseID), function(x){
  eastings <- cbind(x$Easting[1:(nrow(x)-1)],
                    x$Easting[2:nrow(x)])
  northings <- cbind(x$Northing[1:(nrow(x)-1)],
                     x$Northing[2:nrow(x)])
  days <-cbind(x$EncounterDate[1:(nrow(x)-1)],
               x$EncounterDate[2:nrow(x)])
  
  dist <- sqrt(apply(eastings, 1, function(y) diff(y)^2) + 
                 apply(northings, 1, function(y) diff(y)^2))
  day.diff <- (apply(days, 1, function(y) diff(y)))
  midpointE <- apply(eastings, 1, function(y) sum(y)/2)
  midpointN <- apply(northings, 1, function(y) sum(y)/2)
  data.frame(dist=dist, day.diff=day.diff, 
             EncDate=x$EncounterDate[1:(nrow(x)-1)],Easting=x$Easting[1:(nrow(x)-1)],
             Northing=x$Northing[1:(nrow(x)-1)], midpointE = midpointE, midpointN = midpointN,
             NextEnc = x$EncounterDate[2:nrow(x)],
             PrevEnc = c(x$EncounterDate[1]-7,x$EncounterDate[2:nrow(x)-2]))
})


# Create Dataframe of mean movement rates (m/day)
Enc.movement.means <- na.omit(ddply(filter(Enc.movement,day.diff>0), .(year, TortType2,TortoiseID), summarise,
                 mean=mean(dist/day.diff)))


# Plot the movement densities

group.lookup <- data.frame(TortType2=c('ControlEast','ControlWest','Resident','Translocated Short 2012','I-15 Pen'),
                           label=c('CE','CW','RE','TS_12','TL'),
                           stringsAsFactors=FALSE)

Movement.plot <- dlply(Enc.movement.means, .(TortType2), function(df,fullDf=Enc.movement.means) {
  groupLab <- group.lookup$label[group.lookup$TortType2 == df$TortType2[1]]
  p1 <- ggplot(fullDf,aes(mean,group=TortType2)) + 
    geom_density(aes(fill='gray'),linetype='dotted') + #alpha=0.5
    geom_density(data=df, aes(mean, fill='white'),alpha=1) +
    #     scale_fill_discrete(values=c("white"="white"),name="Group") +
    scale_fill_manual(values=c("gray"="gray","white"="white"), labels=c('Others',groupLab),
                      name="Study group") +
    guides(fill = guide_legend(override.aes = list(colour = NULL), reverse = TRUE)) + 
    facet_grid(. ~ year) +
    xlab('') +
    ylab('') +
    theme(##plot.margin=unit(c(0,0.25,0,0)-0.15, "cm"),
      strip.text = element_text(size = rel(1.5)),
      legend.key = element_rect(colour = "black",size=rel(1.75)),
      legend.text=element_text(size=rel(1.1)),
      legend.title=element_text(size=rel(1.1)),
      axis.text = element_text(colour = "black",size=rel(1.1)))
  p2 <- theme(strip.background = element_blank(), 
              strip.text = element_blank())
  p3 <- theme(axis.title.x=element_blank(), 
              axis.text.x=element_blank(), 
              axis.ticks=element_blank())
  
  if(df$TortType2[1] == 'ControlEast') {
    outGrob <- p1 + 
      theme(##plot.margin=unit(c(0.25,0.25,-0.15,0)-0.15, "cm"),
        strip.text = element_text(size = rel(1.5))) +
      p3
  }
  if(df$TortType2[1] %in% c('ControlWest','Resident','I-15 Pen')) {
    outGrob <- p1 + 
      #       theme(plot.margin=unit(c(0.15,0,0.15,0)-0.20, "cm"),
      #             strip.text = element_text(size = rel(1.5))) +
      p2 + p3
  }
  if(df$TortType2[1] == 'Translocated Short 2012') {
    outGrob <- p1 + 
      theme(##plot.margin=unit(c(-0.20,0.25,0,0)-0.15, "cm"),
        strip.text = element_text(size = rel(1.5))) +
      p2
  }
  outGrob
})

do.call(grid.arrange, c(Movement.plot, 
                        nrow=5,ncol=1))  #left='Density',sub='Mean distance (m/day)'

