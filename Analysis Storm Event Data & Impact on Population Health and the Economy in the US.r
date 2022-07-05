data <- read.csv('repdata_data_StormData.csv.bz2', header=TRUE, sep=",")

library('ggplot2')
library('plyr')
library('ggplot2')
library('gridExtra')

stormdata <- data[c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
stormdata$PROPDMGEXP <- mapvalues(stormdata$PROPDMGEXP, from = c("K", "M","", "B", "m", "+", "0", "5", "6", "?", "4", "2", "3", "h", "7", "H", "-", "1", "8"), to = c(10^3, 10^6, 1, 10^9, 10^6, 0,1,10^5, 10^6, 0, 10^4, 10^2, 10^3, 10^2, 10^7, 10^2, 0, 10, 10^8))
stormdata$PROPDMGEXP <- as.numeric(as.character(stormdata$PROPDMGEXP))
stormdata$PROPDMGTOTAL <- (stormdata$PROPDMG * stormdata$PROPDMGEXP)/1000000000

stormdata$CROPDMGEXP <- mapvalues(stormdata$CROPDMGEXP, from = c("","M", "K", "m", "B", "?", "0", "k","2"), to = c(1,10^6, 10^3, 10^6, 10^9, 0, 1, 10^3, 10^2))
stormdata$CROPDMGEXP <- as.numeric(as.character(stormdata$CROPDMGEXP))
stormdata$CROPDMGTOTAL <- (stormdata$CROPDMG * stormdata$CROPDMGEXP)/1000000000

stormdata$DAMAGETOTAL <- stormdata$PROPDMGTOTAL + stormdata$CROPDMGTOTAL

aggdata <- aggregate(cbind(FATALITIES, INJURIES,PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) ~ EVTYPE, stormdata, sum, na.rm=TRUE)


fatal_events <- head(aggdata[order(aggdata$FATALITIES, decreasing = T), ], 10)
injury_events <- head(aggdata[order(aggdata$INJURIES, decreasing = T), ], 10)

prop_damages <- head(aggdata[order(aggdata$PROPDMGEXP, decreasing = T), ], 10)

crop_damages <- head(aggdata[order(aggdata$CROPDMGEXP, decreasing = T), ], 10)

p1<- ggplot(data=fatal_events,
            aes(x=reorder(EVTYPE, FATALITIES), y=FATALITIES, fill=FATALITIES)) +
  geom_bar(stat="identity") +
  ylab("Total number of fatalities") +
  xlab("Event type") +
  theme(axis.text.x = element_text(angle=90)) +
  expand_limits(y=c(0,6000))
p2<- ggplot(data=injury_events,
            aes(x=reorder(EVTYPE, INJURIES), y=INJURIES, fill=INJURIES)) +
  geom_bar(stat="identity") +
  ylab("Total number of injuries") +
  xlab("Event type") +
  theme(axis.text.x = element_text(angle=90)) +
  expand_limits(y=c(0,6000))

grid.arrange (p1,p2,ncol=2)

p3<-ggplot(data=prop_damages,
           aes(x=reorder(EVTYPE, PROPDMGEXP), y=PROPDMGEXP, fill=PROPDMGEXP)) +
  geom_bar(stat="identity") +
  ylab("Total number of property damagies") +
  xlab("Event type") +
  theme(axis.text.x = element_text(angle=90)) +
  expand_limits(y=c(0,6000))
p4<- ggplot(data=crop_damages,
            aes(x=reorder(EVTYPE, CROPDMGEXP), y=CROPDMGEXP, fill=CROPDMGEXP)) +
  geom_bar(stat="identity") +
  ylab("Total number of crop damagies") +
  xlab("Event type") +
  theme(axis.text.x = element_text(angle=90)) +
  expand_limits(y=c(0,6000))
grid.arrange (p3,p4,ncol=2)


