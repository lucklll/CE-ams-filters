# CE-ams-filters

#Average AMS hourly data to filter data

library(readr)
##ludata<-CE_dplyr_oridate <- read.csv("C:/Users/kubelova/Desktop/A1 IK Kosetice/CE/KoSu/CE dplyr_datum.csv")
##ludata<-CE_dplyr_oridate <- read.csv("C:/Users/kubelova/Desktop/A1 IK Kosetice/CE/PV check CE/PV Check.csv")
ludata<-CE_dplyr_oridate <- read.csv("C:/Users/kubelova/Desktop/A1 IK Kosetice/CE/KoWi/KoWi CE.csv")

ludata$F_start<-as.POSIXct(ludata$F_start,format="%d/%m/%Y %H:%M")
ludata$F_end<-as.POSIXct(ludata$F_end,format="%d/%m/%Y %H:%M")
ludata$AMS_start<-as.POSIXct(ludata$AMS_start,format="%d/%m/%Y %H:%M")
ludata$AMS_end<-as.POSIXct(ludata$AMS_end,format="%d/%m/%Y %H:%M")

library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## prin
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
# vytvori prazdnou data frame
xx<-data.frame()

# loop pres pocet dni
for (x in 1:length(ludata[!is.na(ludata$F_start),]$F_start)){
  # zacatek a konec mereni se updatuji v kazdem kroce
  # zacatek mereni
  start1=ludata[x,1]
 
  # konec mereni
  end1=ludata[x,2]
  siran=ludata[x,3]
  #yy <<- ludata %>% select(AMS_start, AMS_end, AMS_SO4)
  yy <<- ludata %>% 
    select(AMS_start, AMS_end, AMS_SO4) %>%
    # vytvoris cast data framu, ktera bude obsahovat zacatky a konce etap mereni
    filter(AMS_start >= start1 & AMS_end <= end1 &!is.na(AMS_SO4) ) %>%
    # vyfiltrujes data frame
    mutate(startingDay=start1,endingDay=end1,s=siran)
    
  # pridas sloupecek, podle ktereho se budes pozdeji orientovat
  # rozsiris novou data frame
  xx<-rbind(xx,yy)
}
# vzniklou data frame setridis podle data a pro kazde datum zvlast spocitas prumer
xx2<-xx %>% group_by(startingDay) %>% summarize(mean=mean(AMS_SO4),so4=mean(s))%>% print(2)
