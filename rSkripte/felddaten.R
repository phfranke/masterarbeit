library(tidyverse)

tag <- 0:200

## Catching numbers 2021 and 2022, Creating plot 
catched21 <- read.table("excel/felddaten2021.csv",sep=';',header=TRUE)
colnames(catched21)[2]<-"gewaesser"
catched21$dayofyear <- yday(as.Date(catched21$Datum,format="%d.%m.%Y"))
catched21$date <- format(as.Date(catched21$Datum,format="%d.%m.%Y"),'%d.%m')
eachDay21 <- catched21 %>% group_by('tag'=dayofyear) %>% summarize(Anzahl=sum(Anzahl)) %>% as.data.frame()
eachDay21$year <- '2021'
catched22 <- read.table("excel/felddaten2022.csv",sep=';',header=TRUE)
colnames(catched22)[2]<-"gewaesser"
catched22$dayofyear <- yday(as.Date(catched22$Datum,format="%d.%m.%Y"))
catched22$date <- format(as.Date(catched22$Datum,format="%d.%m.%Y"),'%d.%m')
eachDay22 <- catched22 %>% group_by('tag'=dayofyear) %>% summarize(Anzahl=sum(Anzahl)) %>% as.data.frame()
eachDay22$year <- '2022'

eachDay_all <- rbind(eachDay21,eachDay22)
eachDay_all[is.na(eachDay_all)] <- 0

date_label <- format(as.Date(seq(0,199,10),origin='2020-01-01'),'%d.%m')

plottitel <- paste("Fangstatistik 2021 & 2022")
plotFangzahlen <- ggplot() +
  geom_col(data=eachDay_all[eachDay_all$year==2021,],aes(x=tag,y=Anzahl,fill='transparent'),alpha=0.5,
           color='black',linewidth=0.2, position='identity') + 
  geom_col(data=eachDay_all[eachDay_all$year==2022,],aes(x=tag,y=Anzahl,fill='black'),
           alpha=0.5, position='identity') + 
  labs(x="Datum",y="Total gefangener Tiere") +
  scale_x_continuous(breaks=seq(1,200,10),labels=date_label) +
  labs(fill = "Legend") +
  scale_fill_identity(name = "Jahr",
                       breaks = c("transparent", "black"),
                       labels = c("2021", "2022"),
                       guide = "legend")
 

gewaesser21 <- c(levels(factor(catched21$gewaesser)),'Total')
gewaesser22 <- c(levels(factor(catched22$gewaesser)), 'Total')
gewaesser22<- gewaesser22[2:length(gewaesser22)]

fem21 <- catched21 %>% filter(catched21$Geschlecht=='FEM') %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
fem21[nrow(fem21)+1,2] <- sum(fem21$Anzahl)
adu21 <- catched21 %>% 
  filter(catched21$Geschlecht=='ADU') %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
adu21[nrow(adu21)+1,2] <- sum(adu21$Anzahl)
juv21 <- catched21 %>% 
  filter(catched21$Geschlecht=='JUV') %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
juv21[nrow(juv21)+1,2] <- sum(juv21$Anzahl)
mal21 <- catched21 %>% 
  filter(catched21$Geschlecht=='MAL') %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
mal21[nrow(mal21)+1,2] <- sum(mal21$Anzahl)
tot21 <- catched21 %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
tot21[nrow(tot21)+1,2] <- sum(tot21$Anzahl)

tabelle21 <- cbind(gewaesser21,fem21[,2]+mal21[,2]+adu21[,2],juv21[,2],tot21[,2])
colnames(tabelle21) <- c('Gewässer','Adulte','Juvenile', 'Summe')

mal22 <- catched22 %>% 
  filter(catched22$Geschlecht=='MAL') %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
mal22[nrow(mal22)+1,2] <- sum(mal22$Anzahl)
fem22 <- catched22 %>% 
  filter(catched22$Geschlecht=='FEM') %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
fem22[nrow(fem22)+1,2] <- sum(fem22$Anzahl)
juv22 <- catched22 %>% 
  filter(catched22$Geschlecht=='JUV') %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
juv22[nrow(juv22)+1,2] <- sum(juv22$Anzahl)
tot22 <- catched22 %>% filter(gewaesser!="") %>%
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))
tot22[nrow(tot22)+1,2] <- sum(tot22$Anzahl,na.rm=TRUE)

adu22tot <- catched22 %>% 
  filter(catched22$Geschlecht %in% c('MAL','FEM','ADU')) %>% 
  group_by(gewaesser) %>% 
  summarize(Anzahl=sum(Anzahl))


tabelle22 <- cbind(gewaesser22,fem22[,2]+mal22[,2],juv22[,2],tot22[,2]) %>%
  add_row("gewaesser22":="BL195s",.before=2)
colnames(tabelle22) <- c('Gewässer','Adulte','Juvenile','Summe')
# tabelle22[is.na(tabelle22)]<- '-'
# tabelle22 <- tabelle22[,2:4]



## Weight and Length
weight21 <- read.table("excel/waegung2021.csv",sep=';',header=TRUE) %>% drop_na()
weight22 <- read.table("excel/waegung2022.csv",sep=';',header=TRUE) %>% drop_na()
weight21$geschlecht <- as.factor(weight21$geschlecht)
weight21$gewicht <- as.numeric(weight21$gewicht)
weight22$geschlecht <- as.factor(weight22$geschlecht)
weight21$year <- 2021
weight22$year <- 2022
weight_all <- rbind(weight21,weight22)

boxplot_weight <- ggplot(weight_all[weight_all$geschlecht!='J',],aes(x=geschlecht, y=gewicht)) +
  geom_boxplot() + 
  labs(x = "Geschlecht",y = "Gewicht") +
  coord_cartesian(ylim = c(0,15)) +
  facet_wrap(~year)
