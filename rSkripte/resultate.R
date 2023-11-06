library(tidyverse)
data1 <- read.table("excel/resultate_4.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")
data <- read.table("excel/resultate.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")
str(data)
pondList <- c()
for ( pond in 1:nrow(data)){
  text <- data[pond,ncol(data)]
  if (nchar(text) <= 4) {pondList[pond] <- NA}
  else {pondList[pond] <- substring(text,2,nchar(text)-1)}
}
data <- cbind(data[,1:ncol(data)-1],pondList)

colnames(data) <- c("simulation", "currentDate","probCatch","egg", "larva", "juvenil", "adult", "adultSlipped", "adultNotSliped",
                    "totCatched_adult", "totCatched_juv", "eggs_tot","larva_tot","juv_tot", "adult_tot", "migrantJuv_tot", "migrantAdult_tot", "pondList")
colnames(data1) <- c("currentDate","probCatch","egg", "larva", "juvenil", "adult", "adultSlipped", "adultNotSliped",
                    "totCatched_adult", "totCatched_juv", "eggs_tot","larva_tot","juv_tot", "adult_tot", "migrantJuv_tot", "migrantAdult_tot")


levels <- as.numeric(levels(as.factor(data$probCatch)))

data$year <- as.integer(substring(data$currentDate,8,11))
level_year <- as.numeric(levels(as.factor(data$year)))
data$fangquote_adult <- data$totCatched_adult/data$adult

for ( pc in 1:length(levels)) {
  
  data_pc <- data[data$probCatch==levels[pc],]
  
  data_min <- c()
  data_max <- c()
  data_mean <- c()
  data_fangquote_1 <- c()
  data_fangquote_2 <- c()
  data_fangquote_3 <- c()
  
  for (i in 1:length(level_year)) {
    data_min[i] <- min((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
    data_max[i] <- max((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
    data_mean[i] <- mean((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
    data_fangquote_1[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[1]
    data_fangquote_2[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[2]
    data_fangquote_3[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[3]
  }
  
  data_graph <- data.frame(
    year = level_year,
    min = data_min,
    max = data_max,
    mean = data_mean,
    fq_1 = data_fangquote_1*2000,
    fq_2 = data_fangquote_2*2000,
    fq_3 = data_fangquote_3*2000
  )
  filter_year <- (data_graph %>% filter(data_graph$max > 2000))$year
  data_graph[data_graph$year==filter_year,3] <- 2000
  
  assign(paste("adult",pc,sep=""), ggplot(data_graph, aes(year, mean)) + 
        geom_point() +
        # geom_point(aes(year, fq_1), shape=4, color='grey')  +
        geom_linerange(aes(ymin=min, ymax=max)) +
        ggtitle(paste("Fangwahrscheinlicheit", levels[pc], sep=" ")) +
        ylab(("Anzahl Adulte")) +
        xlab("Jahr")
        # scale_y_continuous(limits=c(0,2000),
                           # sec.axis = sec_axis(~./2000,name="Fangquote in %"))
  )
}
data <- data1

levels <- as.numeric(levels(as.factor(data$probCatch)))

data$year <- as.integer(substring(data$currentDate,8,11))
level_year <- as.numeric(levels(as.factor(data$year)))
data$fangquote_adult <- data$totCatched_adult/data$adult

for ( pc in 1:length(levels)) {
  
  data_pc <- data[data$probCatch==levels[pc],]
  
  data_min <- c()
  data_max <- c()
  data_mean <- c()
  data_fangquote_1 <- c()
  data_fangquote_2 <- c()
  data_fangquote_3 <- c()
  
  for (i in 1:length(level_year)) {
    data_min[i] <- min((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
    data_max[i] <- max((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
    data_mean[i] <- mean((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
    data_fangquote_1[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[1]
    data_fangquote_2[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[2]
    data_fangquote_3[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[3]
  }
  
  data_graph <- data.frame(
    year = level_year,
    min = data_min,
    max = data_max,
    mean = data_mean,
    fq_1 = data_fangquote_1*2000,
    fq_2 = data_fangquote_2*2000,
    fq_3 = data_fangquote_3*2000
  )
  filter_year <- (data_graph %>% filter(data_graph$max > 2000))$year
  data_graph[data_graph$year==filter_year,3] <- 2000
  
  assign(paste("adult_alt",pc,sep=""), ggplot(data_graph, aes(year, mean)) + 
           geom_point() +
           # geom_point(aes(year, fq_1), shape=4, color='grey')  +
           geom_linerange(aes(ymin=min, ymax=max)) +
           ggtitle(paste("Fangwahrscheinlicheit", levels[pc], sep=" ")) +
           ylab(("Anzahl Adulte")) +
           xlab("Jahr")
         # scale_y_continuous(limits=c(0,2000),
         # sec.axis = sec_axis(~./2000,name="Fangquote in %"))
  )
}

