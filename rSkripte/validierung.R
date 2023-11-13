library(tidyverse)

data_11_11 <- read.table("excel/resultate_11.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")
data_13_11 <- read.table("excel/resultate_13.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")

str(data_13_11)

for (datum in 1:2){
  
  if (datum == 1) {
    data<-data_11_11
  } else {
    data <- data_13_11
  }
  
  pondList <- c()
  pondCount <- c()
  for ( pond in 1:nrow(data)){
    text <- data[pond,ncol(data)]
    if (nchar(text) <= 4) {pondList[pond] <- NA}
    else {pondList[pond] <- strsplit(substring(text,3,nchar(text)-2),",")}
    pondCount[pond] <- length(unlist(pondList[pond]))
  }
  
  pondList_summary <- levels(as.factor(unlist(pondList)))
  pondList_count <- c()
  for (pond in 1:length(pondList_summary)){
    pondList_count[pond] <- 0
    for (list in 1:length(pondList)) {
      if (pondList_summary[pond] %in% pondList[list][[1]]) {pondList_count[pond] <- pondList_count[pond]+1}
    }
  }
  pondList_Count <- round(pondList_count / 750,2)
  pondBesiedelt <- cbind(pondList_summary, pondList_Count)
  
  stand <- rep(datum,nrow(data))
  
  colnames(data) <- c("simulation", "currentDate","probCatch","egg", "larva", "juvenil", "adult", "adultSlipped", "adultNotSlipped",
                      "totCatched_adult", "totCatched_juv", "eggs_tot","larva_tot","juv_tot", "adult_tot", "migrantJuv_tot", "migrantAdult_tot","list")
  
  var_name <- paste("data_2",datum,sep="_")
  assign(var_name , cbind(data[,1:ncol(data)-1],pondCount,stand))
}
data_tot <- rbind(data_2_1,data_2_2)

levels <- as.numeric(levels(as.factor(data_tot$probCatch)))

data_tot$year <- as.integer(substring(data_tot$currentDate,8,11))

catched_adult_tot_2022 <- data_tot$totCatched_adult[data_tot$year==2023]
catched_adult_tot_2021 <- data_tot$totCatched_adult[data_tot$year==2022]
catched_adult_tot <- catched_adult_tot_2022 - catched_adult_tot_2021
pc <- data_tot$probCatch[data_tot$year==2022]
data_graph <- as.data.frame(pc, catched_adult_tot)
graph_val <- ggplot(data_graph, aes(pc,catched_adult_tot,group=pc)) + 
  geom_boxplot() + 
  geom_hline(yintercept=1192, linewidth=1, linetype=2) +
  scale_x_continuous(breaks=levels, name="Fangwahrscheinlichkeit") +
  scale_y_continuous(name="Gefangene adulte Kammmolche 2022")
