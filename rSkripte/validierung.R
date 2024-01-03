library(tidyverse)

data_11_11 <- read.table("excel/resultate_11.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")
data_13_11 <- read.table("excel/resultate_13.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")

# str(data_13_11)

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
  
  year <- as.integer(substring(data$currentDate,8,11))
  data$year <- year
  
  sim_levels <- levels(as.factor(data$simulation))
  
  data_catch_2021 <- c()
  data_catch_diff <- c()
  data_catch_prob <- c()
  data2021 <- data[data$currentDate=="date ('2022-01-01 00:00:00')",]
  data2022 <- data[data$currentDate=="date ('2023-01-01 00:00:00')",]
  temp_year <- c()
  temp_data <- c()
  temp_sim <- c()
  
  for (sl in 1:length(sim_levels)) {
    catch2021 <- data2021$totCatched_adult[data2021$simulation==sim_levels[sl]]
    catch2022 <- data2022$totCatched_adult[data2022$simulation==sim_levels[sl]]
    data_catch_2021[sl] <- catch2021;
    data_catch_diff[sl] <- catch2022 - catch2021
    data_catch_prob[sl] <- data2021$probCatch[data2021$simulation==sim_levels[sl]]
    temp_year <- append(temp_year,data$year[data$simulation==sim_levels[sl]])
    temp_data <- append(temp_data, data$adult[data$simulation==sim_levels[sl]])
    temp_sim <- append(temp_sim,rep(sim_levels[sl],length(data$year[data$simulation==sim_levels[sl]])))
  }
  
  df <- as.data.frame(cbind(data_catch_2021,data_catch_diff,data_catch_prob))
  assign(paste("data_catch",datum,sep="_") , df)
  df2 <- as.data.frame(cbind(data$year,data$probCatch,data$totCatched_adult))
  assign(paste("data_catch_year",datum,sep="_") , df2)
  sortierung <- (as.integer(substring(temp_sim,12))+1) * temp_year
  df3 <- as.data.frame(cbind(temp_year,temp_sim,temp_data,sortierung))[order(sortierung),]
  assign(paste("data_sim",datum,sep="_"),df3)
}

data_tot <- rbind(data_catch_1,data_catch_2)
data_tot <- data_tot[order(data_tot$data_catch_prob),]

data_tot_year <- rbind(data_catch_year_1,data_catch_year_2)
year_levels <- as.integer(levels(as.factor(data_tot_year[,1])))
catch_year_min <- c()
catch_year_max <- c()
catch_year_mean <- c()
for (i in 2:(length(year_levels))) {
  catched <- data_tot_year[data_tot_year[,1]==year_levels[i] & data_tot_year[,2]>0.7 , 3] - 
              data_tot_year[data_tot_year[,1]==year_levels[i-1] & data_tot_year[,2]>0.7 , 3]
  catch_year_min[i-1] <- min(catched)
  catch_year_max[i-1] <- max(catched)
  catch_year_mean[i-1] <- mean(catched)
}

year_levels <- year_levels[1:(length(year_levels)-1)]

data_tot_year_graph <- as.data.frame(cbind(year_levels,catch_year_min, catch_year_mean,catch_year_max))
levels_pc <- as.numeric(levels(as.factor(data_tot$data_catch_prob)))

graph_val <- ggplot(data_tot, aes(data_catch_prob,data_catch_diff,group=data_catch_prob)) + 
  geom_boxplot() + 
  geom_hline(yintercept=1192, linewidth=1, linetype=2) +
  scale_x_continuous(breaks=levels, name="Fangwahrscheinlichkeit") +
  scale_y_continuous(name="Gefangene adulte Kammmolche") +
  ggtitle("Modellierte Fänge im Jahr 2022")

graph_val2 <- ggplot(data_tot, aes(data_catch_prob,data_catch_2021,group=data_catch_prob)) + 
  geom_boxplot() + 
  geom_hline(yintercept=1192, linewidth=1, linetype=2) +
  scale_x_continuous(breaks=levels_pc, name="Fangwahrscheinlichkeit") +
  scale_y_continuous(name="Gefangene adulte Kammmolche") +
  ggtitle("Modellierte Fänge im Jahr 2021")

graph_val_year <- ggplot(data_tot_year_graph[data_tot_year_graph$year>=2020,], aes(year_levels,catch_year_mean)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=catch_year_min,ymax=catch_year_max), fill='darkgrey', alpha=0.3) +
  scale_x_continuous(name="Jahr", limits=c(2020,2050), breaks=seq(2020,2050,5)) +
  scale_y_continuous(name="Gefangene Adulte") +
  ggtitle("Gefangene Kammmolche 2020-2048")


data_sim_1[,1] <- as.integer(data_sim_1[,1])
data_sim_1[,3] <- as.integer(data_sim_1[,3])
data_sim_2[,1] <- as.integer(data_sim_2[,1])
data_sim_2[,3] <- as.integer(data_sim_2[,3])

graph_val_sim_1 <-  ggplot(data_sim_1,aes(temp_year,temp_data,group=temp_sim)) +
  geom_line(aes(linetype=temp_sim,color=temp_sim)) +
  scale_x_continuous(name="Jahr", limits=c(2000,2050), breaks=seq(2000,2050,5)) +
  scale_y_continuous(name="Adulte Kammmolche", limits=c(0,2000)) +
  scale_linetype_manual("Legende", values=rep(c("solid","dashed","twodash"),5)) +
  scale_color_manual("Legende", values=rep(c("#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00"),3)) +
  theme(legend.position="right") + 
  ggtitle("Populationsgrösse mit der Modellversion 1")
graph_val_sim_2 <-  ggplot(data_sim_2,aes(temp_year,temp_data,group=temp_sim)) +
  geom_line(aes(linetype=temp_sim,color=temp_sim)) +
  scale_x_continuous(name="Jahr", limits=c(2000,2050), breaks=seq(2000,2050,5)) +
  scale_y_continuous(name="Adulte Kammmolche", limits=c(0,2000)) +
  scale_linetype_manual("Legende",values=rep(c("solid","dashed","twodash"),5)) +
  scale_color_manual("Legende", values=rep(c("#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00"),3)) +
  theme(legend.position="right") +
  ggtitle("Populationsgrösse mit der Modellversion 2")
