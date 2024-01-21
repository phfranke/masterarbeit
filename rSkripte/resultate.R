library(tidyverse)

data_4_11 <- read.table("excel/resultate_4.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")
data_11_11 <- read.table("excel/resultate_11.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")
data_13_11 <- read.table("excel/resultate_13.11.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")

str(data_13_11)


for (datum in 2:3){
  
  if (datum == 1) {
    data<-data_4_11
  } else if (datum == 2) {
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
  
  
  if (datum==1){
    data_2 <- cbind(data[,1:ncol(data)-1],pondCount)
    colnames(data_2) <- c("currentDate","probCatch","egg", "larva", "juvenil", "adult", "adultSlipped", "adultNotSliped",
                      "totCatched_adult", "totCatched_juv", "eggs_tot","larva_tot","juv_tot", "adult_tot", "migrantJuv_tot", "migrantAdult_tot")
  } else {
  data_2 <- cbind(data[,1:ncol(data)-1],pondCount)
  
  colnames(data_2) <- c("simulation", "currentDate","probCatch","egg", "larva", "juvenil", "adult", "adultSlipped", "adultNotSlipped",
                      "totCatched_adult", "totCatched_juv", "eggs_tot","larva_tot","juv_tot", "adult_tot", "migrantJuv_tot", "migrantAdult_tot", "pondCount")
  }
  
  levels <- as.numeric(levels(as.factor(data_2$probCatch)))
  
  data_2$year <- as.integer(substring(data_2$currentDate,8,11))
  level_year <- as.numeric(levels(as.factor(data_2$year)))
  
  for (pc in 1:length(levels)) {
      
    data_pc <- data_2[data_2$probCatch==levels[pc],]
    
    data_min <- c()
    data_max <- c()
    data_mean <- c()
    pond_min <- c()
    pond_max <- c()
    pond_mean <- c()
    
    for (i in 1:length(level_year)) {
      data_min[i] <- min((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
      data_max[i] <- max((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
      data_mean[i] <- mean((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
      pond_min[i] <- min((data_pc %>% filter(data_pc$year==level_year[i]))$pondCount)*100
      pond_max[i] <- max((data_pc %>% filter(data_pc$year==level_year[i]))$pondCount)*100
      pond_mean[i] <- mean((data_pc %>% filter(data_pc$year==level_year[i]))$pondCount)*100
    }
    
    data_graph_res <- data.frame(
      year = level_year,
      min = data_min,
      max = data_max,
      mean = data_mean,
      pond_min = pond_min,
      pond_max = pond_max,
      pond_mean = pond_mean
    )
    
    filter_year <- (data_graph_res %>% filter(data_graph_res$max > 2000))$year
    if (length(filter_year > 1)) {
      for (fy in 1:length(filter_year)) {
        data_graph_res[data_graph_res$year==filter_year[fy],3] <- 2000
      }
    }
    
    assign(paste("data_res",datum,pc,sep="_"),data_graph_res)
    
    assign(paste("graph_res",datum,pc,sep="_"), ggplot(data_graph_res, aes(year, mean)) + 
          # geom_point(aes(y=pond_mean,x=year),shape=15,color='blue',size=1.5, alpha=0.5) +
          geom_ribbon(aes(ymin=pond_min,ymax=pond_max), fill='blue', alpha=0.3) +
          geom_line(aes(y=pond_mean,x=year),color='blue',linewidth=1.5,alpha=0.3) +
          # geom_point() +
          geom_ribbon(aes(ymin=min,ymax=max), color='black',linetype=2, fill=NA) +
          geom_line(color='black',size=1,alpha=0.8) +
          # geom_linerange(aes(ymin=min, ymax=max)) +
          ggtitle(paste("Fangwahrscheinlicheit", paste(levels[pc]*100,'%'), sep=" ")) +
          ylab(("Anzahl Adulte")) +
          xlab("Jahr") + 
          scale_y_continuous(limits=c(0,2000),
            sec.axis = sec_axis(~./100,name="Anzahl besiedelter Gewaesser")
          ) +
          theme(axis.title.y.right=element_text(angle = 90),
                axis.line.y.right=element_line(colour='blue'),
                axis.ticks.y.right=element_line(colour='blue'),
                axis.line.y.left=element_line(colour='black'))+
          theme_bw()
      )
    }
}

  # ##### alte Daten
  # 
  # 
  # data <- data1
  # 
  # levels <- as.numeric(levels(as.factor(data$probCatch)))
  # 
  # data$year <- as.integer(substring(data$currentDate,8,11))
  # level_year <- as.numeric(levels(as.factor(data$year)))
  # data$fangquote_adult <- data$totCatched_adult/data$adult
  # 
  # for ( pc in 1:length(levels)) {
  #   
  #   data_pc <- data[data$probCatch==levels[pc],]
  #   
  #   data_min <- c()
  #   data_max <- c()
  #   data_mean <- c()
  #   data_fangquote_1 <- c()
  #   data_fangquote_2 <- c()
  #   data_fangquote_3 <- c()
  #   
  #   for (i in 1:length(level_year)) {
  #     data_min[i] <- min((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
  #     data_max[i] <- max((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
  #     data_mean[i] <- mean((data_pc %>% filter(data_pc$year==level_year[i]))$adult)
  #     data_fangquote_1[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[1]
  #     data_fangquote_2[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[2]
  #     data_fangquote_3[i] <- (data_pc %>% filter(data_pc$year==level_year[i]))$fangquote_adult[3]
  #   }
  #   
  #   data_graph <- data.frame(
  #     year = level_year,
  #     min = data_min,
  #     max = data_max,
  #     mean = data_mean,
  #     fq_1 = data_fangquote_1*2000,
  #     fq_2 = data_fangquote_2*2000,
  #     fq_3 = data_fangquote_3*2000
  #   )
  #   filter_year <- (data_graph %>% filter(data_graph$max > 2000))$year
  #   data_graph[data_graph$year==filter_year,3] <- 2000
  #   
  #   assign(paste("adult_alt",pc,sep=""), ggplot(data_graph, aes(year, mean)) + 
  #            geom_point() +
  #            # geom_point(aes(year, fq_1), shape=4, color='grey')  +
  #            geom_linerange(aes(ymin=min, ymax=max)) +
  #            ggtitle(paste("Fangwahrscheinlicheit", levels[pc], sep=" ")) +
  #            ylab(("Anzahl Adulte")) +
  #            xlab("Jahr")
  #          # scale_y_continuous(limits=c(0,2000),
  #          # sec.axis = sec_axis(~./2000,name="Fangquote in %"))
  #   )
  # }
# }