library(tidyverse)
data <- read.table("excel/kalibration_migration.csv",sep=',', fill=TRUE, header=TRUE)
str(data)

time <- as.numeric(levels(as.factor(data[,3])))
ponds_count <- data[,4]
mittel <- c(mean(ponds_count[1:5]),mean(ponds_count[6:10]), mean(ponds_count[11:15]))
max <-  c(max(ponds_count[1:5]),max(ponds_count[6:10]), max(ponds_count[11:15]))
min <-  c(min(ponds_count[1:5]),min(ponds_count[6:10]), min(ponds_count[11:15]))
data <- data.frame(
  time = time,
  mean = mittel,
  max = max,
  min = min
)

time_levels <- as.numeric(levels(as.factor(data$time)))

bp_pond <- ggplot(data, aes(time, mean))
ponds <- bp_pond + geom_linerange(aes(ymin=min, ymax=max)) +
 geom_point() + 
  geom_hline(yintercept=5, linewidth=1, linetype=2) +  
  geom_hline(yintercept=6, linewidth=1, linetype=2) + 
  geom_smooth(method = "lm", se = FALSE, linewidth=0.5, color="grey")

ponds_graph <- ponds + 
  geom_point(data=data.frame(x=13,y=5.5), aes(x=x,y=y), shape=3, size=3) + 
  ylim(0,max(max)) + 
  scale_y_continuous(breaks=seq(min(min),max(max)+1, by=2), name="Anzahl besiedelte Weiher") +
  scale_x_continuous(breaks=c(time), name="Wanderdauer (Tage)")

                                                                 