library(tidyverse)

data <- read.table("excel/kalibration_density.csv",sep=',', fill=TRUE, header=TRUE)
colnames(data) <- c("Modelldatum","simulation", "koeff", "tot_egg", "tot_larva", "tot_juv", "tot_adult", "tot_migrJuv", "tot_migrAdult", "anz_migrJuv", "anz_migrAdult", "count_pond", "count_egg", "count_larv", "count_juv", "count_adult","count_adult_slipped","count_adult_not_slipped", "count_juv_slipped", "count_juv_not_slipped", "catched_adult_tot", "catched_juv_tot","min_age_adult", "max_age_adult","mean_age_adult", "median_age_adult","sd_age_adult", "catched_adult", "catched_juv", "min_age_catched", "max_age_catched", "mean_age_catched")
str(data)

koeff_levels <- as.numeric(levels(as.factor(data$koeff)))
min <- c(min(data$catched_adult_tot[data$koeff==0.006]),min(data$catched_adult_tot[data$koeff==0.008]),min(data$catched_adult_tot[data$koeff==0.01]))
max <- c(max(data$catched_adult_tot[data$koeff==0.006]),max(data$catched_adult_tot[data$koeff==0.008]),max(data$catched_adult_tot[data$koeff==0.01]))
mean <- c(mean(data$catched_adult_tot[data$koeff==0.006]),mean(data$catched_adult_tot[data$koeff==0.008]),mean(data$catched_adult_tot[data$koeff==0.01]))
data_graph <- data.frame(koeff_levels,min,max,mean)

# bp_adult <- ggplot(data, aes(group= koeff, x=koeff, y=catched_adult_tot)) + geom_boxplot()
# bp_adult + scale_x_continuous(trans='log2')
# bp_adult + scale_x_continuous(trans='log2') + scale_y_continuous(trans='log2')
# bp_print <- bp_adult + scale_x_continuous(trans='log2', breaks=koeff_levels, name="Koeffizient") + scale_y_continuous(name="Anzahl gefangene Adulte")

graph_density <- ggplot(data_graph, aes(koeff_levels,mean))
graph <- graph_density  + geom_linerange(aes(ymin=min, ymax=max)) +
  scale_x_continuous(breaks=koeff_levels, name="Dichtekoeffizient") +
  scale_y_continuous(name="Gefangene adulte Kammmolche 2021") +
  geom_point() +
  geom_hline(yintercept=700, linewidth=1, linetype=2) +
  geom_hline(yintercept=1200, linewidth=1, linetype=2)


