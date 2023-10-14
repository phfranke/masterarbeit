library(tidyverse)

data <- read.table("excel/kalibration.csv",sep=',', fill=TRUE, header=TRUE)
colnames(data) <- c("Modelldatum","simulation_name", "koeff", "tot_egg", "tot_larva", "tot_juv", "tot_adult", "tot_migrJuv", "tot_migrAdult", "anz_migrJuv", "anz_migrAdult", "catched_adult", "catched_juv", "count_pond", "count_egg", "count_larv", "count_juv", "count_adult", "mean_age_adult","min_age_adult", "max_age_adult","sd_age_adult", "median_age_adult")
str(data)

koeff_levels <- as.numeric(levels(as.factor(data$koeff)))

bp_adult <- ggplot(data, aes(group= koeff, x=koeff, y=count_adult)) + geom_boxplot()
bp_adult + scale_x_continuous(trans='log2')
bp_adult + scale_x_continuous(trans='log2') + scale_y_continuous(trans='log2')
bp_adult + scale_x_continuous(trans='log2', breaks=koeff_levels, name="Koeffizient") + scale_y_continuous(name="Anzahl Adulte")

bp_pond <- ggplot(data, aes(group= koeff, x=koeff, y=count_pond)) + geom_boxplot()
bp_pond + scale_x_continuous(trans='log2', breaks=koeff_levels, name="Koeffizient") + scale_y_continuous(name="Anzahl besiedelte Weiher")
