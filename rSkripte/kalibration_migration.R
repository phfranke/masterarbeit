library(tidyverse)

data <- read.table("excel/kalibration_migration.csv",sep=',', fill=TRUE, header=TRUE)
colnames(data) <- c("Modelldatum","simulation", "migrationsdauer", "pondCount", "eggCount", "larvaCount", "juvenilCount", "adultCount", "min_age", "max_age", "mean_age", "median_age", "sd_age", "catchedAdult_count", "catchedJuv_count", "min_age_catched", "max_age_catched", "mean_age_catched")
str(data)

levels <- as.numeric(levels(as.factor(data$migrationsdauer)))

bp_pond <- ggplot(data, aes(group= migrationsdauer, x=migrationsdauer, y=pondCount)) + geom_boxplot()
bp_pond + scale_x_continuous(trans='log2', breaks=levels, name="Migrationsdauer (Tage)") + scale_y_continuous(name="Anzahl besiedelte Weiher")
