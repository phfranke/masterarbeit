library(tidyverse)
library(stringr)
calibration_data <- read.table("excel/kalibration_listeAdult.csv",sep=';', fill=TRUE)
calibration_length <- c()
max_data_length <- 0
for (i in 2:nrow(calibration_data)) {
  row_string <- toString(calibration_data[i,])
  row_string <- str_replace_all(row_string,"[\\[\\]]","")
  row_string <- noquote(row_string)
  name <- as.character(i-1)
  calibration_length[[i-1]] <- as.numeric(unlist(strsplit(row_string, ',')))
  if (length(calibration_length[[i-1]]) > max_data_length ) {
    max_data_length <- length(calibration_length[[i-1]])
  }
}

for ( i in 1:length(calibration_length) ) {
  if (length(calibration_length[[i]]) < max_data_length) {
    for (j in length(calibration_length[[i]]):max_data_length) {
      calibration_length[[i]][j] <- NA
    }
  }
}
calibration_length_df <- data.frame(calibration_length)
colnames(calibration_length_df) <- seq(1,length(calibration_length))
boxplot(calibration_length_df)

