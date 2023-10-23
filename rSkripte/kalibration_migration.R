library(tidyverse)

time <- c(20,20,20,20,20,30,30,30,30,30,40,40,40,40,40)
ponds_count <- c(2,2,2,2,2,5,5,4,5,5,6,6,6,6,5)

mittel_2 <- mean(c(5,5,4,5,5))
mittel_3 <- mean(c(6,6,6,6,5))
data <- data.frame(
  time = c(20,30,40),
  mean = c(2,mittel_2,mittel_3),
  max = c(2,5,6),
  min = c(2,4,5)
)

levels <- as.numeric(levels(as.factor(data$time)))

bp_pond <- ggplot(data, aes(time, mean))
ponds <- bp_pond + geom_linerange(aes(ymin=min, ymax=max)) +
 scale_x_continuous(breaks=c(20,30,40), name="Wanderdauer (Tage)") +
 scale_y_continuous(name="Anzahl besiedelte Weiher") +
 geom_point()

ponds + geom_hline(yintercept=5, linewidth=1, linetype=2)
