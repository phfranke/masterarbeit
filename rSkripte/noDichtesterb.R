library(tidyverse)

data_noDichtesterb <- read.table("excel/resultate_ohneDichtesterb.csv",sep=',', fill=TRUE, header=TRUE, quote="\"")
# str(data_noDichtesterb)

jahr <- as.integer(substring(data_noDichtesterb$current_date,8,11))
adult <- data_noDichtesterb[ ,7]

data_nd <- as.data.frame(cbind(jahr,"Kontrolle",adult))
data_nd$jahr <- as.integer((data_nd$jahr))
data_nd$adult <- as.integer((data_nd$adult))

# ggplot(data_nd, aes(x=jahr,y=adult)) + geom_line() +
#   scale_x_continuous(name="Jahr") +
#   scale_y_continuous(name="Anzahl Adulte", limits=c(0,2000)) +
#   ggtitle("Modell ohne dichteabhaengige Sterblichkeit")
names(data_nd) <- names(data_sim_1[1:3])
data_sim_kopie <- data_sim_1 %>% filter(data_sim_1$temp_sim=='Simulation 7')
data_graph_kontrolle <- rbind(data_sim_kopie[1:length(data_nd$temp_year),c(1,2,3)],data_nd) 

graph_dichte <- 
  ggplot(data_graph_kontrolle, aes(x=temp_year,y=temp_data,group=temp_sim)) +
  geom_line(aes(color=temp_sim)) +
  scale_color_manual(values=c("#009E73","black")) +
  scale_y_continuous(name="Adulte Kammmolche", limits=c(0,4000)) +
  scale_x_continuous(limits=c(2000,2040), name="Jahr") +
  ggtitle("Mit und ohne Dichteabhaengigkeit") +
  theme(legend.position="right", legend.title = element_blank())

            