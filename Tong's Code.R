library(ggplot2)
library(dplyr)
library(reshape2)
##Peak hours, RG final stop
Peak <- read.csv("PeakData.csv", header = T)
Peak_1 <- Peak[-1,-1]
#View(Peak_1)
#Calculate the difference between each stop
Peak_RG <- Peak_1
Peak_RG[lower.tri(Peak_1)] <- 0
Peak_RG_1 <- as.data.frame(t(Peak_RG))
difference <- diff(as.matrix(Peak_RG_1)) %>% as.data.frame()

#Set index for plot
difference <- cbind(difference, index = c(2:21))
df <- melt(difference, id.vars = 'index', variable.name = 'series')

ggplot(df, aes(index, value)) +
  geom_line(aes(color = series)) 
