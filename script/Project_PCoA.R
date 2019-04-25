##### LIBRARY #####
library(ggplot2)
library(tidyr)
library(dplyr)

##### SET WORKING DIRECTORY #####
setwd("~/Desktop/ANSC595/project")

###### DATA #####
pcoaload <-read.table(file = "final.tre1.weighted.ave.pcoa.axes", sep = "\t", header = T)
meta <- read.csv(file="groupmeta.csv",header=TRUE, sep=',')

#Makes sure that the meta table and the otu table have the same samples
meta <- meta[meta$Group %in% rownames(pcoaload),] 
pcoaload <- pcoaload[rownames(pcoaload) %in% meta$Group,]

#merging 
metapcoa <- merge(meta, pcoaload, by.x = 'Group', by.y = 'group')

###### PLOTS #######
ggplot(metapcoa, aes(x=axis1, y=axis2, color=Region)) + 
  geom_point(aes(color=Region, shape=age)) +
  labs(title="Weighted Unifrac") +
  stat_ellipse()

ggsave("graphs/unifracellipse.png", height = 5, width = 5)
