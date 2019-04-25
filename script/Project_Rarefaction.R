##### LIBRARY #####
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)

##### SET WORKING DIRECTORY #####
setwd("~/Desktop/ANSC595/project")

###### DATA #####
meta <- read.csv(file="groupmeta.csv",header=TRUE, sep=',')

V4rfact <- read_tsv(file = "final.opti_mcc.groups.rarefaction")%>%
  select(-contains("lci-"), -contains("hci-")) %>%
  gather(-numsampled, key=sample, value=coverage) %>%
  mutate(sample=str_replace_all(sample, pattern="0.03-", replacement="")) %>%
  drop_na()

V4meta_rare <- meta%>%
  #sample_n(20) %>%
  merge(., V4rfact, by.x= "Group", by.y = "sample")

###### PLOTS #######
ggplot(V4meta_rare, aes(x=numsampled, y=coverage, group=individual, color=individual)) +
  geom_line()+
  geom_vline(xintercept=3000) +
  coord_cartesian(xlim=c(0,20000)) +
  labs(x="Number of Sequences Sampled per Subject",
       y="Number of OTUs per Subject") +
  theme_classic()
ggsave("graphs/Rarefaction_all.png")

