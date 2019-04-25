##### LIBRARY #####
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)

##### SET WORKING DIRECTORY #####
setwd("~/Desktop/ANSC595/project")

###### DATA #####
taxonomy <- read.table(file="final.cons.taxonomy", sep = "\t", header = T) #Read the taxonomy file into R
taxonomy <- separate(data = taxonomy, col = Taxonomy, into = c("kingdom", "phylum", "class", "family", "order", "genus", "species"), sep = ";")
test <- read_tsv(file = "final.opti_mcc.0.03.subsample.0.03.lefse_summary")
name <- data_frame(name=c("ales","leva"),region=c("Piedmont","Briesca"))

#arranging table
qvalue<-p.adjust(test$pValue, method="fdr")
test2<-cbind(test, qvalue)
test3<-na.omit(test2)
test3<-filter(test3, pValue <= 0.05, qvalue <=0.05)
test4<-test3%>%merge(., taxonomy, by.x= "OTU", by.y = "OTU")
test4$OTUgen <- paste(test4$OTU, "_", test4$genus)
test5<-data.frame(Class=test4$Class, region=name[match(test4$Class, name$name),2])
test6<-cbind(test4,region=test5$region)
test7<-test6[c(1:10),]
results <- test6 %>% select(OTUgen,region, Size, LDA, pValue, qvalue)
results <- test7 %>% select(OTUgen,region, Size, LDA, pValue, qvalue)
write.csv(results, file = paste0("Tables/lefse.csv"), quote = FALSE)

###### PLOTS #######
ggplot(test6, aes(x = interaction(OTUgen,region), y = LDA)) +
  geom_bar(aes(fill = region), stat = 'identity') +  # color by class
  #geom_text(aes(label = genus, y = 1.5), size=2) +
  #theme(axis.text.y = element_text() ) +
  coord_flip() +  # horizontal bars
  xlab("Genus") +
  theme(axis.ticks.y = element_blank()) +
  #scale_y_discrete("Class",breaks=test4$Class,labels=test4$Class)
  scale_x_discrete(breaks=interaction(test6$OTUgen,test6$region),labels=test6$OTUgen)
ggsave("graphs/lefse_region.png")

ggplot(test7, aes(x = interaction(OTUgen,region), y = LDA)) +
  geom_bar(aes(fill = region), stat = 'identity') +  # color by class
  #geom_text(aes(label = genus, y = 1.5), size=2) +
  #theme(axis.text.y = element_text() ) +
  coord_flip() +  # horizontal bars
  xlab("Genus") +
  theme(axis.ticks.y = element_blank()) +
  #scale_y_discrete("Class",breaks=test4$Class,labels=test4$Class)
  scale_x_discrete(breaks=interaction(test6$OTUgen,test6$region),labels=test6$OTUgen)
ggsave("graphs/lefse_region_10.png")

