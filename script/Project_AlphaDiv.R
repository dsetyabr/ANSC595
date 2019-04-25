##### LIBRARY #####
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)

##### SET WORKING DIRECTORY #####
setwd("~/Desktop/ANSC595/project")

###### DATA #####
alpha_div <- read.table(file = "final.opti_mcc.groups.summary2", sep = "\t", header = T)
test <- read.table(file = "../mothur_data/data/Koziol_metadata.txt", sep = "\t", header = TRUE)
meta <- read.csv(file="groupmeta.csv",header=TRUE, sep=',')

#removing sequencing ID and leaving just sample name
alpha_div_merge <- merge(meta, alpha_div, by.x = "Group", by.y = "group")
unique(alpha_div_merge$Region)
unique(alpha_div_merge$age)
unique(alpha_div_merge$studyage)
unique(alpha_div_merge$individual)

results <- alpha_div_merge[,-c(2,4,5,6)]
results <- results[,c(1:3,5:7,8,11,14,15,18)]
write.csv(results, file = paste0("Tables/alphadiv.csv"), quote = FALSE)

#checking boxplots
qplot(Region, chao, geom = "boxplot", colour = Region, data = alpha_div_merge, size = I(0.3))
qplot(age, chao, geom = "boxplot", colour = age, data = alpha_div_merge, size = I(0.3))
qplot(studyage, chao, geom = "boxplot", colour = age, data = alpha_div_merge, size = I(0.3))
qplot(individual, chao, geom = "boxplot", colour = individual, data = alpha_div_merge, size = I(0.3))
qplot(Region, shannon, geom = "boxplot", colour = Region, data = alpha_div_merge, size = I(0.3))
qplot(age, shannon, geom = "boxplot", colour = age, data = alpha_div_merge, size = I(0.3))
qplot(studyage, shannon, geom = "boxplot", colour = age, data = alpha_div_merge, size = I(0.3))
qplot(individual, shannon, geom = "boxplot", colour = individual, data = alpha_div_merge, size = I(0.3))
qplot(Region, invsimpson, geom = "boxplot", colour = Region, data = alpha_div_merge, size = I(0.3))
qplot(age, invsimpson, geom = "boxplot", colour = age, data = alpha_div_merge, size = I(0.3))
qplot(studyage, invsimpson, geom = "boxplot", colour = age, data = alpha_div_merge, size = I(0.3))
qplot(individual, invsimpson, geom = "boxplot", colour = individual, data = alpha_div_merge, size = I(0.3))

###### PLOTS #######
#Get figures for manuscript.
chao <- ggplot(alpha_div_merge, aes(age, chao)) + 
  geom_boxplot(aes(color = age)) + 
  ylim(c(0,10000))
ggsave("graphs/chao_age.png")
chao <- ggplot(alpha_div_merge, aes(Region, chao)) + 
  geom_boxplot(aes(color = Region)) + 
  ylim(c(0,10000))
ggsave("graphs/chao_Regions.png")
chao <- ggplot(alpha_div_merge, aes(studyage, chao)) + 
  geom_boxplot(aes(color = studyage)) + 
  ylim(c(0,10000))
ggsave("graphs/chao_studyage.png")
chao <- ggplot(alpha_div_merge, aes(individual, chao)) + 
  geom_boxplot(aes(color = individual)) + 
  ylim(c(0,10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("graphs/chao_individual.png")

shannon <- ggplot(alpha_div_merge, aes(age, shannon)) + 
  geom_boxplot(aes(color = age)) + 
  ylim(c(0,4))
ggsave("graphs/shannon_age.png")
shannon <- ggplot(alpha_div_merge, aes(Region, shannon)) + 
  geom_boxplot(aes(color = Region)) + 
  ylim(c(0,4))
ggsave("graphs/shannon_region.png")
shannon <- ggplot(alpha_div_merge, aes(studyage, shannon)) + 
  geom_boxplot(aes(color = studyage)) + 
  ylim(c(0,4))
ggsave("graphs/shannon_studyage.png")
shannon <- ggplot(alpha_div_merge, aes(individual, shannon)) + 
  geom_boxplot(aes(color = individual)) + 
  ylim(c(0,4)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("graphs/shannon_individual.png")

shannoneven <- ggplot(alpha_div_merge, aes(age, shannoneven)) + 
  geom_boxplot(aes(color = age)) + 
  ylim(c(0,1))
ggsave("graphs/shannoneven_age.png")
shannoneven <- ggplot(alpha_div_merge, aes(Region, shannoneven)) + 
  geom_boxplot(aes(color = Region)) + 
  ylim(c(0,1))
ggsave("graphs/shannoneven_region.png")
shannoneven <- ggplot(alpha_div_merge, aes(studyage, shannoneven)) + 
  geom_boxplot(aes(color = studyage)) + 
  ylim(c(0,1))
ggsave("graphs/shannoneven_studyage.png")
shannoneven <- ggplot(alpha_div_merge, aes(individual, shannoneven)) + 
  geom_boxplot(aes(color = individual)) + 
  ylim(c(0,1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("graphs/shannoneven_individual.png")

invsimpson <- ggplot(alpha_div_merge, aes(age, invsimpson)) + 
  geom_boxplot(aes(color = age)) + 
  ylim(c(0,15))
ggsave("graphs/invsimpson_age.png")
invsimpson <- ggplot(alpha_div_merge, aes(Region, invsimpson)) + 
  geom_boxplot(aes(color = Region)) + 
  ylim(c(0,15))
ggsave("graphs/invsimpson_region.png")
invsimpson <- ggplot(alpha_div_merge, aes(studyage, invsimpson)) + 
  geom_boxplot(aes(color = studyage)) + 
  ylim(c(0,15))
ggsave("graphs/invsimpson_studyage.png")
invsimpson <- ggplot(alpha_div_merge, aes(individual, invsimpson)) + 
  geom_boxplot(aes(color = individual)) + 
  ylim(c(0,15))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("graphs/invsimpson_individual.png")

simpson <- ggplot(alpha_div_merge, aes(age, simpson)) + 
  geom_boxplot(aes(color = age)) + 
  ylim(c(0,0.5))
ggsave("graphs/simpson_age.png")
simpson <- ggplot(alpha_div_merge, aes(Region, simpson)) + 
  geom_boxplot(aes(color = Region)) + 
  ylim(c(0,0.5))
ggsave("graphs/simpson_region.png")
simpson <- ggplot(alpha_div_merge, aes(studyage, simpson)) + 
  geom_boxplot(aes(color = studyage)) + 
  ylim(c(0,0.5))
ggsave("graphs/simpson_studyage.png")
simpson <- ggplot(alpha_div_merge, aes(individual, simpson)) + 
  geom_boxplot(aes(color = individual)) + 
  ylim(c(0,0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("graphs/simpson_individual.png")

##Run the ANOVAs for statistics
region <- unique(alpha_div_merge$Region)
age <- unique(alpha_div_merge$age)
studyage <- unique(alpha_div_merge$studyage)
ad_metrics <- c("chao", "shannon", "invsimpson", "simpson","shannoneven")

##Region
for(m in ad_metrics){
  print(m)
  aov_temp <- aov(get(m) ~ Region, data = alpha_div_merge)
  summary(aov_temp)
  anova_summary <- as.data.frame(summary(aov_temp)[[1]])
  write.table(anova_summary, file = paste0("Tables/region_anova_", m, ".txt"), sep = "\t", quote = FALSE)
  tukey_out <- TukeyHSD(aov_temp)
  tukey_out_df <- as.data.frame(tukey_out$Region)
  tukey_out_df$ad_metric <- m
  write.table(tukey_out_df, file = paste0("Tables/region_tukey_", m, ".txt"), sep = "\t", quote = FALSE)
}

##age
for(m in ad_metrics){
  print(m)
  aov_temp <- aov(get(m) ~ age, data = alpha_div_merge)
  summary(aov_temp)
  anova_summary <- as.data.frame(summary(aov_temp)[[1]])
  write.table(anova_summary, file = paste0("Tables/age_anova_", m, ".txt"), sep = "\t", quote = FALSE)
  tukey_out <- TukeyHSD(aov_temp)
  tukey_out_df <- as.data.frame(tukey_out$age)
  tukey_out_df$ad_metric <- m
  write.table(tukey_out_df, file = paste0("Tables/age_tukey_", m, ".txt"), sep = "\t", quote = FALSE)
}

##regionage
for(r in region){
print(r)
for(m in ad_metrics){
  print(m)
  aov_temp <- aov(get(m) ~ studyage, data = subset(alpha_div_merge, Region==r))
  summary(aov_temp)
  anova_summary <- as.data.frame(summary(aov_temp)[[1]])
  write.table(anova_summary, file = paste0("Tables/studyage_anova_",r,"_", m, ".txt"), sep = "\t", quote = FALSE)
  tukey_out <- TukeyHSD(aov_temp)
  tukey_out_df <- as.data.frame(tukey_out$studyage)
  tukey_out_df$ad_metric <- m
  write.table(tukey_out_df, file = paste0("Tables/studyage_tukey_",r,"_", m, ".txt"), sep = "\t", quote = FALSE)
}
}

##regionyage

for(m in ad_metrics){
    print(m)
    aov_temp <- aov(get(m) ~ studyage,data = alpha_div_merge)
    summary(aov_temp)
    anova_summary <- as.data.frame(summary(aov_temp)[[1]])
    write.table(anova_summary, file = paste0("Tables/studyage_anova_", m, ".txt"), sep = "\t", quote = FALSE)
    tukey_out <- TukeyHSD(aov_temp)
    tukey_out_df <- as.data.frame(tukey_out$studyage)
    tukey_out_df$ad_metric <- m
    write.table(tukey_out_df, file = paste0("Tables/studyage_tukey_", m, ".txt"), sep = "\t", quote = FALSE)
  }





