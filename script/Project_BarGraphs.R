##### LIBRARY #####
library(ggplot2)
library(vegan)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(tidyr)

##### SET WORKING DIRECTORY #####
setwd("~/Desktop/ANSC595/project")

###### DATA #####
otu_subsample <- read.table(file="final.tx.shared", sep = "\t", header = T) 
taxonomy <- read.table(file="final.tx.taxonomy", sep = "\t", header = T) 
nmds <- read.table(file = "tables/nmds.txt", sep =' ', header = TRUE)
meta <- read.csv(file="groupmeta.csv",header=TRUE, sep=',')

row.names(otu_subsample) <- otu_subsample$Group
otu_subsample <- otu_subsample[,-c(1:3)]

taxonomy <- separate(data = taxonomy, col = Taxonomy, into = c("kingdom", "phylum", "class", "family", "order", "genus", "species"), sep = ";")
str(taxonomy)

metanmds <- merge(nmds, meta, by.x="group", by.y="Group")


# Set colors for plotting
my_colors <- c(
  '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
  '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928', 
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD",
   "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861", "black"
)

#If you want different taxonomic level, find and replace the taxonomic level listed here
rm(otu.summary)
otu.summary <- prop.table(as.matrix(otu_subsample), 1) 
otu_abund <- colSums(otu.summary)
otu.summary <- rbind(otu_abund, otu.summary)
otu.summary_sorted <- otu.summary[,order(otu.summary[1,], decreasing = TRUE)]


#top 16 most abundant genera
num_genera <- 16 
melt_otu <- melt(otu.summary_sorted[,c(1:num_genera)])
colnames(melt_otu) <- c("Sample", "OTU", "Abundance")
tail(melt_otu)


#Putting it all together: merge melt_otu, metadata, taxonomy tables
meta_otu <- merge(metanmds, melt_otu, by.x = "group", by.y = "Sample")
str(meta_otu)
str(taxonomy)
meta_otu_tax <- merge(meta_otu, taxonomy)
str(meta_otu_tax)
summary(meta_otu_tax$group)
#sorting based on MDS1 from negative to positive (NMDS axis 1)
meta_otu_tax <- meta_otu_tax[order(meta_otu_tax$MDS1),]
#ordering samples based on NMDS axis 1
meta_otu_tax$group <- factor(meta_otu_tax$group, levels=unique(as.character(meta_otu_tax$group)) )


###### PLOTS #######
ggplot(meta_otu_tax, aes(x = individual, y = Abundance, fill = order)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  ylim(c(0,1)) +
  guides(fill = guide_legend(reverse = F, keywidth = .5, keyheight = .5, ncol = 1)) +
  theme(legend.text=element_text(size=8)) +
  #theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab(paste0("Relative Abundance (top ", num_genera, " genera)")) +
  ggtitle("OrderComposition ") 
ggsave("graphs/orderBarPlotNMDS1_Phylo_16.png", width = 10, height = 4)

##genus
ggplot(meta_otu_tax, aes(x = individual, y = Abundance, fill = genus)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  ylim(c(0,1)) +
  guides(fill = guide_legend(reverse = F, keywidth = .5, keyheight = .5, ncol = 1)) +
  theme(legend.text=element_text(size=8)) +
  #theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab(paste0("Relative Abundance (top ", num_genera, " genera)")) +
  ggtitle("OrderComposition ") 
ggsave("graphs/genusBarPlotNMDS1_Phylo_16.png", width = 10, height = 4)


