##### LIBRARY #####
library(vegan)
library(ggplot2)
library(tidyr)
library(dplyr)

##### SET WORKING DIRECTORY #####
setwd("~/Desktop/ANSC595/project")

##### FUNCTIONS #####
veganCovEllipse <- function (cov, center = c(0,0), scale = 1, npoints = 100){
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

###### DATA #####
otu_subsample <-read.table(file = "final.opti_mcc.0.03.subsample.shared", sep = "\t", header = T)
meta <- read.csv(file="groupmeta.csv",header=TRUE, sep=',')

#Stores the sample name info as the rownames of the dataframe rather
rownames(otu_subsample) <- otu_subsample$Group

# removes extra info that mothur includes in their OTU tables and outlier points
otu_subsample <- otu_subsample[,-c(1:3)]  

#Makes sure that the meta table and the otu table have the same samples
meta <- meta[meta$Group %in% rownames(otu_subsample),] 
otu_subsample <- otu_subsample[rownames(otu_subsample) %in% meta$Group,]

##################################################
##################################################

# this calculates the distance matrix using Bray-Curtis distances with vegan 
dist.matr.bray <- vegdist(otu_subsample, method = 'bray')

# this is vegan's function to make an NMDS ordination using k=2 dimensions
mds <- metaMDS(dist.matr.bray, k = 2,trymax = 1000, autotransform = FALSE)

#Calculation of the irdination stress
mds$stress

#merging MDS and metadata
nmds <-as.data.frame(mds$points)
nmds$group <- rownames(nmds)
metanmds <- merge(meta, nmds, by.x = 'Group', by.y = 'group')
metanmds$age <- factor(metanmds$age)
str(metanmds)

###### PLOTS #######

#General plots with basic facets
ggplot(metanmds, aes(x=MDS1, y=MDS2)) + geom_point(aes(color=age))
ggplot(metanmds, aes(x=MDS1, y=MDS2)) + geom_point(aes(color=Region))
ggplot(metanmds, aes(x=MDS1, y=MDS2)) + geom_point(aes(color=studyage))
ggplot(metanmds, aes(x=MDS1, y=MDS2)) + geom_point(aes(color=Region, shape=age))

#Plot for manuscript
ggplot(metanmds, aes(x=MDS1, y=MDS2)) + geom_point(aes(color=Region, shape=age)) +
  labs(x='Axis 1', y= 'Axis 2', caption = paste('Ordination stress: ', round(mds$stress, digits = 2)))
ggsave("graphs/nmds.png", height = 5, width = 7)

#Save MDS data for later
write.table(nmds, file="tables/nmds.txt")

#Adding in a 95% confidence ellipse
#this generates a dataframe containing the group centroids
metanmds$NMDS <- if_else(metanmds$MDS1 > 0, "pos", "neg")
str(metanmds$NMDS)
metanmds$NMDS <- factor(metanmds$NMDS)   ##Make this column a factor and not a character type
str(metanmds$NMDS)
NMDS.mean <- aggregate(metanmds[,8:9], list(group=metanmds$NMDS), mean)
colnames(NMDS.mean) <- c('design', 'groupX', 'groupY')

#merging the group centroids with the rest of the NMDS data #
metanmds <- merge(metanmds, NMDS.mean , by.x = 'NMDS', by.y='design')
nmds$group == metanmds$Group 
metanmds <- metanmds[match(nmds$group,metanmds$Group),] 
nmds$group == metanmds$Group  
ord <- ordiellipse(mds, metanmds$NMDS, label = TRUE, conf = .95, kind = 'se', draw = 'none')

#this little loop generates a dataframe containing the ellipse data for each group
df_ell <- data.frame()
for (d in levels(metanmds$NMDS)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(metanmds[metanmds$NMDS == d,],
                                                   veganCovEllipse(ord[[d]]$cov, ord[[d]]$center, ord[[d]]$scale))),NMDS=d))
}

colnames(df_ell) <- c('MDS1', 'MDS2', 'design') # just making it so our column names are consistent

###### PLOTS #######
# now we are adding metadata to the ellipse dataframe
# probably an easier way to do this but oh well...
#meta_sub <- meta[,-1]
#meta_sub2 <- unique(meta_sub)
#df_ell2 <- merge(df_ell, meta_sub2, by.x = 'design', by.y = 'day_location_treatment')
str(df_ell)
df_ell$design <- factor(df_ell$design)

ggplot(metanmds, aes(x=MDS1, y=MDS2)) + geom_point(aes(color=Region, shape=age)) +
  labs(x='Axis 1', y= 'Axis 2', caption = paste('Ordination stress: ', round(mds$stress, digits = 2))) +
  geom_path(data = df_ell, aes(x=MDS1, y=MDS2, group=design))


ggplot(metanmds, aes(x=groupX, y=groupY)) +
  geom_point(aes(color=Region, shape=age)) + 
  geom_polygon(data = df_ell, aes(x=MDS1, y=MDS2, group=design, fill=design), alpha = 0.25) + 
  labs(x='NMDS 1', y= 'NMDS 2', caption = paste('Ordination stress: ', round(mds$stress, digits = 2))) 

ggsave("graphs/ellipses2.png")
