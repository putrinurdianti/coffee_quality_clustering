setwd("G:/PROJECTS/Coffee Quality")
library(cluster) #Clustering
library(factoextra) #Clustering & Viz
library(tidyverse) #Data Manipulation
library(dplyr)
library(fpc) #Clustering
library(ggplot2)
library(ggiraphExtra) #Radar plot


#Data Preparation
data <- read.csv("df_arabica_clean.csv", header=TRUE)
head(data)
str(data)
aroma <- aggregate(data$Aroma,list(data$Country.of.Origin),FUN=mean)
colnames(aroma) <- c("Country","Aroma")
flavor <- aggregate(data$Flavor,list(data$Country.of.Origin),FUN=mean)
colnames(flavor) <- c("Country","Flavor")
aftertaste <- aggregate(data$Aftertaste,list(data$Country.of.Origin),FUN=mean)
colnames(aftertaste) <- c("Country","After_taste")
acidity <- aggregate(data$Acidity,list(data$Country.of.Origin),FUN=mean)
colnames(acidity) <- c("Country","Acidity")
body <- aggregate(data$Body,list(data$Country.of.Origin),FUN=mean)
colnames(body) <- c("Country","Body")
balance <- aggregate(data$Balance,list(data$Country.of.Origin),FUN=mean)
colnames(balance) <- c("Country","Balance")
data_list <- list(aroma, flavor, aftertaste, acidity, body, balance)
data2 <- data_list %>% reduce(full_join,by='Country')
head(data2)

#Checking outliers
par(mfrow=c(2,3))
boxplot(data2$Aroma, col='#9a714d', xlab='Aroma')
boxplot(data2$Flavor, col='#b38b67', xlab='Flavor')
boxplot(data2$After_taste, col='#c89f73', xlab='After Taste')
boxplot(data2$Acidity, col='#d9b380', xlab='Acidity')
boxplot(data2$Body, col='#f1cc8f', xlab='Body')
boxplot(data2$Balance, col='#fbe7a1', xlab='Balance')

#Detecting Missing Value
sum(is.na(data2))

#Exploratory Data Analysis
#Aroma by Country
aroma_viz <- ggplot(data=data2, aes(x=Country,y=Aroma))+
  geom_bar(stat='identity',col='#000000', fill='#503014')+
  geom_text(aes(label=round(Aroma,2)),vjust=1.5,color='white')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
aroma_viz
#Flavor by Country
flavor_viz <- ggplot(data=data2, aes(x=Country,y=Flavor))+
  geom_bar(stat='identity',col='#000000', fill='#8d5524')+
  geom_text(aes(label=round(Flavor,2)),vjust=1.5,color='white')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
flavor_viz
#After Taste by Country
at_viz <- ggplot(data=data2, aes(x=Country,y=After_taste))+
  geom_bar(stat='identity',col='#000000', fill='#c68642')+
  geom_text(aes(label=round(After_taste,2)),vjust=1.5,color='white')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
at_viz
#Acidity by Country
acidity_viz <- ggplot(data=data2, aes(x=Country,y=Acidity))+
  geom_bar(stat='identity',col='#000000', fill='#e0ac69')+
  geom_text(aes(label=round(Acidity,2)),vjust=1.5,color='black')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
acidity_viz
#Body by Country
body_viz <- ggplot(data=data2, aes(x=Country,y=Body))+
  geom_bar(stat='identity',col='#000000', fill='#f1c27d')+
  geom_text(aes(label=round(Body,2)),vjust=1.5,color='black')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
body_viz
#Balance by Country
balance_viz <- ggplot(data=data2, aes(x=Country,y=Balance))+
  geom_bar(stat='identity',col='#000000', fill='#ffdbac')+
  geom_text(aes(label=round(Balance,2)),vjust=1.5,color='black')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
balance_viz

#K-Medoids Clustering
rownames(data2) <- data2$Country
dataclus <- data2[,2:7];head(dataclus)
#Choose Optimum k for K-Medoids
fviz_nbclust(dataclus, pam, method="wss") #Elbow Method
fviz_nbclust(dataclus, pam, method="silhouette") #Silhouette Method

#Run K-Medoids
set.seed(123)
#pam(data yg digunakan, banyak cluster)
res=pam(dataclus,2)
summary(res)
df.clus=data.frame(dataclus,res$cluster) #Adding Cluster to DF
head(df.clus)
#win.graph()
fviz_cluster(res,data=dataclus, labelsize=10, ggtheme=theme_classic()) #Cluster Plot
res$medoids #Represented object from each clusters

#Model Criterions
sil_score=mean(silhouette(df.clus$res.cluster, dmatrix=as.matrix(res$diss))[,3]) #Silhouette Score
sil_score
mod_cri=function(Data, nc, c)
{
  n = dim(Data)[1]
  p = dim(Data)[2]
  X = Data[,1:(p-1)]
  Group = Data[,p]
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (nc+1))
  for (i in 1:nc)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(nc+1),j] = mean(X[,j])
    }
  }
  SST = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
    }
  }
  SST = sum(sum(SST))
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:nc)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  Rsq = (SST-SSE)/SST
  icdrate = 1-Rsq
  Pseudof = (Rsq/(c-1))/((icdrate)/(nc-c))
  ssb=SST-SSE
  list(SSW=SSE, SST=SST, SSB=ssb, Rsq=Rsq, icdrate=icdrate, pseudof=Pseudof)
}
mod_cri(df.clus,length(df.clus),2)

#Exploring each cluster
#Radar plot
coffee_df <- as.data.frame(dataclus) %>% rownames_to_column()
cluster_pos <- as.data.frame(res$cluster) %>% rownames_to_column()
colnames(cluster_pos) <- c("rowname", "cluster")
coffee_final <- inner_join(cluster_pos, coffee_df)
ggRadar(coffee_final[-1], aes(group = cluster), rescale = FALSE, legend.position = "none", size = 1, interactive = FALSE, use.label = TRUE) + facet_wrap(~cluster) + scale_y_discrete(breaks = NULL) + 
  theme(axis.text.x = element_text(size = 10)) + scale_fill_manual(values = rep(c("#53310b","#7C3F00"), nrow(coffee_final))) +
  scale_color_manual(values = rep(c("#53310b","#7C3F00"), nrow(coffee_final)))+theme_classic()
#Cluster summary
table(res$clustering) #Number of members in each clusters
mins <- df.clus %>%
  mutate(cluster=res.cluster) %>%
  group_by(cluster) %>%
  summarise_all(list("min")) 
mins
means <- df.clus %>%
  mutate(cluster=res.cluster) %>%
  group_by(cluster) %>%
  summarise_all(list("mean"))
means
medians <- df.clus %>%
  mutate(cluster=res.cluster) %>%
  group_by(cluster) %>%
  summarise_all(list("median"))
medians
maxs <- df.clus %>%
  mutate(cluster=res.cluster) %>%
  group_by(cluster) %>%
  summarise_all(list("max"))
maxs