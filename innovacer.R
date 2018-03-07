#set the working directory
setwd("/")


#install the packages
install.packages("dplyr")
install.packages("Boruta")
install.packages("DMwR")
install.packages("caret")
install.packages("corrplot")
install.packages("pROC")
install.packages("plyr")
install.packages("cluster") # for gower similarity and pam
install.packages("Rtsne") # for t-SNE plot
install.packages("magrittr")
install.packages("ggplot2")
install.packages("stringdist")

library(dplyr)
library(Boruta)
library(DMwR)
library(caret)
library(corrplot)
library(pROC)
library(plyr)
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(magrittr)
library(ggplot2)
library(stringdist)

#read the csv
Df <- read.csv(file="Sample Dataset.csv")
fn <- as.vector(Df['fn'])
fn <- Df[['fn']]
class(fn)
fn <- Df[,4]
ln <- Df[,1]

#checking how many uniwue names are present in the document
fnU<-unique(Df$fn)
df <-Df
df[,2] <- as.factor(df[,2])
df[,3] <- as.factor(df[,3])
df[,4] <- as.factor(df[,4])
kmeans(df[,2:4],1)
nrow(unique(df[,2:4]))
nrow(unique(df))


 
k=1;j=1;

# #clustering in the above clusters on the basis of the date of birth
# #Elbow Method for finding the optimal number of clusters

df$id <- NA
for(i in 1:nrow(df))
{
  df$id[i]=i
}
gower_dist <- daisy(df[,c(2,3,5)],
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:60){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:60, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:60, sil_width)

#from here we can deduce that there should be 23 clusters 
pam_fit <- pam(gower_dist, diss = TRUE, k = 23)

#creating the clusters
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE,perplexity = 10)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
          name = df$id)
clus.plot=as.data.frame(tsne_data)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#making the clusters
test.cluster1 <- subset(clus.plot, cluster==1)
test.cluster2 <- subset(clus.plot, cluster==2)
test.cluster3 <- subset(clus.plot, cluster==3)
test.cluster4 <- subset(clus.plot, cluster==4)
test.cluster5 <- subset(clus.plot, cluster==5)
test.cluster6 <- subset(clus.plot, cluster==6)
test.cluster7 <- subset(clus.plot, cluster==7)
test.cluster8 <- subset(clus.plot, cluster==8)
test.cluster9 <- subset(clus.plot, cluster==9)
test.cluster10 <- subset(clus.plot, cluster==10)
test.cluster11 <- subset(clus.plot, cluster==11)
test.cluster12 <- subset(clus.plot, cluster==12)
test.cluster13 <- subset(clus.plot, cluster==13)
test.cluster14 <- subset(clus.plot, cluster==14)
test.cluster15 <- subset(clus.plot, cluster==15)
test.cluster16 <- subset(clus.plot, cluster==16)
test.cluster17 <- subset(clus.plot, cluster==17)
test.cluster18 <- subset(clus.plot, cluster==18)
test.cluster19 <- subset(clus.plot, cluster==19)
test.cluster20 <- subset(clus.plot, cluster==20)
test.cluster21 <- subset(clus.plot, cluster==21)
test.cluster22 <- subset(clus.plot, cluster==22)
test.cluster23 <- subset(clus.plot, cluster==23)
test.level1_cluster <- merge(test.cluster1, df, by.x = "name", by.y = "id")
test.level2_cluster <- merge(test.cluster2, df, by.x = "name", by.y = "id")
test.level3_cluster <- merge(test.cluster3, df, by.x = "name", by.y = "id")
test.level4_cluster <- merge(test.cluster4, df, by.x = "name", by.y = "id")
test.level5_cluster <- merge(test.cluster5, df, by.x = "name", by.y = "id")
test.level6_cluster <- merge(test.cluster6, df, by.x = "name", by.y = "id")
test.level7_cluster <- merge(test.cluster7, df, by.x = "name", by.y = "id")
test.level8_cluster <- merge(test.cluster8, df, by.x = "name", by.y = "id")
test.level9_cluster <- merge(test.cluster9, df, by.x = "name", by.y = "id")
test.level10_cluster <- merge(test.cluster10, df, by.x = "name", by.y = "id")
test.level11_cluster <- merge(test.cluster11, df, by.x = "name", by.y = "id")
test.level12_cluster <- merge(test.cluster12, df, by.x = "name", by.y = "id")
test.level13_cluster <- merge(test.cluster13, df, by.x = "name", by.y = "id")
test.level14_cluster <- merge(test.cluster14, df, by.x = "name", by.y = "id")
test.level15_cluster <- merge(test.cluster15, df, by.x = "name", by.y = "id")
test.level16_cluster <- merge(test.cluster16, df, by.x = "name", by.y = "id")
test.level17_cluster <- merge(test.cluster17, df, by.x = "name", by.y = "id")
test.level18_cluster <- merge(test.cluster18, df, by.x = "name", by.y = "id")
test.level19_cluster <- merge(test.cluster19, df, by.x = "name", by.y = "id")
test.level20_cluster <- merge(test.cluster20, df, by.x = "name", by.y = "id")
test.level21_cluster <- merge(test.cluster21, df, by.x = "name", by.y = "id")
test.level22_cluster <- merge(test.cluster22, df, by.x = "name", by.y = "id")
test.level23_cluster <- merge(test.cluster23, df, by.x = "name", by.y = "id")


#creating csv for the clusters (these clusters are specific for this data set ,for different data set number of clusters may be different)
write.csv(test.level1_cluster,"test.cluster1.csv")
write.csv(test.level2_cluster,"test.cluster2.csv")
write.csv(test.level3_cluster,"test.cluster3.csv")
write.csv(test.level4_cluster,"test.cluster4.csv")
write.csv(test.level5_cluster,"test.cluster5.csv")
write.csv(test.level6_cluster,"test.cluster6.csv")
write.csv(test.level7_cluster,"test.cluster7.csv")
write.csv(test.level8_cluster,"test.cluster8.csv")
write.csv(test.level9_cluster,"test.cluster9.csv")
write.csv(test.level10_cluster,"test.cluster10.csv")
write.csv(test.level11_cluster,"test.cluster11.csv")
write.csv(test.level12_cluster,"test.cluster12.csv")
write.csv(test.level13_cluster,"test.cluster13.csv")
write.csv(test.level14_cluster,"test.cluster14.csv")
write.csv(test.level15_cluster,"test.cluster15.csv")
write.csv(test.level16_cluster,"test.cluster16.csv")
write.csv(test.level17_cluster,"test.cluster17.csv")
write.csv(test.level18_cluster,"test.cluster18.csv")
write.csv(test.level19_cluster,"test.cluster19.csv")
write.csv(test.level20_cluster,"test.cluster20.csv")
write.csv(test.level21_cluster,"test.cluster21.csv")
write.csv(test.level22_cluster,"test.cluster22.csv")
write.csv(test.level23_cluster,"test.cluster23.csv")


#MAIN ALGORITHM OF STRING MATCHING IN THE EVEY CLUSTERS TO GET DESIRED RESULT
n=1
newdf<- Df[-c(1:103),]
for(i in 1:23){
  
  str<- NULL
  p <- paste("test.cluster",i,sep="")
  p_f <- paste(p,".csv",sep = "")
  df <- p_f
  df <- read.csv(p_f)
  for (i in 1:nrow(df))
  {
    if(df[i,8]=="FALSE")
      df[i,8]="F"
  }
  for(j in 1:nrow(df))
  { a<- as.character(df[j,6])
    b <- as.character(df[j,9])
    str[j]= paste(b,a,sep=" ")
    
  }
  c<- vector(mode="numeric", length=nrow(df))
 
  k=1
  for(i in 1:nrow(df)){ 
   h <- vector(mode="numeric", length=0)
     for(j in i+1:nrow(df)){
     h[j-1]=stringsim(str[i],str[j],method = "lcs")
     }
   #h<- na.omit(h)
  #print(h)
  b<-k
   while( b <= (nrow(df)-1))
   { 
     if(h[b]>0.87)
       {c[i]=1
       print("ghus gya")
      break
     }
     b=b+1
   }
  k=k+1
 }
 for( a in 1:length(c))
 {  if(c[a]==0)
 {
   newdf[n,1]=df[a,6]
   newdf[n,2]=df[a,7]
   newdf[n,3]=df[a,8]
   newdf[n,4]=df[a,9]
   n=n+1
 }
   
 }
}

#final csv file 
write.csv(newdf,"final_records.csv")


