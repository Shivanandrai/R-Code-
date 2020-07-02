Chipotle= Chipotle %>% 
  filter(patronage==0) %>% 
  filter(chipotlehealthy>2) %>% 
  filter(importanthealthy>2) %>% 
  select(c(1,2,3,4,18,19,20,21,22,23,24))

names(Chipotle)
  Chipotle= Chipotle[,(c(2,3,4,6,7,8,9,10,11))]

c<-names(Chipotle)
Chipotle[c]<-lapply(Chipotle[c],factor)
str(Chipotle)

gower.dist <- daisy(Chipotle, metric = c("gower"))
summary(gower.dist)


divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

mixedClusters<-kmeans(gower.dist, centers=3)
table(mixedClusters$cluster)
View(mixedClusters$cluster)
gower_mat<-as.matrix(gower.dist)

Chipotle[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

Chipotle[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
sil_width<-c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower.dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam(gower.dist, diss = TRUE, k = 4)

str(Chipotle)
library(dplyr)

pam_results <- Chipotle %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

Chipotle[pam_fit$medoids,]

library(Rtsne)
tsne_obj <- Rtsne(gower.dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         age=Chipotle$age,female=Chipotle$female,income=Chipotle$income, WOM=Chipotle$wom, SM=Chipotle$sm, 
         Walk=Chipotle$walk, Planimpt=Chipotle$plan, Spendimpt=Chipotle$spending, Localimpt=Chipotle$buylocal)

ggplot(aes(x= X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

summary(tsne_data)

Cluster_1=tsne_data[tsne_data$cluster==1,]
Cluster_2=tsne_data[tsne_data$cluster==2,]
Cluster_3=tsne_data[tsne_data$cluster==3,]
Cluster_4=tsne_data[tsne_data$cluster==4,]
Cluster_1$income<-as.numeric(Cluster_1$income)
Cluster_2$income<-as.numeric(Cluster_2$income)
Cluster_3$income<-as.numeric(Cluster_3$income)
Cluster_1$age<-as.numeric(Cluster_1$age)
Cluster_2$age<-as.numeric(Cluster_2$age)
Cluster_3$age<-as.numeric(Cluster_3$age)
Cluster_1$Planimpt<-as.numeric(Cluster_1$Planimpt)
Cluster_2$Planimpt<-as.numeric(Cluster_2$Planimpt)
Cluster_3$Planimpt<-as.numeric(Cluster_3$Planimpt)
Cluster_1$Spendimpt<-as.numeric(Cluster_1$Spendimpt)
Cluster_2$Spendimpt<-as.numeric(Cluster_2$Spendimpt)
Cluster_3$Spendimpt<-as.numeric(Cluster_3$Spendimpt)
Cluster_1$Localimpt<-as.numeric(Cluster_1$Localimpt)
Cluster_2$Localimpt<-as.numeric(Cluster_2$Localimpt)
Cluster_3$Localimpt<-as.numeric(Cluster_3$Localimpt)




boxplot(Cluster_1$income)
boxplot(Cluster_2$income)
Cluster_1[is.na(Cluster_1$income)]<-0
Cluster_2$income[is.na(Cluster_2$income)]<-0
Cluster_1$age[is.na(Cluster_1$age)]<-0
Cluster_2$age[is.na(Cluster_2$age)]<-0



median(Cluster_1$income)
median(Cluster_2$income)
median(Cluster_3$income)
mean(Cluster_1$age)
mean(Cluster_2$age)
mean(Cluster_3$age)

boxplot(Cluster_1$Planimpt,Cluster_1$Spendimpt,Cluster_1$Localimpt, names =c("Plan", "Spend", "Local"))
boxplot(Cluster_2$Planimpt,Cluster_2$Spendimpt,Cluster_2$Localimpt, names =c("Plan", "Spend", "Local"))
boxplot(Cluster_3$Planimpt,Cluster_3$Spendimpt,Cluster_3$Localimpt, names =c("Plan", "Spend", "Local"))

boxplot(Cluster_1$income)
boxplot(Cluster_2$income)
boxplot(Cluster_3$income)

boxplot(Cluster_1$age)
boxplot(Cluster_2$age)
boxplot(Cluster_3$age)

