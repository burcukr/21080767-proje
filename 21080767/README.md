---
title: "21080767"
author: "burcu kara"
date: "2024-03-30"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

customer_data=read.csv("customer_data=read.csv(“/Users/burcukara/Desktop/R-studio\proje/Mall_Customers.csv")")
str(customer_data)
names(customer_data)
head(customer_data)

summary(customer_data$Age)
sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)
sd(customer_data$Spending.Score..1.100.)

#gerekli paketler ön-indirme
install.packages('tidyverse')
install.packages('ggplot2')

#Müşteri Cinsiyeti Görselleştirme

a=table(customer_data$Gender)
barplot(a,main="Cinsiyet Karşılaştırmasını Görüntülemek için BarPlot Kullanımı",
        ylab="Sayı",
        xlab="Cinsiyet",
        col=rainbow(2),
        legend=rownames(a))


pct=round(a/sum(a)*100)
lbs=paste(c("Kadın","Erkek")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Kadın ve Erkek Oranını Gösteren Pasta Grafik")


#Yaş Dağılımının Görselleştirilmesi


summary(customer_data$Age)

hist(customer_data$Age,
     col="blue",
     main="Yaş Sınıfı Sayısını Gösteren Histogram",
     xlab="Yaş Sınıfı",
     ylab="Sıklık",
     labels=TRUE)

boxplot(customer_data$Age,
        col="#ff0066",
        main="Yaşın Betimsel Analizi için Boxplot")

#Müşterilerin Yıllık Gelirlerinin Analizi

summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="#660033",
     main="Yıllık Gelir İçin Histogram",
     xlab="Yıllık Gelir Sınıfı",
     ylab="Sıklık",
     labels=TRUE)

plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Yıllık Gelir için Yoğunluk Grafiği",
     xlab="Yıllık Gelir Sınıfı",
     ylab="Yoğunluk")
polygon(density(customer_data$Annual.Income..k..),
        col="#ccff66")

boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="Harcama Puanının Tanımlayıcı Analizi için BoxPlot")

hist(customer_data$Spending.Score..1.100.,
     main="Harcama Puanı için HistoGram",
     xlab="Harcama Puan Sınıfı",
     ylab="Sıklık",
     col="#6600cc",
     labels=TRUE)

#K-ortalamalar Algoritması

library(purrr)
set.seed(123)
# küme içi kareler toplamını hesaplayan fonksiyon 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Küme sayısı K",
     ylab="Toplam küme içi kareler toplamı")

#Ortalama Siluet Yöntemi

library(cluster) 
library(gridExtra)
library(grid)


k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customer_data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customer_data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customer_data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customer_data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))



library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6


#İlk İki Temel Bileşeni Kullanarak Kümeleme Sonuçlarını Görselleştirme

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]



set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Küme 1", "Küme 2", "Küme 3", "Küme 4", "Küme 5","Küme 6")) +
  ggtitle("Alışveriş Merkezi Müşterilerinin Segmentleri", subtitle = "K-ortalamalar Kümelemesini Kullanma")



ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Küme 1", "Küme 2", "Küme 3", "Küme 4", "Küme 5","Küme 6")) +
  ggtitle("Alışveriş Merkezi Müşterilerinin Segmentleri", subtitle = "K-ortalamalar Kümelemesini Kullanma")



kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))

