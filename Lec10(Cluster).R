
## K-means
dt<-USArrests # số liệu phạm tội theo từng bang ở USA
attach(USArrests)
library(factoextra)
library(cluster)
 ## Loại dòng mà bị mất giá trị
dt<-na.omit(dt)
is.na(dt)#Xem dữ liệu có dòng nào bị thiếu
## Chuẩn hoá dữ liệu
dt<-scale(dt)
head(dt)

## Dùng hàm fviz_nbclust() để vẽ số nhóm
fviz_nbclust(dt,kmeans,method = "wss")
# Chọn k=4
# calculate gap statistic based on number of clusters
gap_stats<-clusGap(dt,FUN=kmeans,nstart=25,K.max = 10,B=50)
fviz_gap_stat(gap_stats)

##
set.seed(2)
km<-kmeans(dt,centers = 4,nstart = 25)
km
fviz_cluster(km,data = dt)

# tính trung bình của mỗi nhóm
aggregate(USArrests,by=list(cluster=km$cluster),mean)

#add cluster assigment to orginal data
final_data<-cbind(USArrests,cluster=km$cluster)
head(final_data)

## Hierarchical Clustering
dt<-USArrests # số liệu phạm tội theo từng bang ở USA
attach(USArrests)
library(factoextra)
library(cluster)
## Loại dòng mà bị mất giá trị
dt<-na.omit(dt)
is.na(dt)#Xem dữ liệu có dòng nào bị thiếu
## Chuẩn hoá dữ liệu
dt<-scale(dt)
head(dt)

## Xác định phương pháp nào 
m<-c("average","single","complete","ward")
names(m)<-c("average","single","complete","ward")
ac<-function(x){
  agnes(dt,method = x)$ac
}
## Tính hệ số tích tụ ( agglomerative coefficient) 
# hệ số nào gần 1 nhất thì chọn
sapply(m,ac)
# => Chọn được pp Ward

##
clust<-agnes(dt,method="ward")
## produce dendrogram
pltree(clust,cex=0.6,hang=-1,main = "Dendrogram")

# Dùng gap statistic để xác định số nhóm
gap_stats<-clusGap(dt,FUN = hcut,nstart=25,K.max = 10,B=50)
fviz_gap_stat(gap_stats)
