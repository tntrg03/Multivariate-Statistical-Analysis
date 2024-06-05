
library(readr)
insect <- read_csv("DATA/insect.csv")
attach(insect)

insect[2:4]=scale(insect[2:4])
set.seed(2)
sample1<- sample(c(TRUE, FALSE), nrow(insect), replace=TRUE, prob=c(0.7,0.3))
train1<-insect[sample1,]
test1<-insect[!sample1,]
library(MASS)
model1 <- lda(species~., data=train1)
(model1)
predicted1 <- predict(model1, test1)
predicted1
#Tỷ lệ dự báo đúng
mean(predicted1$class==test1$species,na.rm=TRUE)
lda_plot1 <- cbind(train1, predict(model1)$x)
library(ggplot2)
ggplot(data = lda_plot1, mapping=aes(LD1)) + geom_point(aes(color = species))


## Thêm dữ liệu
x<-list("b",195,120,45)
datanew=rbind(insect,x)
datanew[2:4]=scale(datanew[2:4])
View(datanew)
set.seed(2)
sample2<- sample(c(TRUE, FALSE), nrow(datanew), replace=TRUE, prob=c(0.8,0.2))
train2<-datanew[sample2,]
test2<-datanew[!sample2,]
model2 <- lda(species~., data=train2)
(model2)
predicted2 <- predict(model2, test2)
predicted2
mean(predicted2$class==test2$species,na.rm=TRUE)
