  ##Lecture 2:Review
#create data frame
data<-data.frame(x1=c(20,25,14,30,50),x2=c(25,32,28,45,60),x3=c(5,4,3,12,20))
data
#
colMeans(data)
cor(data)
cov(data)
(sd(data$x1))^2
library(Distance)
dist(data,method = "euclidean")

#
x<-c(seq(1,9,1))
matrix1=matrix(x,nrow = 3,byrow = TRUE)
matrix1
y<-c(seq(10,18,1))
matrix2=matrix(y,nrow = 3,byrow = TRUE)
matrix2
mean(matrix1[,1])
#transpose a matrix(chuyển vị)
t(matrix1)
z=c(3,5,8,2,1,9,2,10,4)
mtrix=matrix(z,nrow = 3,byrow = TRUE)
solve(mtrix) #nghịch đảo

reg1=lm(data$x1~data$x2+data$x3)
summary(reg1)
 # estimate the linear model of x1 on x2 and x3
cont = c(rep(1,5))
designmtrix=cbind(cont,data$x2,data$x3)
designmtrix
t(designmtrix)
as.matrix(designmtrix)
beta = (solve(t(designmtrix)%*%designmtrix))%*%(t(designmtrix)%*%data$x1)
beta


