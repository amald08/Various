set.seed(5)

library(e1071)
# n<-500
# l<-2
# for(i in 1:5){
#   
#   m<-(i-1)/3.25
#   
#   x1<-runif(n,3+m,3+m+l)
#   y1<-runif(n,3,3+m,3+m+l)
#   
#   x2<-runif(n,6,6+l)
#   y2<-runif(n,6,6+l)
#   
#   y<-as.factor(c(rep(-1,n),rep(1,n)))
# 
# }

g1<-cbind(runif(500,3,5),runif(500,3,5))

g2<-cbind(runif(500,6,8),runif(500,6,8))

sim<-rbind(g1,g2)

plot(g1,col = 2,xlim=c(3,8),ylim=c(3,8))
points(g2,col = 3)

y<-as.factor(c(rep(-1,500),rep(1,500)))

svm.sim <- svm(y~., data = sim, 
               type =c("C"),
               cost = 1, 
               kernel = c("linear"), 
               gamma =1,
               scale = F,
               degree = 1,
               coef0 = 0)

print(svm.sim)

beta_0 <- -svm.sim$rho

num.sop.vec <- svm.sim$tot.nSV

svm.sim$SV

svm.sim$index
