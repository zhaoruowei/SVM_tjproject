setwd("~/tmp/TCGA-LIHC/test/s4/论文/数据/SVM/")
data <- read.csv(file = file.choose())
row.names(data) <- data[,1]
data <- data[,-1]
dat <- as.data.frame(t(data))
group <- as.factor(dat$group)
dat <- dat[,-183]
training <- data.frame(x=dat,y=group)

#linear
tune.out=tune(svm,y~.,data=training,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))

#sigmoid
tune.out=tune(svm,y~.,data=training,kernel="sigmoid",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=1e-04))

#polynomial
tune.out=tune(svm,y~.,data=training,kernel="polynomial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=1e-04))

#radial
tune.out=tune(svm,y~.,data=training,kernel="radial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=1e-04))

bestmod=tune.out$best.model
summary(bestmod)

ypred=predict(bestmod,training)
table(predict=ypred,truth=training$y)

test<-read.csv(file.choose())
test<-test[,-1]
rownames(test)<-test[,1]
test<-test[,-1]
test<-as.data.frame(t(test))
test_group<-as.factor(test$group)
test<-test[,-183]
test<-data.frame(x=test,y=test_group)

library(ROCR)
################################################################
#1
rocplot<-function(pred,truth,...){
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)
}
fitted=attributes(predict(bestmod,svmdat,decision.values=TRUE))$decision.values
rocplot(fitted,svmdat[,"y"],main="test")

#2
pred<-predict(bestmod,test,decision.values = TRUE)  
p<--attr(pred,"decision.values")  
pred <- prediction(p, test$y)  
perf <- performance(pred,"tpr","fpr")  
plot(perf,colorize=TRUE)  
grid(5, 5, lwd = 1)  
points(c(0,1), c(0,1), type="l", lty=2, lwd=2, col="grey")
###############################################################

auc.tmp <- performance(pred,"auc")  
auc <- as.numeric(auc.tmp@y.values)  
auc <- round(auc, 4)
