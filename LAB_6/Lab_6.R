library(class)
f<-read.csv('wbc_csv.csv')
f$diagnosis<-as.factor(f$diagnosis)
set.seed(123)
f<-f[sample(nrow(f)),]
n <- function(b) {
(b-min(b))/(max(b)-min(b))
}
fnor<-as.data.frame(lapply(f[3:32],n)) 
train<-fnor[1:469,]
test<-fnor[470:569,]
tlable<-f[1:469,2]
testlable<-f[470:569,2]
p<-knn(train,test,tlable,7)
t<-table(actual=testlable,predict=p)
print(t)
TN<-t[1,1]
TP<-t[2,2]
FN<-t[2,1]
FP<-t[1,2]
Accuracy<-((TN+TP)/(TN+TP+FN+FP))*100
Sensitivity <- (TP/(TP+FN))*100
Specificity<-(TN/ (TN+FP))*100
Precision<-(TP/(TP+FP))*100
cat("\n Accuracy ",Accuracy)
cat("\n Sensitivity ",Sensitivity)
cat("\n Specificity ",Specificity)
cat("\n Precision ",Precision)

