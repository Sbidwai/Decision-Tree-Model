setwd("C:/Users/Shreyas/Desktop/homework 2")

library(rpart)
library(caret)
library(rpart.plot)

ilpd=read.csv("ILPD.csv", header = T, sep=",")

set.seed(100)

index<- sample(1:nrow(ilpd), size = 0.6*nrow(ilpd))
train<- ilpd[index, ]
test <- ilpd[-index, ]

library(psych)
pairs.panels(ilpd, pch=19)
#a)
#i)Strongest correlated pair :- db and tb
#ii)Weakest correlated pair :-  tp and db, sex and ag,sgpaa and ag
#iii)Most negatively correlated :- age and alb
#iv)Variables that appear to follow a Gaussian distribution :- age, tp, alb, ag

#b)Yes, I think normalising or scaling the attributes will help the classification task. Because, normalising the attributes will result in data which are similar to each other. It will also help in providing a better correlation and there is no point in normalising the data that are similar to each other. 
  #Attributes with varied range of values that should be normalised are:- Age, tp, alb, ag


#c)
model <- rpart(label~ ., method = "class", data =train)
rpart.plot(model)
pred <- predict(model, test, type = "class")
confusionMatrix(pred, test[,11], positive = "1")
#Accuracy: 68.8%
#TPR(Sensitivity) : 0.8683          
#TNR(Specificity) : 0.2388          
#PPV(Pos Pred Value) : 0.7398  

#d)
#-Prune
plotcp(model)
printcp(model)

model.pruned <- prune(model, cp = 0.021)
pred.pruned <- predict(model.pruned, test, type = "class")
confusionMatrix(pred.pruned, test[, 11], positive = "1")

rpart.plot(model.pruned)
#Accuracy of the model is 68.8% and after changing the values of cp I got a better accuracy of 69.66%.
#Thus pruned tree has more accuracy because of lesser complexity, as their are less number of nodes to traverse in decision tree.

#e)Build a new model
new_model<- rpart(label ~ alb+ag+aap, method = "class", data = train)
rpart.plot(new_model)
new_pred <- predict(new_model,test,type = "class")
confusionMatrix(new_pred,test[,11], positive = "1")

summary(ilpd)

# Accuracy :- 72.65%
#TPR(Sensitivity) : 0.9102
#TNR(Specificity) : 0.2687         
#PPV(Pos Pred Value) : 0.7562  

#f)
#(i) a ROC curve using the ROCR package.
#ROC for first model
pred.roc <- predict(model,newdata=test,type="prob")[,2]
f.pred <- prediction(pred.roc,test$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3, main = "ROC")
abline(0,1)
auc<-performance(f.pred,measure = "auc")
auc@y.values[[1]]

#ROC for new model
pred.roc1 <- predict(new_model,newdata=test,type="prob")[,2]
f.pred1 <- prediction(pred.roc1,test$label)
f.perf1 <- performance(f.pred1, "tpr", "fpr")
plot(f.perf1, colorize=T, lwd=3, main = "ROC")
abline(0,1)
auc<-performance(f.pred1,measure = "auc")
auc@y.values[[1]]

#ii) AUC for model: 0.6675
#   AUC for newmodel: 0.6455

#iii) Previous Model is better because the auc is closesr to 1 and greater than as compared to new model.



