library(readxl)
ckd_class <- read_excel("D:/R dataset/ckd_d.xlsx")
ckd_class <- ckd_class[1:417,1:13]
tail(ckd_class)
View(ckd_class)
ckd_class$`Classification`=factor(ckd_class$`Classification`,
                                    levels = c('CKD','NOT CKD'),
                                    labels = c(1,0))

ckd_class$GENDER = factor(ckd_class$GENDER,
                          levels = c('F', 'M'),
                          labels = c(0, 1))
library(ggplot2)
d <- density(ckd_class$AGE)
plot(d, main = "Kernel density of Age")
polygon(d, col = "red", border = "blue")

#splitting the data set
library(caret)
set.seed(12)
intrain <- createDataPartition(y = ckd_class$Classification, p= 0.7, list = FALSE)
train_set <- ckd_class[intrain,]
test_set <- ckd_class[-intrain,]
dim(train_set);dim(test_set);
View(train_set)
View(test_set)
summary(ckd_class)
anyNA(ckd_class)

train_set[["Classification"]] = factor(train_set[["Classification"]])
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#SVM Linaer Model
set.seed(123)
svm_Linear <- train(Classification ~., data = train_set, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10, na.action = na.pass )
svm_Linear
test_set <- test_set[complete.cases(test_set),]
test_pred <- predict(svm_Linear, newdata = test_set) 
test_pred
#Confusion Matrix for test data in SVM Linear Model
confusionMatrix(test_pred, test_set$Classification)

Creatinine#SVM Radial Model
set.seed(123)

svm_Radial <- train(Classification ~., data = train_set, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10, na.action = na.pass )
svm_Radial
test_set <- test_set[complete.cases(test_set),]
test_pred <- predict(svm_Radial, newdata = test_set) 
test_pred
#Confusion Matrix for SVM Radial Model
confusionMatrix(test_pred, test_set$Classification)

#Naive Bayes
library(naivebayes)
Naive_Bayes_Model=naive_bayes(Classification ~., data=train_set, laplace = 1)
Naive_Bayes_Model
NB_Predictions<-predict(Naive_Bayes_Model,train_set, type = 'prob')
head(cbind(NB_Predictions, train_set))

#train dataset
NB_Pred<-predict(Naive_Bayes_Model, train_set)
tab<-table(NB_Pred,train_set$Classification)
1-sum(diag(tab))/sum(tab)
#Confusion Matrix for train data
confusionMatrix(NB_Pred, train_set$Classification)

#test dataset
NB_Pred2<-predict(Naive_Bayes_Model, test_set)
tab2<-table(NB_Pred2,test_set$Classification)
1-sum(diag(tab2))/sum(tab2)
#Confusion Matrix for test data
confusionMatrix(NB_Pred2, test_set$Classification)

#Random forest
library(randomForest)
ckd_class$Classification<-factor(ckd_class$Classification)
set.seed(123)
names(train_set) <- make.names(names(train_set))
names(test_set) <- make.names(names(test_set))
model1 <- randomForest(Classification ~ ., data = train_set,  ntree = 1000, mtry = 1, importance = TRUE)
model1
plot(model1)

# Predicting on train data
predTrain <- predict(model1, train_set, type = "class")
#Confusion Matrix
confusionMatrix(predTrain, train_set$Classification)
# Checking classification accuracy
table(predTrain, train_set$Classification)  

# Predicting on Test data
predTest <- predict(model1, test_set, type = "class")
#Confusion Matrix
confusionMatrix(predTest, test_set$Classification)
# Checking classification accuracy
mean(predTest == test_set$Classification)                    
table(predTest,test_set$Classification)

# To check important variables
importance(model1)        
varImpPlot(model1) 

#generating the logistic regression
log_reg=glm(formula = ckd_class$Classification~ckd_class$Creatinine,
            family = binomial,
            data = train_set)

#generating the prediction
y_pred <- predict(log_reg,type = 'response',newdata = test_set[-13])
prop_pred <- ifelse(y_pred>0.5,1,0)
prop_pred



