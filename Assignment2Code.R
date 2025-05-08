# De-comment the two lines below to install the needed packages.
# install.packages(c("tidyverse","ggpubr", "moments", "factoextra" ))  
# install.packages(c("caret", "glmnet", "rpart", "ipred", "pROC"))

# Load needed packages - after installed.
library(tidyverse)
library(moments) 
library(ggpubr)
library(factoextra)
library(caret)
library(glmnet)
library(rpart)
library(ipred)
library(pROC)

# You may need to change/include the path of your working directory
setwd("")
# Reading whole dataset into dat variable.
dat <- read.csv("HealthCareData_2024.csv", stringsAsFactors = TRUE)

#----Data Cleaning---------------------------------------------------------------------------------------

# combining categories of NetworkEventType, Policy_Violation and PolicyViolation.
dat$NetworkEventType <- factor(dat$NetworkEventType, 
                               levels = c("NormalOperation", "Policy_Violation", "PolicyViolation", "ThreatDetected"), 
                               labels = c("NormalOperation", "PolicyViolation", "PolicyViolation", "ThreatDetected"))

# combining categories of AlertCategory, Info and Informational
dat$AlertCategory <- factor(dat$AlertCategory, 
                            levels = c("Alert", "Info", "Informational", "Warning"), 
                            labels = c("Alert", "Informational", "Informational", "Warning"))

# Replace outliers (values below 0) in NetworkAccessFrequency column with NA
dat$NetworkAccessFrequency[dat$NetworkAccessFrequency < 0] <- NA

# Replace outliers (values above 150) in ResponseTime column with NA
dat$ResponseTime[dat$ResponseTime > 150] <- NA

# Removing SystemAccessRate column due to high missing values.
dat$SystemAccessRate <- NULL

# In NetworkInteractionType, merging Regular and Unkown categories to form a Others category.
dat$NetworkInteractionType <- forcats::fct_collapse(dat$NetworkInteractionType, 
                                                    Others = c("Regular", "Unknown"))

# Selecting only the completed cases from the entire dataset.
dat.cleaned <- na.omit(dat)

#----Generating Training and Testing data----------------------------------------------------------------

# Separate samples of normal and malicious events
dat.class0 <- dat.cleaned %>% filter(Classification == "Normal") # normal
dat.class1 <- dat.cleaned %>% filter(Classification == "Malicious") # malicious

# Randomly select 9600 non-malicious and 400 malicious samples using your student ID
set.seed(100000)
rows.train0 <- sample(1:nrow(dat.class0), size = 9600, replace = FALSE)
rows.train1 <- sample(1:nrow(dat.class1), size = 400, replace = FALSE)

# Your 10000 ‘unbalanced’ training samples
train.class0 <- dat.class0[rows.train0,] # Non-malicious samples
train.class1 <- dat.class1[rows.train1,] # Malicious samples
mydata.ub.train <- rbind(train.class0, train.class1)

# Your 19200 ‘balanced’ training samples, i.e. 9600 normal and malicious samples each.
set.seed(100000)
train.class1_2 <- train.class1[sample(1:nrow(train.class1), size = 9600, replace = TRUE),]
mydata.b.train <- rbind(train.class0, train.class1_2)

# Your testing samples
test.class0 <- dat.class0[-rows.train0,]
test.class1 <- dat.class1[-rows.train1,]
mydata.test <- rbind(test.class0, test.class1)


#----Selecting two supervised learning modelling algorithms----------------------------------------------
set.seed(100000)
models.list1 <- c("Logistic Ridge Regression",
                  "Logistic LASSO Regression",
                  "Logistic Elastic-Net Regression")
models.list2 <- c("Classification Tree",
                  "Bagging Tree",
                  "Random Forest")
myModels <- c(sample(models.list1, size = 1),
              sample(models.list2, size = 1))
myModels %>% data.frame


#----Lasso Model Training and Testing---------------------------------------------------------------------

lambdas <- 10^seq(-3,3,length=100) #A sequence 100 lambda values
set.seed(100000)

# Balanced Lasso model
model.cyber.LASSO.b <- train(Classification ~., #Formula
                             data = mydata.b.train, #Balanced training data
                             method = "glmnet",  #Penalised regression modelling
                             #Set preProcess to c("center", "scale") to standardise data
                             preProcess = NULL,
                             #Perform 10-fold CV, 2 times over.
                             trControl = trainControl("repeatedcv", number = 10, repeats = 2),
                             tuneGrid = expand.grid(alpha = 1,lambda = lambdas)
)
# Optimal lambda value
model.cyber.LASSO.b$bestTune
# Model coefficients
coef(model.cyber.LASSO.b$finalModel, model.cyber.LASSO.b$bestTune$lambda)
# predicted probability of classification on the test data
pred.class.LASSO.b <- predict(model.cyber.LASSO.b,new=mydata.test) 
# Confusion matrix without reordering of levels
cf.LASSO.b <- table(pred.class.LASSO.b, mydata.test$Classification)
# Proportions by columns
prop <- prop.table(cf.LASSO.b,2); prop %>% round(digit=3) 
# Summary of confusion matrix
cf.summary.LASSO.b <-confusionMatrix(cf.LASSO.b)
print(cf.summary.LASSO.b)
# Computing the precision, recall, and F1-score
# Referenced from Stackoverflow, https://rb.gy/ki8wwq
precision.b <- cf.summary.LASSO.b$byClass["Pos Pred Value"]
recall.b <- cf.summary.LASSO.b$byClass["Sensitivity"]
f1_score.b <- 2 * (precision.b * recall.b) / (precision.b + recall.b)
# Print precision, recall, F1-score
print(precision.b)
print(recall.b)
print(f1_score.b)

# Generate predicted probabilities for the test data for Lasso (balanced data)
pred.prob.LASSO.b <- predict(model.cyber.LASSO.b, newdata=mydata.test, type="prob")[,2]
# Compute ROC and AUC for Lasso model on test data
roc.LASSO.b <- roc(mydata.test$Classification, pred.prob.LASSO.b)
auc.LASSO.b <- auc(roc.LASSO.b)
# Output the accuracy and AUC of the Lasso regression model
acc.LASSO.b <- confusionMatrix(predict(model.cyber.LASSO.b, newdata=mydata.test), 
                               mydata.test$Classification)$overall['Accuracy']
c(Acc_Test=acc.LASSO.b,
  AUC_Test=auc.LASSO.b)



# Unbalanced Lasso model
model.cyber.LASSO.ub <- train(Classification ~., #Formula
                         data = mydata.ub.train, #Unbalanced training data
                         method = "glmnet",  #Penalised regression modelling
                         #Set preProcess to c("center", "scale") to standardise data
                         preProcess = NULL,
                         #Perform 10-fold CV, 2 times over.
                         trControl = trainControl("repeatedcv", number = 10, repeats = 2),
                         tuneGrid = expand.grid(alpha = 1,lambda = lambdas)
)
# Optimal lambda value
model.cyber.LASSO.ub$bestTune
# Model coefficients
coef(model.cyber.LASSO.ub$finalModel, model.cyber.LASSO.ub$bestTune$lambda)
# predicted probability of classification on the test data
pred.class.LASSO.ub <- predict(model.cyber.LASSO.ub,new=mydata.test) 
# Confusion matrix without reordering of levels
cf.LASSO.ub <- table(pred.class.LASSO.ub, mydata.test$Classification)
# Proportions by columns
prop <- prop.table(cf.LASSO.ub,2); prop %>% round(digit=3) 
# Summary of confusion matrix
cf.summary.LASSO.ub <-confusionMatrix(cf.LASSO.ub)
print(cf.summary.LASSO.ub)
# Compute the precision, recall, F1-score, FPR, and FNR
# Referenced from Stackoverflow, https://rb.gy/ki8wwq
precision.ub <- cf.summary.LASSO.ub$byClass["Pos Pred Value"]
recall.ub <- cf.summary.LASSO.ub$byClass["Sensitivity"]
f1_score.ub <- 2 * (precision.ub * recall.ub) / (precision.ub + recall.ub)
fpr.ub <- cf.LASSO.ub["Normal", "Malicious"] / sum(cf.LASSO.ub["Normal", ])
fnr.ub <- cf.LASSO.ub["Malicious", "Normal"] / sum(cf.LASSO.ub["Malicious", ])
# Print precision, recall, F1-score, FPR, and FNR
print(precision.ub)
print(recall.ub)
print(f1_score.ub)
print(fpr.ub)
print(fnr.ub)

# Generate predicted probabilities for the test data for Lasso (unbalanced data)
pred.prob.LASSO.ub <- predict(model.cyber.LASSO.ub, newdata=mydata.test, type="prob")[,2]
# Compute ROC and AUC for Lasso model on test data
roc.LASSO.ub <- roc(mydata.test$Classification, pred.prob.LASSO.ub)
auc.LASSO.ub <- auc(roc.LASSO.ub)
# Output the accuracy and AUC of the Lasso regression model
acc.LASSO.ub <- confusionMatrix(predict(model.cyber.LASSO.ub, newdata=mydata.test), 
                               mydata.test$Classification)$overall['Accuracy']
c(Acc_Test=acc.LASSO.ub,
  AUC_Test=auc.LASSO.ub)

#----Bagging Balanced Data Model---------------------------------------------------------------------
# Tuning the hyperparameter to further improve the results
# Intialise the hyperparamter search grid
grid.bc.b <- expand.grid(nbagg = c(25, 50, 75),  # Default, below, and above the default values for nbagg
                       cp = c(0.0, 0.1, 0.2),  # Default, below, and above the default values for cp
                       minsplit = c(5, 10, 20),  # Default, below, and above the default values for minsplit
                       OOB.misclass=NA, #Initialize columns to store the OOB misclassification rate
                       test.sens=NA, #Initialize columns to store sensitivity
                       test.spec=NA, #Initialize columns to store specificity
                       test.acc=NA #Initialize columns to accuracy of bagging at each run
                       )  
# check how many combinations.
dim(grid.bc.b)

# Bagging Balanced Data Model
# Perform bagging with different combinations of hyperparameters defined above.
for (I in 1:nrow(grid.bc.b))
{
  set.seed(100000)
  #Perform bagging
  btree.bc <- bagging(Classification~.,
                      data=mydata.b.train,
                      nbagg=grid.bc.b$nbagg[I],  
                      coob=TRUE,
                      control=rpart.control(cp=grid.bc.b$cp[I],
                                            minsplit=grid.bc.b$minsplit[I]));
  #OOB misclassification error rate
  grid.bc.b$OOB.misclass[I] <- btree.bc$err*100
  #Summary of predictions on test set
  test.pred.bc.b <- predict(btree.bc,newdata=mydata.test,type="class");  #Class prediction
  #Confusion matrix
  test.cf.bc.b <- confusionMatrix(test.pred.bc.b, mydata.test$Classification)
  # Add values to the grid
  prop.cf.bc.b <- test.cf.bc.b$table %>% prop.table(2)
  grid.bc.b$test.sens[I] <- prop.cf.bc.b[1,1]*100  #Sensitivity
  grid.bc.b$test.spec[I] <- prop.cf.bc.b[2,2]*100  #Specificity
  grid.bc.b$test.acc[I] <- test.cf.bc.b$overall[1]*100  #Accuracy
}

#Sort the results by the OOB misclassification rate (lowest first) and display them.
grid.bc.b[order(grid.bc.b$OOB.misclass,decreasing=FALSE)[1:5],] %>% round(2)


# Perform bagging with the best hyperparameter tuning combination.
set.seed(100000)
# Perform bagging
btree.normal.b <- bagging(Classification~.,
                          data=mydata.b.train,
                          nbagg=75,  
                          coob=TRUE,
                          control=rpart.control(cp=0,
                                                minsplit=5))
# Make predictions on the test set
test.pred.normal.bc <- predict(btree.normal.b, newdata = mydata.test, type = "class")
# Confusion matrix
test.cf.normal.bc <- confusionMatrix(test.pred.normal.bc, mydata.test$Classification)
test.cf.normal.bc
# Calculate the F1-score
precision.btree.b <- test.cf.normal.bc$byClass["Pos Pred Value"]
recall.btree.b <- test.cf.normal.bc$byClass["Sensitivity"]
f1_score.btree.b <- 2 * (precision.btree.b * recall.btree.b) / (precision.btree.b + recall.btree.b)
print(f1_score.btree.b)


#Getting the AUC
pred.prob.bagging.b <- predict(btree.normal.b, newdata=mydata.test, type="prob")[,2]
# Compute ROC and AUC for Bagging model on test data
roc.bagging.b <- roc(mydata.test$Classification, pred.prob.bagging.b)
auc.bagging.b <- auc(roc.bagging.b)
# Output the accuracy and AUC of the Bagging model
acc.bagging.b <- confusionMatrix(predict(btree.normal.b, newdata=mydata.test), 
                                 mydata.test$Classification)$overall['Accuracy']

c(Acc_Test=acc.bagging.b,
  AUC_Test=auc.bagging.b)


#----Bagging Unbalanced Data Model-------------------------------------------------------------------

# Tuning the hyperparameter to further improve the results
# Intialise the hyperparamter search grid
grid.ubc <- expand.grid(nbagg = c(25, 50, 75),# Default, below, and above the default values for nbagg
                         cp = c(0.0, 0.1, 0.2),# Default, below, and above the default values for cp
                         minsplit = c(5, 10, 20),# Default, below, and above the default values for minsplit
                         OOB.misclass=NA, #Initialize columns to store the OOB misclassification rate
                         test.sens=NA, #Initialize columns to store sensitivity
                         test.spec=NA, #Initialize columns to store specificity
                         test.acc=NA #Initialize columns to accuracy of bagging at each run
)  
# check how many combinations.
dim(grid.ubc)

# Bagging Unbalanced Data Model
# Perform bagging with different combinations of hyperparameters defined above.
for (I in 1:nrow(grid.ubc))
{
  set.seed(100000)
  #Perform bagging
  btree.ubc <- bagging(Classification~.,
                      data=mydata.ub.train,
                      nbagg=grid.ubc$nbagg[I],  
                      coob=TRUE,
                      control=rpart.control(cp=grid.ubc$cp[I],
                                            minsplit=grid.ubc$minsplit[I]));
  #OOB misclassification error rate
  grid.ubc$OOB.misclass[I] <- btree.ubc$err*100
  #Summary of predictions on test set
  test.pred.bc.ub <- predict(btree.ubc,newdata=mydata.test,type="class");  #Class prediction
  #Confusion matrix
  test.cf.bc.ub <- confusionMatrix(test.pred.bc.ub, mydata.test$Classification)
  # Add values to the grid
  prop.cf.bc.ub <- test.cf.bc.ub$table %>% prop.table(2)
  grid.ubc$test.sens[I] <- prop.cf.bc.ub[1,1]*100  #Sensitivity
  grid.ubc$test.spec[I] <- prop.cf.bc.ub[2,2]*100  #Specificity
  grid.ubc$test.acc[I] <- test.cf.bc.ub$overall[1]*100  #Accuracy
}

#Sort the results by the OOB misclassification rate (lowest first) and display them.
grid.ubc[order(grid.ubc$OOB.misclass,decreasing=FALSE)[1:5],] %>% round(2)



# Perform bagging with the best hyperparameter tuning combination.
set.seed(100000)
# Perform bagging
btree.normal.ub <- bagging(Classification~.,
                          data=mydata.ub.train,
                          nbagg=25,  
                          coob=TRUE,
                          control=rpart.control(cp=0.1,
                                                minsplit=5))
# Make predictions on the test set
test.pred.normal.ubc <- predict(btree.normal.ub, newdata = mydata.test, type = "class")
# Confusion matrix
test.cf.normal.ubc <- confusionMatrix(test.pred.normal.ubc, mydata.test$Classification)
test.cf.normal.ubc
# Calculate the F1-score
precision.btree.ub <- test.cf.normal.ubc$byClass["Pos Pred Value"]
recall.btree.ub <- test.cf.normal.ubc$byClass["Sensitivity"]
f1_score.btree.ub <- 2 * (precision.btree.ub * recall.btree.ub) / (precision.btree.ub + recall.btree.ub)
print(f1_score.btree.ub)


#Getting the AUC
pred.prob.bagging.ub <- predict(btree.normal.ub, newdata=mydata.test, type="prob")[,2]
# Compute ROC and AUC for Bagging model on test data
roc.bagging.ub <- roc(mydata.test$Classification, pred.prob.bagging.ub)
auc.bagging.ub <- auc(roc.bagging.ub)
# Output the accuracy and AUC of the Bagging model
acc.bagging.ub <- confusionMatrix(predict(btree.normal.ub, newdata=mydata.test), 
                                 mydata.test$Classification)$overall['Accuracy']

c(Acc_Test=acc.bagging.ub,
  AUC_Test=auc.bagging.ub)



