###########################################################
#####Analysis of Brainwave (EEG) Signals for Confusion#####
#####################Team Aaayana##########################
###########################################################

#####Cleaning of Dataset######
#Importing required libraries
library("ggplot2")

eegData <- read.csv("EEG data.csv") #Reading dataset
eegData$Sample <-
  ave( 1:nrow(eegData), eegData$subject.ID, factor(eegData$Video.ID), FUN=function(x) 1:length(x)) # adding sample column - converting it to a kind of timeseries
p1 <- ggplot(eegData, aes(x=Sample, y=Raw, colour=as.factor(subject.ID))) #plotting the value of raw against the value of sample for all subjects
p1 + geom_line()
p2 <- ggplot(eegData, aes(x=Sample, y=Raw, colour=as.factor(Video.ID))) #plotting the value of raw against the value of sample for all videos
p2 + geom_line()
#Exceedingly noisy dataset as evident in graphs
for (i in unique(eegData$subject.ID)) {
  for (j in unique(eegData$Video.ID)) {
    subject_df <- subset(eegData, subject.ID == i & Video.ID == j)
    print(paste("Subject", i, "-", "Video", j))
    plot(subject_df$Raw, type = 'l', ylim = c(-200, 200), xlab = "Sample", ylab = "mV")
  }
}
#From the plots plotted above, it is clear that subject 6 is the cause for contamination of data

eegData_clean <- subset(eegData, subject.ID != 6) #Removing subject 6 from the dataset
#Replotting Raw v/s Sample
p1 <- ggplot(eegData_clean, aes(x=Sample, y=Raw, colour=as.factor(subject.ID)))
p1 + geom_line()
p2 <- ggplot(eegData_clean, aes(x=Sample, y=Raw, colour=as.factor(Video.ID)))
p2 + geom_line()
#Dataset no more messy


#####Principal Component Analysis#####
#Importing required libraries
library("FactoMineR")
library("factoextra")
f_unk <- read.csv("EEG data.csv") #Reading dataset
f_unk <- f_unk[, c(-1, -2, -15)] #Removing 'subject.ID', 'Video.ID' and 'Self.defined.label' from dataset
res.pca <- PCA(f_unk, scale.=TRUE) #Performing PCA
fviz_pca_contrib(res.pca, choice = "var", axes = 1) #Determination of most contributing factors
#Result - predefined.label is not a contributing factor. Hence, it has been removed from the dataset while building models



######################################
#####Student Independent Analysis#####
######################################

#####Decision Trees#####
library("rpart")
library("rpart.plot")
library(caret)
f_unk <- read.csv("EEG data.csv")
f_unk1 <- subset(f_unk,subject.ID!=6)#Removing student 6 as he contributes to noise
x <- double()
for(i in 0 : 9)#Iterating for all students from 0 to 9
{
  if(i!=6)#Ignoring 6th student
  {
    trainData <- f_unk1[f_unk1$subject.ID != i,]#Removing student 6 as he contributes to noise
    trainData <- trainData[-1]# Removing student.ID column
    trainData[,14] <- as.factor(trainData[,14])# Factoring the Self.defined.label column as we will use it for prediction
    testData <- f_unk1[f_unk1$subject.ID == i,]#Testing data - All records of the student with subject.ID equal to i from the testing
    testData <- testData[-1]
    tree <- rpart(Self.defined.label ~ . , data = trainData[,c(-1, -13)])#building the tree
    printcp(tree)
    bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
    tree.pruned <- prune(tree, cp = bestcp)#pruning the tree
    conf.matrix <- table(trainData$Self.defined.label, predict(tree.pruned,type="class"))
    rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")#building the rows of the tree
    colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")#building the columns of the tree
    print(conf.matrix)
    boxcols <- c("pink", "palegreen3")[tree.pruned$frame$yval]#assigning colors to the roots of the tree
    prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1, box.col = boxcols)
    surv <- predict(tree.pruned, testData[,c(-1,-13, -14)], type = "class")#predict based on the pruned tree
    testData <- cbind(testData, newLabel = surv)
    cm <- confusionMatrix(data=testData$newLabel, 
                          reference=testData$Self.defined.label, 
                          positive='1')#creation of the confusion matrix
    x <- c(x,cm$overall['Accuracy'])#attaining the accuracy
  }
}
x
summary(x)
#The prediction accuracy obtained is 56.36%



#####C5.0 Algorithm#####
library(C50)
library(caret)
f_unk <- read.csv("EEG data.csv")
f_unk1 <- subset(f_unk,subject.ID!=6)#Removing student 6 as he contributes to noise
x <- double()
for(i in 0 : 9)#Iterating for all students from 0 to 9
{
  if(i!=6)#Ignoring 6th student
  {
    trainData <- f_unk1[f_unk1$subject.ID != i,]#Training data - All records of students with subject.ID not equal to i from the training set
    trainData <- trainData[-1]# Removing student.ID column
    trainData[,14] <- as.factor(trainData[,14])# Factoring the Self.defined.label column as we will use it for prediction
    testData <- f_unk1[f_unk1$subject.ID == i,]#Testing data - All records of the student with subject.ID equal to i from the testing
    testData <- testData[-1]
    built_tree<-C5.0(Self.defined.label~.,data=trainData[,-1])#Building the tree
    summary(built_tree)
    predicted <- predict(built_tree,testData[,c(-1,-14)])#Predicting based on test data
    # Create Confusion Matrix
    cm <- confusionMatrix(data=predicted, reference=testData$Self.defined.label, positive='1')
    x <- c(x,cm$overall['Accuracy'])#Finding accuracy
  }
}
x
summary(x)
#Prediction accuracy obtained is 51.83%



#####Random Forests#####
library("randomForest")
library(caret)
f_unk <- read.csv("EEG data.csv")
f_unk1 <- f_unk
f_unk1 <- subset(f_unk1,subject.ID!=6) #Removing student 6 as he contributes to noise
x <- double()
for(i in 0 : 9) #Iterating for all students from 0 to 9
{
  if(i!=6) #Ignoring 6th student
  {
    trainData <- f_unk1[f_unk1$subject.ID != i,] #Training data - All records of students with subject.ID not equal to i from the training set
    trainData <- trainData[-1] # Removing student.ID column
    trainData[,14] <- as.factor(trainData[,14])# Factoring the Self.defined.label column as we will use it for prediction
    testData <- f_unk1[f_unk1$subject.ID == i,] #Testing data - All records of the student with subject.ID equal to i from the testing
    testData <- testData[-1]
    model <- randomForest(Self.defined.label~.,trainData[,c(-1,-13)],ntree=500)#Optimal Modedl
    pred <- predict(model, testData[,c(-1,-13,-14)])
    # Create Confusion Matrix
    cm <- confusionMatrix(data=pred, reference=testData$Self.defined.label, positive='1')
    x <- c(x,cm$overall['Accuracy']) #Adding the accuracy for this iteration to the array
  }
}
print(x)
summary(x) #Mean so obtained is the final accuracy
#We obtain ~56% when number of trees are 10, ~57 when number of trees are 30, ~57.5% when number of trees are 50, ~58% when number of trees are 500.



#####KNN Algorithm#####
library("class")
library(caret)
f_unk <- read.csv("EEG data.csv")
normalize <- function(x) { #Normalizing Function
  return ((x-min(x))/(max(x)-min(x)))
}
f_unk1 <- as.data.frame(lapply(f_unk[,3:13], normalize)) #Normalizing the data as we are using KNN
f_unk1 <- cbind(cbind(f_unk[,1:2],f_unk1),f_unk[,14:15]) #Adding the remaining columns to the normalized data
f_unk1 <- subset(f_unk1,subject.ID!=6) #Removing student 6 as he contributes to noise
x <- double()
for(i in 0 : 9) #Iterating for all students from 0 to 9
{
  if(i!=6) #Ignoring 6th student
  {
    trainData <- f_unk1[f_unk1$subject.ID != i,] #Training data - All records of students with subject.ID not equal to i from the training set
    trainData <- trainData[-1] # Removing student.ID column
    trainData[,14] <- as.factor(trainData[,14])# Factoring the Self.defined.label column as we will use it for prediction
    testData <- f_unk1[f_unk1$subject.ID == i,] #Testing data - All records of the student with subject.ID equal to i from the testing
    testData <- testData[-1]
    pred <- knn(train = trainData[,c(-1,-13,-14)], test = testData[,c(-1,-13,-14)],cl = trainData[,14], k=499)#Optimal Model
    # Create Confusion Matrix
    cm <- confusionMatrix(data=pred, reference=testData$Self.defined.label, positive='1')
    x <- c(x,cm$overall['Accuracy']) #Adding the accuracy for this iteration to the array
  }
}
print(x)
summary(x) #Mean so obtained is the final accuracy
#We obtain max accuracy of ~59% with k=499.



#####Naive Bayes Classifier#####
require(e1071)
library(caret)
f_unk <- read.csv("EEG data.csv")
x <- double()

for(i in 0 : 9)
{
  if(i!=6) #Removing student 6 as he contributes to noise
  {
    #Training data - All records of students with subject.ID not equal to i from the training set
    f_unk.train <- f_unk[f_unk$subject.ID != i,]
    # Removing student.ID column
    f_unk.train <- f_unk.train[-1]
    # Factoring the Self.defined.label column as we will use it for prediction
    f_unk.train[,14] <- as.factor(f_unk.train[,14])
    #Testing data - All records of the student with subject.ID equal to i from the testing
    f_unk.test <- f_unk[f_unk$subject.ID == i,]
    # Removing student.ID column
    f_unk.test <- f_unk.test[-1]
    
    #invoke naiveBayes model
    #The first argument is the dependent variable (to be predicted) - Self.defined.label
    #The second argument is the independent variable (predictors or features) - dataframe that contains the training data
    #The dot (.) is simply shorthand for "all variable other than the dependent one." 
    naive.Bayes <- naiveBayes(Self.defined.label~., data=f_unk.train[c(-1, -13)])
    #predicting using the Naive Bayes model by feeding test data into the model
    f_unk.test$Predicted_SDL <- predict(naive.Bayes, f_unk.test[,c(-1, -13, -14)])
    # Create Confusion Matrix
    cm <- confusionMatrix(data=f_unk.test$Predicted_SDL,
                          reference=f_unk.test$Self.defined.label,
                          positive='1')
    #Adding the accuracy for this iteration to the array
    x <- c(x,cm$overall['Accuracy'])
  }
}
x
#Mean so obtained is the final accuracy
summary(x)
#Student Independent Accuracy in report is 51%. We obtain 52.98%.



#####Boosting Algorithm#####
library(Metrics)
library(caret)
f_unk <- read.csv("EEG data.csv")
#the function trainControl generates parameters that further control how models are created
#method: The resampling method
#method = "repeatedcv", number = 4 and repeats = 4,then four separate 4-fold cross-validations are used as the resampling method
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
x <- double()

for(i in 0 : 9)
{
  if(i!=6) #Removing student 6 as he contributes to noise
  {
    #Training data - All records of students with subject.ID not equal to i from the training set
    f_unk.train <- f_unk[f_unk$subject.ID != i,]
    # Removing student.ID column
    f_unk.train <- f_unk.train[-1]
    # Factoring the Self.defined.label column as we will use it for prediction
    f_unk.train[,14] <- as.factor(f_unk.train[,14])
    #Testing data - All records of the student with subject.ID equal to i from the testing
    f_unk.test <- f_unk[f_unk$subject.ID == i,]
    # Removing student.ID column
    f_unk.test <- f_unk.test[-1]
    #invoke gbm - Generalized Boosted Model
    #The train function chooses the optimal model across the parameters and estimates model performance from the training set
    gbmFit <- train(Self.defined.label~., data=f_unk.train[c(-1, -13)], method = "gbm", trControl = fitControl,verbose = FALSE)
    #predicting using the Generalized Boosted Model by feeding test data into the model
    f_unk.test$Predicted_SDL <- predict(gbmFit, f_unk.test[,c(-1, -13, -14)])
    # Create Confusion Matrix
    cm <- confusionMatrix(data=f_unk.test$Predicted_SDL,
                          reference=f_unk.test$Self.defined.label,
                          positive='1')
    #Adding the accuracy for this iteration to the array
    x <- c(x,cm$overall['Accuracy'])
  }
}
x
#Mean so obtained is the final accuracy
summary(x)
#Student Independent Accuracy in report is 51%. We obtain 56.69%.



#####Support Vector Machines#####
#Importing required libraries
require(e1071)
library(caret)

#Loading dataset
f_unk <- read.csv("EEG data.csv")
f_unk <- subset(f_unk, subject.ID != 6)

x <- double() #variable x to store the accuracies
for(i in 0 : 9) { #loop to find out accuracy for every student as test data
  if(i != 6) {
    f_unk.dev <- f_unk[f_unk$subject.ID != i,] #training data
    f_unk.dev <- f_unk.dev[-1] #removing subject.ID
    f_unk.dev[,14] <- as.factor(f_unk.dev[,14]) #converting 'Self.defined.label' to 
    f_unk.tes <- f_unk[f_unk$subject.ID == i,] #test data
    f_unk.tes <- f_unk.tes[-1] #removing subject.ID
    svm.model <- svm(Self.defined.label~.,
                     data=f_unk.dev[c(-1, -13)], kernel = "linear") #building model based on all parameters except 'Video.ID' and 'predefined.label'
    f_unk.tes$Predicted_SDL <- predict(svm.model, 
                                       f_unk.tes[,c(-1, -13, -14)]) #predicting for the test data based on the model built
    
    
    cm <- confusionMatrix(data=f_unk.tes$Predicted_SDL,
                          reference=f_unk.tes$Self.defined.label,
                          positive='1')
    x <- c(x,cm$overall['Accuracy']) #Extracting accuracy and adding it to 'x'
  }
}
summary(x) #pick up mean from the summary
#With linear kernel, SVM gives an accuracy of 58.11%



###############################
#####Random Sampling Tests#####
###############################

#####KNN Algorithm#####
library("class")
library(caret)
f_unk <- read.csv("EEG data.csv")
normalize <- function(x) { #Normalizing Function
  return ((x-min(x))/(max(x)-min(x)))
}
f_unk1 <- as.data.frame(lapply(f_unk[,3:13], normalize)) #Normalizing the data as we are using KNN
f_unk1 <- cbind(cbind(f_unk[,1:2],f_unk1),f_unk[,14:15]) #Adding the remaining columns to the normalized data
f_unk1 <- subset(f_unk1,subject.ID!=6) #Removing student 6 as he contributes to noise
sample1 <- sample(2, nrow(f_unk1), replace = T, prob = c(0.6,0.4))#Generating a random sample
trainData <- f_unk1[sample1==1,]#Training set
trainData$Self.defined.label <- as.factor(trainData$Self.defined.label)
testData <- f_unk1[sample1==2,]#Testing set
x <- double()
trainData <- trainData[-1] # Removing student.ID column
testData <- testData[-1]
pred <- knn(train = trainData[,c(-1,-13,-14)], test = testData[,c(-1,-13,-14)],cl = trainData[,14], k=200)#Optimal Model
# Create Confusion Matrix
cm <- confusionMatrix(data=pred, reference=testData$Self.defined.label, positive='1')
x <- c(x,cm$overall['Accuracy']) #Adding the accuracy to the array
print(x)
summary(x) #Mean so obtained is the final accuracy
#We obtain max accuracy of ~59% with k=200.



#####Support Vector Machines#####
#Importing required libraries
require(e1071)
library(caret)

f_unk <- read.csv("EEG data.csv") #Reading data
f_unk <- subset(f_unk, subject.ID != 6)

f_unk <- f_unk[-1] #Removing 'subject.ID'
sample.ind <- sample(2, 
                     nrow(f_unk),
                     replace = T,
                     prob = c(0.6,0.4)) #Obtaining a list with 60% 1s and 40% 0s
f_unk.dev <- f_unk[sample.ind==1,] #Training data
f_unk.dev[,14] <- as.factor(f_unk.dev[,14]) #Coverting 'self.defined.labels' to factors
f_unk.tes <- f_unk[sample.ind==2,] #Test data
f_unk.dev <- f_unk.dev[-1] #Removing 'Video.ID' from training data
f_unk.tes <- f_unk.tes[-1] #Removing 'Video.ID' from testing data
svm.model <- svm(Self.defined.label~.,
                 data=f_unk.dev[c(-12)]) #Building model on all parameters of training data except 'predefined.label'
f_unk.tes$Predicted_SDL <- predict(svm.model, 
                                   f_unk.tes[,c(-12)]) #Predicting for test data based on model built


cm <- confusionMatrix(data=f_unk.tes$Predicted_SDL,
                      reference=f_unk.tes$Self.defined.label,
                      positive='1')
x <- cm$overall['Accuracy'] #Calulating accuracy of calculation
#Accuracy obtained from random sampling using SVM is approximately ~63%



#####Random Forest#####
library("randomForest")
library("caret")
f_unk <- read.csv("EEG data.csv")
f_unk1 <- f_unk
f_unk1 <- subset(f_unk1,subject.ID!=6) #Removing student 6 as he contributes to noise
sample1 <- sample(2, nrow(f_unk1), replace = T, prob = c(0.6,0.4))#Generating a random sample
trainData <- f_unk1[sample1==1,]#Training set
trainData$Self.defined.label <- as.factor(trainData$Self.defined.label)
testData <- f_unk1[sample1==2,]#Testing set
x <- double()
trainData <- trainData[-1] # Removing student.ID column
testData <- testData[-1]
model <- randomForest(Self.defined.label~.,trainData[,c(-1,-13)],ntree=100)#Optimal Model
pred <- predict(model, testData[,c(-1,-13,-14)])
# Create Confusion Matrix
cm <- confusionMatrix(data=pred, reference=testData$Self.defined.label, positive='1')
x <- c(x,cm$overall['Accuracy']) #Adding the accuracy for this iteration to the array
print(x)
summary(x) #Mean so obtained is the final accuracy
#We obtain ~66% when number of trees are 100.


##################################################################################

