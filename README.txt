README

Dataset :
File Name - ‘EEG data.csv’
Specifications - 12,811 rows and 15 columns (12,811 x 15 = 1,92,165 entries)

Code :
File Name - ‘Code_Consolidated.R’

Steps to be followed :
1. The working directory has to be set to the directory where the dataset is present. Can be done by using the setwd(‘path to EEG dataset’) command.
2. The following packages need to be installed for the working of the code : ggplot2, FactoMineR, factoextra, rpart, rpart.plot, C50, randomForest, class, e1071, Metrics, caret
3. The initial code generates several plots which help in highlighting the source of noise in the dataset and these plots may be seen individually.
4. Principal Component Analysis is then performed and we obtain the desired Individuals factor map along with the variables factor map and a plot showing the contribution of each variable to dimension 1.
5. Student Independent Analysis is then done using the following classifiers -
Decision Trees
C5.0 Algorithm
Random Forests
K-Nearest Neighbours (KNN)
Naive Bayes
Boosting Algorithm
Support Vector Machines (SVM)

6. Analysis based on Random Sampling is then performed where 60% of the data is reserved for the training set while the remaining 40% is for the testing set. This is done using the following classifiers (based on the results of the previous test) -
K-Nearest Neighbours (KNN)
Support Vector Machines (SVM)
Random Forest

Note :
Some of the above algorithms can take about 60-80 seconds for their execution.
Comments present in the source code provide a step-by-step explanation of each step.




