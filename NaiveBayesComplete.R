install.packages('caTools') # contains tools for data splitting
install.packages('e1071') #cointain the naive Bayes classifier model

library(naivebayes)
library(ggplot2)
library(lattice)
library(caret)
library(dplyr)
library(psych)

Naivedata <- read.csv("D:/Thesis/HeartFinal.csv", header = TRUE, sep=",")
head(Naivedata)
str(Naivedata)

#missing value find out:
colSums(is.na(Naivedata))

#outlier detect:
which(is.na(Naivedata$Sex))
which(is.na(Naivedata$ChestPainType))
which(is.na(Naivedata$RestingECG))
which(is.na(Naivedata$ExerciseAngina))
which(is.na(Naivedata$ST_Slope))
which(is.na(Naivedata$Age))
which(is.na(Naivedata$RestingBP))
which(is.na(Naivedata$Cholesterol))
which(is.na(Naivedata$FastingBS))
which(is.na(Naivedata$MaxHR))
which(is.na(Naivedata$Oldpeak))
which(is.na(Naivedata$HeartDisease))

#Annotate Sex as 'M' as 1, 'F' as 2:
Naivedata$Sex<-
  factor(Naivedata$Sex,
         levels = c("M", "F"),
         labels = c(1,2))
Naivedata$Sex

#Annotate ChestpainType 'ATA' as 1, 'NAP' as 2, 'ASY' as 3, 'TA' as 4:
Naivedata$ChestPainType<-
  factor(Naivedata$ChestPainType,
         levels = c("ATA", "NAP", "ASY", "TA"),
         labels = c(1,2,3,4))
Naivedata$ChestPainType

##Annotate RestingECG "Normal" as 1, "ST" as 2, "LVH" as 3:
Naivedata$RestingECG<-
  factor(Naivedata$RestingECG,
         levels = c("Normal", "ST", "LVH"),
         labels = c(1,2,3))
Naivedata$RestingECG

#Annotate ExerciseAngina  as 'N' as 1, 'Y' as 2:
Naivedata$ExerciseAngina<-
  factor(Naivedata$ExerciseAngina,
         levels = c("N", "Y"),
         labels = c(1,2))
Naivedata$ExerciseAngina

##Annotate ST_Slope "Up" as 1, "Flat" as 2, "Down" as 3:
Naivedata$ST_Slope<-
  factor(Naivedata$ST_Slope,
         levels = c("Up", "Flat", "Down"),
         labels = c(1,2,3))
Naivedata$ST_Slope

str(Naivedata)

# Delete the rows with missing values:
remove<-na.omit(Naivedata)
Naivedata

#Normalize the function:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
Naivedata1<- as.data.frame(lapply(Naivedata[6:12], normalize))
Naivedata1

summary(Naivedata1)

# Encoding the target variable
Naivedata$HeartDisease = factor(Naivedata$HeartDisease, levels = c(0, 1))
Naivedata$HeartDisease

# Splitting the data into training and test sets
library(caTools)
set.seed(123)
split = sample.split(Naivedata$HeartDisease, SplitRatio = 0.80)
Train_set = subset(Naivedata, split == TRUE)
Test_set = subset(Naivedata, split == FALSE)

# Feature Scaling
Train_set[-4] = scale(Train_set[-4])
Test_set[-4] = scale(Test_set[-4])

dim(Train_set); 
dim(Test_set);

# load the library
library(e1071) 
classifier_naiveBayes = naiveBayes(x = Train_set[-4],
                        y = Train_set$HeartDisease) # Fits Naive Bayes Model to the training set

classifier_naiveBayes

# Predicting the test set output
y_predict = predict(classifier_naiveBayes, newdata = Test_set[-4])
y_predict

# Creating a Confusion Matrix
install.packages("gmodels")
install.packages('caret')

library(gmodels)
library(caret)

confusionMatrix<- table(y_predict ,Test_set$HeartDisease)
confusionMatrix

confusionMatrix(table(y_predict, Test_set$HeartDisease ))

library(lattice)
library(caret)
#Visualization Confusion Matrix
fourfoldplot(as.table(confusionMatrix),
             color=c("IndianRed","Pink"),
             main = "Confusion Matrix")

acc_tr_tree<-(81+102)/184
acc_tr_tree
plot(acc_tr_tree)

train_control <- trainControl(method="cv", number=10)
#Model accuracy visualization
model <- train(HeartDisease~Cholesterol, data= Naivedata, trControl=train_control, method="nb")
print(model)
plot(model)

#Training set accuracy visualization
model_train <- train(HeartDisease~acc_tr_tree, data= Train_set, trControl=train_control, method="nb")
print(model_train )
plot(model_train)

#Test set accuracy visualization
model_test <- train(HeartDisease~., data= Test_set, trControl=train_control, method="nb")
print(model_test)
plot(model_test)



