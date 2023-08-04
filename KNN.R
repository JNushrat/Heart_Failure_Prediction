# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("class")

heartdata<-read.csv("D:/Thesis/HeartFinal.csv", header=TRUE, sep=",")
heartdata
str(heartdata)

#missing value find out:
colSums(is.na(heartdata))

#outlier detect:
which(is.na(heartdata$Sex))
which(is.na(heartdata$ChestPainType))
which(is.na(heartdata$RestingECG))
which(is.na(heartdata$ExerciseAngina))
which(is.na(heartdata$ST_Slope))
which(is.na(heartdata$Age))
which(is.na(heartdata$RestingBP))
which(is.na(heartdata$Cholesterol))
which(is.na(heartdata$FastingBS))
which(is.na(heartdata$MaxHR))
which(is.na(heartdata$Oldpeak))
which(is.na(heartdata$HeartDisease))

#Annotate Sex as 'M' as 1, 'F' as 2:
heartdata$Sex<-
  factor(heartdata$Sex,
         levels = c("M", "F"),
         labels = c(1,2))
heartdata$Sex

#Annotate ChestpainType 'ATA' as 1, 'NAP' as 2, 'ASY' as 3, 'TA' as 4:
heartdata$ChestPainType<-
  factor(heartdata$ChestPainType,
         levels = c("ATA", "NAP", "ASY", "TA"),
         labels = c(1,2,3,4))
heartdata$ChestPainType

##Annotate RestingECG "Normal" as 1, "ST" as 2, "LVH" as 3:
heartdata$RestingECG<-
  factor(heartdata$RestingECG,
         levels = c("Normal", "ST", "LVH"),
         labels = c(1,2,3))
heartdata$RestingECG

#Annotate ExerciseAngina  as 'N' as 1, 'Y' as 2:
heartdata$ExerciseAngina<-
  factor(heartdata$ExerciseAngina,
         levels = c("N", "Y"),
         labels = c(1,2))
heartdata$ExerciseAngina

##Annotate ST_Slope "Up" as 1, "Flat" as 2, "Down" as 3:
heartdata$ST_Slope<-
  factor(heartdata$ST_Slope,
         levels = c("Up", "Flat", "Down"),
         labels = c(1,2,3))
heartdata$ST_Slope

str(heartdata)

# Delete the rows with missing values:
remove<-na.omit(heartdata)
heartdata

#Normalize the function:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
heartdata1<- as.data.frame(lapply(heartdata[6:12], normalize))
heartdata1

summary(heartdata1)

## Loading package
library(e1071)
library(caTools)
library(class)

# Splitting data into train and test data
split <- sample.split(heartdata1, SplitRatio = 0.7)
train_cl <- subset(heartdata1, split == "TRUE")
test_cl <- subset(heartdata1, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

#Fitting KNN Model to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$HeartDisease,
                      k = 1)
classifier_knn

## Model Evaluation - Choosing K Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$HeartDisease)
print(paste('Accuracy =', 1-misClassError))

# K = 13
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$HeartDisease,
                      k = 13)
misClassError <- mean(classifier_knn != test_cl$HeartDisease)
print(paste('Accuracy =', 1-misClassError))

# K = 28
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$HeartDisease,
                      k = 28)
misClassError <- mean(classifier_knn != test_cl$HeartDisease)
print(paste('Accuracy =', 1-misClassError))

# K = 45
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$HeartDisease,
                      k = 45)
misClassError <- mean(classifier_knn != test_cl$HeartDisease)
print(paste('Accuracy =', 1-misClassError))

install.packages("gmodels")
library(gmodels)
CrossTable(x=test_cl$HeartDisease , y=classifier_knn, prop.chisq = FALSE)

install.packages('caret')
library(lattice)
library(caret)
confusionMatrix(table(classifier_knn, test_cl$HeartDisease))
confusionMatrix<- table(classifier_knn ,test_cl$HeartDisease)
confusionMatrix

#Visualization Confusion Matrix
fourfoldplot(as.table(confusionMatrix),color=c("Orange","Yellow"),
             main = "Confusion Matrix")

#Accuracy check with confusion matrix
confusionMatrix(table(classifier_knn ,test_cl$HeartDisease))

#----------------------DATA VISUALIZATION-------------------
library(lattice)
histogram(~RestingBP, data=heartdata)
histogram(~Cholesterol, data=heartdata)
histogram(~HeartDisease, data=heartdata)

bwplot(~HeartDisease, data=heartdata)
bwplot(~MaxHR, data=heartdata)

xyplot(Age~Cholesterol, data=heartdata)
bwplot(~Cholesterol | RestingECG, data=heartdata)
xyplot(~RestingBP | RestingECG, data=heartdata)
barchart(~Age, data=heartdata, horiz=FALSE, xlab="Age",
         ylab="HeartDisease", legend=rownames(~HeartDisease), 
         col=c("red","blue"))

counts<-table(heartdata$ST_Slope,heartdata$ChestPainType) #rightside=x
barplot(counts, main="ST_Slope vs ChestpainType",
        xlab="",
        col=c("grey","blue","red"),
        legend=rownames(counts),
        beside = TRUE)

barchart(MaxHR~Cholesterol, data=heartdata)

barplot(Age)
barplot(Age, main="HeartDisease with refernce to age", xlab="Age",
        ylab="Frequency", legend=rownames(Age), col=c("red","yellow"))

densityplot(~HeartDisease, data=heartdata)
densityplot(~MaxHR, data=heartdata)

install.packages("assignPOP")
library(assignPOP)

#accuracy graph
heartdata <- read.table(system.file("extdata/Rate.txt", package="assignPOP"), header=TRUE)
accuracy.plot(heartdata, pop="all")

#ROC Curve
install.packages("verification")
library(verification)
x<- c(0,0,0,1,1,1)
y<- c(.7, .7, 0, 1,5,.6)
data<-data.frame(x,y)
names(data)<-c("yes","no")
roc.plot(data$yes,data$no)

















