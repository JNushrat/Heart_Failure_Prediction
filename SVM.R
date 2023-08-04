library(caret)

library(tidyverse)		 # for data manipulation

# Load ggplot2
library(ggplot2)

heart <- read.csv("D:/Thesis/Others/HeartFinal.csv")
str(heart)
head(heart)
intrain <- createDataPartition(y = heart$HeartDisease, p= 0.7, list = FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]
dim(training); 
dim(testing);
anyNA(heart)
summary(heart)
training[["HeartDisease"]] = factor(training[["HeartDisease"]])
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_Linear <- train(HeartDisease ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)
test_pred
confusionMatrix(table(test_pred, testing$HeartDisease))

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(HeartDisease ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, testing$HeartDisease))
