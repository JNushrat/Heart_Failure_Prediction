install.packages(c('neuralnet','keras','tensorflow'),dependencies = T)

library(tidyverse)
# Load required libraries
library(neuralnet)

# Read the dataset
heart <- read.csv("D:/Thesis/Others/HeartFinal.csv")
heart
# Convert variables to factors or numeric type
heart$Sex <- as.factor(heart$Sex)
heart$ChestPainType <- as.factor(heart$ChestPainType)
heart$FastingBS <- as.factor(heart$FastingBS)
heart$RestingECG <- as.factor(heart$RestingECG)
heart$ExerciseAngina <- as.factor(heart$ExerciseAngina)
heart$ST_Slope <- as.factor(heart$ST_Slope)

# Convert numeric variables to the appropriate type
heart$Age <- as.numeric(heart$Age)
heart$RestingBP <- as.numeric(heart$RestingBP)
heart$Cholesterol <- as.numeric(heart$Cholesterol)
heart$MaxHR <- as.numeric(heart$MaxHR)
heart$Oldpeak <- as.numeric(heart$Oldpeak)

# Perform one-hot encoding for factor variables
heart_encoded <- cbind(heart[, !(names(heart) %in% c("Sex", "ChestPainType", "FastingBS", "RestingECG", "ExerciseAngina", "ST_Slope"))],
                       model.matrix(~ Sex + ChestPainType + FastingBS + RestingECG + ExerciseAngina + ST_Slope - 1, data = heart))

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(heart_encoded), 0.7 * nrow(heart_encoded))
train_data <- heart_encoded[train_indices, ]
test_data <- heart_encoded[-train_indices, ]

# Convert the target variable to numeric
train_data$HeartDisease <- as.numeric(train_data$HeartDisease)

# Create the neural network model
model <- neuralnet(
  HeartDisease ~ Age + RestingBP + Cholesterol + MaxHR + Oldpeak + .,
  data = train_data,
  hidden = c(4, 2),
  linear.output = FALSE
)
plot(model,rep = "best")

pred <- predict(model, test_data)
labels <- c("HeartDisease")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()

table(test_data$HeartDisease, prediction_label)
check = as.numeric(test_data$HeartDisease) == max.col(pred)
accuracy = (sum(check)/nrow(test_data))*100
print(accuracy)

confusionMatrix<- table(test_data$HeartDisease, pred)
confusionMatrix

