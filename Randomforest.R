library(ggplot2)
library(randomForest)

heart$Sex<-as.factor(heart$Sex)
levels(heart$Sex)<-c("Female","Male")

heart$ChestPainType<-as.factor(heart$ChestPainType)
levels(heart$ChestPainType)<-c("asymptomatic","atypical","non-anginal","typical")

heart$FastingBS<-as.factor(heart$FastingBS)
levels(heart$FastingBS)<-c("False","True")

heart$RestingECG<-as.factor(heart$RestingECG)
levels(heart$RestingECG)<-c("LVH","Normal","hypertrophy")

heart$ExerciseAngina<-as.factor(heart$ExerciseAngina)
levels(heart$ExerciseAngina)<-c("No","Yes")

heart$ST_Slope<-as.factor(heart$ST_Slope)
levels(heart$ST_Slope)<-c("Downsloping","Flat","Up")

heart$HeartDisease<-as.factor(heart$HeartDisease)
levels(heart$HeartDisease)<-c("No", "Yes")

#barplot(table(heart$HeartDisease))

str(heart)
set.seed(120)
samp <- sample(nrow(heart),0.8*nrow(heart))
train <- heart[samp,]
test<-heart[-samp,]

#Moving onto Data visualization

ggplot(heart,aes(Age,RestingECG))+geom_point(aes(color=HeartDisease))

model <- randomForest(HeartDisease ~ . - FastingBS, data = train, ntree = 800, mtry = 10)
model
model$confusion

prediction <- predict(model, newdata = test)
table(prediction, test$HeartDisease)
prediction

# Now, let’s display the predicted vs. the actual values

results<-cbind(prediction,test$HeartDisease)
results

colnames(results)<-c('pred','real')
results<-as.data.frame(results)
View(results)

#accuracy
sum(prediction==test$HeartDisease) / nrow(test)
