heart <- read.csv("D:/Thesis/Others/HeartFinal.csv")
head(heart)
tail(heart)

colSums(is.na(heart))
str(heart)
summary(heart)


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

str(heart)
summary(heart)

library(ggplot2)
ggplot(heart,aes(x=Age,fill=HeartDisease,color=HeartDisease)) 
+ geom_histogram(binwidth = 1,color="black") 
+ labs(x = "Age",y = "Frequency", 
       title = "Heart Disease with reference to Age")

mytable <- table(heart$ChestPainType)
pct<-round(mytable/sum(mytable)*100)
lbls1<-paste(names(mytable),pct)
lbls<-paste(lbls1, "%", sep="")
pie(mytable, labels = lbls,
    col = rainbow(length(lbls)),
    main="Pie Chart of Chest Pain",radius = 0.9)

set.seed(100) 
#100 is used to control the sampling permutation to 100. 
index<-sample(nrow(heart),0.75*nrow(heart))
train<-heart[index,]
test<-heart[-index,]
modelblr<-glm(HeartDisease~.,data = train,family = "binomial")
# family = " binomial" means it contains only two outcomes.
train$pred<-fitted(modelblr)
# fitted can be used only to get predicted score of the data on which model has been generated.
head(train)
train$pred<-NULL
test$pred<-NULL
library(rpart)
tree<-rpart(HeartDisease~.,method = "class",data = train)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree)

test$pred<-predict(tree,test,type = "class")
library(caret)
confusionMatrix(test$pred,test$HeartDisease)
confusionMatrix<- table(test$pred,test$HeartDisease)
confusionMatrix

acc_tr_tree<-(74+113)/230;acc_tr_tree

#Visualization of confusion matrix
fourfoldplot(as.table(confusionMatrix),
             color=c("Grey","SkyBlue"),
             main = "Confusion Matrix")
