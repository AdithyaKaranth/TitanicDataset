setwd("D:/R projects/Titanic dataset")

titanic.train<- read.csv(file="train.csv", stringsAsFactors=FALSE, header=TRUE)
titanic.test<- read.csv(file="test.csv", stringsAsFactors=FALSE, header=TRUE)
#Adding another column to distinguish between training and testing set after combining
titanic.train$IsTrainSet<-TRUE
titanic.test$IsTrainSet<-FALSE

# To match colums for combining
titanic.test$Survived<- NA

#combining the two datasets
titanic.full <- rbind(titanic.train,titanic.test)

#Data cleaning by eliminating empty lines
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

age.median<-median(titanic.full$Age,na.rm=TRUE)

titanic.full[is.na(titanic.full$Age),"Age"] <- age.median 

#Remove missing values from fare
fare.median<-median(titanic.full$Fare,na.rm=TRUE)

titanic.full[is.na(titanic.full$Fare),"Fare"]<- fare.median

#Categorical Casting
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

#Seperate the training and testing set after cleaning
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]

titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived<- as.factor(titanic.train$Survived)

survived.equation<-"Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked"
#Random forest requires equation in formula format
survived.formula<-as.formula(survived.equation)

library(randomForest)

titanic.model<-randomForest(formula=survived.formula,data=titanic.train,ntree=500,mtry=3,nodesize=0.01*nrow(titanic.train))
features.equation<="Pclass+Sex+Age+SibSp+Parch+Fare+Embarked"

Survived<-predict(titanic.model,newdata=titanic.test)

PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived

write.csv(output.df,file="kaggle_submission.csv",row.names=FALSE)
 