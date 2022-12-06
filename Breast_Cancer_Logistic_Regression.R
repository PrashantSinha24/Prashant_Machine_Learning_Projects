getwd()
setwd("E:\\R_File\\Logistic Regression")

cancer_data<-read.csv("E:\\R_File\\Logistic Regression\\cancerdata.csv",na.strings = "",stringsAsFactors = T)

head(cancer_data)
str(cancer_data)
summary(cancer_data)

library(Amelia)
library(caTools)
library(ggplot2)
library(dplyr)
library(psych)

View(cancer_data)

#checking the missing value in complete data set
missmap(cancer_data) #no missing value in data set

names(cancer_data)

table(cancer_data$diagnosis)

#split the dataset in test and train

cancer_data$diagnosis<-factor(cancer_data$diagnosis,levels =c("B","M"),
                              labels = c(0,1))
split=sample.split(cancer_data$diagnosis, SplitRatio = 0.7)
cancer_data<-cancer_data[-33]

bcancer_data_train<-subset(cancer_data, split==T)
bcancer_data_validat<-subset(cancer_data, split==F)

str(bcancer_data_train)
str(bcancer_data_validat)
View(bcancer_data_train)
View(bcancer_data_validat)

table(bcancer_data_train$diagnosis)
table(bcancer_data_validat$diagnosis)

#Visualization of diagnosis data

ggplot(bcancer_data_train, aes(x=diagnosis))+ggtitle("Distribution of Diagnosis data")+
    geom_bar(fill=c("blue","red"))+theme(legend.position = "none")
                         
str(bcancer_data_train$diagnosis)

###correlation between variables

bcancer_data_train$diagnosis<-as.numeric(bcancer_data_train$diagnosis)
co<-cor(as.matrix(bcancer_data_train[,1:31]))
corrplot::corrplot(co, order = "hclust", tl.cex = 0.7)
co
cor(bcancer_data_train$concave.points_worst, bcancer_data_train$texture_worst)
summary(co)
dev.off()

###check the correlation of all the variables in chart
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(co, histogram=TRUE, pch=22)

#####Comparing the radius, area and concavity of Benign and malignant stage

boxplot(bcancer_data_train$radius_mean~bcancer_data_train$diagnosis,
     main="Radius_mean of Benign and Malignant",ylab="radius_mean", xlab="diagnosis",
     col=c("Red", "Yellow"))


boxplot(bcancer_data_train$area_mean~bcancer_data_train$diagnosis,
        main="area_mean Benign and malignant",ylab="area_mean", xlab="diagnosis",
        col=c("Green", "Yellow"))

boxplot(bcancer_data_train$concavity_mean~bcancer_data_train$diagnosis,
        main="concavity_mean Benign and malignant",ylab="concavity_mean", xlab="diagnosis",
        col=c("Green", "blue"))

#####Comparing the smoothness_worst, area_worst and concavity_worst of Benign and malignant stage

boxplot(bcancer_data_train$smoothness_worst~bcancer_data_train$diagnosis,
        main="smoothness_worst of Benign and malignant",ylab="smoothness_worst", xlab="diagnosis",
        col=c("Green", "blue"))

boxplot(bcancer_data_train$area_worst~bcancer_data_train$diagnosis,
        main="area_worst of Benign and malignant",ylab="area_worst", xlab="diagnosis",
        col=c("Green", "red"))

boxplot(bcancer_data_train$concavity_worst~bcancer_data_train$diagnosis,
        main="concavity_worst of Benign and malignant",ylab="concavity_worst", xlab="diagnosis",
        col=c("Yellow", "red"))

#Normalisation
bcancer_data_train[,3:32]<-scale(bcancer_data_train[,3:32])
View(bcancer_data_train)

bcancer_data_validat[,3:32]<-scale(bcancer_data_validat[,3:32])
View(bcancer_data_validat)

####Lets apply the LOGISTIC REGRESSION (glm) to our training dataset
####Considering all variables in data set

bcancer_data_train$diagnosis<-factor(bcancer_data_train$diagnosis)

reg_model<-glm(diagnosis~., family = "binomial", data = bcancer_data_train)

summary(reg_model)

#####
reg_model2<-glm(diagnosis~smoothness_worst+area_mean+area_worst+radius_worst+
                     perimeter_worst+concavity_se+concavity_worst, family = "binomial",
                   data = bcancer_data_train)

summary(reg_model2)

#####
reg_model3<-glm(diagnosis~smoothness_worst+area_worst+concavity_worst, 
                   data = bcancer_data_train,family = "binomial")


summary(reg_model3)# all the p values are less for all the variables, Hence, good model

library(car)
vif(reg_model2)
vif(reg_model3)

### in "reg_model3" model all the variable VIF is less than 5%
##Hence, we are considering the "reg_model3" is a good model

### Let's Predict the value on test data set

predicttest<-predict(reg_model3, newdata = bcancer_data_validat, type= "response")
summary(predicttest)

predicttest[1:10]

View(bcancer_data_validat)

#Accuracy calculation

table(bcancer_data_validat$diagnosis, predicttest>0.5)
table(bcancer_data_validat$diagnosis, predicttest>0.20)
table(bcancer_data_validat$diagnosis, predicttest>0.75)

acc_log_model<-(106+59)/(106+5+1+59)
acc_log_model

acc_log_model_1<-(106+57)/(106+7+1+57)
acc_log_model_1

acc_log_model_2<-(102+61)/(102+3+5+61)
acc_log_model_2

library(ROCR)
ROCRpred<-prediction(predicttest,bcancer_data_validat$diagnosis)

ROCRpref<-performance(ROCRpred,"tpr","fpr")

plot(ROCRpref)

plot(ROCRpref,print.cutoffs.at=seq(0,1,.1),colorize=T)

#to calculate the area under the curve

as.numeric(performance(ROCRpred,"auc")@y.values)


#For adding an additional coloumn in the test data set classifying Diagnosis for cancer
#We are able to get the class (Cancer yes or No) for each  diagnosis observation

bcancer_data_validat$cancer<-ifelse(predicttest>0.5, "Y", "N")
table(bcancer_data_validat$cancer)
View(bcancer_data_validat)


#####Aszad code

library(InformationValue)
confusionMatrix(Testing$diagnosis,prediction_model)

#calculate sensitivity
sensitivity(Testing$diagnosis, prediction_model)

#  #calculate specificity
specificity(Testing$diagnosis, prediction_model)

# #calculate total misclassification error rate
misClassError(Testing$diagnosis, prediction_model, threshold=optimal)

plotROC(Testing$diagnosis, prediction_model)

library(caret)

caret::varImp(Full_model_3)

