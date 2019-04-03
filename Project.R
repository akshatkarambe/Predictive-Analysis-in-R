
#Step1: Importing the Data
Data_Mean <- read.csv(file="C:/Users/AXAT/Desktop/Sem 2/Data Mining/Course Project/Final Extracted Data/new_data_mean.csv", header=TRUE, sep=",")
View(Data_Mean)
names(Data_Mean)
str(Data_Mean)



#Step2: Data Cleaning
# Removing generic parameters like "Gender"  "Height"  "ICUType" "RecordID" 
Col_Clean <- Data_Mean[,c(-14, -19, -20,-33)]
Col_Clean_Names <- Data_Mean[,c(14, 19,33,20)]
names(Col_Clean_Names)


## Removing columns/parameters which have more than 60% null values.
Data_Clean <- Col_Clean[, -which(colMeans(is.na(Col_Clean)) > .6)]
Removed_Col_Names <- Col_Clean[, which(colMeans(is.na(Col_Clean)) > .6)]
names(Removed_Col_Names)
# Removed ("Cholesterol" "RespRate"    "TroponinI"   "TroponinT" )


# Removing Rows which have more than 68% of null values. ie the records which have less than or equal to 10 parameters.
Data_RowCleaned <- Data_Clean[-which(rowMeans(is.na(Data_Clean)) > 0.68), ]
# 20 rows were removed.



#Step3: Separating alive and died patients and fill all null values with the mean of the column 
library(dplyr)
survived <- filter(Data_RowCleaned,In.hospital_death == '0')
died <- filter(Data_RowCleaned,In.hospital_death == '1')

#Getting list of columns having missing values in both "alive" and "died"
mylist_survived <- c(unlist(lapply(survived , function(x) any(is.na(x)))))
View(mylist_survived)
names(mylist_survived)

mylist_died <- c(unlist(lapply(died , function(x) any(is.na(x)))))
View(mylist_died)
#Apart from In_Hospital_Death, Weight and Age all other columns have the null values.



#step4: Filling missing values with mean values for the obtained columns for Patients alive.
survived$ALP[is.na(survived$ALP)] <- mean(survived$ALP, na.rm = T)
survived$ALT[is.na(survived$ALT)] <- mean(survived$ALT, na.rm = T)
survived$AST[is.na(survived$AST)] <- mean(survived$AST, na.rm = T)
survived$Albumin[is.na(survived$Albumin)] <- mean(survived$Albumin, na.rm = T)
survived$BUN[is.na(survived$BUN)] <- mean(survived$BUN, na.rm = T)
survived$Bilirubin[is.na(survived$Bilirubin)] <- mean(survived$Bilirubin, na.rm = T)
survived$Creatinine[is.na(survived$Creatinine)] <- mean(survived$Creatinine, na.rm = T)
survived$DiasABP[is.na(survived$DiasABP)] <- mean(survived$DiasABP, na.rm = T)
survived$FiO2[is.na(survived$FiO2)] <- mean(survived$FiO2, na.rm = T)
survived$GCS[is.na(survived$GCS)] <- mean(survived$GCS, na.rm = T)
survived$Glucose[is.na(survived$Glucose)] <- mean(survived$Glucose, na.rm = T)
survived$HCO3[is.na(survived$HCO3)] <- mean(survived$HCO3, na.rm = T)
survived$HCT[is.na(survived$HCT)] <- mean(survived$HCT, na.rm = T)
survived$HR[is.na(survived$HR)] <- mean(survived$HR, na.rm = T)
survived$K[is.na(survived$K)] <- mean(survived$K, na.rm = T)
survived$Lactate[is.na(survived$Lactate)] <- mean(survived$Lactate, na.rm = T)
survived$MAP[is.na(survived$MAP)] <- mean(survived$MAP, na.rm = T)
survived$MechVent[is.na(survived$MechVent)] <- mean(survived$MechVent, na.rm = T)
survived$Mg[is.na(survived$Mg)] <- mean(survived$Mg, na.rm = T)
survived$NIDiasABP[is.na(survived$NIDiasABP)] <- mean(survived$NIDiasABP, na.rm = T)
survived$NIMAP[is.na(survived$NIMAP)] <- mean(survived$NIMAP, na.rm = T)
survived$NISysABP[is.na(survived$NISysABP)] <- mean(survived$NISysABP, na.rm = T)
survived$Na[is.na(survived$Na)] <- mean(survived$Na, na.rm = T)
survived$PaCO2[is.na(survived$PaCO2)] <- mean(survived$PaCO2, na.rm = T)
survived$PaO2[is.na(survived$PaO2)] <- mean(survived$PaO2, na.rm = T)
survived$Platelets[is.na(survived$Platelets)] <- mean(survived$Platelets, na.rm = T)
survived$SaO2[is.na(survived$SaO2)] <- mean(survived$SaO2, na.rm = T)
survived$SysABP[is.na(survived$SysABP)] <- mean(survived$SysABP, na.rm = T)
survived$Temp[is.na(survived$Temp)] <- mean(survived$Temp, na.rm = T)
survived$Urine[is.na(survived$Urine)] <- mean(survived$Urine, na.rm = T)
survived$WBC[is.na(survived$WBC)] <- mean(survived$WBC, na.rm = T)
survived$pH[is.na(survived$pH)] <- mean(survived$pH, na.rm = T)

#Filling missing values with mean values for the obtained columns for the Patients who are Died
died$ALP[is.na(died$ALP)] <- mean(died$ALP, na.rm = T)
died$ALT[is.na(died$ALT)] <- mean(died$ALT, na.rm = T)
died$AST[is.na(died$AST)] <- mean(died$AST, na.rm = T)
died$Albumin[is.na(died$Albumin)] <- mean(died$Albumin, na.rm = T)
died$BUN[is.na(died$BUN)] <- mean(died$BUN, na.rm = T)
died$Bilirubin[is.na(died$Bilirubin)] <- mean(died$Bilirubin, na.rm = T)
died$Creatinine[is.na(died$Creatinine)] <- mean(died$Creatinine, na.rm = T)
died$DiasABP[is.na(died$DiasABP)] <- mean(died$DiasABP, na.rm = T)
died$FiO2[is.na(died$FiO2)] <- mean(died$FiO2, na.rm = T)
died$GCS[is.na(died$GCS)] <- mean(died$GCS, na.rm = T)
died$Glucose[is.na(died$Glucose)] <- mean(died$Glucose, na.rm = T)
died$HCO3[is.na(died$HCO3)] <- mean(died$HCO3, na.rm = T)
died$HCT[is.na(died$HCT)] <- mean(died$HCT, na.rm = T)
died$HR[is.na(died$HR)] <- mean(died$HR, na.rm = T)
died$K[is.na(died$K)] <- mean(died$K, na.rm = T)
died$Lactate[is.na(died$Lactate)] <- mean(died$Lactate, na.rm = T)
died$MAP[is.na(died$MAP)] <- mean(died$MAP, na.rm = T)
died$MechVent[is.na(died$MechVent)] <- mean(died$MechVent, na.rm = T)
died$Mg[is.na(died$Mg)] <- mean(died$Mg, na.rm = T)
died$NIDiasABP[is.na(died$NIDiasABP)] <- mean(died$NIDiasABP, na.rm = T)
died$NIMAP[is.na(died$NIMAP)] <- mean(died$NIMAP, na.rm = T)
died$NISysABP[is.na(died$NISysABP)] <- mean(died$NISysABP, na.rm = T)
died$Na[is.na(died$Na)] <- mean(died$Na, na.rm = T)
died$PaCO2[is.na(died$PaCO2)] <- mean(died$PaCO2, na.rm = T)
died$PaO2[is.na(died$PaO2)] <- mean(died$PaO2, na.rm = T)
died$Platelets[is.na(died$Platelets)] <- mean(died$Platelets, na.rm = T)
died$SaO2[is.na(died$SaO2)] <- mean(died$SaO2, na.rm = T)
died$SysABP[is.na(died$SysABP)] <- mean(died$SysABP, na.rm = T)
died$Temp[is.na(died$Temp)] <- mean(died$Temp, na.rm = T)
died$Urine[is.na(died$Urine)] <- mean(died$Urine, na.rm = T)
died$WBC[is.na(died$WBC)] <- mean(died$WBC, na.rm = T)
died$pH[is.na(died$pH)] <- mean(died$pH, na.rm = T)

#Removing the MechVent column from both 
names(survived)
Patient_Survived <- survived[,-20]
Patient_Died <- died[,-20]
Final_Data_Cleaned <- rbind(Patient_Survived,Patient_Died)
str(Final_Data_Cleaned)

#Removing The In.hospital_death column
Final_Data_Cleaned1 <- Final_Data_Cleaned[,-1]
str(Final_Data_Cleaned1)


#Step5: Performing Dimension Reduction Techniques On both un-scaled and scaled data.
## selecting numeric data
Final_Data_Cleaned2<- Final_Data_Cleaned1[sapply(Final_Data_Cleaned1,is.numeric)] 
str(Final_Data_Cleaned2)

#Performing Dimensionality Reduction on Un-Scaled Data
dim(Final_Data_Cleaned2)

#Calculating PCA on data
Pca_Data_Cleaned <- prcomp(Final_Data_Cleaned2)
summary(Pca_Data_Cleaned)
plot(Pca_Data_Cleaned)
plot(Pca_Data_Cleaned, type = 'line')

install.packages("factoextra")
library(factoextra)

cleaneddata <- fviz_eig(Pca_Data_Cleaned, addlabels = TRUE, ylim = c(0, 20),xlim=c(0,11))
cleaneddata

#1.) Scale the data and run the PCA again.
scaling<- scale(Final_Data_Cleaned2)
plot(scaling)
Pca_Data_Cleaned1<- prcomp(scaling,center=TRUE)
summary(Pca_Data_Cleaned1)
plot(Pca_Data_Cleaned1)
plot(Pca_Data_Cleaned1,type='line')
library(factoextra)
cleaneddata1 <- fviz_eig(Pca_Data_Cleaned1, addlabels = TRUE, ylim = c(0, 20),xlim=c(0,11))
cleaneddata1

#2.) After plotting graph and manual observation of PCA Components,we need to choose PCA Components having 95% Variance.
## or using covariances 
eig.val <- get_eigenvalue(Pca_Data_Cleaned1)
eig.val


#we get 95% variance till 27th varaible
dim(Final_Data_Cleaned2)
cor(Final_Data_Cleaned2)
covariances<-cor(scaling)
library(corrplot)
corrplot(covariances, order = "hclust")
dim(covariances)
library(caret)
highlyCor <- findCorrelation(covariances, 0.6)

#Apply correlation filter at 0.60,
#then we remove all the variable correlated with more 0.7.
datMyFiltered.scale <- scaling[,-highlyCor]
covariances1 <- cor(datMyFiltered.scale)
corrplot(covariances1, order = "hclust")
dim(datMyFiltered.scale)
str(datMyFiltered.scale)

#Concatenated the final data frame with the In_hospital_Death column .
z <- as.data.frame(datMyFiltered.scale, Header = TRUE)
Final_Data <- data.frame(c(z, Final_Data_Cleaned[c("In.hospital_death")]))
Final_Data$In.hospital_death<-as.numeric(Final_Data$In.hospital_death)
#Summary and proportions
str(Final_Data)
summary(Final_Data)
prop.table(table(Final_Data$In.hospital_death))

#Cross validation and Modelling
## data partition
intrain <- createDataPartition(y=Final_Data$In.hospital_death, p =0.7,list=FALSE)
training <- Final_Data[intrain,]
testing <- Final_Data[-intrain,]

## sampling/balancing the original data
library(ROSE)
both <- ovun.sample(In.hospital_death~., data = Final_Data, method = "both", p = 0.5, seed = 222, N = 3980)$data
prop.table(table(both$In.hospital_death))

both1 <- ovun.sample(In.hospital_death~., data = training, method = "both", p = 0.5, seed = 222, N = 2786)$data
prop.table(table(both1$In.hospital_death))
both1$In.hospital_death<- as.factor(both1$In.hospital_death)
str(training)

both2 <- ovun.sample(In.hospital_death~., data = testing, method = "both", p = 0.5, seed = 222, N = 1194)$data
prop.table(table(both2$In.hospital_death))
str(both1$In.hospital_death)


#svm

## using cross validation
library(caret)
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(3233)

## on whole data
svm_Linear <- train(In.hospital_death ~., data = Final_Data, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"))

str(Final_Data)
print(svm_Linear)
svm_Linear$trainingData
predict(svm_Linear,Final_Data)
confusionMatrix(svm_Linear$trainingData$.outcome,Final_Data$In.hospital_death, positive = '1')
#Here if, we use the whole data for the cross validation then the accuracy is coming out to be 1 which is wrong.


## on balanced training and testing data 
model4<- train(In.hospital_death~., data=both1, trControl=trctrl, method="svmLinear", family=binomial())

pred.svm = predict(model4, both2)
print(pred.svm)
str(both2$In.hospital_death)
str(pred.svm)
both2$In.hospital_death <- as.factor(both2$In.hospital_death)
pred.svm <- as.factor(pred.svm)
confusionMatrix(pred.svm, both2$In.hospital_death, positive = '1')



#LDA
library(MASS)
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(3233)


lda.fit1 = train(In.hospital_death ~ ., data= both1, method="lda",
                 trControl = trctrl)

pred.lda = predict(lda.fit1, both2)

confusionMatrix(pred.lda, both2$In.hospital_death, positive = '1')

#Logistic Regression
str(both1)

library(caret)

# define training control
train_control<- trainControl(method="repeatedcv", number=5,repeats=3)

# train the model 
#for total data
model<- train(In.hospital_death~., data=Final_Data, trControl=train_control, method="glm", family=binomial())
model$resample
print(model)

## on balanced training and testing data
model5<- train(In.hospital_death~., data=both1, trControl=train_control, method="glm", family=binomial())

pred.glm = predict(model5, both2)
print(pred.glm)
str(pred.glm)


str(pred.glm)
both2$In.hospital_death <- as.factor(both2$In.hospital_death)
str(both2$In.hospital_death)
logRegCM <- confusionMatrix(pred.glm, both2$In.hospital_death, positive = '1' )









#Step6: Data Partition
set.seed(123)
#Divide the data into 70 % traing and 30 % testing
ind <- sample(2,nrow(Final_Data), replace=TRUE, prob=c(0.7, 0.3))
Train_Data <- Final_Data[ind==1,]
Test_Data = Final_Data[ind==2,]
table(Train_Data$In.hospital_death)
prop.table(table(Train_Data$In.hospital_death))
table(Test_Data$In.hospital_death)
prop.table(table(Test_Data$In.hospital_death))

head(Train_Data)
#Step7: Developing Predictive Model
#1.)Random Forest
install.packages("randomForest")
library(randomForest)
rftrain <- randomForest(In.hospital_death~.,data = Train_Data, mtry = 5)
rfover <- randomForest(In.hospital_death~.,data = over)
rfunder <- randomForest(In.hospital_death~.,data = under)
rfboth <- randomForest(In.hospital_death~.,data = both)
#rfrose <- randomForest(In.hospital_death~.,data = rose)


#Model Evaluation with Test_Data
library(caret)
library(e1071)
confusionMatrix(predict(rftrain,Test_Data), Test_Data$In.hospital_death, positive = '1')
confusionMatrix(predict(rfover,Test_Data), Test_Data$In.hospital_death, positive = '1')
confusionMatrix(predict(rfunder,Test_Data), Test_Data$In.hospital_death, positive = '1')
confusionMatrix(predict(rfboth,Test_Data), Test_Data$In.hospital_death, positive = '1')

#Oversampling 
install.packages("ROSE")
library(ROSE)
over <- ovun.sample(In.hospital_death~., data = Train_Data, method = "over", N = 4796)$data
table(over$In.hospital_death)

under <- ovun.sample(In.hospital_death~., data = Train_Data, method = "under", N = 762)$data
table(under$In.hospital_death)

both <- ovun.sample(In.hospital_death~., data = Train_Data, method = "both", p = 0.5, seed = 222, N = 1201)$data
table(both$In.hospital_death)

#rose <- ROSE(In.hospital_death~., data = Train_Data, N = 500, seed = 111)
#table(rose$In.hospital_death)
#summary(rose)

#2.) Support Vector Machine
svmtrain <- svm(In.hospital_death ~ ., kernel = "linear",  type = "C-classification", cost =1 ,gamma=0.01, data = Train_Data, scale = F)
svmover1 <- svm(In.hospital_death ~ ., kernel = "linear",  type = "C-classification", cost =1 ,gamma=0.01, data = over1, scale = F)
svmunder1 <- svm(In.hospital_death ~ ., kernel = "linear",  type = "C-classification", cost =1 ,gamma=0.01, data = under1, scale = F)
svmboth1 <- svm(In.hospital_death ~ ., kernel = "linear",  type = "C-classification", cost =1 ,gamma=0.01, data = both1, scale = F)

#Model Evaluation with Test_Data
confusionMatrix(predict(svmtrain,Test_Data), Test_Data$In.hospital_death, positive = '1')
confusionMatrix(predict(svmover1,Test_Data), Test_Data$In.hospital_death, positive = '1')
confusionMatrix(predict(svmunder1,Test_Data), Test_Data$In.hospital_death, positive = '1')
confusionMatrix(predict(svmboth1,Test_Data), Test_Data$In.hospital_death, positive = '1')

#Oversampling 
install.packages("ROSE")
library(ROSE)
over1 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "over", N = 4796)$data
table(over1$In.hospital_death)

under1 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "under", N = 762)$data
table(under1$In.hospital_death)

both1 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "both", p = 0.5, seed = 222, N = 1201)$data
table(both1$In.hospital_death)


#3.) Linear Discriminant Analysis
library(MASS)
ldatrain <- lda(In.hospital_death ~ ., data = Train_Data)
summary(ldatrain)
ldaover2 <- lda(In.hospital_death ~ ., data = over2)
ldaunder2 <- lda(In.hospital_death ~ ., data = under2)
ldaboth2 <- lda(In.hospital_death ~ ., data = both2)

#Model Evaluation with Test_Data
testpred <- predict(ldatrain, newdata = Test_Data[,1:28])$class
confusionMatrix(testpred, Test_Data$In.hospital_death, positive = '1')

testpredover <- predict(ldaover2, newdata = Test_Data[,1:28])$class
confusionMatrix(testpredover, Test_Data$In.hospital_death, positive = '1')

testpredunder <- predict(ldaunder2, newdata = Test_Data[,1:28])$class
confusionMatrix(testpredunder, Test_Data$In.hospital_death, positive = '1')

testpredboth <- predict(ldaboth2, newdata = Test_Data[,1:28])$class
confusionMatrix(testpredboth, Test_Data$In.hospital_death, positive = '1')

#Sampling
over2 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "over", N = 4796)$data
table(over2$In.hospital_death)

under2 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "under", N = 762)$data
table(under1$In.hospital_death)

both2 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "both", p = 0.5, seed = 222, N = 1201)$data
table(both1$In.hospital_death)




#4.)Logistic Regression
Logtrain = glm(In.hospital_death ~.,data=Train_Data,family="binomial")
summary(Logtrain)
Logover3 = glm(In.hospital_death ~.,data=over3, family="binomial")
Logunder3 = glm(In.hospital_death~.,data=under3, family="binomial")


#Model Evaluation with Test_Data
tespred1 <- predict(Logtrain, Test_Data, type =  "response")
confusionMatrix(testpred1, Test_Data$In.hospital_death, positive = '1')

testpredover1 <- predict(Logover3, Test_Data, type =  "response")
predover <- ifelse(testpredover1 > 0.5, 1, 0)
confusionMatrix(predover, Test_Data$In.hospital_death, positive = '1')

testpredunder1 <- predict(Logunder3, Test_Data, type =  "response")
predunder <- ifelse(testpredunder1 > 0.5, 1, 0)
confusionMatrix(testpredunder1, Test_Data$In.hospital_death, positive = '1')

#Sampling
over3 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "over", N = 4796)$data
table(over2$In.hospital_death)

under3 <- ovun.sample(In.hospital_death~., data = Train_Data, method = "under", N = 762)$data
table(under1$In.hospital_death)