Diabetes <- read.csv("diabetes.csv", stringsAsFactors =  FALSE)
View(Diabetes)
summary(Diabetes[,c(1,2,3,4,5,6,7,8)])
# Preparing the dataset
Diabetes_1 = na.omit(Diabetes)
dim(Diabetes_1)

# Frequency Plot for attributes
Diabetes.P <- par(mfrow=c(3, 3))
hist(Diabetes$Pregnancies, main = "Number of Times Pregnant",
     xlab = "Pregnancies")
hist(Diabetes$Glucose, main = "Plasma Glucose Concentration",
     xlab = "Glucose")
hist(Diabetes$BloodPressure, main = "Blood Pressure",
     xlab = "Diastolic blood pressure (mm Hg)")
hist(Diabetes$SkinThickness, main = "SkinThickness",
     xlab = "Triceps skin fold thickness (mm)")
hist(Diabetes$Insulin, main = "Insulin",
     xlab = "2-Hour serum insulin (mu U/ml)")
hist(Diabetes$BMI, main = "BMI",
     xlab = "BMI")
hist(Diabetes$DiabetesPedigreeFunction, main = "DiabetesPedigreeFunction",
     xlab = "DiabetesPedigreeFunction")
hist(Diabetes$Age, main = "Age",
     xlab = "Age")
hist(Diabetes$Outcome, main = "Outcome",
     xlab = "Outcome")

# Boxplot for attributes
Diabetes.P <- par(mfrow=c(3, 3))
boxplot(Diabetes$Pregnancies, main = "Number of Times Pregnant",
     xlab = "Pregnancies")
boxplot(Diabetes$Glucose, main = "Plasma Glucose Concentration",
     xlab = "Glucose")
boxplot(Diabetes$BloodPressure, main = "Blood Pressure",
     xlab = "Diastolic blood pressure (mm Hg)")
boxplot(Diabetes$SkinThickness, main = "SkinThickness",
     xlab = "Triceps skin fold thickness (mm)")
boxplot(Diabetes$Insulin, main = "Insulin",
     xlab = "2-Hour serum insulin (mu U/ml)")
boxplot(Diabetes$BMI, main = "BMI",
     xlab = "BMI")
boxplot(Diabetes$DiabetesPedigreeFunction, main = "DiabetesPedigreeFunction",
     xlab = "DiabetesPedigreeFunction")
boxplot(Diabetes$Age, main = "Age",
     xlab = "Age")
boxplot(Diabetes$Outcome, main = "Outcome",
     xlab = "Outcome")

                 
#Normalization
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
Diabetes_N <- as.data.frame(lapply(Diabetes, normalize))


# Create training and testing data

Diabetes_D <- Diabetes_N[-9]
Diabetes_DL <- Diabetes_N[9]
D_train<-Diabetes_D[1:537,]
D_test<-Diabetes_D[538:768,]

# Create labels for training and testing data

D_train_labels <- Diabetes_DL[1:537, ]
D_test_labels <- Diabetes_DL[538:768, ]
D_train_labels <- as.factor(D_train_labels)
D_test_labels <- as.factor(D_test_labels)

################################################################################

# KNN Method


library(class)
library(gmodels)

test_pred_21 <- knn(train = D_train, test = D_test,cl = D_train_labels, k = 21)
CrossTable(x = D_test_labels, y = test_pred_21, prop.chisq = TRUE)
Accuracy.21 <- sum(D_test_labels == test_pred_21)/NROW(D_test_labels)
Accuracy.21



test_pred_10 <- knn(train = D_train, test = D_test,cl = D_train_labels, k = 10)
CrossTable(x = D_test_labels, y = test_pred_10, prop.chisq = FALSE)
Accuracy.10 <- sum(D_test_labels == test_pred_10)/NROW(D_test_labels)
Accuracy.10


test_pred_5 <- knn(train = D_train, test = D_test,cl = D_train_labels, k = 5)
CrossTable(x = D_test_labels, y = test_pred_5, prop.chisq = FALSE)
Accuracy.5 <- sum(D_test_labels == test_pred_5)/NROW(D_test_labels)
Accuracy.5


#############################################################################
# Decision Tree

#install.packages("C50")
library(C50)
? help("C5.0")
library(gmodels)
? C5.0
DT_model <- C5.0(D_train, D_train_labels)
summary(DT_model)
plot(DT_model)
DT_pred <- predict(DT_model, D_test, type = "class")
DT_pred
CrossTable(D_test_labels, DT_pred, dnn = c("Actual Value", "Predicted Value"))
ACC.DT1 <- sum(D_test_labels == DT_pred)/NROW(D_test_labels)
ACC.DT1

  ``
DT_model <- C5.0(D_train, D_train_labels, trials = 5)
summary(DT_model)
plot(DT_model)
DT_pred <- predict(DT_model, D_test, type = "class")
DT_pred
CrossTable(D_test_labels, DT_pred, dnn = c("Actual Value", "Predicted Value"))
ACC.DT1 <- sum(D_test_labels == DT_pred)/NROW(D_test_labels)
ACC.DT1

DT_model <- C5.0(D_train, D_train_labels, trials = 20)
summary(DT_model)
plot(DT_model)
DT_pred <- predict(DT_model, D_test, type = "class")
DT_pred
CrossTable(D_test_labels, DT_pred, dnn = c("Actual Value", "Predicted Value"))
ACC.DT1 <- sum(D_test_labels == DT_pred)/NROW(D_test_labels)
ACC.DT1

######################################################
#Neural Network

Diabetes_norm <- as.data.frame(lapply(Diabetes, normalize))
dim(Diabetes)

D_train <- Diabetes_N[1:537,]
D_test <- Diabetes_N[538:768,]
Diabetes[,'Outcome']<-factor(Diabetes[,'Outcome'])
summary(Diabetes)



# neuralnetwork

#install.packages("neuralnet")
library(neuralnet)

RNGversion("3.5.2")
set.seed(12345)

Diabetes_model <- neuralnet(formula = Outcome ~ Pregnancies+ Glucose + BloodPressure +
                              SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + 
                              Age, data = D_train)

plot(Diabetes_model)


#predicting method 

?compute
model_results <- compute(Diabetes_model, D_test[,1:8])
model_results2 <- predict(Diabetes_model, D_test[,1:8])
predicted_Outcome <- model_results$net.result
head(predicted_Outcome)

cor(predicted_Outcome, D_test$Outcome) #the accuracy
cor(model_results2, D_test$Outcome) #the accuracy
cor(model_results$net.result, D_test$Outcome)

D_model2 <- neuralnet(Outcome ~., data = D_train, hidden = 2)
plot(D_model2)

predicted_Outcome2 <- predict(D_model2, D_test[,1:8])
cor(predicted_Outcome2, D_test$Outcome)


softplus = function(x){log(1+exp(x))}

D_model3 = neuralnet(Outcome~.,data=D_train, hidden = (c(2,1)), act.fct=softplus)
plot(D_model3)
predicted_Outcome3 = predict(D_model3,D_test[,1:8])
cor(predicted_Outcome3, D_test$Outcome)
A <- D_test[,1:8]
head(A)
################################################################################

# Logistic regression
str(D_train)
D_Log <- glm(Outcome ~ Pregnancies + Glucose + 
               BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, family = binomial,
             data = D_train)

summary(D_Log)

# predict the probability of Outcome
D_Log_probs=predict(D_Log, D_test,type="response")
Log_pred =ifelse(D_Log_probs>=0.5,1,0)
#Accuracy
Accuracy_Log = mean(Log_pred==D_test$Outcome)
Accuracy_Log 
Log_Table1 <- table(Log_pred, D_test$Outcome)
Log_Table1

D_Log2 <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure +
               BMI + DiabetesPedigreeFunction, family = binomial, data = D_train)

summary(D_Log2)
D_Log_probs2=predict(D_Log2, D_test,type="response")
Log_pred2 =ifelse(D_Log_probs2>=0.5,1,0)
Accuracy_Log2 = mean(Log_pred2==D_test$Outcome)
Accuracy_Log2
Log_Table2 <- table(Log_pred, D_test$Outcome)
Log_Table2

#################################################################################
