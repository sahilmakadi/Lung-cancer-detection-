getwd()
setwd("D:/Rlabns")


library(caret)
library(rpart)
library(randomForest)
library(smotefamily)
library(ROSE)
library(e1071)
data <- read.csv("survey lung cancer.csv")
#data <- data[, -2]  # Exclude the first column 

set.seed(123)
row <- sample(nrow(data))
shuffled_data <- data[row, ]

# Factorize all independent columns except the first two
for (i in 3:(ncol(shuffled_data))) {  # Exclude the last column
  shuffled_data[, i] <- as.factor(shuffled_data[, i])
}

shuffled_data$GENDER<-as.factor(shuffled_data$GENDER)

summary(shuffled_data)
data.balanced.over<-ovun.sample(class~.,data = shuffled_data,p=0.45,seed=123,method = "over")$data
levels(data.balanced.over)
with(data.balanced.over,
     {print(table(class));
     })

#fEATURE SELECTION USING BORUTA
library(Boruta)
set.seed(1234)
boruta<-Boruta(class~.,data=data.balanced.over,doTrace=2)
print(boruta)
plot(boruta,las=2,ces.axis=0.7)
plotImpHistory(boruta)

getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)
attStats(boruta)

outcome_variable <- data.balanced.over$class
set.seed(123) #makes our results consistent every time we run the program
train_indices <- createDataPartition(outcome_variable, p = 0.75, list = FALSE)

train_data <- data.balanced.over[train_indices, ]
test_data <- data.balanced.over[-train_indices, ]

test_data_without_outcome <- test_data[, -which(names(test_data) == "class")]

ctrl <- trainControl(method = "cv", number = 10)

formula <- class ~ .

# Random forest model
print("rf with all features/n")
model_rf <- train(formula,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl,ntree=300)

rf_model <- model_rf$finalModel

print(model_rf)



importance_rf <- importance(rf_model)
importance_rf<-sort(rf_model$importance[,1],decreasing = TRUE)
print(importance_rf)






predictions_rf <- predict(model_rf, newdata = test_data_without_outcome)

conf_mat_rf <- confusionMatrix(predictions_rf, test_data$class)
print(conf_mat_rf)




#Rf8----
print("rf with 8 features/n")
# Train a Random Forest model with specified features
rf8 <- randomForest(class ~ ALLERGY + SWALLOWING_.DIFFICULTY + AGE + ALCOHOL_.CONSUMING + FATIGUE +
                       YELLOW_FINGERS + PEER_PRESSURE + ANXIETY + COUGHING,
                     data = train_data)


predictions_rf8 <- predict(rf8, newdata = test_data_without_outcome)

conf_mat_rf8 <- confusionMatrix(predictions_rf8, test_data$class)

print(conf_mat_rf8)




 


# SVM model
model_svm <- train(formula,
                   data = train_data,
                   method = "svmLinear",
                   trControl = ctrl)#ctrl = cross validation

print(model_svm)

predictions_svm <- predict(model_svm, newdata = test_data_without_outcome)

conf_mat_svm <- confusionMatrix(predictions_svm, test_data$class)
print(conf_mat_svm)


#svm8----
print("svm with 8 features  ")
# Train a svm with specified features
svm8 <- svm(class ~ ALLERGY + SWALLOWING_.DIFFICULTY + AGE + ALCOHOL_.CONSUMING + FATIGUE +
                      YELLOW_FINGERS + PEER_PRESSURE + ANXIETY + COUGHING,
                    data = train_data, kernel = "linear")


predictions_svm8 <- predict(svm8, newdata = test_data_without_outcome)

conf_mat_svm8 <- confusionMatrix(predictions_svm8, test_data$class)

print(conf_mat_svm8)


# Decision Tree model
model_dt <- train(formula,
                  data = train_data,
                  method = "rpart",
                  trControl = ctrl)

print(model_dt)

predictions_dt <- predict(model_dt, newdata = test_data_without_outcome)

conf_mat_dt <- confusionMatrix(predictions_dt, test_data$class)


print(conf_mat_dt)


# DT8----
print("DT with 8 features")

# Train the decision tree model
tree_model <- rpart(formula = class ~ ALLERGY + SWALLOWING_.DIFFICULTY + AGE + ALCOHOL_.CONSUMING + FATIGUE +
                      YELLOW_FINGERS + PEER_PRESSURE + ANXIETY + COUGHING,
                    data = train_data)

# Make predictions using the test data
predictions_tree <- predict(tree_model, newdata = test_data_without_outcome, type = "class")

# Convert predictions and actual classes to factors
predictions_tree <- factor(predictions_tree, levels = c("NO", "YES"))
test_data$class <- factor(test_data$class, levels = c("NO", "YES"))

# Create confusion matrix
conf_mat_tree <- confusionMatrix(predictions_tree, test_data$class)

print(conf_mat_tree)


#lr model----
model <- train(formula,
               data = train_data, 
               method = "glm",
               trControl = ctrl,
               family = binomial)


print(model)


predictions <- predict(model, newdata = test_data_without_outcome, type="prob")
threshold <- 0.5
predictions <- ifelse(predictions[, "YES"] > threshold, "YES", "NO")

test_data$class <- factor(test_data$class, levels = c("NO", "YES"))
predictions <- factor(predictions, levels = c("NO", "YES"))



conf_mat <- confusionMatrix(predictions, test_data$class)
print(conf_mat)
# #lr8----
print("LR with 8 features")

# Train Logistic Regression model with 8 features
lr8_time <- system.time({
  model_lr8 <- train(class ~ ALLERGY + SWALLOWING_.DIFFICULTY + AGE + ALCOHOL_.CONSUMING + 
                       FATIGUE + YELLOW_FINGERS + PEER_PRESSURE + ANXIETY + COUGHING,
                     data = train_data, method = "glm", trControl = ctrl, family = binomial)
})
print(model_lr8)
print(lr8_time)

# Make predictions on the test data without outcome variable
predictions_lr8 <- predict(model_lr8, newdata = test_data_without_outcome, type = "prob")
threshold <- 0.5
predictions_lr8 <- ifelse(predictions_lr8[, "YES"] > threshold, "YES", "NO")  # Corrected index

# Convert class and predictions to factors with the same levels
test_data$class <- factor(test_data$class, levels = c("NO", "YES"))
predictions_lr8 <- factor(predictions_lr8, levels = c("NO", "YES"))

# naive bayes Compute confusion matrix----
conf_mat_lr8 <- confusionMatrix(predictions_lr8, test_data$class)
print(conf_mat_lr8)


nb_grid <- expand.grid(fL = 1, usekernel = TRUE, adjust = 1)
model_nb <- train(formula, data = train_data, method = "nb", trControl = ctrl, tuneGrid = nb_grid)

print(model_nb)

# Ensure predictions and actual class levels are consistent
predictions_nb <- predict(model_nb, newdata = test_data_without_outcome)
predictions_nb <- factor(predictions_nb, levels = levels(test_data$class))
test_data$class <- factor(test_data$class, levels = levels(test_data$class))

conf_mat_nb <- confusionMatrix(predictions_nb, test_data$class)
print(conf_mat_nb)

# Naive Bayes with 8 Features and Laplace smoothing
nb8 <- naiveBayes(class ~ ALLERGY + SWALLOWING_.DIFFICULTY + AGE + ALCOHOL_.CONSUMING + FATIGUE +
                    YELLOW_FINGERS + PEER_PRESSURE + ANXIETY + COUGHING, data = train_data, laplace = 1, usekernel = TRUE)

predictions_nb8 <- predict(nb8, newdata = test_data_without_outcome)
predictions_nb8 <- factor(predictions_nb8, levels = levels(test_data$class))
test_data$class <- factor(test_data$class, levels = levels(test_data$class))

conf_mat_nb8 <- confusionMatrix(predictions_nb8, test_data$class)
print(conf_mat_nb8)
