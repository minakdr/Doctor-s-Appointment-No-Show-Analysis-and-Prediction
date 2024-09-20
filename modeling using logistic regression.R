#Step 4 : modeling with logistic regression

summary(balanced_data)

#Convert `No.show` to a factor
balanced_data$No.show <- as.factor(balanced_data$No.show)
?glm
#Fit the logistic regression model
logit_model <- glm(No.show ~ Gender + Age + Neighbourhood + Scholarship + Hipertension + 
                     Diabetes + Alcoholism + Handcap + SMS_received + period, 
                   data = balanced_data, family =binomial)

summary(logit_model)


#load caret for train-test splitting and model evaluation
library(caret)

#split the data into training and test sets (70% training, 30% test)
set.seed(123)
trainIndex <- createDataPartition(balanced_data$No.show, p = 0.7, list = FALSE)
train_data <- balanced_data[trainIndex, ]
test_data <- balanced_data[-trainIndex, ]

#training
logit_model <- glm(No.show ~ Gender + Age + Neighbourhood + Scholarship + Hipertension + 
                     Diabetes + Alcoholism + Handcap + SMS_received + period, 
                   data = train_data, family = binomial)

#make predictions on the test data
pred_prob <- predict(logit_model, newdata = test_data, type = "response")

#convert probabilities to class labels (threshold = 0.5)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
pred_class <- as.factor(pred_class)

# Load caret for train-test splitting and model evaluation
library(caret)


set.seed(123)
trainIndex <- createDataPartition(balanced_data$No.show, p = 0.7, list = FALSE)
train_data <- balanced_data[trainIndex, ]
test_data <- balanced_data[-trainIndex, ]

#Train the logistic regression model
logit_model <- glm(No.show ~ Gender + Age + Neighbourhood + Scholarship + Hipertension + 
                     Diabetes + Alcoholism + Handcap + SMS_received + period, 
                   data = train_data, family = binomial)

#Make predictions on the test data
pred_prob <- predict(logit_model, newdata = test_data, type = "response")

#Convert probabilities to class labels (threshold = 0.5) #after several tests , I concluded that 0.5 gave the best result
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
pred_class <- as.factor(pred_class)


confusionMatrix(pred_class, test_data$No.show)



# Load pROC for ROC curve and AUC calculation
library(pROC)

# Generate ROC curve
roc_curve <- roc(test_data$No.show, as.numeric(pred_prob))

# Plot the ROC curve
plot(roc_curve)

# Compute AUC
auc(roc_curve)

