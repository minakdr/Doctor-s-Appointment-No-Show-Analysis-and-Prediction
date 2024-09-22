#Step 5 : modeling with  decision tree
#load caret for train-test splitting and model evaluation
library(caret)

#split the data into training and test sets (70% training, 30% test)
set.seed(123)
trainIndex <- createDataPartition(balanced_data$No.show, p = 0.7, list = FALSE)
train_data <- balanced_data[trainIndex, ]
test_data <- balanced_data[-trainIndex, ]
install.packages("rpart")

library(rpart)

# Fit the decision tree model
tree_model <- rpart(No.show ~ Gender + Age + Neighbourhood + Scholarship +
                      Hipertension + Diabetes + Alcoholism + Handcap + 
                      SMS_received + period, 
                    data = balanced_data, 
                    method = "class", 
                    control = rpart.control(cp = 0.005))  # After several tests i concluded that 0.5 was the best parametre
pred_class_tree <- predict(tree_model, test_data, type = "class")
library(caret)  # For confusion matrix
confusionMatrix(pred_class_tree, test_data$No.show)
library(rpart.plot)
rpart.plot(tree_model)






