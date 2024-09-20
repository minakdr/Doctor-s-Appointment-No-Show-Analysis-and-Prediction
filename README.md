**Doctor Appointment No-Show Prediction**
This project aims to predict whether patients will show up for their scheduled doctor appointments based on various factors. The dataset contains information on over 110,000 appointments, including both patients who attended (78%) and those who did not (22%). The primary goal of this project is to build predictive models that can help clinics reduce the number of missed appointments.

**Project Objectives**
Perform exploratory data analysis (EDA) to understand the underlying patterns.
Build and evaluate machine learning models to predict no-shows.
Compare the performance of multiple models, including Logistic Regression and Decision Trees.

**Data Preprocessing:**

Handle missing values, outliers, and data imbalances.
Feature engineering to create meaningful predictors.
Exploratory Data Analysis (EDA):

Visualize key relationships between variables like age, appointment date, and appointment status.
Analyze the impact of factors such as gender, waiting time, and SMS reminders on patient attendance.

![neighbourhood](https://github.com/user-attachments/assets/44b955b2-3bf0-431b-a835-5d910cbeb745)
![period](https://github.com/user-attachments/assets/c3f3b524-e43d-4e60-8230-03bcb4ad04f1)
![Rplot](https://github.com/user-attachments/assets/b29eca6e-8ef5-43cb-b5d2-56a2fab2e322)
![Rplot01](https://github.com/user-attachments/assets/f6c7d041-26be-427a-96aa-dc54530f02a9)


Model Building:

Train a Logistic Regression model and evaluate its performance.
Build a Decision Tree model for better accuracy and interpretability.
Evaluation:


Confusion Matrix, Accuracy, Sensitivity, Specificity, and Kappa scores used to compare models.
Decision Tree outperformed Logistic Regression with an accuracy of 71.98% and a Kappa score of 0.4278, showing better ability to identify no-shows.
Technologies Used
Programming Languages: R
Libraries: caret, ggplot2, rpart, e1071
Machine Learning Models: Logistic Regression, Decision Tree
