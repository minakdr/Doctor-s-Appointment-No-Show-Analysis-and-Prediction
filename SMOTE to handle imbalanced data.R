library(dplyr)

data<-read.csv("D:\\Projects\\KaggleV2-May-2016.csv")
head(data)



#We remove useless columns
data <- subset(data , select = -PatientId)
data <- subset( data , select = -AppointmentID)

#Numerisation of columns 

data$Gender<- as.numeric(factor(data$Gender)) #F=1 , M=2
data$ScheduledDay <- as.Date(factor(data$ScheduledDay))
data$AppointmentDay <- as.Date(factor(data$AppointmentDay))
data$No.show<-as.numeric(factor(data$No.show)) #No=1 , Yes=2


# I will create a mapping of the original Neighbourhood names to their numeric values so I need the factors
neighbourhood_factor <- factor(data$Neighbourhood)
data$Neighbourhood <- as.numeric(neighbourhood_factor)
neighbourhood_mapping <- data.frame(
  Original_Neighbourhood = levels(neighbourhood_factor),
  Numeric_Value = 1:length(levels(neighbourhood_factor))
)

print(neighbourhood_mapping)



#creating a period column ; maybe there is a corelation between the period and no shows 

Calculate_Period <- function(df , col1 , col2) {
  period <- as.numeric(difftime(df[[col2]], df[[col1]], units = "days"))
  return(period)}
data$period <- Calculate_Period( data ,"ScheduledDay", "AppointmentDay")
data <- subset(data , select = -ScheduledDay)
data <- subset( data , select = -AppointmentDay)
head (data)

numerical_columns <- c("Age", "Scholarship", "Hipertension", "Diabetes", "Alcoholism", "Handcap", "SMS_received","period","No.show")
correlation_matrix1 <- cor(data[, numerical_columns], use = "complete.obs", method = "pearson")  
print(correlation_matrix1)



library(reshape2)
library(ggplot2)

#melt the correlation matrix for ggplot ( make it long format which is more suiable for plotting)
melted_corr <- melt(correlation_matrix1)

# Plot the heatmap
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()




#Before starting I want to know How many No values we got as the outcome
d=sum(data$No.show=="2")  # So 22319 peson didn't show up 
# the data is imbalanced , therefor we need to use some resampling  techniques to deal wiht this  
# We will use SMOTE

library(smotefamily)

data$No.show <- as.factor(data$No.show)

smote_result <- SMOTE(data[, -which(names(data) == "No.show")], 
                      data$No.show, 
                      K = 5, 
                      dup_size = 44500) 

# we will combine the results into a balanced dataset
balanced_data <- smote_result$data
balanced_data$No.show <- smote_result$class

table(balanced_data$No.show)

write.csv(balanced_data, 'SmoteBalencedData.csv', row.names = FALSE)
dd=sum(balanced_data$class=="2")  # So 66957 peson didn't show up over 1551656 which is way more balanced


library(reshape2)
library(ggplot2)

balanced_data$class <- as.numeric(balanced_data$class)

colnames(balanced_data)[colnames(balanced_data) == "class"] <- "No.show"

correlation_matrix2 <- cor(balanced_data[sapply(balanced_data, is.numeric)])


#melt the correlation matrix for ggplot ( make it long format which is more suiable for plotting)
melted_corr <- melt(correlation_matrix)

#Plot the heatmap
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()




















