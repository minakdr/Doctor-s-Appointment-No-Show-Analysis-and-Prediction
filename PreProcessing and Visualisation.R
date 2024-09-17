library(dplyr)
library(ggplot2)


data<-read.csv("D:\\Projects\\Appintmens no show\\KaggleV2-May-2016.csv")
head(data)

#Step 1 : Pre processing and data cleaning
#We remove useless columns
data <- subset(data , select = -PatientId)
data <- subset( data , select = -AppointmentID)


#Ceck for duplicate rows
duplicate_rows <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]

#View duplicated rows
duplicate_rows

#remove duplicated rows
data <- data[!duplicated(data), ]

#We check the percentage of missing values per column
colMeans(is.na(data)) * 100  

#Encoding

#Convert "No" to 0 and "Yes" to 1 as integers
data$No.show <- as.integer(ifelse(data$No.show == "Yes", 1, ifelse(data$No.show == "No", 0, data$No.show)))

#Convert "M" to 0 and "F" to 1 as integers
data$Gender <- as.integer(ifelse(data$Gender == "F", 1, ifelse(data$Gender == "M", 0, data$Gender)))

#Numerisation of columns 

data$ScheduledDay <- as.Date(factor(data$ScheduledDay))
data$AppointmentDay <- as.Date(factor(data$AppointmentDay))



#Create a mapping of the original Neighbourhood names to their numeric values so I use the factors
neighbourhood_factor <- factor(data$Neighbourhood)
data$Neighbourhood <- as.numeric(neighbourhood_factor)
neighbourhood_mapping <- data.frame(
  Original_Neighbourhood = levels(neighbourhood_factor),
  Numeric_Value = 1:length(levels(neighbourhood_factor))
)

print(neighbourhood_mapping)



#Creating a period column ; maybe there is a correlation between the period and no shows 

Calculate_Period <- function(data , col1 , col2) {
  period <- as.numeric(difftime(data[[col2]], data[[col1]], units = "days"))
  return(period)}
data$period <- Calculate_Period( data ,"ScheduledDay", "AppointmentDay")
#Removing scheduleday and appointmentday
data <- subset(data , select = -ScheduledDay)
data <- subset( data , select = -AppointmentDay)



#Calculating the percentage of values in binay columns to check for anomalies 
percentage <- prop.table(table(data$Gender)) * 100
print(percentage)
percentage <- prop.table(table(data$Scholarship)) * 100
print(percentage)
percentage <- prop.table(table(data$Hipertension)) * 100
print(percentage)
percentage <- prop.table(table(data$Diabetes)) * 100
print(percentage)
percentage <- prop.table(table(data$Alcoholism)) * 100
print(percentage)
percentage <- prop.table(table(data$Handcap)) * 100
print(percentage)
percentage <- prop.table(table(data$SMS_received)) * 100
print(percentage)
percentage <- prop.table(table(data$No.show)) * 100
print(percentage)

#Handcap has a weird codification , we will turn 2 3 and 4 into 1
data$Handcap <- ifelse(data$Handcap %in% c(2, 3, 4), 1, data$Handcap)

#Verify the changes
percentage <- prop.table(table(data$Handcap)) * 100
print(percentage)

#We remove weird ages 
data <- subset(data, age >= 0 & age <= 160)


#Part 2 Analysis and visualization

#Correlation matrix 
correlation_matrix1 <- cor(data, use = "complete.obs")  
print(correlation_matrix1)
#SMS_received and No.show have a moderate positive correlation (0.128), indicating that seding an SMS doesnt improve the chances of showing up
#Period and SMS_received show a significant correlation (0.401), possibly implying that the longer the period, the more likely an SMS was received.
#Age and Hipertension have a strong positive correlation (0.504), suggesting older individuals tend to have more hypertension.

library(reshape2)
library(ggplot2)

#melt the correlation matrix for ggplot ( make it long format which is more suiable for plotting)
melted_corr <- melt(correlation_matrix1)

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

#Now I want to check the kind of the people who didn't show up 

no_show_df <- data[data$`No.show` == 1, ]


no_show_dfScholarship <- factor(no_show_df$Scholarship)
no_show_df$Diabetes <- factor(no_show_df$Diabetes)
no_show_df$SMS_received <- factor(no_show_df$SMS_received)
no_show_df$Alcoholism <- factor(no_show_df$Alcoholism)
no_show_df$Gender <- factor(no_show_df$Gender)
no_show_df$Handcap <- factor(no_show_df$Handcap)
no_show_df$Hipertension <- factor(no_show_df$Hipertension)
no_show_df$Neighbourhood <- factor(no_show_df$Neighbourhood)


# Gender
ggplot(no_show_df , aes(x = Gender, fill = Gender)) + 
  geom_bar() + 
  scale_fill_manual(values = c("lightblue", "pink")) +  # Custom colors
  labs(title = "Gender Distribution", x = "Gender", y = "Count") + 
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Shcolarship
ggplot(no_show_df , aes(x = Scholarship, fill = Scholarship)) + 
  geom_bar() + 
  scale_fill_manual(values = c("darkblue", "violet")) +  # Custom colors
  labs(title = "Scholarship Distribution", x = "Scholarship", y = "Count") + 
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#SMS_received
ggplot(no_show_df, aes(x = factor(SMS_received), fill = factor(SMS_received))) + 
  geom_bar() + 
  scale_fill_manual(values = c("lightgreen", "cyan")) +  # Custom colors
  labs(title = "SMS Received Distribution", x = "SMS Received", y = "Count") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


# Age
ggplot(no_show_df, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.8) + 
  labs(title = "Age Distribution", x = "Age", y = "Frequency") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Period
ggplot(no_show_df, aes(x = period)) + 
  geom_histogram(binwidth = 10, fill = "lightcoral", color = "black", alpha = 0.8) + 
  labs(title = "Period Distribution", x = "Period (days)", y = "Frequency") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
# Alcoholism
ggplot(no_show_df, aes(x = factor(Alcoholism), fill = factor(Alcoholism))) + 
  geom_bar() + 
  scale_fill_manual(values = c("lightblue", "orange")) +  # Custom colors for Alcoholism
  labs(title = "Alcoholism Distribution", x = "Alcoholism", y = "Count") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


# Neighborhood
ggplot(no_show_df, aes(x = factor(Neighbourhood), fill = factor(Neighbourhood))) + 
  geom_bar() + 
  scale_fill_manual(values = rainbow(length(unique(no_show_df$Neighbourhood)))) +  # Custom colors for Neighborhood
  labs(title = "Neighbourhood Distribution", x = "Neighborhood", y = "Count") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust x-axis text for better readability
  )

# Diabetes
ggplot(no_show_df, aes(x = factor(Diabetes), fill = factor(Diabetes))) + 
  geom_bar() + 
  scale_fill_manual(values = c("lightcoral", "lightgoldenrod")) +  # Custom colors for Diabetes
  labs(title = "Diabetes Distribution", x = "Diabetes", y = "Count") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Hypertension
ggplot(no_show_df, aes(x = factor(Hipertension), fill = factor(Hipertension))) + 
  geom_bar() + 
  scale_fill_manual(values = c("lightpink", "lightgreen")) +  # Custom colors for Hypertension
  labs(title = "Hypertension Distribution", x = "Hipertension", y = "Count") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
