
#Before starting I want to know How many No values we got as the outcome
no=sum(data$No.show=="0")# So 87794 person showed up 
yes=sum(data$No.show=="1")#and 22098 did 
# the data is imbalanced , therefor we need to use some resampling  techniques to deal wiht this  
# We will use SMOTE

library(smotefamily)

data$No.show <- as.factor(data$No.show)

smote_result <- SMOTE(data[, -which(names(data) == "No.show")], 
                      data$No.show, 
                      K = 5, 
                      dup_size = 0) 

# we will combine the results into a balanced dataset
balanced_data <- smote_result$data

yesb=sum(balanced_data$class=="1")# the number of no shows increased and now is 66294 
table(balanced_data$No.show)

#66294 peson didn't show up over 154087 which is way more balanced


write.csv(balanced_data, 'SmoteBalencedData.csv', row.names = FALSE)
library(reshape2)
library(ggplot2)

balanced_data$class <- as.numeric(balanced_data$class)

colnames(balanced_data)[colnames(balanced_data) == "class"] <- "No.show"

correlation_matrix <- cor(balanced_data[sapply(balanced_data, is.numeric)])


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

















