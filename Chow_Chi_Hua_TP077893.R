##### TP077893 Chow Chi Hua #####

### Objective:  To determine how a customer's credit amount affects their credit class

#####################################################################################################

# Read csv and store in 'result'
result <- read.csv("D:\\cleaned_bank_set.csv")

# Change character to factor
for (name in names(result)) {
  if (is.character(result[[name]])) {
    result[[name]] <- as.factor(result[[name]])
  }
} 

# Check dataset and datatype
View(result)
str(result)

#####################################################################################################

# Question 1: How is the credit amount distributed across the good and bad credit classes?

# Analysis 1.1: Visualize the distribution of credit amount by class using Boxplot with mean.

ggplot(result, aes(x = class, y = credit_amount, fill = class)) +       # Take x-axis and fill = class, y-axis = credit_amount
  geom_boxplot() +                                                      # Construct Boxplot
  stat_summary(fun = mean, geom = "point", shape = 4, 
               size = 3, color = "blue", stroke = 1.5) +                # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 0)), 
               hjust = -0.3, color = "blue", size = 3.5) +              # Display mean value
  labs(title = "Credit Amount Distribution by Credit Class", 
       x = "Credit Class", y = "Credit Amount") +                       # Add labels
  theme(plot.title = element_text(hjust=0.5, size = 14, face = "bold")) # Adjust the fonts

# Analysis 1.2: Visualize the distribution of credit amount by class using Density plot. (Extra Feature)

ggplot(result, aes(x = credit_amount, fill = class)) +                  # Take x-axis = credit_amount, fill = class
  geom_density(alpha = 0.5) +                                           # Construct Density plot
  labs(title = "Density of Credit Amount by Credit Class", 
       x = "Credit Amount", y = "Density") +                            # Add labels
  theme(plot.title = element_text(hjust=0.5, size = 14, face = "bold")) # Adjust the fonts

#####################################################################################################

# Question 2: What is the relationship between credit amount and credit class?

# Analysis 2.1: Hypothesis testing using t-test. (Extra Feature)

# Split data into two groups based on credit class
good_credit <- result$credit_amount[result$class == "good"]
bad_credit <- result$credit_amount[result$class == "bad"]

# Perform t-test
t_test <- t.test(bad_credit, good_credit, alternative = "two.sided",    # Check difference in means in either direction
                 var.equal = FALSE)                                     # Assume variances not equal (Welch's t-test)

# Display results
print(t_test)

#####################################################################################################

# Question 3: Why do customers with low credit amounts still fall into the bad credit class? 
#             How do variables such as credit amount, loan duration, age, and job type influence the classification?

# Analysis 3.1: Find coefficient and p-value of credit amounts and other variables using logistic regression. (Extra Feature)

# Display numbers in fixed notation
options(scipen = 999) 

# Build Logistic model - dependent = class, independent = credit_amount, duration, age, 
logistic_model <- glm(class ~ credit_amount + duration + age + job, data = result,  
                      family = "binomial")                                           # Specify dependent is binary

# Show Summary of the model
summary(logistic_model) 

#####################################################################################################

# Question 4: What is the relationship between checking status, credit history, employment, housing, other parties,
#             and purpose with the likelihood of being classified as good class?

# Analysis 4.1: Assess the relationship between selected features and credit class using Chi-Square Test and Heatmap. (Extra Feature)

library(reshape2)

# Select specific variables from result dataset
selected_vars <- result[, c("checking_status", "purpose", "other_parties", 
                            "housing", "employment", "credit_history")]

# Perform Chi-Square Test
chi_square_values <- sapply(selected_vars, function(var) {   # Apply Chi-Square Test to each variables using function
  test <- chisq.test(table(var, result$class))               # Create table for each variable against class to do Chi-Square Test
  test$statistic                                             # Extract Chi-Square Test Statistic
})

# Create Data Frame for Heatmap
heatmap_data <- data.frame(Variable = names(chi_square_values), ChiSquare = chi_square_values)

# Create Heatmap
ggplot(heatmap_data, aes(x = Variable, y = "Credit Class", fill = ChiSquare)) + # Take x-axis = Variables, y-axis = credit class, fill = ChiSquare
  geom_tile() +                                                                 # Construct Heatmap
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",               # Set low Chi-Square = blue, high Chi-Square = red, midpoint = white
                       midpoint = median(chi_square_values),                    # Set midpoint = median Chi_Square
                       name = "Chi-Square") +                                   
  labs(title = "Chi-Square Test: Variables vs Credit Class", x = "Variables", y = "") + # Add labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))                              # Adjust labels

#####################################################################################################

# Question 5: Can the loan duration improve the predictive power of credit amount for credit class classification?

# Analysis 5.1: Evaluate the predictive performance of credit amount and load duration using Random Forest. (Extra Feature)

library(randomForest)
library(caTools)
library(caret)

# Split the data into training and testing sets
set.seed(110)
split = sample.split(result$class, SplitRatio = 0.8)   # split the data
training_set = subset(result, split == T)              # 80% for training set
test_set = subset(result, split == F)                  # 20% for testing set


# Build Random Forest models
rf_model_1 = randomForest(class ~ credit_amount,                # Model 1 - predict class based on credit amount
                          data = training_set, ntree = 500)  # specify 500 decision trees
rf_model_2 = randomForest(class ~ credit_amount + duration,     # Model 2 - predict class based on credit amount and loan
                          data = training_set, ntree = 500)     # specify 500 decision trees

# Predictions on testing set
prediction_model_1 = predict(rf_model_1, newdata = test_set)    # using Model 1 to do prediction
prediction_model_2 = predict(rf_model_2, newdata = test_set)    # using Model 2 to do prediction

# Confusion Matrix - compare predicted values with actual values from test set.
confusion_matrix_model_1 = confusionMatrix(table(prediction_model_1,test_set$class)) # Model 1 Confusion Matrix
confusion_matrix_model_2 = confusionMatrix(table(prediction_model_2,test_set$class)) # Model 2 Confusion Matrix

# Display Confusion Matrix for both models
print(confusion_matrix_model_1)
print(confusion_matrix_model_2)

# Variable Importance Plot
varImpPlot(rf_model_2)

# Analysis 5.2: Evaluate the predictive performance using ROC curve and AUC value. (Extra Feature)

library(pROC) 

# Predict probabilities for "good" class
prob_model_1 <- predict(rf_model_1, newdata = test_set, type = "prob")[, 2] 
prob_model_2 <- predict(rf_model_2, newdata = test_set, type = "prob")[, 2]  

# Generate ROC curves
roc_model_1 <- roc(test_set$class, prob_model_1, levels = c("bad", "good"))    # Specify levels for positive class
roc_model_2 <- roc(test_set$class, prob_model_2, levels = c("bad", "good"))

# Plot ROC curves
plot(roc_model_1, col = "blue", main = "ROC Curve Comparison: Random Forest Models")  # Blue - Model 1
plot(roc_model_2, col = "red", add = TRUE)                                            # Red  - Model 2
legend("bottomright",                                                                 # Add legend and adjust position
       legend = c("Model 1: Credit Amount", "Model 2: Credit Amount + Duration"),
       col = c("blue", "red"), lwd = 2, 
       cex = 0.8)

# Calculate AUC value
auc_model_1 <- auc(roc_model_1)
auc_model_2 <- auc(roc_model_2)

# Display AUC
print(auc_model_1)  
print(auc_model_2)  

