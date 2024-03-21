install.packages("emmeans")
install.packages("ggplot2")
install.packages("psych")
install.packages("dplyr")
install.packages("corrplot")

library(emmeans)
library(ggplot2)
library(psych)
library(dplyr)
library(corrplot)
library(tidyverse)
library(car)
library(statmod)
library(pROC)
# Read CSV file

default <- read.csv('Loan_default.csv')

#Initial EDA 
summary(default)
describe(default)


# Further EDA

# This R-code shows the imbalance between the number of non-default and default labels.
ggplot(default, aes(x = as.factor(Default))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(x = "Default", y = "Count", title = "Distribution of Default Variable") +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "Non-default", "1" = "Default")) # Labeling for clarity

# Correlation Matrix
df_numeric <- default[, sapply(default, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(df_numeric, use = "complete.obs") # 'use' argument handles missing values

corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, 
         # Add color; blue-white-red is common for correlations
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))


# Histogram for the Income variable
ggplot(default, aes(x = Income)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "Interest Rate", y = "Count", title = "Distribution of Income") +
  theme_minimal()

# Removing any outliers
# Identify numerical columns
numerical_cols <- sapply(default, is.numeric)

# Function to find outliers based on IQR
find_outliers <- function(x) {
  iqr <- IQR(x)
  lower <- quantile(x, 0.25) - 1.5 * iqr
  upper <- quantile(x, 0.75) + 1.5 * iqr
  x < lower | x > upper
}

# Applying the function to each numerical column and get a logical matrix of outliers
outliers <- sapply(default[, numerical_cols], find_outliers)

# Determine rows with at least one outlier
rows_with_outliers <- apply(outliers, 1, any)

# Removing rows with outliers
default_clean <- default[!rows_with_outliers, ]


# Building initial model
# One-hot encode the categorical variables using model.matrix
default <- default %>%
  mutate(across(c(Education, EmploymentType, MaritalStatus, HasMortgage, HasDependents, LoanPurpose, HasCoSigner), as.factor)) %>%
  select(-c(Education, EmploymentType, MaritalStatus, HasMortgage, HasDependents, LoanPurpose, HasCoSigner, LoanID)) %>%
  bind_cols(as.data.frame(model.matrix(~ Education + EmploymentType + MaritalStatus + HasMortgage + HasDependents + LoanPurpose + HasCoSigner - 1, data = default)))

# Ensuring 'Default' is a factor, which is required for GLM with binomial family
df$Default <- as.factor(df$Default)

# Building the model

# Building the GLM model with all other variables as predictors
model <- glm(Default ~ ., family = binomial(link = "logit"), data = default)

# View the summary of the model
summary(model)



# Removing variables with high p-value
df_reduced <- default %>%
  select(-LoanTerm, -LoanPurposeEducation, -LoanPurposeOther, -`EducationPhD`)

# building the GLM model with the reduced dataframe
model_reduced <- glm(Default ~ ., family = binomial(link = "logit"), data = df_reduced)

# View the summary of the updated model
summary(model_reduced)


# QQ Plot
# Calculate the standardized Pearson residuals
std_pearson_resid <- rstandard(model_reduced)

# Generate a Q-Q plot for the standardized Pearson residuals
qqnorm(std_pearson_resid)
qqline(std_pearson_resid, col = "red")

# Add title
title("Q-Q Plot of GLM Model Standardized Pearson Residuals")


# Predicted probabilities
predictions <- predict(model_reduced, type = "response")

# Actual responses
actual_responses <- df_reduced$Default

# Calculate the ROC curve
roc_curve <- roc(as.numeric(actual_responses), predictions)

# Plot the ROC curve
plot(roc_curve, main="ROC Curve", col="#1c61b6", lwd=2)
abline(a=0, b=1, lty=2, col="gray")

# Calculate and print the AUC
auc(roc_curve)




