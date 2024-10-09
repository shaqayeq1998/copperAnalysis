# Load necessary libraries
library(dplyr)         # For data manipulation
library(car)           # For regression analysis
library(psych)         # For correlation analysis
library(readxl)
library(reshape2)
library(readr)
library(ggplot2)
library(scales)
library(Hmisc)
library(tidyverse)
library(mgcv)

install.packages(c('readr', 'ggplot2', 'tidyr'))

# Load the data from CSV file

data <- read.csv("RGEO6.csv")


# View the first few rows of the dataset
head(data)

# Summary statistics for all columns
summary(data)

# Correlation matrix using Pearson's method, Select relevant columns (ignoring 'plant')
data_clean <- data %>%
  select(Cusol, Cuexch, Cuacid, Cutot, basR)

# Remove rows with missing values
data_clean <- na.omit(data_clean)

# Calculate correlation matrix
correlation_matrix <- cor(data_clean)

# Display the correlation matrix
print(correlation_matrix)

# Check the individual correlations with basR
cor(data_clean$Cusol, data_clean$basR)
cor(data_clean$Cuexch, data_clean$basR)
cor(data_clean$Cuacid, data_clean$basR)
cor(data_clean$Cutot, data_clean$basR)

# Calculate descriptive statistics (mean, SD, etc.)
describe(data[, -which(names(data) == "plant")])


######################################

#Visualize Correlation with a Heatmap


# Melt the correlation matrix, Reshape the correlation matrix for ggplot2
correlation_melt <- melt(correlation_matrix)

# Create a heatmap
ggplot(data = correlation_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  ggtitle("Heatmap of Copper Fractions and Basal Soil Respiration")

#####################################
#Simple Linear Regression

# Step 2: Check the structure and column names
print("Initial Structure of Data:")
str(data)
print("Column Names:")
print(colnames(data))

# Step 3: Clean column names by removing any potential white spaces and converting to lowercase
colnames(data) <- tolower(trimws(colnames(data)))

# Verify column names after cleaning
print("Cleaned Column Names:")
print(colnames(data))

# Step 4: Check for any NA values in the dataset
na_count <- colSums(is.na(data))
print("NA Values Count in Each Column:")
print(na_count)

# Step 5: Remove rows with any missing values
data_clean <- na.omit(data)

# Step 6: Verify data types and convert if necessary
print("Data Types After Cleaning:")
print(sapply(data_clean, class))

# Convert columns to numeric if they are not already (for example, 'cusol')
data_clean$cusol <- as.numeric(data_clean$cusol)
data_clean$cuexch <- as.numeric(data_clean$cuexch)
data_clean$cuacid <- as.numeric(data_clean$cuacid)
data_clean$cutot <- as.numeric(data_clean$cutot)
data_clean$basr <- as.numeric(data_clean$basr)

# Step 7: List of copper fractions
cu_fractions <- c("cusol", "cuexch", "cuacid", "cutot")

# Simple linear regression for each fraction vs basR
for (fraction in cu_fractions) {
  model <- lm(as.formula(paste("basr ~", fraction)), data = data_clean)
  cat("Regression for:", fraction, "\n")
  print(summary(model))
  cat("\n")
}


# Scatter plot with regression line for cusol
ggplot(data_clean, aes(x = cusol, y = basr)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("basR vs Cusol") +
  xlab("Cusol") + ylab("basR")

# Scatter plot with regression line for cutot
ggplot(data_clean, aes(x = cutot, y = basr)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("basR vs Cutot") +
  xlab("Cutot") + ylab("basR")
###################################################

# Step 8: Multiple linear regression with all Cu fractions
multi_model <- lm(basr ~ cusol + cuexch + cuacid + cutot, data = data_clean)
cat("Multiple Linear Regression Summary:\n")
print(summary(multi_model))


# Diagnostic plots for multiple regression
par(mfrow = c(2, 2))
plot(multi_model)

####################################

#Check for Multicollinearity
# Load necessary library

# Fit the multiple linear regression model
multi_model <- lm(basr ~ cusol + cuexch + cuacid + cutot, data = data_clean)

# Calculate VIF for each predictor# residaul vs leverage
vif_values <- vif(multi_model)
print(vif_values)

# Check for multicollinearity issues
if(any(vif_values > 5)) {
  cat("Multicollinearity detected: VIF values greater than 5 found.")
} else {
  cat("No significant multicollinearity detected.")
}
#############################################

#R Code for Non-linear Models
# Fit a polynomial regression model (2nd degree)
poly_model <- lm(basr ~ poly(cusol, 2) + poly(cuexch, 2) + poly(cuacid, 2) + poly(cutot, 2), data = data_clean)

# Summary of the polynomial model
summary(poly_model)

# Residual Analysis
# Fit the original multiple linear regression model
multi_model <- lm(basr ~ cusol + cuexch + cuacid + cutot, data = data_clean)

# Residuals vs Fitted plot
plot(multi_model, which = 1, main = "Residuals vs Fitted")

# Q-Q plot for normality of residuals
plot(multi_model, which = 2, main = "Normal Q-Q")

# Histogram of residuals
hist(residuals(multi_model), breaks = 15, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")

# Boxplot of residuals
boxplot(residuals(multi_model), main = "Boxplot of Residuals", ylab = "Residuals")

#######################################################

#Model Improvement: Exploring Transformations and Interactions
#Transformations

# Log-transforming the predictors (make sure to add a small constant to avoid log(0) issues)
data_clean$log_cusol <- log(data_clean$cusol + 1)
data_clean$log_cuexch <- log(data_clean$cuexch + 1)
data_clean$log_cuacid <- log(data_clean$cuacid + 1)
data_clean$log_cutot <- log(data_clean$cutot + 1)

# Fit the model with transformed predictors
log_model <- lm(basr ~ log_cusol + log_cuexch + log_cuacid + log_cutot, data = data_clean)

# Summary of the transformed model
summary(log_model)

#Interactions
# Fit the model with interaction terms
interaction_model <- lm(basr ~ log_cusol * log_cuexch + log_cuacid * log_cutot, data = data_clean)

# Summary of the interaction model
summary(interaction_model)

#Generalized Additive Models (GAMs)

# Fit a GAM
gam_model <- gam(basr ~ s(log_cusol) + s(log_cuexch) + s(log_cuacid) + s(log_cutot), data = data_clean)

# Summary of the GAM model
summary(gam_model)

# Plotting the GAM
plot(gam_model, pages = 1, shade = TRUE)

# Plotting the GAM smooth terms
plot(gam_model, pages = 1, shade = TRUE)

# Residuals vs Fitted plot for GAM
par(mfrow = c(2, 2))
plot(gam_model, residuals = TRUE, main = "GAM Residuals vs Fitted")
























