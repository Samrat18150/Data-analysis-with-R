install.packages("MASS")
library(MASS)
data("Boston")
library(ggplot2)
install.packages("GGally")
library(GGally)
install.packages("car")
library(ggplot2)


# 1. First, summarize the dataset, use ggpairs, and other visualization tools to familiarize
# yourself with the data. Create plots to explore the dataset and explain your findings

summary(Boston)
ggpairs(Boston)

# Scatter plot of rooms vs median value
ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Number of Rooms vs Median Value of Homes", x = "Average Number of Rooms", y = "Median Value of Homes ($1000)")
# Rooms vs House Prices: There is usually a positive relationship between the number of rooms in a house and its price, indicating that larger homes tend to be more expensive.



#2. Create a new variable called age.50. If the age is greater than 50, the value should be true.
# If it is not, then false.
Boston$age.50 <- Boston$age > 50
head(Boston[, c("age", "age.50")])


# 3. Next logistically regress age.50 against all of the predictors other than age in the dataset.
model <- glm(age.50 ~ . - age, data = Boston, family = binomial)
summary(model)


# 4. Interpret each coefficient in your own words as comments in the script you create
# In logistic regression, the intercept represents the log-odds of the outcome (age > 50) when all 
# the predictor variables are equal to zero. However, this value doesn't hold much practical significance unless it makes sense for all the predictors to be zero, which is often not the case in real-world scenarios.
# Thus, the intercept usually has limited interpretive value outside of the context of the model.
# Crim (per capita crime rate by town): A positive coefficient would suggest that an increase in the crime rate increases the odds that a house is more than 50 years old.Zn (proportion of residential land zoned for large lots): If this coefficient is positive, it indicates that higher zoning for large lots is associated with older homes. A negative coefficient would suggest newer homes in areas with more zoning for larger lots.
# Nox (nitrogen oxide concentration): A positive coefficient indicates that higher nitrogen oxide concentrations (often associated with urban or industrial areas) increase the likelihood of homes being older than 50 years. A negative coefficient would mean the opposite.
# Rm (average number of rooms per dwelling): If the coefficient for rm is positive, homes with more rooms are more likely to be older than 50 years. A negative coefficient would indicate that homes with more rooms are newer.
# Dis (weighted distances to employment centers): A positive coefficient suggests that homes further from employment centers are more likely to be older than 50 years, while a negative coefficient would suggest newer homes tend to be further away.
# Lstat (percentage of lower status of the population): A positive coefficient implies that areas with a higher percentage of lower-status individuals have older homes. If it's negative, newer homes are more prevalent in such areas.
# Medv (median value of owner-occupied homes in $1000s): A negative coefficient would suggest that areas with higher home values are more likely to have newer homes, while a positive coefficient means higher home values are associated with older homes.




# 5.Perform model diagnostics over the model you created in the second step.

# Residuals plot
plot(model, which = 1)

# Plotting the residuals
sample_residuals <- residuals(model, type = "deviance")
plot(sample_residuals, main = "Deviance Residuals", ylab = "Residuals", xlab = "Index")


# Multicollinearity Check:
library(car)

# Calculate VIF for each predictor
vif(model)






# Plot the residuals versus fitted values, the Q-Q plot and a histogram of residuals.
# Comment in your code about what the residuals tell you about the model fit.

# Extract residuals and fitted values
residuals <- residuals(model, type = "deviance")
fitted_values <- fitted(model)

# Plot 1: Residuals vs. Fitted values
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)
# This plot helps check for non-linearity or heteroscedasticity. 
# Residuals vs Fitted Values Plot: If there’s no clear pattern, it suggests the model captures the relationship between predictors and the outcome well. If there’s a pattern, the model might not be adequate.


# Plot 2: Q-Q plot
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)
# Comment: The Q-Q plot checks if the residuals are normally distributed.
# This shows how well the residuals follow a normal distribution. If the points deviate significantly from the line, the residuals are not normally distributed, which could mean the model is not fitting well.

# Plot 3: Histogram of residuals
hist(residuals, breaks = 30, col = "lightgreen", 
     main = "Histogram of Residuals", 
     xlab = "Residuals")
# Comment: This histogram shows the distribution of the residuals.
# This should show a roughly normal distribution of residuals centered around zero. If the histogram is skewed, this could mean the model does not fully explain the variation in the data.




summary(model)


# 7. Identify the statistically significant predictors and create a logistic regression model for
# each of them individually.
# # Extract significant predictors
# predictors <- names(coef(logistic_model))[which(summary(logistic_model)$coefficients[,4] < 0.05)]
# print(predictors)
# # Loop through each significant predictor and fit a logistic regression model
# for (predictor in predictors) {
#   formula <- as.formula(paste("age.50 ~", predictor))
#   
#   # Fit the logistic regression model
#   model <- glm(formula, data = Boston, family = binomial)
#   
#   # Output the summary of the individual model
#   cat("\nLogistic regression for:", predictor, "\n")
#   print(summary(model))
# This code showed error


predictors <- predictors[predictors != "(Intercept)"]

# Loop through each significant predictor and fit a logistic regression model
for (predictor in predictors) {
  formula <- as.formula(paste("age.50 ~", predictor))  # Dynamically create the formula
  
  # Fit the logistic regression model for each predictor
 model <- glm(formula, data = Boston, family = binomial)
  
  cat("\nLogistic regression for predictor:", predictor, "\n")
  print(summary(model))
}




# a. Create a logistic model that uses all of the statistically significant predictors
predictors <- names(coef(logistic_model))[which(summary(logistic_model)$coefficients[,4] < 0.05)]
print(predictors)
# Build logistic regression model with statistically significant predictors
significant_model <- glm(age.50 ~ crim + zn + nox + rm + dis + lstat + medv, 
                         data = Boston, family = binomial)
summary(significant_model)

# b. Calculate the AIC and BIC for every model created in the assignment and
# compare. According to AIC and BIC, which model gives the best fit? Comment
# your answer in the code.



aic_logistic_model <- AIC(model)
bic_logistic_model <- BIC(model)

aic_significant_model <- AIC(significant_model)
bic_significant_model <- BIC(significant_model)

# Print the AIC and BIC for comparison
cat("AIC and BIC for model (all predictors):\n")
cat("AIC:", aic_logistic_model, "\nBIC:", bic_logistic_model, "\n\n")

cat("AIC and BIC for significant_model (significant predictors):\n")
cat("AIC:", aic_significant_model, "\nBIC:", bic_significant_model, "\n\n")

# According to the AIC and BIC values the logistic model named 'model' gives the best fit


