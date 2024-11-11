# 1. Load the mtcars dataset into R and examine the first few rows using the head() function. Using
# comments in your code
data(mtcars)
head(mtcars)
# The row names represents the car models.The columns show different aspects of the models of cars such as miles per gallon,
# cyl,disp,hp,weight, gear etc


# 2. Summarize the mtcars dataset using the summary() function. Using comments in your code,
# describe the characteristics of the data set and anything that would be of potential concern
summary(mtcars)
# mpg ranges from 10.40 to 33.90 with mean being 20.09
#mean no. of cylinders is 6.188
#Displacement ranges from 71.1 to 472 cubic inches, with a mean of 230.7
#hp ranges from 52 to 335, with a mean of 146.7.
#wt represents weignt of the car and it ranges from 1.513 to 5.434
#gear ranges from 3.000 ti 5.000 with mean being 3.688
#Potential concern is that the value of some of the variable such as hp, wt etc vary greatly, which may affect during analysis


# 3. Create a scatter plot of mpg (miles per gallon) against wt (weight) using ggplot2. Using
# comments in your code, interpret the results of the plot
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +   
  labs(title = "Scatter plot of MPG vs Weight",
       x = "Weight",
       y = "MPG") +   
  theme_minimal()
#Interpretation
#The scatter plot shows that the relationship between weight and MPG is inversely proportional.
#We can see that as the weight increases the value of miles per gallon decreases which signifies the decrease in fuel efficiency.



# 4. Fit a simple linear regression model to predict mpg based on wt.
reg_model <- lm(mpg ~ wt, data = mtcars)


# 5. Using the summary function for the model, interpret the coefficients of the simple linear
# regression model in Problem 4 as comments in your code
summary(reg_model)
# Interpretation
# Intercept: This represents the estimated mpg when wt is 0.
#For example, if the intercept is 37.285, it means that a car with 0 weight would get 37.285 mpg which is not possible.
# wt (Weight): This is the slope of the regression line that represents the change in mpg for each 1-unit increase in weight.
# For example, if the coefficient for wt is -5.344, it means that for each additional 1000 lbs, 
# the mpg decreases by approximately 5.344.
#  R-squared: R-squared of 0.74 means that 74% of the variability in mpg can be predicted by weight (wt).
#  P-values:The p-value for the coefficient of wt tests the null hypothesis that the coefficient is equal to 0.
# If the p-value is less than 0.05 (a common significance level), it suggests that weight has a statistically significant
# ffect on mpg.
# For example, a p-value of <0.001 would indicate strong evidence that wt significantly affects mpg.
#    
 




# 6. Create a scatter plot of mpg against both wt (weight) and hp (horsepower) using ggplot2 and
# interpret the plot results as comments in your code.
library(ggplot2)

# Create a scatter plot of mpg vs wt, colored by hp
ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
  geom_point(size = 3) +    # Plot points with size 3, color indicating horsepower (hp)
  scale_color_gradient(low = "blue", high = "red") +  # Gradient from blue (low hp) to red (high hp)
  labs(title = "Scatter plot of MPG vs Weight and Horsepower",
       x = "Weight (1000 lbs)",
       y = "Miles per Gallon (MPG)",
       color = "Horsepower (HP)") +   # Label the color scale
  theme_minimal() 

# Interpretation
# The scatter plot illustrates the relationship between a car's weight and its fuel efficiency. Heavier cars generally have lower fuel efficiency, as indicated by the downward trend. Cars with higher horsepower tend to be heavier and less fuel-efficient. However, there are exceptions to this trend, suggesting that some cars may have features or designs that improve efficiency despite their weight



# 7. Fit a multiple linear regression model to predict mpg based on wt and hp.
multiple_model <- lm(mpg ~ wt + hp, data = mtcars)
summary(multiple_model)


# 8. Compare the R-squared values of the simple and multiple linear regression models. Using
# comments in your code, interpret the comparisons
r_squared_simple <- summary(reg_model)$r.squared
r_squared_multiple <- summary(multiple_model)$r.squared
print(paste("R-squared for simple linear regression model:", r_squared_simple))
print(paste("R-squared for multiple linear regression model:", r_squared_multiple))
#The R-squared value measures how well a linear regression model fits the data. It represents the proportion of the variation in the dependent variable that can be explained by the independent variable(s).
# The R-squared of 0.75 indicates that 75% of the variability in the dependent variable is explained by the independent variable in the model.
#The R-squared value for a multiple linear regression model shows how well the model can explain the variation in the dependent variable using multiple independent variables
#The increase in R-squared value from the simple to the multiple regression model indicates that horsepower is a significant predictor of fuel efficiency, and including it in the model improves its ability to predict fuel efficiency.


# 9. Create a residual plot to assess the fit of the multiple linear regression model. Using comments
# in your code, interpret the results.
multiple_model


# Extract residuals and fitted values from the model
residuals <- multiple_model$residuals
fitted_values <- multiple_model$fitted.values

# Create a residual plot using ggplot2
ggplot(data = mtcars, aes(x = fitted_values, y = residuals)) +
  geom_point(color = "blue", size = 3) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at zero
  labs(title = "Residual Plot for Multiple Linear Regression Model",
       x = "Predicted MPG",
       y = "Residuals") +
  theme_minimal()

# 10. Use the predict() function to predict the mpg for a new car with wt = 2.5 and hp = 100.
multiple_model

# Create a new data frame with the new car's weight and horsepower
new_car <- data.frame(wt = 2.5, hp = 100)

# Use the predict function to predict mpg for the new car
predicted_mpg <- predict(multiple_model, newdata = new_car)

print(paste("Predicted MPG for a car with wt = 2.5 and hp = 100:", predicted_mpg))


# 11. Using ggplot2, plot the predicted mpg for a new car with wt = 2.5 and hp = 100

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3, color = "blue") +  # Original data points
  geom_point(aes(x = new_car$wt, y = predicted_mpg), 
             size = 4, color = "green") +  # New car's predicted mpg
  labs(title = "Predicted MPG for New Car",
       x = "Weight (1000 lbs)",
       y = "Miles per Gallon (MPG)") +
  theme_minimal() 
  