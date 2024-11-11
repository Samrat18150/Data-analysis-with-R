# Using the USArrests data set in the "datasets" package, create a function
# which takes as input the name of a US state and returns the following text
# (Read Chapter 9 - If and IfElse) :
#   a. If the Murder rate is higher than the median state murder rate, print “[state
#                                                                              name] has a higher-than-average murder rate”.
# b. If its murder rate is not higher than the median rate, print “[state name] has
# a lower-than-average murder rate.”



# Load the necessary dataset
library(datasets)
data("USArrests")

# Calculate the median murder rate
median_rate <- median(USArrests$Murder)

check_murder_rate <- function(state_name) {
  
  # Get the murder rate for the specified state
  state_murder_rate <- USArrests[state_name, "Murder"]
  
  # Compare the state's murder rate with the median murder rate
  if (state_murder_rate > median_rate) {
    print(paste(state_name, "has a higher-than-average murder rate"))
  } else {
    print(paste(state_name, "has a lower-than-average murder rate"))
  }
}
check_murder_rate("Missouri")
check_murder_rate("New Hampshire")



#2. Create a control loop that loops through the integers from 1 to 100. If the
# integer is divisible by 7, print “[number] is divisible by 7!”. If it is not, do not
# print anything.

for(i in 1:100){
  if(i%%7==0)
    print(paste(i, "is divisible by 7!"))
  else
    next #next helps to skip past the numbers that is nt divisible by 7
}


#3. Use the aggregate function applied to the "mpg" dataset in the ggplot2
# package to determine which manufacturer had the highest average city gas
# mileage over the sample. Create a graph using ggplot2 to illustrate your
# findings

library(ggplot2)
data("mpg")

# Calculates the average city mileage for each manufacturer
average_city_mileage <- aggregate(mpg$cty, by = list(Manufacturer = mpg$manufacturer), FUN = mean)

colnames(average_city_mileage)[2] <- "AverageCityMileage"

# Finds the manufacturer with the highest average city mileage
highest_avg_mileage <- average_city_mileage[which.max(average_city_mileage$AverageCityMileage), ]
print(highest_avg_mileage)

# Creates a bar graph to illustrate the average city mileage for each manufacturer
ggplot(average_city_mileage, aes(x = reorder(Manufacturer, -AverageCityMileage), y = AverageCityMileage)) +
  geom_bar(stat = "identity") +
  labs(title = "Average City Mileage by Manufacturer",
       x = "Manufacturer",
       y = "Average City Mileage (mpg)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#4. Using the "mpg" data set and the aggregation function to calculate average
# highway and city miles per gallon by manufacturer and class of car. Be sure
# to use the cbind command to ensure output is in a readable format.
library(ggplot2)
data("mpg")

# Calculates the average city mileage by manufacturer and class
average_cty <- aggregate(mpg$cty, by = list(Manufacturer = mpg$manufacturer, Class = mpg$class), FUN = mean)

# Calculates the average highway mileage by manufacturer and class
average_hwy <- aggregate(mpg$hwy, by = list(Manufacturer = mpg$manufacturer, Class = mpg$class), FUN = mean)

# Renames columns for clarity
colnames(average_cty)[3] <- "AverageCityMileage"
colnames(average_hwy)[3] <- "AverageHighwayMileage"

# Combines the two data frames using cbind
combined_averages <- cbind(average_cty, AverageHighwayMileage = average_hwy$AverageHighwayMileage)

# Displays the result
print(combined_averages)



#5. Load the "anscombe" dataset from the datasets package. Using the dplyr
# package, use the "starts_with" option in the select function to generate a
# subset of the "anscombe" dataset that contains columns that start with “x”.
# (Chapter 12.3)
library(dplyr)
data("anscombe")
# Generates a subset of the anscombe dataset with columns that start with "x"
anscombe_subset <- anscombe %>% select(starts_with("x"))

print(anscombe_subset)


# 6. Load the "midwest" dataset from the ggplot2 package. Use the filter
# command to select observations in Illinois from metro areas. What is the
# average percentage of college educated people for Illinois counties?



library(ggplot2)
library(dplyr)

data("midwest")

# Filters for Illinois counties in metro areas
illinois_metro <- midwest %>%
  filter(state == "IL" & inmetro == 1)

# Calculates the average percentage of college-educated people
average_college_educated <- mean(illinois_metro$percollege, na.rm = TRUE)
print(average_college_educated)