#Name: Samrat Tharu
#Date: 2nd of sept, 2024
#CSDA 5110 Week 2 Homework

#2. Which (if any) of the following variable’s names is/are invalid?
thisone <- 1
THISONE <- 2
#1This <-3 This is invalid
this.one <- 4
This.1 <- 5
ThIS.....ON...E <- 6
#This!On!e <- 7 This is invalid
lkjasdfkjsdf <- 8


#3. 2021 was a good year for the price of Gold – your investment portfolio made $100,800 from investing in gold. Create a variable called gold.in.2021 and assign the value of your investment portfolio to it.
gold.in.2021<- 100800

#4. During the last bank audit, we discovered that there was an error caused by a system calculation error and $5000 per year wasn’t added to your gold investment portfolio for the years 2018, 2019, and 2020. Create a new variable called calcError and assign it the amount miscalculated by the system. Create another variable called corrected.total and add the variables gold.in.2021 and calcError to this new variable. What is the new total amount?
calcError<- 15000
corrected.total<- gold.in.2021+calcError
print(corrected.total)


#5. Create 2 matrices and store them in variables.
#a. Name the row names and column names of both. Do not use the same row or column names for the variables.
#b. Perform matrix multiplication on the two variables.
a<- matrix(1:4,nrow=2)
b<- matrix(5:8,nrow=2)
rownames(a)<- c('c','d')
colnames(a)<- c('e','f')
rownames(b)<- c('g','h')
colnames(b)<- c('i','j')
product<- a*b
rownames(product)<- c('k','l')
colnames(product)<- c('m','n')
print(product)

#6.  Joanne thinks that she completes more tasks when she’s had a cup of coffee than when
# she doesn’t. To test this, she recorded how many tasks she completed over 7 days
# without drinking any coffee, and then did the same over 7 days while drinking coffee.
# Her results are stored in a comma delimited CSV file called Joannes Tasks
# a. Import the comma delimited CSV file into R and assign the data to a variable
# called joannesTasks
# b. Verify the variable holding the data is a data.frame variable
# c. Using the appropriate function, write the code to verify that there are 7 rows
# and 4 columns in the data frame.


joannesTasks <- read_csv("/Users/samratchaudhary/Desktop/Assignment/Joannes Tasks.csv")

is.data.frame(joannesTasks)

dim(joannesTasks)

# 7. Use the appropriate function that will display summary descriptive statistics about each
# column within the data frame.
# a. In your code, create comments about information you deduced from the
# summary descriptive statistics.

summary(joannesTasks)
#the mean for total task is 14.14 and the meadian is 14.00. The mean for task without coffee is higher than task with coffee.


# 8. Using the appropriate syntax, write the code that allows you to access the column that
# contains the Number of Tasks Completed Without Coffee data.
# a. Use the appropriate function to display the mean value of that column.
# b. Use the appropriate function to display the total of the values in the column.

NumberofTasksCompletedWithoutCoffee<- joannesTasks$`Number of Tasks Without Coffee`
print(NumberofTasksCompletedWithoutCoffee)

mean(NumberofTasksCompletedWithoutCoffee)

sum(NumberofTasksCompletedWithoutCoffee)


# 9. Create a histogram of Joanne’s total tasks. Accurately name the x axis and the graph
# title.

hist(joannesTasks$'Total Tasks', main="Total Task Histogram", xlab="Total Tasks")

# 10. Use ggplot2 to make a scatterplot of the relationship between Joanne’s total tasks and
# the day of the week. Total tasks is the dependent variable.
library(ggplot2)
ggplot(joannesTasks, aes(x='Day of the Week', y='Total Tasks')) + geom_point()


# 11. Was Joanne able to complete more tasks when she drank coffee or when she did not
# drink coffee? Write a function for this question that returns the correct answer.
# a. Depending on your analysis findings, the function should print one of the options
# below. Use the round function ( round() ) to limit the number of decimals to 2.
# You should print the actual average of the column, not <MEAN>.
# 1. Joanne completed more tasks when she drank coffee. The average was
# <MEAN>.
# 2. Joanne completed more tasks when she did not drink coffee. The average
# was <MEAN>.

answer<- function(joannesTasks){
  mean_coffee<- round(mean(joannesTasks$`Number of Tasks With Coffee`),2)
  mean_without_coffee<- round(mean(joannesTasks$`Number of Tasks Without Coffee`),2)
  
  if(mean_coffee > mean_without_coffee)
    print(paste(" Joanne completed more tasks when she drank coffee. The average was", mean_coffee))
  else
    print(paste("Joanne completed more tasks when she did not drink coffee. The average was", mean_without_coffee))
}





