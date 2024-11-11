# 1 Load the midwest dataset from the ggplot2 package. Use the correct functions in dplyr to
# select observations in Illinois from the dataset.

data(midwest,package = 'ggplot2')
library(dplyr)
library(magrittr)
observation_IL<- midwest%>%filter(state=='IL')
print(observation_IL)


# a. What is the average percentage of college educated among these counties? Use
# the correct function from purrr to find this answer. Your output should only
# contain 2 columns: county and avgCollege
library(ggplot2)
library(dplyr)
install.packages("purrr")
library(purrr)

data("midwest")
observation_IL <- midwest %>%
  filter(state == 'IL')

# Calculate the average percentage of college-educated individuals
result <- observation_IL %>%
  group_by(county) %>%
  summarise(avgCollege = mean(percollege, na.rm = TRUE)) %>%
  select(county, avgCollege)


library(ggplot2)
data("economics")
econ2 <- economics %>% select(-date) %>% map_if(is.integer, ~.x/1000)
df1 <- data.frame(wages = rlnorm(100, meanlog = log(50000)), pension = rlnorm(100,meanlog =
                                                                                log(1e+05)), hhid = 1:100)
df2 <- data.frame(wages = rlnorm(100, meanlog = log(50000)), pension = rlnorm(100,meanlog =
                                                                                log(1e+05)), hhid = 1:100)


# 2. Using commands found in the tidyverse package, perform the following joins
# individually using hhid as the key. Utilize the “by” argument in every join
# function. Using comments in your script, explain the results of the output for each
# join.
# i. Perform a left join using df1 and df2. (5pts)
# ii. Perform a right join using df1 and df2. (5pts)
# iii. Perform a full join using df1 and df2. 



install.packages("tidyverse")
library(tidyverse)
library(dplyr)

result1<- left_join(df1, df2, by='hhid')
# The result of a left join keeps all rows from df1 and only matching rows from df2.
result2<- right_join(df1, df2, by='hhid')
# The result of a right join keeps all rows from df2 and only matching rows from df1.
result3<- full_join(df1, df2, by='hhid')
# The result of a full join keeps all rows from both df1 and df2.
head(result1)
head(result2)
head(result3)

#3.In your own words, when should you use cbind and rbind?
x <- c("a", "b", "c")
y <- c("d", "e", "f")
cbind(x,y) #cbind is used when you want to add new columns to an existing data frame or matrix.
rbind(x,y) #rbind is used when you want to add new row to an existing data frame

# 4. Load the data from ramen-ratings.csv into a variable called ramenRatings
# a. Using the right function from the stringr package, split the data in the Brand
# column by each space. (5pts)
# b. Using the right function from the stringr package, get the first two characters from
# the Brand column. Store the result in a new column called BrandStart in the
# ramenRatings dataset. (5pts)
# c. Using the right function from the stringr package, how many records in the
# Variety column of the dataset contain “ANT”? Use the regex() function in stringr
# to ignore the case of the pattern. (5pts)

library(readr)
ramenratings<- read.csv('/Users/samratchaudhary/Downloads/ramen-ratings.csv')
install.packages("stringr")
library(stringr)
library(dplyr)

Brand_Split <- str_split(ramenratings$Brand, " ")#Splits brand column by each space

ramenratings<- ramenratings%>% mutate(BrandStart= str_sub(Brand,1,2)) #gets the first 2 character from the brand column and store them in brandStart



library(dplyr)
library(stringr)

# Count the number of records in the Variety column that contain "ANT" (case-insensitive)
num_records <- ramenratings %>%
  filter(str_detect(Variety, regex("ANT", ignore_case = TRUE))) %>%
  nrow()



