### Assignment 6
## 23 January 2023
## JMF

library(tidyverse)

#### 1. Examine the structure of the iris data set. How many observations and variables are in the data set?

glimpse(iris)

#There are 150 observations and 5 variables.


#### 2. Create a new data frame iris1 that contains only the species virginica and versicolor with sepal lengths longer than 6 cm and sepal widths longer than 2.5 cm. How many observations and variables are in the data set?

iris1 <- filter(iris, Species== c("virginica", "versicolor") & Sepal.Length > 6 & Sepal.Width > 2.5)

#### 3. Now, create a iris2 data frame from iris1 that contains only the columns for Species, Sepal.Length, and Sepal.Width. How many observations and variables are in the data set?

iris2 <- select(iris1, Species,Sepal.Length,Sepal.Width)
glimpse(iris2)

# There are 28 observations and 3 variables.

#### 4. Create an iris3 data frame from iris2 that orders the observations from largest to smallest sepal length. Show the first 6 rows of this data set.

iris3 <- arrange(iris2, by=desc(Sepal.Length))
head(iris3)

#### 5. Create an iris4 data frame from iris3 that creates a column with a sepal area (length * width) value for each observation. How many observations and variables are in the data set?

iris4 <- mutate(iris3, Sepal.Area=Sepal.Length*Sepal.Width)
glimpse(iris4)

#There are 28 observations and 4 variables in the data set. 

#### 6. Create the variable irisTab that shows the average sepal length, the average sepal width, and the sample size of the entire iris4 data frame and print irisTab.

irisTab <- summarize(iris4, AverageSepalLength = mean(Sepal.Length), AverageSepalWidth = mean(Sepal.Width), SampleSize= n())
glimpse(irisTab)
print(irisTab)

#### 7. Finally, create iris5 that calculates the average sepal length, the average sepal width, and the sample size for each species of in the iris4 data frame and print iris5.

iris5 <- iris4 %>% 
  group_by(Species) %>% 
  summarize(AverageSepalLength = mean(Sepal.Length),AverageSepalWidth = mean(Sepal.Width), SampleSize= n())
print(iris5)

#### 8. Rework all of your previous statements (except for irisTab) into an extended piping operation that uses iris as the input and generates irisFinal as the output.

irisFinal <- iris %>% 
  filter(Species== c("virginica", "versicolor") & Sepal.Length > 6 & Sepal.Width > 2.5) %>% 
  select(Species,Sepal.Length,Sepal.Width) %>%
  arrange(by=desc(Sepal.Length)) %>%
  mutate(Sepal.Area=Sepal.Length*Sepal.Width) %>%
  group_by(Species) %>% 
  summarize(AverageSepalLength = mean(Sepal.Length),AverageSepalWidth = mean(Sepal.Width), SampleSize= n()) 
print(irisFinal)  



  
  



