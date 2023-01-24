#### Entering the tidyverse (dplyr)
## 23 January 2023
## JMF



#tidyverse: collection of packages that show philosophy, grammar (or hoew the code is structured), and data structures

## Operators: symbols that tell R to perform different operations (udually go between variable, functions, etc.)

## Arthmetic Operators: + - * / ^ ~
## Assignment operator: <-
## Logical operators: !(means not) & |(means or)
## Relational operators: == != > < <= >=
## Miscellaneous operators: %>%(forward pipe operator) %in%


### Only need to install packages once

library(tidyverse) #use the library function to load in packages 

##dplyer: a new(er) package that provides a set of tools for manipulating data sets in R
#specifically written to be fast!
#has individual functions that correspond to the most common operations
#makes it easier for you to figure out what to do with your data

#####t The core ‘verbs’
## filter()
## arrange()
## select()
## summarize() and group_by()
## mutate()


## built-in dataset
data(starwars)
class(starwars)


## Tibble: a modern take on data frames

# keeps the great aspects of data frames and drops the frustrating ones (i.e. changing variable names, changing an input type)

glimpse(starwars) #more effective than str() in this case // gives an overview of data

### NAs
anyNA(starwars) #is.na, complete.cases // other options
starwarsClean <- starwars[complete.cases(starwars[,1:10]),] #getting rid of NAs in all rows but first ten columns 
anyNA(starwarsClean[,1:10])


### filter(): picks/subsets
observations (ROWS) by their values

filter(starwarsClean, gender == "masculine" & height < 180) # you can also use commas in place of &   

filter(starwars, gender == "masculine", height <180, height > 100) # can add multiple conditions for the same variable   

filter(starwars, gender == "masculine" | gender== "feminine")



### The %in% operator (matching); 

#can be used to identify if an element (e.g., a number) belongs to a vector or data frame.
#Using the %in% operator you can compare vectors of different lengths to see if elements of one vector match at least one element in another. The length of output will be equal to the length of the vector being compared (the first one). This is not possible when utilizing the == operator.

a<- LETTERS[1:10]
length(a) #length of vector

b <- LETTERS[4:10]
length(b)

# output depends on first vector
a %in% b # seeing if b is in the first vector
b %in% a #seeing if all of the elements in b are in a 

#use %in% to subset
eyes<- filter(starwars, eye_color %in% c("blue", "brown")) 
# we can use %in% for a few conditions, similar to ==

View(eyes) #opens another tab

eyes2<- filter(starwars, eye_color=="blue" | eye_color== "brown") #can't use c funciton with ==
View(eyes2)


### arrange(): Reorder rows

arrange(starwarsClean, by = height) #default is ascending order // don't need to add df vaiable 


arrange(starwarsClean, by = desc(height)) #we can use desc() to change that // it's a helper function

arrange(starwarsClean, height, desc(mass)) # each additional column used to break ties with the preceding column // mass with break ties with height

sw<- arrange(starwars, by= height) # missing values are at the end; note we haven't been assigning anything to a variable, just printing (until now) 
tail(sw)


### select(): Choose variables (COLUMNS) by their names

## All of these do the same thing (subset)

select(starwarsClean, 1:10) # you can use numbers to subset
select(starwarsClean, name:species) # you can use variables names too

select(starwarsClean, -(films:starships)) # you can subset everything except particular variables // using the minus sign operator

#same as saying
starwarsClean[,1:11]


#### Rearrange columns

select(starwarsClean, name, gender, species, everything()) # using the everything() helper function is useful if you have a few variables to move to the beginning
#not saving anything, just showing what it is doing 


select(starwarsClean, contains("color")) ## other helpers include: ends_with, starts_with, matches (reg ex), num_range (numeric range)

## Renaming columns

# select can rename columns 
select(starwars, haircolor = hair_color) #haircolor is the new variable name // returns only renamed column // can use everything () too

rename(starwarsClean, haircolor = hair_color) #returns whole df // keeps all the variables



#### mutate(): Create new variables with functions of existing variables

# Let's create a new column that is a height divided by mass
mutate(starwarsClean, ratio = height/mass) # note we use arithmetic operators // just reference column names


starwars_lbs <- mutate(starwarsClean, mass_lbs=mass*2.2) # now let's convert kg to lbs


starwars_lbs <- select(starwars_lbs, 1:3, mass_lbs, everything()) #reorganizing the order of the data
glimpse(starwars_lbs) #brought it to the front using select


#Transmute 

# If we only wanted the new variable
transmute(starwarsClean, mass_lbs=mass*2.2) # only returns mutated columns 

transmute(starwarsClean, mass, mass_lbs=mass*2.2, height) # you can mention variables you want to keep in the new dataset


##### summarize() and group_by(): Collapse many values down to a single summary

summarize(starwarsClean, meanHeight = mean(height)) # gives summary statistics for entire tibble // mean heigh is a new header name // throws NA if any NAs are in df - need to use na.rm

# working with NAs
summarize(starwars, meanHeight = mean(height)) # does not calculate mean if NAs are present

summarize(starwars, meanHeight = mean(height, TotalNumber = n())
          
#use group_by for maxium usefelness

starwarsGenders<- group_by(starwars, gender) #comparing different subgroups within one column
head(starwarsGenders) # now we see the groups mentioned at the top of the tibble

summarize(starwarsGenders, meanHeight = mean(height, na.rm = TRUE), TotalNumber = n()) # now we can compare height and sample size between groups


### Piping: taking the output of one functoin and using it as the input of another function

# uses the forward piping operator %>%
# used to emphasize a sequence of actions
# it lets you pass an intermediate result onto the next function (it takes the output of one statement and makes it the input of the next statement)

#avoid when you need to manipulate more than one object/variable at a time or there are meaningful intermediate objects


#formatting: should always have a space before it and usually followed by a new line (usually automatic indent)


starwarsClean %>% #our pipe
  group_by(gender) %>% #indents automatically
  summarize(meanHeight = mean(height, na.rm=T), TotalNumber=n()) #na.rm=T skips NAs
# so much cleaner! 

##more compelx coding
## case_when() is useful for multiple if/ifelse statements

starwarsClean %>%
  mutate(sp = case_when(species == "Human" ~ "Human", TRUE ~ "Non-Human")) %>% #first section is if it is true, second is if it's not true // uses condition, puts "human" if True  in sp column, puts "Non-human" if not true
 













