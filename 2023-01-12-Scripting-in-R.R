###### Programming in R 
###### 12 January 2023
###### JMF 

# Advantages 
## interactive use
## graphics, statistics
## very active community of contributors
## works on multiple platforms

# Disadvantages 
## interpreted (slow speed)
## lazy evaluation (so it skips a few steps)
## functions hard to learn (steep learning curve)
## some packages are poorly documented
## unreliable packages
## problems with big data (multiple GBs)


# Let's start with the basics 

## Assignment operator: used to assign a new value to a variable 

x <- 5
print(x) #blue in console is input and white is output
x

### make sure to run things in order

y = 4 #legal but not used except in function arguments
print(y)
y = y + 1.1

y <- y + 1.1 #both ways do the same thing but this is perferred bc its assigning a value, rather than just looking like an equation

## can use command shift c to add number symbol to highlighted section

print(y)

#to run everything at once, highlight what you want and then run

### Variables are used to store information (a container)

z <- 3 # single letter variables 
plantHeight <- 10 # camel case formatiing 
plant_height <- 3.3 # snakw case format (preferred because easier to read)\
plant.height <- 4.2


. <- 5.5 # using a period is resevered for a temporary varible (a place holder)

### Functions: blocks of code that perform a task; you can use a single short command over and over again, rather than writing it out multiple times.

# you can create your own function

square <- function(x = NULL){
  x <- x^2
  print(x)
}

z<-103
square (3) #the argument name is x

### or there are built functions
sum(109, 3, 10) ## look at package info using ?sum or going to Help panel

### Atomic Vectors
# one dimensional (a single row)
# one of the fundamental data structures in R Programming 

## Types
# character strings (usally within quotes)
# integers (whole numbers)
# double (real numbers, decimal)
# both integers and doubles are numeric 
# logical (TRUE or FALSE)
# factor (categorizes and groups variables)

# c funtion: combine

z<- c(3.2, 5, 5, 6)
print(z) #[1] in console just is the number it's starting with
class(z) Z #returns class (date type)
typeof(z) # returns type of variable (double)
is.numeric(z) # returns T/F (when you are asking a question )

# when just rue false it is always logical 

## c()always "flattens" to a vector (this is overkill/to show adding multiple c just makes thing longer
# c() useful for creating vectors 

z <- c(c(3,4), c(5,6))

c(3,4, "dog", TRUE, 4,5) # returns with lowest demoniator (dont do)

# character vectors 
z<- c("perch", "bass", "trout")
print(z)
z<- c("This is only 'one' character string", "a second", 'a third')
print(z)
typeof(z)
is.character(z) #is. functions tests whether it is a certian data type

## Logical (Boolean), no quotes, all caps; TRUE and/or FLASE

z<-c(TRUE, FALSE, T, F) # as. functions coerce data
z<- as.character(z) # can use with numbers if you didn't want to use quotes
is.logical(z)

# type
# is.numeric / as.character
# typeof()

# Length tells how long the vector is 
length(z)
dim(z) #no dimension in vector because it onle has 1

## Names

z <- runif(5) # gives random uniform numbers
names(z) # no name

# add names
names(z) <- c("chow", "pug", "beagle", "greyhound", "akita") #allows us to give it names
print(z) # now have names associated 
names(z) # shows names
names(z) <- NULL # gets rid of names

#### NA (missing data)
z<- c(3.2, 3.4, NA)
typeof(z) # double
mean(z) #runs NA so check data
sum(z)

#check for NAs
anyNA(z)
is.na(z) # gives F,F,T
which(is.na(z)) # tells you exactly what positon it is in

### Subsetting 
# vectors are indexed
z<- c(3.1, 9.2, 1.3, 0.4, 7.5)
z[4] # use square brackets to subset by index (tells you the value of the number in that positon)
z[c(4,5)] # need to use c for multiple indices (multiple values)
z[-c(2,3)] # minus sign exludes elements/values 
z[-1] # also woeks with one number 
z[z==7.5] # gives the exact numbers that equal that value
z[z==3.3] # says numeric(0) becasue none of the values match that value
z[z<7.5] # use logical statments within squaer brackets to subset by conditions 

which(z<7.5) # gives which indices are less than 7.5
z[which(z<7.5)] # gives the values of the numbers less than 7.5

#. which only outputs indices; with [] it subsets those values 

# creating logical vector
z <- 7.5 

## subsetting characters (named elements)
names(z) <- c("a","b","c","d","e")     #?????????
z[c("a","d")]

# subset
subset(x=z, subset=z>1.5)

# randomly sample
# sample function 
story_vec <- c("A", "frog", "jumped", "Here")
sample(story_vec) #randomly shuffles with no arguments 

# randomly select three arguments from vector
sample(story_vec, size =3)

story_vec <- c(story vec[1],"Green", story_vec[2:4]) #???

story_vec[2] <- "Blue"

# vector function
vec <- vector(mode= "numeric", length=5)

### rep and seq function (can use to make patterns of values)

z<- rep(x=0, times=100)
z<-rep(x=1:4) # gives a string of numbers in order
z<-rep(x=1:4, each=3) 

z<-seq(from= 2, to=4) # gives you 2-4
seq(from=2, to=4, by=0.5) # gives you 2-4 in increments of 0.5

z<- runif(5)
seq(from=1,to=length(z)) # will give you length of numbers based on what z is


