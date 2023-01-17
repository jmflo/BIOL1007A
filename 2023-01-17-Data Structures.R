#### Vectors, Matrices, Data Frames, and Lists
#### 17 January 2023
#### JMF

#### Vectors Continued.
## Properties

## Coercion 

### All atomic vectors are of the same data type
### If you use c() to assemble different data types, R coerces them

## logical -> integer -> double -> character (always goes to lowest type)

a <- c(2, 2.2)
a

#coerces to double 

b <- c("purple","green")
typeof(b)

d <- c(a,b)
print (d) # coerces to character which is why the numbers have "" around them in the console

### comparison operators yeild a logical result
a<- runif(10)
print(a)

a > 0.5 # conditional statement will give a logical result/ string of T/F of the different elements

### How many elements in the vector are greater than 0.5
sum(a>0.5)
mean(a>0.5) #what proportion of vector are greater than 0.5

#### Vectorization
## add a constant to a vector (does it automatically instead of you doing it by row)

z <- c(10, 20, 30)
z + 1 

##what happens when vectors are added together

y<- c(1,2,3)
z + y # results in an "element by element" operation on the vector 

z^2

## Recycling 
# what if vector lengths are not equal?

z
x<- c(1,2)
z+x #warning is issued but the calculation is still made. The shorter vector is always recycled
## recycling is the first number being used again if the numbers are not the same length

#### Simulating data: runif and rnorm()

runif (5) #5 random numbers
runif(n=5, min=5,max=10) 

set.seed(111) #set.seed can be any number, sets random number generator (is reproduciable)
runif(n=5, min=5,max=10)

##rnorm: random normal values with mean 0 and sd 1
randomNormalNumbers<-rnorm(100)
mean(randomNormalNumbers) #hist function shows distribution 

hist(randomNormalNumbers) # you get a histogram obvi hehe
















