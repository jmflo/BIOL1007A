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


rnorm(n=100,mean=100,sd=30)
hist(rnorm(n=100,mean=100,sd=30))

#### Matix Data Structures
## leaving atomic vectors behind
## 2 dimensional (rows and columns)
## homogenous data types

#matrix is an atomic vector organized into rows and columns
my_vec<-1:12
m<-matrix(data=my_vec, nrow=4)
m

m<-matrix(data=my_vec, ncol=3)
m

m<-matrix(data=my_vec, ncol=3, byrow=T)
m

### Lists 
## are like atomic vectors BUT each element can hold different data types (and different sizes)

myList <- list(1:10, matrix (1:8, nrow=4, byrow=TRUE), letters[1:3], pi)
#making a list with 4 different elements
class(myList)
str(myList)


### Subsetting lists
## using [] gives you a single item BUT not the elements 
myList[4]
myList[4] - 3 #won't work // single bracket gives you only elements in the slot which is always type list

# to grab object itself, use [[]]

myList[[4]] #now looks like a vector // only one item // can access
myList[[4]] - 3

myList[[2]][4,1] # 4th row, 1st column // 2 dim subsetting  -> first number is row index, second is column 

myList[c(1,2)]# to obtain multiple compartments of list

c(myList[[1]], myList[[2]]) # to obtain multiple elements within list 

### Name list items when they are created
myList2<- list(Tester=FALSE, littleM = matrix(1:9, nrow=3))
myList2$Tester ## $ accesses named elements 

myList2$littleM[2,3] # extracts second row, third column of littleM

myList2$littleM [2,] # leave colmun blank if you want all elements of a row

myList2$littleM[2] #gives second element

### unlist to string everything back to vector

unRolled<- unlist(myList2)
unRolled


data(iris)
head(iris) # head is a function that lets us see the first 6 rows
plot(Sepal.Length ~ Petal.Length, data= iris) # y variable ~ x variable

# r regression to test significance 
# lm = linear model
model <- lm(Sepal.Length ~ Petal.Length, data=iris)
results <- summary(model)

#we care about the estimate

str(results)
results$coefficients
#use [] to extract Petal.Length pvalue
# different way: use unlist()

results$coefficients[2,4] #extracts value

results[[4]][2,4] #another way to do it

unlist(results)$coefficients8 #another way to do it

### Data Frames
## (list of) Equal length vectors, each of which is a column

varA <- 1:12
varB <- rep(c("Con", "LowN", "HighN"), each=4)
varB

varC <- runif(12)

dFrame <- data.frame(varA, varB, varC, StringAsFactors=FALSE)
print(dFrame)

str(dFrame) # shows what each type is 

# add another row 

newData <- list(varA=13, varB="HighN", varC= O.678)  ???
  
# USE rbind()
  dframe <- rbind(dFrame, newData) ??
  
### why can't we use C?
  
newData2 <- c(14, "HighN", 0.668)
dFrame <- rbind (dFrame, Newdata2)


### add a column
newVar <- runif(12)
# use dim to check how many rows and columns you have
#use cbind() funciton to add column

dFrame <- cbind(dFrame, newVar)


### Data Frames vs Matricies 

zMat <- matrix(data=1:30,ncol=3,byrow=TRUE)

zDframe<- as.data.frame(zMat) #as. coerces it into someting?
# you will get "V1" as a heading if you didn't name it and you will have row IDs

str(zDframe) #two different ways to visualize 
str(zMat)

#finding specific integer
zMat[3,3]
zDframe[3,3]

#finding all integers in one column
zMat[,3]
zDframe[,3]
zDframe$V3

#fidning whole row
zMat[3,]
zDframe[3,]

zMat [3] #third number is given
zDframe[3] #different! whole third column is given

##### Eliminating NAs 
# complete.cases() function

zD<- c(NA, rnorm(10), NA, rnorm(3))
complete.cases(zD) #returns a logical output with T/F

#clean out NAs
zD[complete.cases(zD)] #automatically removes NAs 
which(!complete.cases(zD)) # !: is not //use to find which numbers are NA
which(is.na(zD)) # another why to do it

# can use witha matrix

m<-matrix(1:20, nrow=5)
m[1,1]<-NA #assigning that number as an NA
m[5,4]<-NA 
m

complete.cases(m) # gives t/f as to whether whole row is "complete" (no NAs)
m[complete.cases(m),] # gives rows that do not have NAs // , : all rows

## get complete cases for only certain rows
m[complete.cases(m[,c(1:2)]),] # need all the commas

                 
  