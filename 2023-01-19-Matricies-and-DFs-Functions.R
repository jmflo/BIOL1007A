##### Finishing Matricies and Data Frames
### 19 January 2023
## JMF

m<-matrix(data=1:12, nrow=3)
m

##subsetting based on elements 

m[1:2, ] # gives first two rows and then all the columns
m[,3:4] # gives corresponding columns and all the rows

###subset with logical (conditional) statements 
### select all columns for which totals are >15

colSums(m) #gives sums of all columns

colSums(m)>15 #gives columns with sum greater than 15 in T/F (logical operator)
m[,colSums(m)>15] #gives columns with sum greater than 15 with numbers

##row sums now
## select rows that sum up to 22

m[rowSums(m)==22,] #using double equal sign to get specific row

m[rowSums(m)!=22,] #using ! to get rows not equal to 22

### Logical operators: == != < >

##subsetting to a vector changes the data type
z<- m[1,]
print(z)
str(z)

z2<- m[1, ,drop=FALSE] #add argument to keep it as a matrix

m2 <- matrix(data=runif(9),nrow=3)
print(m2)
m2[3,2] #best practice to use both indices 

### Use the assignment operator to substitute values 

m2[m2>0.6] <- NA #don't want anything over 0.6
print(m2)

data <- iris
head(data) #let's you see first 6 rows
tail(data) #gives last six rows

data[3,2] #number indices will work

dataSub <- data[c("Species", "Petal.Length")] # you can specify just the column names

### Sort a data frame by values 

orderedIris <- iris[order(iris$Petal.Length),] #telling it to sort rows (comma need to have columns wiht the information)
head(orderedIris)

#### FUNCTIONS!!

#Everything in R is a function

sum(3,2) # sum // a "prefix" function
3 + 2 # + sign is a function // an "operator", but it is actually a function
sd #tells you stats

### User-defined funcitons 

#functionName<- function(argX=defaultX, argY=defaultY){
  ## curly bracket starts the bodu of the function
  ## Lines of R code ### and notes
  ## create local variable (only visible to R within the function)
#always end with a return(z)
    #return() gets soemthing out of the function
#}


#intial arguments = (a=3,b=4)
myFunc <- function(a=3,b=4) {
  z <- a + b #using same variables you specified 
  return(z)
}
myFunc() #running with open () runs the default
myFunc(a=100, b=3.4) #stil need to use argument functions 

#set argument to something that can be change (in weekly assignment)
z<-myFunc() #it won't recognixe funciton inside // need to run it 
print (z) 

myFuncBad <-function(a=3) {
  z <- a + b
  return(z)
}

myFuncBad() # crashes because it can't find b
b <- 100

myFuncBad() #runs default // error object b not found 
myFuncBad <-function(a=NULL, B=NULL) {
  z <- a + b
  return(z)
}


### Multiple Return statements

#############################################################
# Function: HardyWeinburg
# input: all allele frequency p (0,1)
# output: p and the frequencies of the 3 genotypes AA, AB, BB
#------------------------------------------------- 

HardyWeinberg<- function(p=runif(1)) {
  if (p > 1.0 | p < 0.0) { # | means or
    return("Function failure: p must be >= 0.0 and <= 1.0")
}

  q <- 1 - p
   fAA <- p^2
   fAB <- 2*p*q
    fBB <- q^2
  
 vecOut <- signif(c(p=p,AA=fAA,AB=fAB,BB=fBB),digits=3)
  return(vecOut) #signif rounds to significant units 
      + }

##################################################
HardyWeinberg()
freqs <- HardyWeinburg()
freqs
Hardy Weingburg (p=3)

### Create a complec default value 
##############################################
# FUNCTION:fitlinear2
# fits simple regresssion line
# input: list (p) of predictor (x) and response (y)
# outputs: slope and p-value


#allows you to imput something as a list with two differemt numerical values 

fitLinear2 <- function(p=NULL){
  if(is.null(p)){
    p<- list(x=runif(20), y=runif(20))
}
 myMod <-lm(p$x~p$y)
 myOut <-c(slope= summary(myMod)$coefficients[2,1], 
           pValue=summary(myMod)$coefficients[2,4])
 plot(x=p$x, y=p$y) #quick plot to check output
 return(myOut)
}

fitLinear2() #simulates p for us when p=NULL



myPars<-list(x=1:10,y=runif(10))
fitLinear2(p=myPars)











