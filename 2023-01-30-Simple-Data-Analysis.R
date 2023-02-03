### Simple Data Analysis and More Complex Conrol Structures
#### 30 Jnauary 2023
## JMF

dryadData <- read.table(file="Data/amphibian_data.csv", header=TRUE, sep= ",")

#set up libraries
library (tidyverse)
library(ggthemes)


#independent/explnatory (X-AXIS) versus dependent/response variables (Y-AXIS)
#discrete versus continuous variables
#continuous variables -range of numbers: height,weight, temp (integer and real)
#direction of cause and effect, x axis is independent
#discrete/categorical variable- categories: species, treatments, temp (natural or arbitrary or statistical bins)

#cont+cont=linear regression, scatterplot
#comt+cat = logistic regression curve plot
#cat+cont = boxplot, t-test, anova (2+), barplot
#cat+cat. = chi-sqared, table, mosaic plot

glimpse(dryadDta)


#Basic linear regression analysis in R (using 2 continuous variables)

#Is there a relationship between mean pool hydroperiod and number of breeding frogs caught?

## y ~ x
regModel <- lm(count.total.adults~mean.hydro,data=dryadData)
regModel

summary(regModel) # p=3.27e-06
hist(regModel$residuals) 

summary(regModel)[["r.squared"]] #how much the vairants in x explained the variants in y 
summary(regModel)$"r.squared"


regPlot <- ggplot(data=dryadData,aes(x=mean.hydro,y=count.total.adults)) +
  geom_point() +
  stat_smooth(method=lm,se=0.99) # default se=0.95 
regPlot + theme_few()

###Basic ggplot of ANOVA data
# Was there a statistical difference in the number of adults amoung wetlands?

ANOmodel <- aov(count.total.adults~factor(wetland),data=dryadData) #facotr wetland to make it a group instead of integeers
print(ANOmodel)
summary(ANOmodel)

dryadData %>%
  group_by(wetland) %>%
  summarise(avgHydro = mean(count.total.adults, na.rm=T), n=n())


#Boxplot

## we need to let R know that our wetland variable is categorical and not numeric
dryadData$wetland <-factor(dryadData$wetland) #now it reads as a factor
class(dryadData$wetland)
ANOPlot <- ggplot(data=dryadData,mapping=aes(x=wetland,y=count.total.adults)) +
  geom_boxplot()
ANOPlot

# by species
wetlandSpCount <- ggplot(data=dryadData,aes(x=wetland,y=count.total.adults, fill=species)) +
  geom_boxplot() + scale_fill_grey()
wetlandSpCount
# ggsave(filename="Plot2.pdf",plot=wetlandSpCount,device="pdf")

#ggsave(file= "SpeciesBoxPlots.pdf", plot=wetlandSpCount,device= "pdf") (another way to save)


####Logistic regression analysis in R
## Gamma Distribution: a continuous probability distribution that is widely used in different fields of science to model continuous variables that are always positive and have skewed distributions.
xVar <- sort(rgamma(n=200,shape=5,scale=5))
yVar <- sample(rep(c(1,0),each=100),prob=seq_len(200))
lRegData <- data.frame(xVar,yVar)


logRegModel <- glm(yVar ~ xVar,
                 data=lRegData,
                 family=binomial(link=logit)) #fancy for logistic regression
summary(logRegModel)
summary(logRegModel)$coefficients

#Basic ggplot of logistic regression

lRegPlot <- ggplot(data=lRegData, aes(x=xVar,y=yVar)) +
  geom_point() +
  stat_smooth(method=glm, method.args=list(family=binomial))
print(lRegPlot)

##Data for contingency table analysis
#Are there differences in counts of males and females between spseices?

countData<- dryadData %>%
  group_by(species) %>%
  summarize(Males=sum(No.males, na.rm=T), Females=sum(No.females, na.rm=T)) %>%
  select(-species) %>%
  as.matrix() 

row.names(countData)=c("SS","WF")
countData

#chi-squared
#get residuals 
testResults<- chisq.test(countData)

testResults$expected


###Plotting contingency table analyses

# some simple plots using baseR
mosaicplot(x=countData,
           col=c("goldenrod","grey"),
           shade=FALSE)

countDataLong <- countData %>%
  as_tibble() %>%
  mutate(Species=c(rownames(countData))) %>%
  pivot_longer(cols=Males:Females,
               names_to = "Sex",
               values_to = "Count")
#Plot bargraph 
ggplot(data=countDataLong, mappiong= aes(x=Species, y=Count, fill=Sex)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("black", "darkslateblue"))

#################################

##Control Structures

#if and ifelse statements

#if (condition) {expression1}
#if (condition) {expression1} else {expression2}
#if (somthing is greater than something) {do this} else {but if it is anything else do this

#if (condition1) {expression1} else
#if (condition2) {expression2} else
# can go into new else with a different if statement

#note that final unspecified else captures rest of the (unspecified) conditions
#else statement must appear on the same line as the expression
#typically enclose multiple statements in brackets {} for compound expression


#use it for single values

z <- signif(runif(1),digits=2)
print(z)
z > 0.5 #TRUE

# works with or without {} after if
if (z > 0.8) {cat(z,"is a large number","\n")} else #name large number "is a large number"// /n end w linebreak
  if (z < 0.2) {cat(z,"is a small number","\n")} else
  {cat(z,"is a number of typical size","\n") #anything inbetween name it "is a number of typical size
    cat("z^2 =",z^2,"\n")} #want sq value if typical size
#cat puts everything together 


# if statement requires a single logical value. With a vector,
# it will throw an error.
z <- 1:10
# this does not do anything
if (z > 7) print(z)

# use subsetting!
print(z[z < 7])

###ifelse to fill vectors

#ifelse(test,yes,no) #conditon as first argument 
#test is an object that can be coerced to a logical yes/no
#yes return values for true elements of test
#no return vales for false elements of test


### insect population in which each female lays, on average, 10.2 eggs, following a Poisson distribution with λ=10.2 (discrete probabilty distribution showing the liekly number of times an event will occur)
#35% chance of parasitism, in which case no eggs are laid. Here is the distribution of eggs laid for 1000 individuals:
  
  
  tester <- runif(1000) # start with random uniform elements
eggs <- ifelse(tester>0.35,rpois(n=1000,lambda=10.2),0)
hist(eggs)

#Suppose we have a vector of p values (say from a simulation), and we want to create a vector to highlight the significant ones for plotting purposes.

pVals <- runif(1000)
z <- ifelse(pVals<=0.025,"lowerTail","nonSig")
z[pVals>=0.975] <- "upperTail"
table(z)
#can be used for a two-tail test


####for loops
#The workhorse function for doing repetitive tasks
#Universal in all computer languages
#Controversial in R bc often not necessary (use vectorized operations!)
#very slow with binding operations (c,rbind,cbind,list many operations can be handled by special family of apply functions


###Anatomy of a for loop

#for (variable in a sequence) { # start of for loop
  # body of for loop 
#} // end of for loop
#var is a counter variable that will hold the current value of the loop (i,j,k (other variable))
#seq is an integer vector (or a vector of character strings) that defines the starting and ending values of the loop

for (i in 1:5) {
  cat("stuck in a loop",i,"\n")
  cat(3 + 2,"\n")
  cat(runif(1),"\n")
}
print(i)

### use a counter variable that maps to the position of each element 

my_dogs <- c("chow","akita","malamute","husky","samoyed")
for (i in 1:length(my_dogs)){
  cat("i =",i,"my_dogs[i] =" ,my_dogs[i],"\n")
}

####Using double for loops
m <- matrix(round(runif(20),digits=2),nrow=5)

# loop over rows
for (i in 1:nrow(m)) { # could use for (i in seq_len(nrow(m)))
  m[i,] <- m[i,] + i
} 
print(m)

# Loop over columns
m <- matrix(round(runif(20),digits=2),nrow=5)
for (j in 1:ncol(m)) { #when using columns use j 
  m[,j] <- m[,j] + j
}
print(m)

# Loop over rows and columns
m <- matrix(round(runif(20),digits=2),nrow=5)
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j] <- m[i,j] + i + j
  } # end of column j loop
} # end or row i loop
print(m) 
