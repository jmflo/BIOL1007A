### ggplot2
### 24 January 2023
### JMF 

library(ggplot2)
library(ggthemes)
library(patchwork)

#### Template for ggplot code

## p1 <- ggplot(data=<DATA>,mapping=aes(x=xVAY, Y= YvAR) +
## <GEOMFUNCTION> ## + geomboxplot()
## print()

### Load in built-in data set
d <- mpg
str(mpg)

library(dplyr)
glimpse(d)

#### qplot: quick, plotting
qplot(x=d$hwy) #saying what x axis is

## Histogram 
qplot(x=d$hwy,fill=I("darkblue"),color=I("black")) #fill is inside and color is outside #make sure you add I

## Scatterplots
qplot(x=d$displ,y=d$hwy,geom=c("smooth","point")) # geom is argument # this follows the distribution of the points

qplot(x=d$displ,y=d$hwy,geom=c("smooth","point"),method="lm") #this plots a linear model 

## Boxplot
qplot(x=d$fl, y=d$cty,geom="boxplot",fill=I("green")) 

## Barplot 
qplot(x=d$fl,geom="bar",fill=I("green"))


## Create some data

x_trt <- c("Control","Low","High")
y_resp <- c(12,2.5,22.9)
qplot(x=x_trt,y=y_resp,geom="col",fill=I(c("red","green","blue"))) # col=column


#### ggplot: uses dataframes instead of vectors

p1 <- ggplot(data=d, mapping=aes(x=displ,y=cty, color=cyl)) + 
  geom_point() #can run code with or without actual codes (date/mapping) #color= means group colors by a variable
print(p1)

p1 + theme_base() # mimics base R
p1 + theme_bw() # good with grid lines
p1 + theme_classic() # no grid lines
p1 + theme_linedraw() # black frame
p1 + theme_dark() # good for brightly colored points
p1 + theme_par() # matches current par settings in base
p1 + theme_void() # shows data only
p1 + theme_solarized() # good for web pages
p1 + theme_economist() # many specialized themes
p1 + theme_grey() # ggplots default theme


## Major Theme Modifications
# use theme parameters to modify font and font size
p1 + theme_bw(base_size=40,base_family="serif") #changes font size and font type

# use coordinate_flip to invert entire plot
p2 <- ggplot(data=d, aes(x=fl,fill=fl)) + 
  geom_bar()
print(p2)
p2 + coord_flip() 

p2 + coord_flip() + theme_grey(base_size=20,base_family="sans") #depending on the order you type in it # if geome point is but first then it will be under geome bar

#### Minor theme modifications

p1 <- ggplot(data=d, aes(x=displ,y=cty)) + 
  geom_point(size=7, 
             shape=21, #can look up list of shape on google
             color="black",
             fill="steelblue") +
  labs(title="My graph title here", 
       subtitle="An extended subtitle that will print below the main title",
       x="My x axis label",
       y="My y axis label") + # can also use xlab("count) + ylab("fuel") instead of =/y= before labs
  xlim(1,7.5) + ylim(0,55)
print(p1)

library(viridis)
cols <- viridis(7, option="magma") #plasma, turbo, other viridis options
p2<- ggplot(data=d, aes(x = class, y = hwy, fill=class)) +
   geom_boxplot() +
scale_fill_manual(values =cols)

p2

library(patchwork)

p1+p2 # use / to but them on top
#can group using ()/ 



