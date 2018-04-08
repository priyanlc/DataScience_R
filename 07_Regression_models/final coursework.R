require(ggplot2)
require(mtcars)

# blue dots are manual, black dots are automatic
#1 Displacement vs MPG for manual and automatic
g<-ggplot(mtcars,aes(disp,mpg))
g<-g+geom_point(aes(color=am))
g<-g+ggtitle("Displacement vs MPG for manual and automatic")
g<-g+xlab("Displacement")
g<-g+ylab("MPG")
g<-g+scale_fill_discrete(name="A/M",labels=c("A","M"))
g

# 2 Cylinders vs MPG for manual and automatic
g<-ggplot(mtcars,aes(cyl,mpg))
g<-g+geom_point(aes(color=am))
g<-g+ggtitle("Cylinders vs MPG for manual and automatic")
g<-g+xlab("Cylinders")
g<-g+ylab("MPG")
g


# 2 Cylinders vs MPG for manual and automatic, Inline on bottom V on top
g<-ggplot(mtcars,aes(cyl,mpg))
g<-g+geom_point(aes(color=am))
g<-g+facet_grid(vs ~ .)
g<-g+ggtitle("Cylinders vs MPG for manual and automatic")
g<-g+xlab("Cylinders")
g<-g+ylab("MPG")
g


# 2 Forward Gears vs MPG for manual and automatic
g<-ggplot(mtcars,aes(cyl,gear))
g<-g+geom_point(aes(color=am))
g<-g + ggtitle("cyl vs gear for manual and automatic")
g<-g + xlab("cyl")
g<-g + ylab("gear")
g

# trangles are manual, dots are automatic
# color is qsec, lighter is higher qsec, darker is lower qsec
# gears: top graph 3, middle 4, bottom 5 
g<-ggplot(mtcars,aes(wt,mpg))
g<-g+geom_point(aes(color=qsec,shape=am))
g<-g+facet_grid(gear ~ .)
g<-g+ggtitle("Weight vs MPG for manual and automatic")
g<-g+xlab("Weight")
g<-g+ylab("MPG")
g



# trangles are manual, dots are automatic
# color is qsec, lighter is higher qsec, darker is lower qsec
# gears: top graph 3, middle 4, bottom 5 
g<-ggplot(mtcars,aes(wt,mpg))
g<-g+geom_point(aes(color=qsec,shape=factor(am)))
g<-g+facet_grid(gear ~ .)
g<-g+ggtitle("Weight vs MPG for manual and automatic")
g<-g+xlab("Weight")
g<-g+ylab("MPG")
g

# Part 1
We do liner regression between mpg and  factor(am). The hypothesis we are testing is below.

#H0: There is no difference between the avarages of the two means for Automatic and manual cars
#H1: There is a significant difference between the averages for two means for Automatic and Manual cars

fix0<-lm(mpg~factor(am)-1,mtcars)
coef(summary(fix0))

From the p values coefficients we can see that there is significant difference between  
the Automoatic factor(am)0 and Manual factor(am)1 averages. We can reject the null hypothesis and 
accept the alternative hypothesis.

mean mpg for automatic cars is 
mean mpg for manual cars is 

Part2: 

Now lets try to select multiple variables. Here I have selected the variables manually based on the 
p values

summary(lm(mpg~.,mtcars))

wt          -3.71530    1.89441  -1.961   0.0633 .
am1          2.52023    2.05665   1.225   0.2340
qsec         0.82104    0.73084   1.123   0.2739  


fit1<-lm(mpg ~wt,mtcars)
fit2<-update(fit1,mpg ~wt+factor(am),mtcars)
fit3<-update(fit2,mpg ~wt+factor(am)+qsec,mtcars)

anova(fit1,fit2,fit3)

