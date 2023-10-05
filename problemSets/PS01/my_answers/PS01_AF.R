#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#To get the sample size (n)), we have to use the function length:

length(y)
# We get 25 

mean(y)
#We get 98.44 as the answer 

var(y)
#We get 171.4233

#Having the var we can get S, the formula is S=square(variance)
#There are two ways to do this we can do it with the command sd or type sqrt(var)

sd(y)
sqrt(var(y))
#Both give the same result 13.09287

# To get the Standard Error we need to follow the formula SE=S/square n
# As we already know the value of S = Standard Deviation = 13.09 
# and the value of n = 25, we just have to replace and follow the next command:

sd(y)/sqrt(25)
#We get 2.618575

#At this point we have all the values needed to answer the formula of CI
#CI=X+-Z S/sqrt(n)
#Confidence Interval=Sample mean +-Zscore*Standard deviation /square sample size
#There are two important things about this in this case X with a hat is 
#the sample mean, as previously said some author represent it with y an a hat
#2nd the Zscore we were told to find a 90% CI

mean(y)+1.90*(sd(y)/5)
mean(y)-1.90*(sd(y)/5)

#We are 90% confident that if we pick a random sample the average student IQ
#in that school we fall between 93.46 and 103.42

#For our next exercise we are conducting a study to know if the average student
#IQ from different schools is 100, so we have our hypothesis:

# Hypothesis test a = 0.05
# Ho Null Hypothesis, Ho=100
# Ha Alternative Hypothesis, Ha dif 100

#To get T-test in the old fashion way we will have to follow the formula:
#TS = 
#In R we just have to type the following command:

t.test(y, mu = 100)

#P-value = 0.5569, usually P<=0.05 "statistically significant" result
#Smaller P-Values more strongly contradict the null.
#For this exercise I will not take in consideration P as our n is too small.

#We reject our Null Hypothesis as our Ho is not equal to 100 as we specified
#in our hypothesis. 

#Testing:

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
length(y)
n = length(y)
X = mean(y)
S2 = var(y)
S = sd(y)
SE = sd(y)/sqrt(n)

length(y) = n

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
conf.level <-0.9
z <- qt((1+conf.level)/2,df=25-1)
sd(y)/sqrt(25)
ci <-z*sd

confint(c,parm = 90)
conf.level <-0.9
z <- qt((1+conf.level)/2,df=n-1)
se <-sd(Height)/sqrt(n)
ci <-z*se

m-ci
m+ci




#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)


#####################
# Problem 2
#####################

#Explore the expenditure data set and import data into R

Expenditure <- read.csv("C:/Users/Antonio Felix/OneDrive/Documents/GitHub/StatsI_Fall2023/problemSets/PS01/my_answers/expenditure.csv", header=T)
setwd("C:/Users/Antonio Felix/OneDrive/Documents/GitHub/StatsI_Fall2023/problemSets/PS01/my_answers")

library(readr)
Expenditure <- read_delim("GitHub/StatsI_Fall2023/problemSets/PS01/my_answers/expenditure.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)
View(Expenditure)
library(tidyverse)

#Structure of my data, what type of variables I have, character, numeric, list, etc.
str(Expenditure)
glimpse(Expenditure)

# Number of observations and number of variables 
dim(Expenditure)
length(Expenditure)

View(Expenditure)
head(Expenditure)
tail(Expenditure)


# Selection of variables
# Y. Spending: per capita expenditure on shelters/housing assistance in state 
# X1.Income: per capita personal income in state
# X2.Residents: Number of residents per 100,000 that are "financially insecure" in state
# X3.Urban: Number of people per thousand residing in urban areas in state
# Region: 1=Northeast, 2=North Central, 3=South, 4=West

head(Expenditure)
colnames(Expenditure)[colnames(Expenditure)=="Y"]<-"Spending"
colnames(Expenditure)[colnames(Expenditure)=="X1"]<-"Income"
colnames(Expenditure)[colnames(Expenditure)=="X2"]<-"Residents"
colnames(Expenditure)[colnames(Expenditure)=="X3"]<-"Urban"
head(Expenditure)

summary(Expenditure$Spending)
summary(Expenditure$Income)
summary(Expenditure$Residents)
summary(Expenditure$Urban)

summary(Expenditure$Region)

#Answer of option A
# Relationship among Y, X1, X2, and X3

#For relationship between Y & X1 (see answer C)

#Y & x2 = Expenditure/Spending & Residents "Financially insecure"
hist(Expenditure$Spending) # Distribution
mean(Expenditure$Spending) # Central tendency, mean
var(Expenditure$Spending) # Variability, variance
sd(Expenditure$Spending) # Variability, standard deviation
sd(Expenditure$Spending)/sqrt(length((Expenditure$Spending))) # Variability, standard **error**

hist(Expenditure$Residents)
mean(Expenditure$Residents)
var(Expenditure$Residents)
sd(Expenditure$Residents)
sd(Expenditure$Residents)/sqrt(length((Expenditure$Residents)))

cor(Expenditure$Residents,Expenditure$Spending)

# I think I can delete this line, but not sure if it will affect something
pdf(C:\Users\Antonio Felix\OneDrive\Documents\GitHub\StatsI_Fall2023\problemSets\PS01\my_answers)

pdf("IMAGEP2_A.pdf")
plot(Expenditure$Residents,Expenditure$Spending)
dev.off()

#Y & x2 = Expenditure/Spending & Urban areas
hist(Expenditure$Urban)
mean(Expenditure$Urban)
var(Expenditure$Urban)
sd(Expenditure$Urban)
sd(Expenditure$Urban)/sqrt(length((Expenditure$Urban)))

cor(Expenditure$Urban, Expenditure$Spending)
pdf("IMAGEP2_A1.pdf")
plot(Expenditure$Urban, Expenditure$Spending)
dev.off()
#X1 & X3 = Income/People on Urban areas
cor(Expenditure$Urban, Expenditure$Income)
pdf("IMAGEP2_A2.pdf")
plot(Expenditure$Urban, Expenditure$Income)
dev.off()

#X2 & X3 = Residents "Financially insecure" /People on Urban areas
cor(Expenditure$Residents, Expenditure$Urban)
pdf("IMAGEP2_A3.pdf")
plot(Expenditure$Residents, Expenditure$Urban)
dev.off()

#Answer of option B
# Relationship between Y (Spending=Expenditure) and Region
#Is the personal income related to the region?

cor(Expenditure$Spending,Expenditure$Region)

pdf("IMAGEP2_B1.pdf")
plot(Expenditure$Region,Expenditure$Spending)
dev.off()

pdf("IMAGEP2_B2.pdf")
ggplot(Expenditure,mapping = aes(y = Spending, x = Region))+geom_point()
dev.off()

sum(Expenditure$Spending) #Just to corroborate some calcule about the average


#Answer of option C
#Relationship between Y(Spending) and X1 (Income)

#Questions before the graph or codes
#Does the expenditure on housing assistance is related to the personal income?
#What about the region does it affect the outcome?

hist(Expenditure$Income)
mean(Expenditure$Income)
var(Expenditure$Income)
sd(Expenditure$Income)
sd(Expenditure$Income)/sqrt(length((Expenditure$Income)))

cor(Expenditure$Spending,Expenditure$Income)
plot(Expenditure$Spending,Expenditure$Income) #The order on the code matters
plot(Expenditure$Income,Expenditure$Spending)

library(ggplot2)

ggplot(Expenditure)
ggplot(Expenditure,mapping = aes(x = Income, y = Spending))
ggplot(Expenditure,mapping = aes(x = Income, y = Spending))+geom_point()
ggplot(Expenditure,mapping = aes(x = Income, y = Spending, color = Region
))+geom_point()

ggplot(Expenditure,mapping = aes(x = Income, y = Spending)) +
  geom_point(mapping = aes(color = Region, shape=Region))+
  geom_smooth(method = "lm")

pdf("IMAGEP2_C.pdf")
Expenditure$Region <- as.factor(Expenditure$Region)
ggplot(Expenditure,mapping = aes(x = Income, y = Spending)) +
  geom_point(mapping = aes(color = Region, shape= Region))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("red","blue","green","brown"))+
  scale_shape_manual(values = c("circle","triangle","square","diamond"))
dev.off()





