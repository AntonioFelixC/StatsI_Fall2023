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

#In order to get the Confidence Interval for population mean (CI)
#We have to understand the formula and its elements
#To get the Standard Error (SE), we need the Standard Deviation (S) and 
#To get the S we need to get the Variance (S^2)

#The first thing I did, it was to answer this problem set on paper & using Excel
#You will find a copy of the Excel file in the same folder
#Second learn the commands in R and corroborate the results

#To get the S^2 we need to know the Sample Size (n), Y1,Y2,...Yn (Observations)
#and the Y (Sample mean)
#Note: sometimes we find X instead of Y in the formula that does not matter

#To calculate the mean we just have to use the following command:
#We previously said y is our universe of observations I will put the # of row here

mean(y)
#We get 98.44 as the answer 

#To get the formula of the variance done we just have to type var(y)
#Actually we could have jumped into Variance but is useful to have
#We don't have to do it by parts but is useful to visualize the mean and n
#Also we will need both for our final formula CI

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




