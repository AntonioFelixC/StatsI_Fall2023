#Problem Set 2
x <- c(14,6,7)
y <- c(7,7,1)


# Expected frequencies under the assumption of independence
expected_counts <- chisq.test(observed_counts)$expected


# correlation = Pearson Correlation Coefficient
# r = rxy = ∑XY-nXbarYbar/sqrt(∑X^2-nXbar^2)


# Fastest way on R
r <- cor(x,y)
print(r)

#Step by step

#PEARSON CORRELATION COEFFICIENT Formula
# rxy <- (n)(∑XY)-(∑X)(∑Y)/sqrt[(n)(∑X2)-(∑X)^2)][(n)(∑Y2)-(∑Y)^2]
# We have to find the values for each one:

n <- length(x)
print(n)

#∑XY = SumXY represents the sum of the products of X and Y.
SumXY <- sum(x*y)
print(SumXY)

SumX <- sum(x)
print(SumX)

SumY <- sum(y)
print(SumY)

SumX2 <- sum(x^2)
print(SumX2)

SumY2 <- sum(y^2)
print(SumY2)

# rxy <- (n)(∑XY)-(∑X)(∑Y)/sqrt[(n)(∑X2)-(∑X)^2)][(n)(∑Y2)-(∑Y)^2] 
# As I can't used the symbol ∑ in R I haVe to change it to sum 
rxy <- (n*SumXY-SumX*SumY) / sqrt((n*SumX2-SumX^2)*(n*SumY2-SumY^2))
print(rxy)

# rxy <- (∑XY)-(n)(XYbar)/sqrt[(∑X^2)-(n)(Xbar^2)sqrt(∑Y^2)-(n)(Ybar^2)
#Formula given in the slides

X_bar <- mean(x)
print(X_bar)

Y_bar <- mean(y)
print(Y_bar)

XY_bar <- mean(x*y)
print(XY_bar)

rxy_slides <- (SumXY - n * XY_bar / sqrt((SumX^2 - n * X_bar^2) 
                                         * SumY^2 - n * Y_bar^2))
print(rxy_slides)

#Sorry but I will stick with the original formula

#test statistic = t = r*sqrt(n-2/sqrt(1-r^2))
t_stat <- (r*sqrt(n-2)/sqrt(1-r^2))
print(t_stat)

#Answer Question 1 (B)

df <- n-2
p_value <- 2*(1-pt(abs(t_stat),df))
print(df)
print(p_value)

#Since my p-value (.7399) is greater than my significance level (0.1), I fail
#to reject my null hypothesis. I do not have enough evidence to conclude that
#there is a significant linear relationship between my variables.


#Answer Question 1 (C)
library(data.table)

#Linear Regression Model

#Data frame with my X and Y values
Political_Science <- data.frame (Class = c("Upper class", "Lower class"),
  "Not Stopped" = c(14, 6), "Bribe requested" = c(7, 7),
  "Stopped/given warning" = c(1, 0))
print(Political_Science)

# Standardized residuals
residuals <- Political_Science[, -1]  # Exclude the Class column
residuals <- as.data.table(residuals)
residuals <- residuals - mean(unlist(residuals))
residuals <- residuals / sd(unlist(residuals))

# Second table for standardized residuals
residuals_table <- data.frame(
  Class = Political_Science$Class,
  "Not Stopped" = round(residuals[, 1], 2),
  "Bribe requested" = round(residuals[, 2], 2),
  "Stopped/given warning" = round(residuals[, 3], 2)
)

print(Political_Science)
print(residuals_table)

#Answer Question 1 (D)

lm_model <- lm(y ~ x)

#Residuals
residuals <-(resid(lm_model))

#Standard error of residuals
standard_error_residuals <- sd(residuals)

standardized_residuals <- residuals / standard_error_residuals
print(standardized_residuals)

#It helps me to identify outliers (I can identify data points that are far from
#the mean)

#Answer Question 2

#Answer 2a (Response is on TexStudio)
#To get to that hyphothesis firt I had to analyze my data, here is the code:

url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- fread(url)

#Notes: fread = "fast read" / url has to be on quotation marks
print(data)
head(data)
View(data)

# Number of observations and number of variables 
dim(data)
length(data)


library(tidyverse)

Prediction <- data
head(Prediction)
summary(Prediction$GP)
summary(Prediction$irrigation)
summary(Prediction$water)

hist(Prediction$GP)
hist(Prediction$irrigation)
hist(Prediction$water)

pdf("IMAGEP2_A1.pdf")
plot(Prediction$GP,Prediction$irrigation)
dev.off()

pdf("IMAGEP2_A1.pdf")
plot(Prediction$GP,Prediction$water)
dev.off()

pdf("IMAGEP2_A1.pdf")
plot(Prediction$reserved,Prediction$water)
dev.off()


#Answer 2b
# Y = β0 + β1*X + ε (Biviarate Regression)
pdf("Regression_model.pdf")
Regression_model <- lm(water ~ reserved, data = data)
summary(Regression_model)
dev.off()

file_path <- "data.txt"
write.csv(data,file = file_path, row.names = FALSE)
getwd()

#Step by step

X_Data <- data$reserved
Y_Data <- data$water
X_Bar  <- mean(X_Data)
Y_Bar <- mean(Y_Data)

#Getting B1 (The Coefficient)
B1_Coefficient <- sum((X_Data - X_Bar) * (Y_Data - Y_Bar)) / sum((X_Data - X_Bar)^2)
print(B1_Coefficient)

#Getting B0 (The Intercept)
B0_Intercept <- Y_Bar - B1_Coefficient * X_Bar
print(B0_Intercept)

# Y = β0 + β1*X 
Y_hat <- B0_Intercept + B1_Coefficient * X_Data
print(Y_hat)

# The second part of the formula + E, Residuals (Errors = E)
E <- Y_Data - Y_hat
print(E)


#Answer 2c
#See on TexStudio

#Extra (If my research were focus on the irrigation)

Regression_model <- lm(irrigation ~ reserved, data = data)
summary(Regression_model)

# As p-value is 0.7422 is greater than any significance level I choose (.05 or .01)
# I do not have enough evidence to reject my Null Hypothesis (H0) that the 
# reservation policy has no effect on the number of new or repaired 
# drinking water facilities in the village. 

# Saying in other words and focusing on my Alternative Hypothesis (H1)
# My data does not provide me with sufficient evidence to to conclude
# that the reservation policy has a significant effect on the number
# of new or repaired drinking water facilities in the villages

