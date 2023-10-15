#Problem Set 2

#Answer 1a
#I created the table on R using the values given in the problem set 2
Political_Science <- matrix(c(14,7,6,7,7,1), nrow=2)
print(Political_Science)

#I renamed my rows and columns as in the problem set 2
rownames(Political_Science) <- c("Upper Class","Lower Class")
colnames(Political_Science) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
print(Political_Science)

# X2 test statistic / chi-squared test
chi_sq_result <- chisq.test(Political_Science)
print(chi_sq_result)


#step by step
#X2 test statitic = ∑(O-E)^2/E

#Where E (Expected Frequency) = (Row Total*Column Total) / Gran Total
#Refer to Problem Set 2 (Excel file) for the "manual" calculation
#On R I have to create a new table to place my results from Excel on each row/column

Expected_frequency <- matrix(c(13.50, 7.50, 8.3571, 4.6429, 5.1429, 2.8571), nrow=2)
rownames(Expected_frequency) <- c("Upper Class","Lower Class")
colnames(Expected_frequency) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
print(Expected_frequency)


install.packages("stargazer")
library("stargazer")

output_stargazer <- function(outputfile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), file = outputfile)
}

output_stargazer("Frequency_table.tex", Expected_frequency, type = "latex")


pdf("Expected_frequencyP02AF.pdf", width = 8, height = 4)
plot(0, 0, type = "n", xlim = c(0, 4), ylim = c(0, 1), xlab = "", ylab = "")
text(0.5, 0.9, "Not Stopped", cex = 1.5)
text(1.5, 0.9, "Bribe requested", cex = 1.5)
text(2.5, 0.9, "Stopped/given warning", cex = 1.5)
text(0.5, 0.5, "Upper Class", cex = 1.5)
text(0.5, 0.1, "Lower Class", cex = 1.5)
text(1.5, 0.5, "13.5", cex = 1.5)
text(1.5, 0.1, "7.5", cex = 1.5)
text(2.5, 0.5, "8.3571", cex = 1.5)
text(2.5, 0.1, "4.6429", cex = 1.5)
text(3.5, 0.5, "5.1429", cex = 1.5)
text(3.5, 0.1, "2.8571", cex = 1.5)
dev.off()



#I will do the same for the Chi Squared Statistic

Chi_Squared_Statistic <- matrix(c(0.0185, 0.0333, 0.6648, 1.1967, 0.6706, 1.2071), nrow=2)
rownames(Chi_Squared_Statistic) <- c("Upper Class","Lower Class")
colnames(Chi_Squared_Statistic) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
print(Chi_Squared_Statistic)

output_stargazer <- function(outputfile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), file = outputfile)
}

output_stargazer("Chi_Squared_table.tex", Chi_Squared_Statistic, type = "latex")


#I sum all the chi squared statistic values to get my final answer
#X2 test statitic = ∑(O-E)^2/E
#X2 = 3.79
print(chi_sq_result)

#Degrees of Freedom
# DF = (Number of Rows -1)*(Number of Columns-1)
dim(Political_Science)
DF = (2-1)*(3-1)


#Answer 1B

#Assuming I did not get p_value using the R code:
#chi_sq_result <- chisq.test(Political_Science)
#I can replace the values of chisq and DF into my formula p_value = 1-P(X2<=X2)
#Where my X-suared is 3.7912
#if I go to my statistic table I can see the following number:
#as my df is 2 and my significance level is 0.1 I have 4.610
#Now my formula looks like this p_value = 1-P(3.7912<=4.610)

P_value <- 1-pchisq(3.7912, df = 2)
print(P_value)


#Answer 1C
library(data.table)
standardized_residuals <- chi_sq_result$stdres
print(standardized_residuals)

output_stargazer <- function(outputfile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), file = outputfile)
}

output_stargazer("standardized_residuals.tex", standardized_residuals, type = "latex")


#Answer Question 1 (D)

standardized_residuals <- chi_sq_result$stdres
print(standardized_residuals)


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

