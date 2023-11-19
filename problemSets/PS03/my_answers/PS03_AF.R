head(incumbents_subset)

#Question 1
#1.- Run a regression model where the outcome variable is voteshare and 
#explanatory variable difflog
Regression_1 <- lm(voteshare ~ difflog, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_1)


library(ggplot2)

#2. - Create a scatterplot with regression line
ScatterplotRegression1<-ggplot(incumbents_subset, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Regression 1",
       x = "Difference in Campaign Spending (difflog)",
       y = "Incumbent Vote Share")

# Save Scatterplot as an image
ggsave("Scatterplot_of_Regression_1.pdf", 
       plot = ScatterplotRegression1, 
       width = 6, height = 4, units = "in")

getwd()


#3. - Save the residuals of the model in a separate object.
residuals_1 <- resid(Regression_1)

#Question 2
#1. - Run a regression model where the outcome variable is presvote and 
#the explanatory variable is difflog.
Regression_2 <- lm(presvote ~ difflog, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_2)

#2. - Create a scatterplot with regression line
ScatterplotRegression2<-ggplot(incumbents_subset, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatterplot of Regression 2",
       x = "Difference in Campaign Spending (difflog)",
       y = "Presidential Vote Share")

# Save Scatterplot as an image
ggsave("Scatterplot_of_Regression_2.pdf", 
       plot = ScatterplotRegression1, 
       width = 6, height = 4, units = "in")

#3. - Save the residuals of the model in a separate object.
residuals_2 <- resid(Regression_2)



#Question 3
#1. - Run a regression model where the outcome variable is voteshare and 
#the explanatory variable is presvote.
Regression_3 <- lm(voteshare ~ presvote, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_3)

#2. - Create a scatterplot with regression line
ScatterplotRegression3<-ggplot(incumbents_subset, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of Regression 3",
       x = "Presidential Vote Share",
       y = "Incumbent Vote Share")

# Save Scatterplot as an image
ggsave("Scatterplot_of_Regression_3.pdf", 
       plot = ScatterplotRegression3, 
       width = 6, height = 4, units = "in")


#Question 4
#1. - Run a regression with residuals_1 as the outcome 
# and residuals_2 as the explanatory variable
Regression_4_resid <- lm(residuals_1 ~ residuals_2)

# View the summary of the regression with residuals
summary(Regression_4_resid)

#2. - Make a scatterplot of the two residuals and add the regression line.
ScatterplotRegression4<-ggplot(incumbents_subset, aes(x = residuals_2, y = residuals_1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Scatterplot of Regression 4",
       x = "Residuals from Regression 2",
       y = "Residuals from Regression 1")

# Save Scatterplot as an image
ggsave("Scatterplot_of_Regression_4.pdf", 
       plot = ScatterplotRegression4, 
       width = 6, height = 4, units = "in")

#Question 5
#1. - Run a regression where the outcome variable is the incumbent's voteshare
#and the explanatory variables are difflog and presvote.
Regression_5 <- lm(voteshare ~ difflog + presvote, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_5)

#Exploring the data
summary(incumbents_subset)

#Correlation Matrix
cor_matrix <- cor(incumbents_subset[, c("voteshare", "difflog", "presvote")])
summary(cor_matrix)

# Calculate VIF values
install.packages("car")
library(car)
vif_values <- vif(Regression_5)
print(vif_values)