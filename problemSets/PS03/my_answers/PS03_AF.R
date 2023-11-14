head(incumbents_subset)

#Question 1
#1.- Run a regression model where the outcome variable is voteshare and 
#explanatory variable difflog
Regression_1 <- lm(voteshare ~ difflog, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_1)


library(ggplot2)

#2. - Create a scatterplot with regression line
ggplot(incumbents_subset, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Regression 1",
       x = "Difference in Campaign Spending (difflog)",
       y = "Incumbent Vote Share")

#3. - Save the residuals of the model in a separate object.
residuals_1 <- resid(Regression_1)

#Question 2
#Run a regression model where the outcome variable is presvote and 
#the explanatory variable is difflog.
Regression_2 <- lm(presvote ~ difflog, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_2)

# Create a scatterplot with regression line
ggplot(incumbents_subset, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatterplot of Regression 2",
       x = "Difference in Campaign Spending (difflog)",
       y = "Presidential Vote Share")

#Save the residuals of the model in a separate object.
residuals_2 <- resid(Regression_2)

#Question 3
# Run a regression with residuals_1 as the outcome 
# and residuals_2 as the explanatory variable
regression_resid <- lm(residuals_1 ~ residuals_2)

# View the summary of the regression with residuals
summary(regression_resid)

#Make a scatterplot of the two residuals and add the regression line.
ggplot(incumbents_subset, aes(x = residuals_2, y = residuals_1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Scatterplot of Regression 2",
       x = "Residuals from Regression 2",
       y = "Residuals from Regression 1")

#Question 4
#Run a regression model where the outcome variable is voteshare and 
#the explanatory variable is presvote.
Regression_3 <- lm(voteshare ~ presvote, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_3)

# Create a scatterplot with regression line
ggplot(incumbents_subset, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of Regression 3",
       x = "Presidential Vote Share",
       y = "Incumbent Vote Share")

#Question 5
#Run a regression where the outcome variable is the incumbent's voteshare
#and the explanatory variables are difflog and presvote.
Regression_5 <- lm(voteshare ~ difflog + presvote, data = incumbents_subset)
#Get summary of model with coefficient estimates
summary(Regression_5)