#Question 1: Economics

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

head(Prestige)
summary(Prestige)

# Create a new variable 'professional' by recoding the 'type' variable
Prestige$professional <- ifelse(Prestige$type %in% c("prof", "bc"), 1, 0)

# View the updated dataset
head(Prestige)

# Run a linear model with prestige as the outcome
Regression_1 <- lm(prestige ~ income * professional, data = Prestige)

# Display the summary of the model
summary(Regression_1)

#Question 2: Political Science 

# Option A:
B1 <- 0.042
SEb1 <- 0.016
N <- 131

# Calculate the test statistic
test_statistic <- B1 / SEb1

# Degrees of freedom
df <- N - 1

# Two-tailed test, so multiply by 2
p_value <- 2 * pt(-abs(test_statistic), df)

# Compare p-value to significance level (e.g., 0.05)
if (p_value < 0.05) {
  print("Reject the null hypothesis: Having yard signs in a precinct affects vote share.")
} else {
  print("Fail to reject the null hypothesis: No evidence that yard signs affect vote share.")
}

# Option B:
B2 <- 0.042
SEb2 <- 0.013

# Calculate the test statistic
test_statistic <- B2 / SEb2

# Degrees of freedom
df <- N - 1

# Two-tailed test, so multiply by 2
p_value <- 2 * pt(-abs(test_statistic), df)

# Compare p-value to significance level (e.g., 0.05)
if (p_value < 0.05) {
  print("Reject the null hypothesis: Being next to precincts with yard signs affects vote share.")
} else {
  print("Fail to reject the null hypothesis: No evidence that being adjacent to yard signs affects vote share.")
}
