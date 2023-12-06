#Question 2
#Option B
#Given values
BGDP <- -2
Se_GDP <- 0.00007
critical_value <- qnorm(0.975)  # For a 95% confidence interval

# Calculate margin of error
margin_of_error <- critical_value * Se_GDP

#Calculate confidence interval
confidence_interval <- c(BGDP - margin_of_error, BGDP + margin_of_error)

# Print the result
cat("95% Confidence Interval for the effect of GDP on FDI:", confidence_interval, "\n")

#Option C
# Given values
mean_Education <- 12.04

# Calculate mean GDP as the midpoint of the range
mean_GDP <- (6378.56 + 41031.78) / 2

# Calculate standard deviation of GDP
std_dev_GDP <- (41031.78 - 6378.56) / 2

# Calculate high and low values of GDP (one standard deviation around the mean)
low_GDP <- mean_GDP - std_dev_GDP
high_GDP <- mean_GDP + std_dev_GDP

# Predicted FDI for low and high values of GDP
predicted_FDI_low <- BGDP * low_GDP + mean_Education
predicted_FDI_high <- BGDP * high_GDP + mean_Education

# Difference in predicted FDI
difference_in_predicted_FDI <- predicted_FDI_high - predicted_FDI_low

# Print the result
cat("Difference in predicted FDI between low and high values of GDP:", difference_in_predicted_FDI, "\n")


# Other values given
B0 <- 15.94
BDemocracy <- 4.433
BEducation <- -4.298
N <- 100
SE <- 14.31

#Question 
#Option C

# Given coefficients
B0 <- 2.32
B_well_depth <- 0.07
B_dist100 <- -5.49
B_well_depth_dist100 <- 0.49

# Values for two households
dist100_1 <- 0.36 
dist100_2 <- 2.08 
well_depth <- 1  # Assuming both households have a deep well

# Calculate arsenic levels for each household
arsenic_level_1 <- B0 + (B_well_depth * well_depth) + (B_dist100 * dist100_1) + (B_well_depth_dist100 * well_depth * dist100_1)
arsenic_level_2 <- B0 + (B_well_depth * well_depth) + (B_dist100 * dist100_2) + (B_well_depth_dist100 * well_depth * dist100_2)

# Calculate average difference
average_difference <- (arsenic_level_1 - arsenic_level_2) / 2

# Print the result
cat("Average Difference in Arsenic Levels:", average_difference, "\n")
