# Date: 080824
# Script for validating NB regression assumption
# Run for Safety Science Revision 1 draft
install.packages("httpgd")

library(fixest)
library(ggplot2)

# Assuming your model is already fitted and named m2.1

# Step 1: Extract residuals and fitted values
residuals <- residuals(m2.1)
fitted_values <- fitted(m2.1)

# Step 2: Create a data frame with fitted values and residuals
plot_data <- data.frame(
  fitted = fitted_values,
  residuals = residuals
)
print(nrow(plot_data))

# subset the data to remove outliers
plot_data <- subset(plot_data, fitted <= 5)
print(nrow(plot_data))


# plot_data$fitted
# Create the residual plot
png("residual_plot.png", width = 500, height = 600) # Open a PNG device

plot(plot_data$fitted, plot_data$residuals,
  main = "Residual Plot for Negative Binomial Regression",
  xlab = "Fitted Values",
  ylab = "Residuals",
  pch = 20, # Use small dots for points
  col = rgb(0, 0, 1, 0.5)
) # Blue with 50% transparency

# Add a horizontal line at y = 0
abline(h = 0, col = "red", lty = 2)

# Add a smooth trend line
# smooth_line <- lowess(plot_data$fitted_values, plot_data$residuals)
# lines(smooth_line, col = "green", lwd = 2)

dev.off() # Close the PNG device

# print head
head(plot_data)

# Now running a poisson regression and compare it with the NB regressino residual plot 
m3.1= fepois(Error_Count ~ TotalWL + Auto_fraction 
               +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP
             | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
m3.2= fepois(Error_Count ~ TotalWL + Auto_fraction + Auto_fraction_sq+TotalWL_sq
               +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+TRAF_COMP
               | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

m3.3= fepois(Error_Count ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq
               + PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP
              | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

etable(m3.1,m3.2,m3.3)




# Step 1: Extract residuals and fitted values
residuals <- residuals(m3.1)
fitted_values <- fitted(m3.1)

# Step 2: Create a data frame with fitted values and residuals
plot_data <- data.frame(
  fitted = fitted_values,
  residuals = residuals
)
print(nrow(plot_data))

# subset the data to remove outliers
# plot_data <- subset(plot_data, fitted <= 100)
# print(nrow(plot_data))


# plot_data$fitted
# Create the residual plot
png("residual_plot_Poisson.png", width = 500, height = 600) # Open a PNG device

plot(plot_data$fitted, plot_data$residuals,
     main = "Residual Plot for Poisson Regression",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, # Use small dots for points
     col = rgb(0, 0, 1, 0.5)
) # Blue with 50% transparency

# Add a horizontal line at y = 0
abline(h = 0, col = "red", lty = 2)

# Add a smooth trend line
# smooth_line <- lowess(plot_data$fitted_values, plot_data$residuals)
# lines(smooth_line, col = "green", lwd = 2)

dev.off() # Close the PNG device

# print head
head(plot_data)
