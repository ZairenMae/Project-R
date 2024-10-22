# Load necessary libraries
library(ggplot2)
library(readxl)

# Read the dataset
data <- read_excel("C:/Users/zai/Downloads/fuel_consumption.xls")

# View the structure of the dataset
head(data)

# Rename the columns for ease of use
colnames(data) <- c("Automobile", "EngineSize", "FuelConsumption")

# Check the structure of the data after renaming
str(data)

# Convert 'EngineSize' and 'FuelConsumption' to numeric (in case they are read as characters)
data$EngineSize <- as.numeric(data$EngineSize)
data$FuelConsumption <- as.numeric(data$FuelConsumption)

# Check for NA or problematic values
sum(is.na(data$EngineSize))  # Check for missing values in EngineSize
sum(is.na(data$FuelConsumption))  # Check for missing values in FuelConsumption

# Remove rows with missing or invalid data (NA, NaN, Inf)
data_clean <- data[!is.na(data$EngineSize) & !is.na(data$FuelConsumption) & is.finite(data$EngineSize) & is.finite(data$FuelConsumption), ]

# View the cleaned data
head(data_clean)

# 1a. Scatter Plot: Fuel Consumption vs Engine Size
ggplot(data_clean, aes(x = EngineSize, y = FuelConsumption)) + 
  geom_point() + 
  labs(x = "Engine Size (litres)", y = "Fuel Consumption (litres/100km)", title = "Scatter Plot of Fuel Consumption vs Engine Size")

# 1b. Linear regression model: Fuel Consumption as a function of Engine Size
model <- lm(FuelConsumption ~ EngineSize, data = data_clean)

# View the model summary (which gives us the regression equation)
summary(model)

# The regression equation is: FuelConsumption = Intercept + (Slope * EngineSize)

# 1c. Plot the regression line
ggplot(data_clean, aes(x = EngineSize, y = FuelConsumption)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Engine Size (litres)", y = "Fuel Consumption (litres/100km)", title = "Regression Line: Fuel Consumption vs Engine Size")

# Compute the coefficient of determination (R²)
r_squared <- summary(model)$r.squared

# Output the R² value
r_squared

