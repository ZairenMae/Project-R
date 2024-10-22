# Load necessary libraries
library(ggplot2)
library(readxl)

# Read the dataset
data <- read_excel("C:/Users/zai/Downloads/horsepower.xls")

# Rename columns to fix the issues (removing spaces, and making them more accessible)
colnames(data) <- c("Automobile", "EngineSize", "Horsepower")

# View the structure of the dataset
str(data)

# Check for missing (NA), NaN, or infinite values in the relevant columns
sum(is.na(data)) # Check how many NAs there are
sum(is.nan(as.numeric(data$EngineSize))) # Check for NaNs in EngineSize
sum(is.nan(as.numeric(data$Horsepower))) # Check for NaNs in Horsepower

# Ensure that EngineSize and Horsepower are numeric
data$EngineSize <- as.numeric(data$EngineSize)
data$Horsepower <- as.numeric(data$Horsepower)

# After conversion, recheck the structure
str(data)

# Check again for any remaining NA or NaN values
sum(is.na(data$EngineSize))
sum(is.na(data$Horsepower))

# Remove rows with NA, NaN, or Inf values in either EngineSize or Horsepower
data_clean <- data[!is.na(data$EngineSize) & !is.na(data$Horsepower), ]

# Also, remove infinite values
data_clean <- data_clean[is.finite(data_clean$EngineSize) & is.finite(data_clean$Horsepower), ]

# Preview the cleaned dataset
head(data_clean)

# 1a. Scatter Plot: Horsepower vs Engine Size
ggplot(data_clean, aes(x = EngineSize, y = Horsepower)) + 
  geom_point() + 
  labs(x = "Engine Size (litres)", y = "Horsepower", title = "Scatter Plot of Horsepower vs Engine Size")

# 1b. Linear regression model: Horsepower as a function of Engine Size
model <- lm(Horsepower ~ EngineSize, data = data_clean)

# View the summary of the model (which includes the regression equation)
summary(model)

# The regression equation can be extracted from the model summary:
# Formula is: Horsepower = Intercept + (Slope * EngineSize)

# 1c. Plot the regression line
ggplot(data_clean, aes(x = EngineSize, y = Horsepower)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Engine Size (litres)", y = "Horsepower", title = "Regression Line: Horsepower vs Engine Size")

# Compute the coefficient of determination (R²)
r_squared <- summary(model)$r.squared

# Output the R² value
r_squared

