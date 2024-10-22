# Load necessary libraries
library(ggplot2)
library(readxl)

# Read the dataset
data <- read_excel("C:/Users/zai/Dropbox/PC/Downloads/Disposal-1.xls")

# Preview the first few rows
head(data)
DisposableIncome <- data$`Disposable Income`
VehicleSales <- data$`Vehicle Sales`

# 1a. Plot a scatter diagram
ggplot(data, aes(x=DisposableIncome, y=VehicleSales)) + 
  geom_point() + 
  labs(x="Disposable Income", y="Vehicle Sales", title="Scatter Plot of Disposable Income vs Vehicle Sales")

# 1b. Determine the regression equation
model <- lm(VehicleSales ~ DisposableIncome, data=data)
summary(model)

# 1c. Plot the regression line
ggplot(data, aes(x=DisposableIncome, y=VehicleSales)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  labs(x="Disposable Income", y="Vehicle Sales", title="Regression Line: Disposable Income vs Vehicle Sales")

# 1d. Compute the predicted vehicle sales for disposable income of 16,500 and 17,900
predicted_16500 <- predict(model, data.frame(DisposableIncome=16500))
predicted_17900 <- predict(model, data.frame(DisposableIncome=17900))
predicted_16500
predicted_17900

# 1e. Compute the coefficient of determination (R^2) and coefficient of correlation
r_squared <- summary(model)$r.squared
correlation <- cor(DisposableIncome, VehicleSales)
r_squared
correlation
