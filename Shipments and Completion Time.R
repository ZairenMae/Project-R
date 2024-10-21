# Load necessary libraries
library(ggplot2)
library(readxl)

# Read the dataset
data <- read_excel("C:/Users/zai/Downloads/shipments_completion_time.xls")

# Preview the first few rows to inspect the column names
head(data)

# Clean the data
# It seems like row 1-3 might be metadata, so let's remove them
# We'll also rename the columns based on the proper header row (row 4)
colnames(data) <- c("Day", "CompletionTime", "Shipments")

# Convert columns to numeric (some might have been read as character types)
data$CompletionTime <- as.numeric(data$CompletionTime)
data$Shipments <- as.numeric(data$Shipments)

# Remove rows with missing values
data_clean <- data[!is.na(data$CompletionTime) & !is.na(data$Shipments), ]

# 1a. Scatter Plot: Completion Time vs Shipments
ggplot(data_clean, aes(x=CompletionTime, y=Shipments)) + 
  geom_point() + 
  labs(x="Completion Time", y="Shipments", title="Scatter Plot of Shipments and Completion Time")

# 1b. Determine the regression equation
model <- lm(Shipments ~ CompletionTime, data=data_clean)
summary(model)

# 1c. Plot the regression line
ggplot(data_clean, aes(x=CompletionTime, y=Shipments)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  labs(x="Completion Time", y="Shipments", title="Regression Line: Shipments and Completion Time")

# 1d. Compute the predicted number of Shipments for Completion Times of 300 and 900
# Use the regression model to predict Shipments based on CompletionTime
predicted_300 <- predict(model, newdata = data.frame(CompletionTime=300))
predicted_900 <- predict(model, newdata = data.frame(CompletionTime=900))

# Output the predicted values
predicted_300
predicted_900

# 1e. Compute the coefficient of determination (R^2) and the correlation coefficient
r_squared <- summary(model)$r.squared
correlation <- cor(data_clean$CompletionTime, data_clean$Shipments)

# Output R^2 and correlation coefficient
r_squared
correlation
