# Use data set "women" containing
# Conatians height and weight data for 15 owmen between 30 - 39
# Dependent var = weight
# independent var = height

# Linear model syntax = (dependent ~ Independent)
attach(women)
simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model

plot(height, weight, 
     main = "Scatter plot showing regression line for weight predicted from height", 
     xlab = "Height(inches)", ylab = "Weight(lbs)")

# Line created with Simple lm for the defined varaibles
abline(simple_linear_model)
summary(simple_linear_model)

# No of **** shows the significance in the linear model

# weight woman = -87.52 + 3.45 * Height

# Correlation coefficient of the variables
confint(simple_linear_model)
cor(height, weight)

# If the RSE value is low then the model best fits the data
# Higher R-squared value higher will be the chances of good model
# Mulitple linear regression we will use adjusted r-square
# overall significance of the model - F statistic
# if p-value < cut-off value then it is highly significance
# Diagonise the model
############

######################## CARS DATA ########################
# Cars Dataset - Predict car stopping distance from speed
###########################################################

# First step -checking model assumptions
# For Linear Model:
# Linearity -  linear relationship or not ?
# Normality - If residuals are ND or not ?
# Homoscedasticity - If residuals have constant variance or not ?
# No collinearity - not a linear combination of each other
# Independence - Residuals are independent and not correlated

# Checking linearity
# independent = x-axis (speed)
# dependent = y-axis (distance)
attach(cars)
str(cars)
scatter.smooth(x = speed, y = dist, main = "Distance ~ Speed", 
               xlab = "Car Speed(mph)", 
               ylab = "Stopping distance(feet)")
# One of the value increases/decreases other value also increases/decreases (VICE-VERSA)

# Checking Correlation
cor(speed, dist)
# It is giving a high correlation value = 0.80
# Low correlation = value equal to zero

# Checking Outliers (value far away from the line)
# outlier = 1.5* IQR
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # 1 row * cols
boxplot(speed, main = "Speed", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(speed)$out))

boxplot(dist, main = "Distance", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(dist)$out))

par <- opar

# We want to remove 1 outlier where dist = 120
cars <- subset(cars, cars$dist != 120)
nrow(cars)

# Checking Normality
install.packages("e1071")
library(e1071)

# Skewness < -1 or < 1 = Highly Skewed
# Moderately skewed = -1 to -0.5 & 0.5 to 1
# Highly Skewed = < -1 or > 1
# -0.5 to 0.5 = approx symmetrical
opar <- par(no.readonly = TRUE)
plot(density(speed), 
     main = "Density plot for Speed", 
     ylab = "Frequency", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(speed), 2)))

# Fill in the area under the plot with red
polygon(density(speed), col = "red")
# Skewness = -0.11 = approx symmetrical

plot(density(dist), 
     main = "Density plot for Distance", 
     ylab = "Frequency", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(dist), 2)))


# Fill in the area under the plot with red
polygon(density(dist), col = "red")
# Skewness = 0.5 = approx symmetrical

par <- opar

# Using qqnorm
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
hist(dist)
qqline(dist)
par = opar

# Create Training and Tesing Datasets
set.seed(1)
no_rows_data <- nrow(cars)
data_sample <- sample(1:no_rows_data, 
                      size = round(0.7 * no_rows_data), 
                      replace = FALSE)
# Replace
# Dont put the record into the sample again = FALSE
# Do put the record back into the sample = TRUE

training_data <- cars[data_sample, ]
testing_data <- cars[-data_sample, ]
# 34 + 15 = 49

# Linear model syntax = (dependent ~ Independent)
linear_model <- lm(dist ~ speed, data = training_data)
linear_model

summary(linear_model)
# No of **** shows that the data is more significance in the linear model

# Prediction
predicted_distance <- predict(linear_model, testing_data)
predicted_distance

actuals_predictions <- data.frame(cbind(actuals = testing_data$dist, 
                                        predicted = predicted_distance))


head(actuals_predictions, 15)

correllation_accuracy <- cor(actuals_predictions)
correllation_accuracy











