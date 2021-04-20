# Importing the DF

diabetes_data <- read.csv("Diabetes.csv", na = "")

str(diabetes_data)

# Check the missing variables. Delete if there are any
incomplete_data <- diabetes_data[!complete.cases(diabetes_data),]
incomplete_data

# Install the mice package if not already installed
# install.packages("mice")
library(mice)
md.pattern(diabetes_data)

library(VIM)
missing_values <- aggr(diabetes_data, prop = FALSE, numbers = TRUE)

# Show the summary of missing values
summary(missing_values)


# Create a new variable called a Date that contains the month and year data
diabetes_data$Date <- paste(diabetes_data$Month, diabetes_data$Year, sep = '/')
str(diabetes_data$Date)

# Change the date variable to a Date
# Date has a particular requirement as it should contain day, month, year
converted_date <- as.Date(diabetes_data$Date, "%m/Y") # It an ERROR
converted_date
class(converted_date) # Its an error

# we are fixing it
converted_date <- paste(diabetes_data$Month, diabetes_data$Year, sep='/')

# adding the day element
converted_date <- paste("01", diabetes_data$Month, diabetes_data$Year, "01", sep='/')
converted_date
str(converted_date)

diabetes_data$Date <- as.Date(converted_date, "%d/%m/%Y")
str(diabetes_data)

# Plot the status variable using the plot() function
# convert to a factor first
# you can plot the summary() of the data
diabetes_data$Status <- factor(diabetes_data$Status)
str(diabetes_data)
plot(diabetes_data$Status)
summary(diabetes_data$Status)

# Add titles to the chart that are relevant
attach(diabetes_data)
display_settings <- par(no.readonly = TRUE)
plot(Status)
plot(Status, main="Diabetes Status", xlab="Status", ylab="Count")

# Save the modified diabetes DF
write.csv(diabetes_data, file = "diabetes-data-modified.csv")



