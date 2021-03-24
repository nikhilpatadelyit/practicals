# File downloaded from BB and inserted into DF
# Missing content replace with NA
managers_data <- read.csv("managers.csv", na = "")

# View the structure of the DF
str(managers_data)

# Convert date to a date variable
# It is currently in mm/dd/yy
converted_date <- as.Date(managers_data$Date, "%m/%d/%y")
converted_date
str(converted_date)

# Replace the date field in the DF
managers_data$Date <- converted_date
str(managers_data)

# Convert AGE variable to int
managers_data$Age <- as.integer(managers_data$Age)
str(managers_data)

# Select records within 15-10-18 and 01-11-18
start_date <- as.Date("2018-10-15")
end_date <- as.Date("2018-11-01")
new_date <- managers_data[
  managers_data$Date >= start_date & 
    managers_data$Date <= end_date,]
new_date

#new_date <- managers_data[
 # managers_data$Date >= as.Date("2018-10-15") &
 #   managers_data$date <= as.Date("2018-11-01"),]

names(managers_data)
# Drop attributes (var) from data
# shows where specific var names are
# new_data <- managers_data[c(8, 9)]
include_list <- names(managers_data) %in% c("Q3", "Q4")
include_list

# This DF only contains Q3 and Q4
new_managers <- managers_data[!(include_list)]
new_managers
str(new_managers)

# Using the subset function
# To extract all records where age >35 or age <24
# only select Q1 -Q4
attach(managers_data)
new_data <- subset(
  managers_data, Age > 35 | Age < 24, 
  select = c(Q1, Q2, Q3, Q4))
new_data
detach(managers_data)

# Select a subset of managers_data 
# where gender = M and age > 25 
# only show records from gender to Q4 inclusive
attach(managers_data)
gender_data <- subset(
  managers_data, Age > 25 & Gender == 'M', select = Gender:Q4)
gender_data

















