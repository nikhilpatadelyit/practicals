# 1 reading the DF for the pdf 18 week 4

diabetes_data <- read.csv("Diabetes-md.csv", na = "")

# 2 structure of the data
str(diabetes_data)

# 2 show the & class from the DF
class(diabetes_data)
dim(diabetes_data)

# 3 analysing the missing data
diabetes_data[!complete.cases(diabetes_data),]

# Install the mice package if not already installed
# install.packages("mice")
library(mice)
md.pattern(diabetes_data)

library(VIM)
missing_values <- aggr(diabetes_data, prop = FALSE, numbers = TRUE)

# Show the summary of missing values
summary(missing_values)
