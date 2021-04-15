# 1 reading the DF for the pdf 18 week 4

diabetes_data <- read.csv("Diabetes-md.csv", na = "")

# 2 structure of the data
str(diabetes_data)

# 2 show the & class from the DF
class(diabetes_data)
dim(diabetes_data)

# 3 analyse the missing data
diabetes_data[!complete.cases(diabetes_data),]

# Install the mice package if not already installed
# install.packages("mice")
library(mice)
md.pattern(diabetes_data)

library(VIM)
missing_values <- aggr(diabetes_data, prop = FALSE, numbers = TRUE)

# Show the summary of missing values
summary(missing_values)

# Dealing with missing data
# i have decide to keep data 
# with missing type & heath status
# & the address are not imp.
keep_this_data <- diabetes_data[!complete.cases(diabetes_data$Daibetes.type, diabetes_data$Status),]
keep_this_data
dim(keep_this_data)
# 15 rows with missing data that i like to keep

# reverse the logic contain relevant data
keep_this_data <- diabetes_data[complete.cases(diabetes_data$Daibetes.type, diabetes_data$Status),]
keep_this_data

dim(keep_this_data)
dim(diabetes_data)

# replace the original DF with the content of the processed data
diabetes_data <- keep_this_data
head(diabetes_data, 15)

# 4 Configure the names of the attributes
diabetes_data$Daibetes.type <- factor(diabetes_data$Daibetes.type, order = FALSE, levels = c("Type 1", "Type 2"))

# refactor status
diabetes_data$Status <- factor(diabetes_data$Status, order = TRUE, levels = c("Poor", "Improved", "Excellent"))

# 5 Define the new columns
col_names <- c("Patient Name", "NI address", "Type", "Age", "Health status")
colnames(diabetes_data) <- col_names
str(diabetes_data)

# lets look at the class of each element into the DF
class_list <- lapply(diabetes_data, class)
class_list

class_list <- sapply(diabetes_data, class)
class_list


# Build the DF
dose <- c(20, 30, 40, 45, 60)
drug_a <- c(16, 20, 27, 40, 60)
drug_b <- c(15, 18, 25, 31, 40)

# Create a new DF
drugs <- data.frame(dose, drug_a, drug_b)
str(drugs)
class(drugs)

# General plot of data
plot(drugs)

attach(drugs)
plot(dose, type = "o", col = "blue")
?plot

# option type = "b" shows both lines should be plotted
plot(dose, drug_a, type = "b")

# par - customize any settings such as fonts, colors etc
# par = parameters 

# store the default content of par before we make any changes to them
opar <- par(no.readonly = TRUE)

# lty = line type
# lwd = line width
# lty = 2 dashed line
# pch = 17 solid traingle
par(lty = 2, pch = 17)
plot(dose, drug_a, type = "b")
par = opar

plot(dose, drug_a, type = "b", lty = 2, pch = 17)

graph_range <- range(0, drug_a, drug_b)
graph_range
plot(drug_a, type = "o", col = "blue", ylim = graph_range, axes = FALSE, ann = FALSE)
lines(drug_b, type = "o", pch = 22, lty = 2, col ="red")

# make the x axis labels
axis(1, at = 1:5, labels = c("20 ml", "40 ml", "60 ml", "80 ml", "100 ml"))

# make the y axis that displays ticks at every 5 marks
# marks = 5 * 
axis(2, las = 1, at = 5 * 0:graph_range[2])





