# reading the DF
managers_data <- read.csv("managers.csv")

new_managers_data <- read.csv("MoreData.csv", na = "")

# structure of the data
str(new_managers_data)

# show the headers from the DF
names(managers_data)
names(new_managers_data)

include_list <- new_managers_data[c("Date",
  "Country",
  "Gender",
  "Age",
  "Q1",
  "Q2",
  "Q3",
  "Q4",
  "Q5")]
include_list

# This how we will combine the DF
# but right now it is not working
rbind(managers_data, include_list)

str(include_list)

# Create a new "AgeCat" variable in include_list
# & calculate the containing values
attach(include_list)
include_list$AgeCat[Age >= 45] <- "Elder"
include_list$AgeCat[Age >= 26 & Age <= 44] <- "Middle Aged"
include_list$AgeCat[Age <= 25] <- "Young"

# If NA is found, categorise as "Elder"
include_list$AgeCat[is.na(Age)] <- "Elder"
detach(include_list)

str(managers_data)
str(modified_managers)

names(managers_data)
modified_managers <- managers_data[2:11]
modified_managers

# Update the datefields on both DF so that
# they are in correct format
modified_managers$Date <- as.Date(modified_managers$Date, format = "%m/%d/%y")
str(modified_managers)

include_list$Date <- as.Date(include_list$Date, format = "%m/%d/%Y")
str(include_list)

# Combine both DF
combined_managers <- rbind(modified_managers, include_list)
str(combined_managers)

# Set AgeCat with ordered factor
# so that young < middle aged < elder

combined_managers <- factor(combined_managers$AgeCat, 
                           levels = c("Young", "Middle Aged", "Elder"), ordered = TRUE)
str(combined_managers)


