# Use statistical methods to examine
# the relationship between to variables of interest

# BEAVERS DATASET
# contains the body temp of 4 beavers captures every 10 mins
# over a day
# we want to examine difference in average body temp
# to evaluate whether body temp is affected by activity

# Q1
# H0: mean beavers temp. is not affected by activity
# H1: mean beaver temp. is affected by activity

?beavers
str(beaver2)

# vars we'll need
# We'll evaluate beaver temp and activity
# temp = continous variable
# activty - categorical dicotomous variable

# Copy the data into a DF
beavers_data <- beaver2
str(beavers_data)

# Convert the activ variable to
# a categorical dichotomous variable
beavers_data$activ <- factor(beavers_data$activ, labels = c("No", "Yes"))
str(beavers_data)

# Installing the psych
install.packages("psych")
library(psych)

pairs.panels(beavers_data, 
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = TRUE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered
             factor = 2, # Jittering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals
                                           

# Q2
attach(beavers_data)
plot(activ, temp, pch = 9, col = "LightBlue")


# We can split the dichotomous variable into 2
# & then examine the data
library("lattice")

# 
histogram(~temp | activ, 
          data = beavers_data, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", ylab = "Activity")

# Visual analysis seems to indicate that the data is Normally Distributed
# summarise the
tapply(temp, activ, median)

# Quantile-quantile plot (Q-Q-Plot) allows us to check
# if the data is ND or not

# Is temp ND?
qqnorm(temp)
# Add line that represent the ND
qqline(temp, col = "red")

# Temp appears not to be ND

with(beavers_data, 
     qqplot(temp[activ == "Yes"], 
            temp[activ == "No"], 
            main = "Comparing 2 samples of activity data", 
            xlab = "Active Temp = Yes", 
            ylab = "Active Temp = No"))

# We can add normality line to the plot
# to help evaluate normality
with(beavers_data, {
  qqnorm(temp[activ == "No"], 
         main = "Inactive Data")
  qqline(temp[activ == "No"])})

with(beavers_data, {
  qqnorm(temp[activ == "Yes"], 
         main = "Active Data")
  qqline(temp[activ == "Yes"])})


# formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample
# comes from a ND
# If p> 0.05 = normally distributed
normality_test <- shapiro.test(beavers_data$temp)
normality_test$p.value
# p-value = 7.763623e-05

# This test does not work on a dichotomous variable
with(beavers_data, tapply(temp, activ, shapiro.test))

# Results show
# No = p-value = 0.1231 - ND
# Yes = p-value = 0.5583 - ND
# temp = not ND
# activ = ND

# After consulting the chart, I am aiming
# a dependent var(temp)
# with a independent categorical var(activ)
# Format wilcox.test(dependent var ~ independent var)
wilcox.test(temp~activ)
# cut-off = 0.05
# p-value < 2.2e-16 (2.2 power -16) = 0.0000000000022
# p-value < 0.05 = Reject the H0

# p-value < 0.05 so this indicates the 
# Null (H0) hypothesis is rejected
# therefore this indicates that
# beaver body temp is affected by activity (p = 2.26e-16)

# mean beavers temp. is not affected by activity

































