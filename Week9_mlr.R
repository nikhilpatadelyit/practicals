# Working on USA Murder Rates
# For MLR(Multiple Linear Regression)

# Checking the model assumptions
# First step -checking model assumptions
# For Linear Model:
# Linearity -  linear relationship or not ?
# Normality - If residuals are ND or not ?
# Homoscedasticity - If residuals have constant variance or not ?
# No collinearity - not a linear combination of each other
# Independence - Residuals are independent and not correlated

# use the state.x77 dataset- base package
help("state.x77")
head(state.x77, 15)
class(state.x77)

# convert to a DF
states <- as.data.frame(state.x77)
class(states)
head(states, 15)
str(states)

colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"
str(states)

# Check for NA's
any(is.na(states))
# Also use the VIM or mice if any NA's

# Check the linearity
# variables choosem that will be used for the model
names(states)
pairs(states)

# we could remove a subset of the data first
# choose the vars and then show them in the paris() function
variables_of_interest <- c("Murder", 
                           "Population", 
                           "HS_Grad", 
                           "Illiteracy", 
                           "Income", 
                           "Life_Exp", 
                           "Area", 
                           "Frost")

# To visualize the distribution and correlation
pairs(states[variables_of_interest])

# Install library
library(psych)
# To visualize the distribution and correlation
pairs.panels(states, 
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


# Independent var = x-axis
# Dependent var = y-axis
attach(states)
scatter.smooth(x = Murder, y = Population, 
               main = "Murder ~ Population", 
               xlab = "Murder (per 100,000)",
               ylab = "Population (estimate)")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Murder, Population)
# It is giving a medium correlation value = 0.34
# The correlation tests shows that the corre. between the murder & 
# Population variable = 0.34 indicating a medium correlation.

# Low correlation = value equal to zero
# High Correlation = straight line in plot (value > 0.60) 
# (positive & negative)
# No correlation = No correlation no line

# Visualize
scatter.smooth(x = Murder, y = Frost, 
               main = "Murder ~ Frost", 
               xlab = "Murder (per 100,000)",
               ylab = "Frost (mean min temp below freezing")
# The plot shows there is negative correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Murder, Frost)
# It is giving a strong negative correlation value = -0.53
# The correlation tests shows that the corre. between the murder & 
# Frost variable = -0.53 indicating a negative correlation.

# To check the correlation for all the variables from the DF
paste("Correlation for the murder & frost:", cor(Murder, Frost))
paste("Correlation for the murder & illiteracy:", cor(Murder, Illiteracy))
paste("Correlation for the murder & population:", cor(Murder, Population))
paste("Correlation for the murder & hs_grad:", cor(Murder, HS_Grad))
paste("Correlation for the murder & income:", cor(Murder, Income))
paste("Correlation for the murder & life_exp:", cor(Murder, Life_Exp))
paste("Correlation for the murder & area:", cor(Murder, Area))

# Write the result for the corr. after observing the values for corr.

# Decided to remove the variable(Area) based on the values of the corr.
states <- subset(states, select = -c(Area))
head(states)

# Check the outliers
opar <- par(no.readonly = TRUE)

# To see the outliers
par(mfrow= c(2, 4)) # showing the charts with 2rows * 4cols

boxplot(Murder, 
        main = "Murder", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Murder)$out))

boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Population)$out))
# There is an outlier

boxplot(HS_Grad, 
        main = "Graduation", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(HS_Grad)$out))

boxplot(Income, 
        main = "Income", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Income)$out))
# There is an outlier

boxplot(Illiteracy, 
        main = "Illiteracy", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Illiteracy)$out))

boxplot(Frost, 
        main = "Frost", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Frost)$out))

boxplot(Life_Exp, 
        main = "Life_Expectancy", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Life_Exp)$out))

par(opar)

# use the boxplot.stats() function to extract 
# the outliers for population
outlier_values <- boxplot.stats(Population)$out
paste("Population Outliers: ", 
      paste(outlier_values, 
            collapse = ", "))

# use the boxplot.stats() function to extract 
# the outliers for Income
outlier_values <- boxplot.stats(Income)$out
paste("Income Outliers: ", 
      paste(outlier_values, 
            collapse = ", "))

# We need to remove the outliers present in the 
# var(Population) & var(Income)
# Remove the population outliers
states <- subset(states, 
                 Population != 21198 & 
                   Population != 11197 & 
                   Population != 18076 & 
                   Population != 11860 & 
                   Population != 12237 &
                   Population != 10735)

# Remove the income outliers
states <- subset(states, Income != 6315)

attach(states)
# To check if the oulier is removed
boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Population)$out))

boxplot(Income, 
        main = "Income", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Income)$out))

# Decide whether to delete a few more outliers and what this results in

# Check the normality
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow =c(2,4)) # 3rows * 3cols

plot(density(Murder), 
     main = "Density plot for Murder", 
     ylab = "Frequency", xlab = "Murder",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Murder), 2)))

# Fill in the area under the plot with red
polygon(density(Murder), col = "red")

plot(density(Population), 
     main = "Density plot for Population", 
     ylab = "Frequency", xlab = "Population", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Population), 2)))

# Fill in the area under the plot with red
polygon(density(Population), col = "red")

plot(density(HS_Grad), 
     main = "Density plot for Graduation", 
     ylab = "Frequency", xlab = "Graduation", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(HS_Grad), 2)))

# Fill in the area under the plot with red
polygon(density(HS_Grad), col = "red")

plot(density(Illiteracy), 
     main = "Density plot for Illiteracy", 
     ylab = "Frequency", xlab = "Illiteracy", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Illiteracy), 2)))

# Fill in the area under the plot with red
polygon(density(Illiteracy), col = "red")

plot(density(Income), 
     main = "Density plot for Income", 
     ylab = "Frequency", xlab = "Income", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Income), 2)))

# Fill in the area under the plot with red
polygon(density(Income), col = "red")

plot(density(Frost), 
     main = "Density plot for Frost", 
     ylab = "Frequency", xlab = "Frost", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Frost), 2)))

# Fill in the area under the plot with red
polygon(density(Frost), col = "red")

plot(density(Life_Exp), 
     main = "Density plot for Life Expectancy", 
     ylab = "Frequency", xlab = "Life Expec", 
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Life_Exp), 2)))

# Fill in the area under the plot with red
polygon(density(Life_Exp), col = "red")

paste("Skewness for Murder: ", round(e1071::skewness(Murder), 2))
paste("Skewness for Population: ", round(e1071::skewness(Population), 2))
paste("Skewness for Income: ", round(e1071::skewness(Income), 2))
paste("Skewness for Life_Exp: ", round(e1071::skewness(Life_Exp), 2))
paste("Skewness for Frost: ", round(e1071::skewness(Frost), 2))
paste("Skewness for Illeteracy: ", round(e1071::skewness(Illiteracy), 2))
paste("Skewness for Graduation: ", round(e1071::skewness(HS_Grad), 2))

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # divid ethe graph area in 2 cols
hist(Murder, main = "Mormalility proportion of Murder", xlab = "Murder Rate")

qqnorm(Murder)
qqline(Murder)
par <- opar

attach(states)
mlr_model <- lm(Murder ~ Illiteracy + Population + HS_Grad + 
                  Income + Frost, data = states)
summary(mlr_model)
