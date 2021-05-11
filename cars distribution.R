
#H0: weight of cars has not effect fuel efficiency 
#H1: weight of cars effect fuel efficiency

cars <- mtcars
str(mtcars)

incomplete_data <- cars[!complete.cases(cars),]
incomplete_data
nrow(incomplete_data)
any(is.na(cars))

# visualize the missing data
library(VIM)
missing_values <- aggr(cars, prop = FALSE, numbers = TRUE)
summary(missing_values)

library(psych)

pairs.panels(cars, 
             smooth = FALSE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered
             factor = 2, # Jittering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals

# examine
# Does car weight have an effect on mpg?
# 2 vars
# wt = num
# mpg = num

#H0: Does weight of cars affect fuel efficiency
#H1: Does weight of cars not affect fuel efficiency

# H0 = car weight has no effect on fuel efficiency
# H1 = car weight has an effect on fuel efficiency
attach(cars)

# Check the linearity
plot(wt, mpg, pch = 19, col ="lightblue", 
     main = "Comaprison of car weight with mpg", 
     xlab = "weight (lbs)", ylab = "mpg")

subset(cars, select = c(wt,mpg))

library("lattice")

histogram(~wt | mpg,
          data = cars,
          main = "Distributation of fuel efficiency wrt weight",
          xlab = "Weight of cars (1000 lbs)" ,ylab = "fuel efficiency")

tapply(wt, mpg, median)

qqnorm(wt)

qqline(wt, col = "red")

qqnorm(mpg)

qqline(mpg, col = "red")


normality_test <- shapiro.test(cars$mpg)
normality_test$p.value

normality_test <- shapiro.test(cars$wt)
normality_test$p.value






# They are not ND

with(cars, 
     qqplot(wt[mpg == "Yes"], 
            wt[mpg == "No"], 
            main = "Comparing 2 samples of car data", 
            xlab = "wt mpg = Yes", 
            ylab = "wt mpg = No"))

# We can add normality line to the plot
# to help evaluate normality
with(cars, {
  qqnorm(wt[mpg == "No"], 
         main = "Inactive Data")
  qqline(wt[mpg == "No"])})

with(beavers_data, {
  qqnorm(wt[mpg == "Yes"], 
         main = "Active Data")
  qqline(wt[mpg == "Yes"])})



