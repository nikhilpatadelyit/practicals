
#H0: weight of cars has not effect fuel efficiency 
#H1: weight of cars effect fuel efficiency

cars <- mtcars
str(mtcars)

incomplete_data <- cars[!complete.cases(cars),]
incomplete_data

library(psych)

pairs.panels(cars, 
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

# examine
# Does car weight have an effect on mpg?
# 2 vars
# wt = num
# mpg = num

attach(cars)
plot(wt, mpg, pch = 19, col ="lightblue", 
     main = "Comaprison of car weight with mpg", 
     xlab = "weight (lbs)", ylab = "mpg")

histogram(~wt | mpg,
          data = cars,
          main = "Distributation of fuel efficiency wrt weight",
          xlab = "Weight of cars (1000 lbs)" ,ylab = "fuel efficiency")


