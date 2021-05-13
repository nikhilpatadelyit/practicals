
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
plot(wt, mpg, pch = 19, col ="green", 
     main = "Comaprison of car weight with mpg", 
     xlab = "weight (lbs)", ylab = "mpg")

subset(cars, select = c(wt,mpg))

library("lattice")

histogram(~wt | mpg,
          data = cars,
          main = "Distributation of fuel efficiency wrt weight",
          xlab = "Weight of cars (1000 lbs)" ,ylab = "fuel efficiency")

tapply(wt, mpg, median)



library("ggpubr")
ggscatter(cars, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


# mpg
ggqqplot(cars$mpg, ylab = "MPG")
# wt
ggqqplot(cars$wt, ylab = "WT")

res <- cor.test(cars$wt, cars$mpg, 
                method = "pearson")
res


res$p.value
         
res$estimate
# Thus the car weight affect fuel efficiency


with(cars, 
     qqplot(wt, mpg,
            main = "Comparison of wt v/s mpg", 
            xlab = "weight(lbs)", 
            ylab = "mpg"))

# Visualize the normality of the variables
opar = par(no.readonly = TRUE)


# Arrange the plots in 1rows by 2 cols
par(mfrow = c(1,2))

hist(wt, col = "red", main = " distribution of car weight", 
     xlab = "weight(lbs)")
hist(mpg, col = "red", main = "distribution of car mpg")

par = opar

qqnorm(wt)

qqline(wt, col = "red")

qqnorm(mpg)

qqline(mpg, col = "red")


with(cars,
     {qqnorm(wt, 
                  main ="Normal Q-Q-Plot of wt", 
                  xlab = "", 
                  ylab = "")
       qqline(wt)
       })

with(cars,
     {qqnorm(mpg, 
             main ="Normal Q-Q-Plot of mpg", 
             xlab = "", 
             ylab = "")
       qqline(mpg)
     })
# Examine the linear correlation between both vars
with(cars, qqplot(wt, mpg))

# We ca run the formal test of normality
# provided through the widely used
# shapiro-wilks test
normality_test <- shapiro.test(cars$mpg)
normality_test$p.value
# In this example p-value = 0.12
# 0.12 > 0.05
# Then mpg is ND

normality_test <- shapiro.test(cars$wt)
normality_test$p.value
# p-values tells us the chance that the sample comes from ND
# If p-value < 0.05 then the variable is not ND

# In this example p-value = 0.09
# 0.09 > 0.05
# Then the wt is ND

shapiro.test(cars$mpg)

shapiro.test(cars$wt)

# If one is ND and one is NOT ND then use spearmans
# Both variables are ND
# Both continous
# We will use pearsons
# dependent var = mpg
# independent var = wt
cor.test(wt, mpg, 
                method = "pearson")
# pearsons correlation = -0.86
# p-value = 1.29e-10 = 0.00000129
# cut-off = 0.05
# Thus p-value < 0.05
# we wil reject H0
# Thus there is significant correlation between
# weight and mpg of car

# Thus we can state that the weight of car affects fuel efficiency



