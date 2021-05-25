# Working on MLR for insurance
insurance_data <- read.csv("insurance.csv", na = "")

# several variables need to be converted as required
# N = 4, so we need N-1 dummy variables
# where it would be = 3 variables
# Code variables in a alphabetical order
# (SEX) male = 1, female = 0
# (SMOKER) yes = 1, no = 0
attach(insurance_data)
insurance_data$sex <- ifelse(sex == "male", 1, 0)
insurance_data$smoker <- ifelse(smoker == "yes", 1, 0)

# create a dummy variables for region
# new columns created with the dummy variables using IFELSE
insurance_data$ne <- ifelse(region == "northeast", 1, 0)
insurance_data$nw <- ifelse(region == "northwest", 1, 0)
insurance_data$se <- ifelse(region == "southeast", 1, 0)
insurance_data$sw <- ifelse(region == "southwest", 1, 0)

# round the BMI and charges
insurance_data$bmi <- round(bmi, 1)
insurance_data$charges <- round(charges, 2)

# drop the un-needed variables
names(insurance_data)
insurance_data <- insurance_data[c(1:5, 7:11)]

# check the assumptions
scatter.smooth(x=charges, 
               y=age, 
               main="Insurance charges ~ age", 
               xlab = "Charges", 
               ylab = "Age")

# Install library
library(psych)
# To visualize the distribution and correlation
pairs.panels(insurance_data, 
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


str(insurance_data)

insurance_data$sex <- as.integer(insurance_data$sex)
str(insurance_data)
attach(insurance_data)
# Check the correlations between both variables
paste("Correlation for the insurance & age:", cor(charges, age))
paste("Correlation for the insurance & sex:", cor(charges, sex))
paste("Correlation for the insurance & bmi:", cor(charges, bmi))
paste("Correlation for the insurance & smokers:", cor(charges, children))
paste("Correlation for the insurance & children:", cor(charges, smoker))
paste("Correlation for the insurance & NE:", cor(charges, ne))
paste("Correlation for the insurance & NW:", cor(charges, nw))
paste("Correlation for the insurance & SE:", cor(charges, se))
paste("Correlation for the insurance & SW:", cor(charges, sw))

# Perform all the check first
# Low correlation = value equal to zero
# High Correlation = straight line in plot (value > 0.60) 
# (positive & negative)
# No correlation = No correlation no line

# ASsuming all the correlation check have been performed
model <- lm(formula = charges ~ age + sex + bmi + children + smoker + 
              ne + nw + se + sw, data = insurance_data)

summary(model)
# the *** (stars) have a high correlation between the variables

# modified model based on the values investigated in the first model
model1 <- lm(formula = charges ~ age + sex + bmi + children + smoker + 
              ne + nw + se)

summary(model1)
# Check for colinearity
# done after the model is created
# install.packages("car")
library(car)
vif(model1)

# Collinearity statistics measure the relationship between multiple variables
# The "tolerance" is an indication of the % of variance in the predictor
# that cannot be accounted for by the other predictors
# hence very small values indicate that a predictor is redundant


hist(resid(model1), main = 'Histogram fo residulas', xlab='Standarized Resi', ylab='Filtered')

plot(model1, which = 1)
outlierTest(model1)




