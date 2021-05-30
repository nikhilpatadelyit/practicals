# Analyzing and dealing with the data related to the "HEART"
# for visualizing and predicting the functioning & working 
# of the patients heart and implementing the model for the chances 
# of "HEART-ATTACK" with the variables present.

# Importing & Installing the required packages & libraries
# wherever needed for analysing and visualizing

# Importing the dataset into a DF
heart_data <- read.csv("heart.csv", na="")

# Display the first six entries from the DF
head(heart_data)

# Structure of DF
str(heart_data)

# Verifying that it is a DF
class(heart_data)

# Displaying the number of rows and columns
nrow(heart_data)
ncol(heart_data)

# Display the summary of the data
summary(heart_data)

# Rename the column names
colnames(heart_data)
name <- c("Age","Sex", "Chest_pain", "Resting_BP", "Cholestoral", "Fasting_BS", 
          "Resting_ECG", "Max_heartrate", "Excercise_angina", "Oldpeak", "Slope", "Num_major_vessel", 
          "Thall", "Target")
names(heart_data) <- name

# Display the column-names of DF
colnames(heart_data)

# Displaying the summary
summary(heart_data)

# To check if any NA data present
incomplete_data <- heart_data[!complete.cases(heart_data),]
incomplete_data

# Display the missing data in rows
nrow(incomplete_data)

# visualize the missing data
# install.packages("VIM")
library(VIM)
missing_values <- aggr(heart_data, prop = FALSE, numbers = TRUE)

# Display the summary of missing data
summary(missing_values)
# No missing data present in the DF

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(heart_data))

# Fetching the required columns/variables for analysing & predicting
# the best model & storing the required variables into a new DF
new_heart_data <- heart_data[, c(1,2,3,4,5,6,7,8,9,12,14)]

# Checking if any NA is present in the DF
any(is.na(new_heart_data))

# Display the structure of DF
str(new_heart_data)

# Display the column-names of DF
colnames(new_heart_data)

# Installing the library 'psych'
# install.packages("psych")
library(psych)

# To visualize the distribution and correlation 
# using (pairs) & (pairs.panel)
pairs(new_heart_data)

pairs.panels(new_heart_data, 
             smooth = FALSE, # If TRUE, draws less smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jitered
             factor = 2, # Jitering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals


######################## HEART DATA ################################
# Heart Data-set - Predicting the chances of getting a "Heart-Attack"
# Using Different Technique
####################################################################

# Transforming the categorical variable to the required factor format 
# using (as.factor) function
new_heart_data$Sex <- as.factor(new_heart_data$Sex)
new_heart_data$Chest_pain <- as.factor(new_heart_data$Chest_pain)
new_heart_data$Fasting_BS <- as.factor(new_heart_data$Fasting_BS)
new_heart_data$Resting_ECG <- as.factor(new_heart_data$Resting_ECG)
new_heart_data$Excercise_angina <- as.factor(new_heart_data$Excercise_angina)
new_heart_data$Target <- as.factor(new_heart_data$Target)

# Providing levels and labels for the factored variables
# for Sex category 
# Defining the levels of the Sex as:
# Value 0 = Female
# Value 1= Male
levels(new_heart_data$Sex) <- c("Female", 
                                "Male")

# for Chest_Pain category
# Defining the levels of the Chest Pain as:
# Value 0 = Typical Angina
# Value 1 = Atypical Angina
# Value 2 = Non-Anginal Pain
# Value 3 = Asymptomatic
levels(new_heart_data$Chest_pain) <- c("Typical angina", 
                                       "Atypical angina", 
                                       "Non angina", 
                                       "Asymptomatic")

# for Fasting_Sugar category
# Defining the levels of the Blood SUgar as:
# Value 0 = False
# Value 1= True
levels(new_heart_data$Fasting_BS) <- c("False", 
                                       "True")

# for Resting_ECG category
# Defining the levels of the ECG as:
# Value 0 = Normal
# Value 1 = Abnormal
# Value 2 = Hypertrophy
levels(new_heart_data$Resting_ECG) <- c("Normal", 
                                        "Abnormal", 
                                        "Hypertrophy")

# for Excercise_Angina category 
# Defining the levels of the Excercise as:
# Value 0 = No
# Value 1= Yes
levels(new_heart_data$Excercise_angina) <- c("No", 
                                             "Yes")

# for Target category 
# Defining the levels of the Target as:
# Value 0 = Less chance of HA
# Value 1= More chance of HA
levels(new_heart_data$Target) <- c("Less chance of HA", 
                                   "More chance of HA")

# Observe the structure of DF
str(new_heart_data)

# Display the enties from the DF
head(new_heart_data)

# To visualize the distribution and correlation 
# using (pairs) & (pairs.panel) after transformation & labeling
pairs(new_heart_data)

pairs.panels(new_heart_data, 
             smooth = FALSE, # If TRUE, draws less smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jitered
             factor = 2, # Jitering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals

# Importing & Installing the required packages & libraries
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("rpart.plot")
library(ggplot2)
library(caret)
library(rpart.plot)

# For visual analysis considering the variable
# Dependent var = Target
# Independent var = All the others
# Further will decide which variables to include for predicting model

# For Target
# Plot the graph to analyze the specified attributes Target
ggplot(new_heart_data, aes(Target, fill = Target)) + 
  geom_bar() + labs(x = "Heart-Attack status", y = "Patient Count") + 
  guides(fill = FALSE)
# We can see that the distribution is quite balanced 
# this could be a good idea using accuracy 
# to evaluate how well the models perform

# For Target v/s Age
# Plot the graph to analyze the specified attributes 
# with Target & Age
ggplot(new_heart_data, aes(Age, fill = Target)) + 
  geom_histogram(binwidth = 1) + 
  labs(fill = "HA chances", x = "Age (years)", y = "Patient Count")
# We observed that age is not a risk factor
# higher the age less are the chances to get the HA

# For Target v/s Sex
# Plot the graph to analyze the specified attributes 
# with Target & Sex
ggplot(new_heart_data, aes(Sex, fill = Target)) + 
  geom_bar() + 
  labs(fill = "HA chances", x = "Sex", y = "Patient Count")
# We observed that sex is a risk factor
# Males have the higher chances of getting a HA then Female

# For Target v/s Chest_Pain
# Plot the graph to analyze the specified attributes 
# with Target & Chest_Pain
ggplot(new_heart_data, aes(Chest_pain, fill = Target)) + 
  geom_bar() + 
  labs(fill = "HA chances", x = "Chest_Pain_Type", y = "Patient Count")
# The data do not show how the pain is classified
# we observed that only in typical angina patient have less chance of HA
# but if any other type of pain it is likely to have more chance of HA

# For Target v/s Blood_Pressure
# Plot the graph to analyze the specified attributes 
# with Target & Blood_Pressure
ggplot(new_heart_data, aes(Resting_BP, fill = Target)) + 
  geom_histogram(binwidth = 3) + 
  labs(fill = "HA chances", x = "Blood_Pressure (mm Hg)", y = "Patient Count")
# Observed that the range(100-160) have more chance of getting HA
# Medically proven is higher the sugar level high are the chance of HA
# But our analysis is exactly opposite, higher level have less chance of HA

# For Target v/s Cholesterol
# Plot the graph to analyze the specified attributes 
# with Target & Cholesterol
ggplot(new_heart_data, aes(Cholestoral, fill = Target)) + 
  geom_histogram(binwidth = 10) + 
  labs(fill = "HA chances", x = "Cholestorol (mg/dl)", y = "Patient Count")
# Observed that the range(150+) have more chance of getting a HA
# It can also be controlled
# At certain level also patients have more chance of getting a HA even though
# the cholestorol level is normal

# For Target v/s Sugar
# Plot the graph to analyze the specified attributes 
# with Target & Sugar_Level
ggplot(new_heart_data, aes(Fasting_BS, fill = Target)) + 
  geom_bar() + 
  labs(fill = "HA chances", x = "Sugar_Level (> 120 md/dl)", y = "Patient Count")
# We observed that sugar is also a risk factor
# It can also be controlled
# Analysis shows that if sugar level < (120 mg/dl) patient have more chance of HA

# For Target v/s ECG
# Plot the graph to analyze the specified attributes 
# with Target & ECG
ggplot(new_heart_data, aes(Resting_ECG, fill = Target)) + 
  geom_bar() + 
  labs(fill = "HA chances", x = "ECG", y = "Patient Count")
# We observed that ECG can also be a risk factor depending on its wave
# Analysis showed that the abnormal wave of ECG can have more chances of HA
# compared to the normal and hypertrophy

# For Target v/s Heart_Rate
# Plot the graph to analyze the specified attributes 
# with Target & Heart_Rate
ggplot(new_heart_data, aes(Max_heartrate, fill = Target)) + 
  geom_histogram(binwidth = 10) + 
  labs(fill = "HA chances", x = "Maximum Heart Rate", y = "Patient Count")
# Observed that higher are the chance of HA when Heart_Rate is maximum
# It can be considered as a risk factor but may also vary from the patients age

# For Target v/s Exercise_Angina
# Plot the graph to analyze the specified attributes 
# with Target & Exercise_Angina
ggplot(new_heart_data, aes(Excercise_angina, fill = Target)) + 
  geom_bar() + 
  labs(fill = "HA chances", x = "Angina during Exercise", y = "Patient Count")
# It is also a risk factor to get the HA
# Analysis showed that if there is no angina during exercise patient have 
# more chance to get HA
# Also sometimes the angina cannot be identified so it can be of any type

# For Target v/s Blood_Vessels
# Plot the graph to analyze the specified attributes 
# with Target & Exercise_Angina
ggplot(new_heart_data, aes(Num_major_vessel, fill = Target)) + 
  geom_bar() + 
  labs(fill = "HA chances", x = "Blood_Vessel Seen", y = "Patient Count")
# It is considered to be a risk factor
# Analysis shows that if no blood vessel are seen then there is a high chance of HA

##################################
# Model Validation (Train & Test)
##################################
# Keeping in mind we have dropped the null values & 
# also the categorical variables are converted to factor as required

# The training set will evaluate the model with 10 fold cross validation
# We are not specifying the parameters to train, let the model use them by default
# meaning that some random set of combinations will be selected and the model 
# will be trained for each combinations
# Validating the variables for building a model
# Comparing the model with the ratio of 70% training & 
# 30% for testing the instances
# Observing that the distribution of the Dependent var (Target) need to be same
set.seed(1)
training_data <- createDataPartition(new_heart_data$Target, 
                                     p = 0.70, 
                                     list = FALSE)

new_heart_data_train <- new_heart_data[training_data, ] # 213 records
new_heart_data_test <- new_heart_data[-training_data, ] # 90 records

# Cross-validation (n=10)
fitcontrol <- trainControl(method = "cv", number = 10)

# Summarizing the results
fitcontrol

########################################
# Prediction using "LOGISTIC REGRESSION"
########################################
# Training the model with Logistic-Regression
set.seed(1)
model.lr <- train(Target ~ ., 
                  data = new_heart_data_train, 
                  method = "glm", 
                  family = binomial(), 
                  trControl = fitcontrol)

# Summarizing the results
model.lr
# The results validated that the accuracy and kappa generated for the 
# parameters are the best which suits the model
# As it shows that the variables defined for validating is yields out the 
# accuracy = 79% & kappa = 58%
# We can conclude that model is accurate with the combination and validation 
# we used to trained and test our data

# Evaluating the model for prediction using the test data
# Predicting the model using Logistic-Regression
prediction_lr <- predict(model.lr, 
                         new_heart_data_test)

# Confusion matrix for the predicted model using Logistic-Regression
confusionMatrix(prediction_lr, 
                new_heart_data_test$Target)

# The model evaluated that it is 75% accurate while predicting the outcome
# which almost has the same accuracy, compared with the results of 
# cross-validation technique we performed it showed 79% accuracy

#################################
# Prediction using "NAIVE BAYES"
#################################
# Building a model using Naive-Bayes
# install.packages("naivebayes")
library(naivebayes)
set.seed(1)
model.nb <- train(Target ~ ., 
                  data = new_heart_data_train, 
                  method = "naive_bayes", 
                  trControl = fitcontrol)

# Summarizing the results
model.nb
# The results validated that the accuracy and kappa generated for the 
# parameters are the best which suits the model
# As it shows, the model is 77% accurate when use-kernel = false
# We can conclude that model is accurate with the combination and validation 
# we used to trained and test our data

# Visual representation to show the accuracy performed on the model
# from use-kernel = false to use-kernel = true
plot(model.nb)

# Evaluating the model for prediction using the test data
# Predicting the model using Naive-Bayes
prediction_nb <- predict(model.nb, 
                         new_heart_data_test)

# Confusion matrix for the predicted model using Naive-Bayes
confusionMatrix(prediction_nb, 
                new_heart_data_test$Target)

# The model evaluated that it is 81% accurate while predicting the outcome
# which almost has the same accuracy, compared with the results of 
# cross-validation technique we performed it showed 77% accuracy

##################################
# Prediction using "RANDOM FOREST"
##################################
# Building a model using Random-Forest
# install.packages("randomForest")
library(randomForest)
set.seed(1)
model.rf <- train(Target ~ ., 
                  data = new_heart_data_train, 
                  method = "rf", 
                  trControl = fitcontrol)

# Summarizing the results
model.rf
# The results validated that the accuracy and kappa generated for the 
# parameters are the best which suits the model
# As it shows, the model is 80% accurate when value for the randomly sampled 
# variable (mtry = 7)
# We can conclude that model is accurate with the combination and validation 
# we used to trained and test our data

# Visual representation to show the accuracy performed on the model
# from the randomly sampled variable value = 7
plot(model.rf)

# Evaluating the model for prediction using the test data
# Predicting the model using Random-Forest
prediction_rf <- predict(model.rf, 
                         new_heart_data_test)

# Confusion matrix for the predicted model using Random-Forest
confusionMatrix(prediction_rf, 
                new_heart_data_test$Target)

# The model evaluated that it is 80% accurate while predicting the outcome
# which almost has the same accuracy, compared with the results through the  
# cross-validation technique we performed it showed 80% accuracy

# Comparing with the result evaluated using different techniques in implementing
# a model, in future we might select some of the parameters to tune our model 
# expecting some better accuracy for the model we build.

#############################################################
#############################################################
# NEW MODEL = (fit_model_1) with modified variables using MLR
#############################################################
#############################################################

attach(new_heart_data)
# Building a model with the significant variables including some other variables 
# assuming which are better useful as predictor variables for the model to fit
fit_model_1 <- lm(Target ~ Age + Sex + Chest_pain + 
                    Max_heartrate + Cholestoral + Excercise_angina + 
                    Num_major_vessel, data = training_data)

# Viewing the statistic result for the build model
summary(fit_model_1)
# The Multiple R-squared value explains that there is 41% of the variation 
# of getting the Heart-Attack for the patients data provided.

# Analyzing the confidence interval result of the model build
confint(fit_model_1)

attach(new_heart_data)
# Importing the library
library(car)

# Plots empirical quantiles of studentized residuals from a linear model, 
# against theoretical quantiles of a comparison distribution
qqPlot(fit_model_1, 
       labels=row(new_heart_data), 
       id.method="identify", 
       simulate=TRUE, 
       main = "Q-Q Plot for fit_model_1")
# We identified that there are two values in the plot which have outliers
# We will analyze them and make the decision

# Training the outlier data and analyzing whether it affect or not
training_data["159",]
training_data["140",]

# Fitting the outlier data to the new model
fitted(fit_model_1)["159"]
fitted(fit_model_1)["140"]

# We will use the standardize residuals for better statistical analysis 
# as they are independent and the randomly generated samples are not zero.
library(car)
# Histogram Visualization for the distribution error
# Defining the length of the graph to get them plot
student_fit_model_1 <- rstudent(fit_model_1)
hist(student_fit_model_1,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model_1), col="brown")

curve(dnorm(x, mean=mean(student_fit_model_1), sd=sd(student_fit_model_1)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model_1)$x, density(student_fit_model_1)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)

# We can use the below function to check whether a model 
# contains any outliers
outlierTest(fit_model_1)

# Here we removed the entire row as we came across that there were 
# some outliers present when we performed (outlierTest) on the model to fit
new_heart_data[-c(159, 140), ]
new_heart_data <- new_heart_data[-c(159, 140), ]

# REBUILDING A MODEL
# Training the data available by dropping the outlier row and building 
# the set of new train & test data.
attach(new_heart_data)
set.seed(1)
no_rows_heart_data <- nrow(new_heart_data)
sample <- sample(1:no_rows_heart_data, 
                 size = round(0.8 * no_rows_heart_data), 
                 replace = FALSE)

training_data <- new_heart_data[sample, ]
testing_data <- new_heart_data[-sample, ]

# Training the new model with removing the outliers
fit_model_1 <- lm(Target ~ Age + Sex + Chest_pain + 
                    Max_heartrate + Cholestoral + Excercise_angina + 
                    Num_major_vessel, data = training_data)

# Viewing the statistic result for the build model
summary(fit_model_1)
# The Multiple R-squared value explains that there is 43% of the variation 
# of getting the Heart-Attack for the patients data provided through this model.

# AIC() for the normal model build
AIC(fit_model_1)
# AIC value = 218.24

# After using the outlierTest function and getting reed of the outliers 
# there is a difference in the R-squared values of model build.
# A slight increase of 2% is observed even after removing the outliers 
# by best fitting the model again with the set of train & test data.

opar <- par(no.readonly = TRUE)

# Histogram Visualization for the distribution error after deleting the outliers
# Defining the length of the graph to get them plot
par(mfrow = c(1,1)) # divide the graph area in 1 cols
library(car)
attach(new_heart_data)
student_fit_model_1 <- rstudent(fit_model_1)
hist(student_fit_model_1,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model_1), col="brown")

curve(dnorm(x, mean=mean(student_fit_model_1), sd=sd(student_fit_model_1)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model_1)$x, density(student_fit_model_1)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)
# The plot shows that the outliers are been removed

# We can use the below function to check whether a model 
# contains any outliers
outlierTest(fit_model_1)
# As we decided we can keep the outliers as processed further to build
# and evaluate a model
par <- opar

# Checking the linearity of the data using different plot techniques
# To visualize the linear relationship between the 
# dependent & independent through a linear line.
crPlots(fit_model_1)

# Influential observations using the cooks distance formula on trained data
cutoff <- 4/(nrow(training_data) - length(fit_model_1$coefficients) - 2)

# Plotting the graphical analysis of the values using Cook-D method
plot(fit_model_1, which = 4, cook.levels = cutoff)

# It draws a cutoff line where any data above it can be ignored
abline(h = cutoff, lty = 2, col = "red")

# We will not plot any influence data as we are not observing any influence 
# below the cutoff value line, we can proceed further

# We can create a graphical analysis of the predictor variables with all 
# independent variables which yeild only a single response on the dependent var
# Av plots are known as added-variable plots used to show the regression 
# coefficient of the predictor variables
avPlots(fit_model_1, ask = FALSE)

# Influence Plot
library(car)
influencePlot(fit_model_1, main = "Influence Plot", 
              sub = "Circle size is proportional to Cooks distance")

# The influence plot shows that the row (303, 159, 140) are very close to the 
# boundary level which is selected by default and may be they are the outliers. 

# We can now check the Homoscedasticty Test using (ncvTest) which generates the 
# result for the hypothesis of constant error variance with a fitted model data
# If p-value < 0.05, then the error variance value may change (Homoscedasticity)
# If p-value > 0.05, then the error variance value may not change (Heteroscedasticity)
ncvTest(fit_model_1)
# A result shows that p-value = 0.6, which is greater than cut-off,
# then the error variance value does not change

# The following visualization will show the scatter plot of the 
# standardized residuals versus the fitted model values & draws 
# the line which best fits the data
par(mfrow = c(1,1))
spreadLevelPlot(fit_model_1)
par <- opar
# Suggested power transformation:  0.6467687
# we observed the best fit line with the above function but it not actual to the
# default line, but it best fits with the data used for building a model

# install.packages("gvlma")
# It determines the global validation of the linear model assumptions & also 
# evaluates separately the different test performed while building a model
library(gvlma)
gvmodel <- gvlma(fit_model_1)
summary(gvmodel)
# We observed that the model build accepts all the statistical assumptions we 
# made with the regression model.
# Also the p-values > 0.05, so the decisions are acceptable.
# The results evaluate that the model build shows the Multiple R-Squared = 0.43 
# i.e the model predicts 43% of the variation for getting a Heart-Attack 
# with the information provided.

# Checking the multi-collinearity
# It can be detected with the Variance Inflation Factor(VIF)
library(car)
vif(fit_model_1)

# We can check whether any of the variables indicate a 
# multi-collinearity problem if the value > 2
sqrt(vif(fit_model_1)) > 2
# The individual coefficient of the variables are < 2, so there is no 
# problem of multi-collinearity.

# Log transformation of the variables
# As we have already generated the power transformation value = 0.6, there was
# no need to do the power transformation for the variables

# Just to acknowledge that the value 0.6 lies between (0.5 - 1) I will perform 
# a sqrt() on the new model with the dependent variable
# Transforming the Target variable with the sqrt() funtion
sqrt_transform_Target <- sqrt(training_data$Target)
training_data$Target_sqrt <- sqrt_transform_Target

# Building a model with the log transformation 
fit_test_model_1 <- lm(Target_sqrt ~ Age + Sex + Chest_pain + 
                         Max_heartrate + Cholestoral + Excercise_angina + 
                         Num_major_vessel, data = training_data)

# Summary of the fit_test_model transformation model to view the statistic results
summary(fit_test_model_1)
# The Multiple R-squared value explains that there is 43% of the variation 
# of getting the Heart-Attack for the patients data provided.

# AIC() for the transform model build
AIC(fit_test_model_1)
# AIC value = 218.85

# Comparing the model build with different variables using the AIC() function
AIC(fit_model_1, fit_test_model_1)
# When AIC scored is compared it does not have any difference on the 
# model (fit_model_1) which was normally build using the modified variables 
# & model (fit_test_model_1) which was build after performing transformation.

# So for further evaluation we can use any of the model for further prediction
# as both model does not have any difference between the AIC score.

# Evaluating the analysis of the model using a Step-wise Regression
library(MASS)
stepAIC(fit_test_model_1, direction = "backward")
# Performing the 'backward' stepwise regression on this model it showed 
# that the predictor variables are removed one by one by the stepAIC() function  
# for calculating the AIC score with the better combination of the variables.

# At some point when the combinations of the variables does not change its value
# the stepAIC() stops calculating further and gives a result which better fits.
# Here it suggests that (Sex + Chest_pain + Max_heartrate + Cholestoral + 
# Excercise_angina + Num_major_vessel) is the best combination 
# for the model with AIC score = -431.61
# We cannot conclude that this is a best combination for the predictive model.

# install.packages("leaps")
library(leaps)
leaps <- regsubsets(Target_sqrt ~ Age + Sex + Chest_pain + 
                      Max_heartrate + Cholestoral + Excercise_angina + 
                      Num_major_vessel, data = training_data, nbest = 4)

plot(leaps,scale = "adjr2")
# The leap plot shows the best correlation of the variables with the score of 
# R-squared and Adjusted R-squared values on the y-axis.
# A model (bottom row) with the intercept and Num_major_vessels has an 
# adjusted R-squared = 0.12.
# At the top with the variables(intercept, Age, Sex, Chest_pain, Max_heartrate, 
# Cholestoral, Excercise_angina) shows the adjusted R-squared = 0.41.
# Thus the top row with the value 0.41 shows that the variables used are the best fit
# model build. A stepwise regression check also correlates with this variables.


# Now examine the accuracy of the model predicted
predicted_heart_attack_1 <- predict(fit_test_model_1, testing_data)

actual_prediction_1 <- data.frame(cbind(actuals = testing_data$Target, 
                                        predicted = predicted_heart_attack_1))

head(actual_prediction_1)

correlation_accuracy_1 <- cor(actual_prediction_1)
correlation_accuracy_1

# This model build with transformation of the variables shows 
# 68% correlation accuracy

############################################################################
# COMPARING THE TWO PREDICTIVE MODELs:
############################################################################
# Model1 = fit_model [ACCURACY = 73%]
# Model1: It has all the independent variables as predictor variables
# (Age, Sex, Chest_pain, Resting_BP, Resting_ECG, Fasting_BS, Max_heartrate) 
# (Cholestoral, Excercise_angina, Num_major_vessel)
############################################################################

############################################################################
# Model2 = fit_model_1 [ACCURACY = 68%]
# Model2: It includes only some of the modified predictor variables
# (Age, Sex, Chest_pain,  Max_heartrate, Cholestoral) 
# (Excercise_angina, Num_major_vessel)
############################################################################

# When compared with each other it signifies that the Model1(fit_model) is the 
# best fit with all the predictor variables, as they rely on each other 
# for predicting a good correlation accuracy result of 73%.



