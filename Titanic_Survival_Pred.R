---
  title: "Titanic_Survival_Pred_MD"
author: "Arif Shahriar"
date: "5/30/2021"
output: html_document
---

# In this project, I have used titanic data set. This project is a combination of 
# data cleaning, data visualization, and predictive analysis. In the end of the
# assignment, I have used some code that I have learned in my Applied Regression
# class.

# Load libraries

library(tidyverse)
library(broom)
library(lmtest)

# Read train.csv file

train <- read_csv("train.csv")
train


# Observe all the column and guess which column can have effect on survival
# prediction

glimpse(train)


# Prediction variable: Pclass, sex, age, Fare.
# All the above variables might have an effect on survival. We can first create
# some visualization using those variable against survival.

# Check if Pclass has an effect on survival.

train %>%
  ggplot(aes(Pclass, fill = factor(Survived))) +
  geom_bar()

# Class 1, the left most bar, has more survival count than other two class. And
# class 3 has the highest death count than other two classes. But we can also
# observe that class 3 has the highest number of passengers. 

# We can check the proportion of survival rate to get a clear picture.

train %>%
  ggplot(aes(Pclass, fill = factor(Survived))) +
  geom_bar(position = 'fill')

# This gives us a clear picture and we can say that Pclass has a clear effect on
# survival.

# We can also check why Pclass 1 has more survival rate than others by checking
# the relationship between Pclass and fare.

train %>%  
  group_by(Pclass) %>% 
  summarise(avg_fare = mean(Fare))

# Pclass 1 people have paid more fare than others. So we can tell Pclass 1 people
# were prioritized in case of survival.

# Check if gender has an effect on survival.

train %>%
  ggplot(aes(Sex, fill = factor(Survived))) +
  geom_bar()

# This graph shows a pretty clear image of gender having an effect on survival. On
# the left bar we see total female passenger count is a little over 300, and the
# survival count is over 200. For male bar the total passenger count is close to
# 600 when the survival count is a little over 100.

# Check the proportion

train %>%
  ggplot(aes(Sex, fill = factor(Survived))) +
  geom_bar(position = 'fill')

# Female survival rate is close 75% and male survival rate is close to 20%.

# Check if fare has an effect on survival.

train %>%
  mutate(Fare = cut_number(Fare, 5)) %>%
  ggplot(aes(factor(Fare), fill = factor(Survived))) +
  geom_bar(position = 'fill')

# Fare also seems to have an effect on survival as we see the more you pay the
# survival rate increases. But paying more doesn't guarantee to keep someone out
# of death.

# Check if age has an effect on survival.

train %>%
  mutate(Age = cut_number(Age, 5)) %>%
  ggplot(aes(factor(Age), fill = factor(Survived))) +
  geom_bar()

# By looking at the graph it is hard to come to a conclusion about the effect of
# age on survival. Because we see that people aged under 19 has the highest
# survival rate, and the rate drops significantly for the age between 19-25.
# Then the survival rate increases and drops again. Also we can see that there
# are a lot of passengers whose age is not mentioned.

# In above graphs, we can say that Pclass, sex, age, Fare are the variables
# that can have good effect on survival. 

# logistic regression: predicting dichotomous values
# plot Survived against Fare
train %>% 
  ggplot(aes(Fare, Survived)) +
  geom_point()



# add linear model to use for predicting survival
train %>% 
  ggplot(aes(Fare, Survived)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)



# logistic regression allows prediction of probabilities
train %>% 
  ggplot(aes(Fare, Survived)) +
  geom_point() +
  geom_smooth(
    method = 'glm', 
    method.args = list(family = 'binomial'),
    se = F
  )

# graph crowded at left, so see if can apply log


# add $1 to fare before applying log, then add logistic regression curve
train %>% 
  ggplot(aes(log(Fare + 1), Survived)) +
  geom_point() +
  geom_smooth(
    method = 'glm', 
    method.args = list(family = 'binomial'),
    se = F
  )

# Result is a sigmoid, that is, s-shaped (for sigma, s in Greek)
# define sigmoid function that allows left-right translation
# modify sigmoid so can control steepness of center section and add default
# value of zero for h.

# Sigmoid function
sigmoid <- 
  function(x, h = 0, m){
    denominator <- exp(-m * (x - h)) + 1
    1 / denominator
  }

train %>% 
  glm(Survived ~ Fare, data = ., family = 'binomial') %>% 
  summary()

# p-value < 0.01. We can say it has a significant impact on survival.

# Add predicted survived and mean fare column from the train data set.

log_train <- 
  train %>% 
  mutate(Fare = log1p(Fare)) %>% 
  mutate(Fare_cat = cut_interval(Fare, 8)) %>% 
  group_by(Fare_cat) %>% 
  mutate(p_Survived = mean(Survived)) %>% 
  mutate(m_Fare = mean(Fare)) %>% 
  ungroup()


# plot mean fare and predicted survived

log_train %>% 
  ggplot(aes(Fare, Survived)) +
  geom_point() +
  geom_point(aes(m_Fare, p_Survived), size = 2, color = 'red')

# Add logit column for prediction and plot it against fare

log_train %>% 
  mutate(logit_p = log(p_Survived / (1 - p_Survived))) %>% 
  distinct(m_Fare, p_Survived, logit_p) %>% 
  ggplot(aes(m_Fare, logit_p)) +
  geom_point(size = 2, color = 'red') +
  geom_smooth(method = 'lm', se = F)

# This is the transformed version of the train data set

# Assign log_train

log_train <- 
  log_train %>% 
  mutate(logit_p = log(p_Survived / (1 - p_Survived)))


# Check the details of the linear model

logistic_model <-
  log_train %>% 
  lm(logit_p ~ m_Fare, data = .)

my_b <- logistic_model$coefficients[1]
my_m <- logistic_model$coefficients[2]
my_h <- -my_b/my_m

log_train %>% 
  ggplot(aes(Fare, Survived)) +
  geom_point() +
  geom_point(aes(m_Fare, p_Survived), size = 2, color = 'red') +
  stat_function(fun = sigmoid, args = list(m = my_m, h = my_h)) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = 'binomial'),
    se = F
  )

# Plot showing the comparison between logistic regression built by hand and the
# on glm did.

# Check the details of the linear model from actual data

log_train %>% 
  glm(Survived ~ Fare, data = ., family = 'binomial')

# Create logistic model

log_model_1 <- 
  log_train %>% 
  glm(Survived ~ Fare, data = ., family = 'binomial')

names(log_model_1)

log_b <- log_model_1$coefficients[1]
log_m <- log_model_1$coefficients[2]

log_h <- -log_b/log_m

# The h value for linear model is 3.629 and our h value is 3.621

# Inspect data and model

log_train
log_model_1


# Inspect only columns used to create model

log_train %>% 
  select(Fare, Survived)


# Add fitted (i.e., predicted) values to table

log_train %>% 
  select(Fare, Survived) %>% 
  augment(log_model_1, data = .)


# Plot fitted values in probability space

log_train %>% 
  augment(log_model_1, data = ., type.predict = 'response') %>% 
  ggplot(aes(Fare)) +
  geom_point(aes(y = Survived)) +
  geom_point(aes(y = .fitted), color = 'red')


# Calculate accuracy

prediction_1 <-
  log_train %>% 
  augment(log_model_1, data = ., type.predict = 'response') %>% 
  mutate(Survived_pred = round(.fitted)) %>% 
  mutate(correct = Survived == Survived_pred) %>% 
  summarise(
    prediction_variables = "Fare",
    perc_correct = mean(correct) * 100,
    error_rate = 100 - perc_correct
  )
prediction_1

# Model error rate is 33%. We can add variables and make a better model.

# Add another variable - age

log_model_2 <-
  log_train %>% 
  glm(Survived ~ Fare + Age, data = ., family = 'binomial')


# Add fitted values for new prediction
log_train %>% 
  augment(log_model_2, data = ., type.predict = 'response') 

# The code doesn't work: model seems to have omitted rows from data

# Check for omitted rows

log_model_2$na.action %>% str()

# Inspect omitted rows

log_train %>% 
  slice(log_model_2$na.action)

# Apparently omitted because of missing 'Embarked' values

# Check if there are other rows with missing 'Embarked' values

log_train %>% 
  filter(is.na(Age))

# Confirms that these are the rows that were omitted

# Calculate accuracy

prediction_2 <- 
  log_train %>% 
  glm(
    Survived ~ Fare + Age,
    data = .,
    family = 'binomial'
  ) %>%
  augment(
    data = train %>% filter(!is.na(Age)), 
    type.predict = 'response'
  ) %>% 
  mutate(Survived_pred = round(.fitted)) %>% 
  mutate(correct = Survived == Survived_pred) %>% 
  summarise(
    prediction_variables = "Fare, Age",
    perc_correct = mean(correct) * 100,
    error_rate = 100 - perc_correct
  )
prediction_2

# We see that adding age decreased the error rate.

# Add another variable - Plcass

log_model_3 <-
  log_train %>% 
  glm(Survived ~ Fare + Age + Pclass, data = ., family = 'binomial')


# Add fitted values for new prediction
log_train %>% 
  augment(log_model_3, data = ., type.predict = 'response') 

# It doesn't work: model seems to have omitted rows from data

# We know age has NA in several rows. Check if Pclass has any NAs

log_train %>% 
  filter(is.na(Pclass))

# There is no NAs for Pclass

# Calculate accuracy

prediction_3 <- 
  log_train %>% 
  glm(
    Survived ~ Fare + Age + Pclass,
    data = .,
    family = 'binomial'
  ) %>%
  augment(
    data = train %>% filter(!is.na(Age)), 
    type.predict = 'response'
  ) %>% 
  mutate(Survived_pred = round(.fitted)) %>% 
  mutate(correct = Survived == Survived_pred) %>% 
  summarise(
    prediction_variables = "Fare, Age, Pclass",
    perc_correct = mean(correct) * 100,
    error_rate = 100 - perc_correct
  )
prediction_3

# We see that adding age and Pclass decreased the error rate again.

# Add another variable - sex

log_model_4 <-
  log_train %>% 
  glm(Survived ~ Fare + Age + Pclass + Sex, data = ., family = 'binomial')


# Add fitted values for new prediction
log_train %>% 
  augment(log_model_4, data = ., type.predict = 'response') 

# It doesn't work: model seems to have omitted rows from data
 
# We know age has NA in several rows. Check if sex has any NAs

log_train %>% 
  filter(is.na(Sex))

# There is no NAs in sex column

# Calculate accuracy

prediction_4 <- 
  log_train %>% 
  glm(
    Survived ~ Fare + Age + Pclass + Sex,
    data = .,
    family = 'binomial'
  ) %>%
  augment(
    data = train %>% filter(!is.na(Age)), 
    type.predict = 'response'
  ) %>% 
  mutate(Survived_pred = round(.fitted)) %>% 
  mutate(correct = Survived == Survived_pred) %>% 
  summarise(
    prediction_variables = "Fare, Age, Pclass, Sex",
    perc_correct = mean(correct) * 100,
    error_rate = 100 - perc_correct
  )
prediction_4

# We started our error rate with 33%. Adding 4 variables fare, age, Pclass, and
# Sex decreased the error rate to 21% from 33%.

# Let's try adding two more variables: Sibsp and Embarked in the model to see if
# adding them makes our model better.

# Adding variables - Sibsp & Embarked

log_model_5 <-
  log_train %>% 
  glm(Survived ~ Fare + Age + Pclass + Sex + Embarked + SibSp, 
      data = ., family = 'binomial')


# Add fitted values for new prediction
log_train %>% 
  augment(log_model_5, data = ., type.predict = 'response') 

# It doesn't work: model seems to have omitted rows from data

# We know age has NA in several rows. Check if SibSp has any NAs

log_train %>% 
  filter(is.na(SibSp))

# There is no NAs in SibSp column

# Check if Embarked has any NAs

log_train %>% 
  filter(is.na(Embarked))

# There are 2 NAs in Embarked column

# Calculate accuracy

prediction_5 <- 
  log_train %>% 
  glm(
    Survived ~ Fare + Age + Pclass + Sex + SibSp + Embarked,
    data = .,
    family = 'binomial'
  ) %>%
  augment(
    data = train %>% filter(!is.na(Age) & !is.na(Embarked)), 
    type.predict = 'response'
  ) %>% 
  mutate(Survived_pred = round(.fitted)) %>% 
  mutate(correct = Survived == Survived_pred) %>% 
  summarise(
    prediction_variables = "Fare, Age, Pclass, Sex, Sibsp, Embarked",
    perc_correct = mean(correct) * 100,
    error_rate = 100 - perc_correct
  )
prediction_5

# Create a table with all the predictions

prediction_1 %>% 
  union(prediction_2) %>% 
  union(prediction_3) %>% 
  union(prediction_4) %>% 
  union(prediction_5)

# Adding 6 variables gave us the lowest error rate. Point to be noted, sex has a
# significant effect on this model. Adding variable sex decreased our error rate
# but more that 8%.

# Test prediction using random numbers.

attach(train)

# Data cleaning - Removing NAs

train_clean <-
  train %>% 
  select(Survived, Fare, Pclass, Sex, Age) %>% 
  filter(!is.na(Age))


# Assign the regression model

trainReg <- lm(Survived ~ Fare + Pclass + Sex + Age) 
trainReg


# Predict using Pclass = 3, Fare = 7.25, Sex = "male", Age = 22
newTrain1 <- data.frame(Pclass = 3, Fare = 7.25, Sex = "male", Age = 22)

# Using 90% confidence interval
predict(trainReg, newTrain1, se.fit = T, interval = "confidence", level = .90)
# Prediction interval is: 8.1% to 15% which means survival is 0.

# Use same number changing gender.
newTrain2 <- data.frame(Pclass = 3, Fare = 7.25, Sex = "female", Age = 22)

# Using 90% confidence interval
predict(trainReg, newTrain2, se.fit = T, interval = "confidence", level = .90)

# Prediction interval is: 54% to 64% which means survival is 0.
# Just by changing gender male to female the survival chance increased
# significantly.

# In our prediction table, we noticed Pclass decreased error rate by almost 4%.
# Predict survival by changing Pclass and keep rest of the variables constant.

# Predict using Pclass = 1, Fare = 100, Sex = "male", Age = 41
newTrain3 <- data.frame(Pclass = 1, Fare = 100, Sex = "male", Age = 41)


# Using 90% confidence interval
predict(trainReg, newTrain3, se.fit = T, interval = "confidence", level = .90)
# Prediction interval is: 37% to 47% which means survival is 0.

# Predict using Pclass = 3, Fare = 100, Sex = "male", Age = 41
newTrain4 <- data.frame(Pclass = 2, Fare = 100, Sex = "male", Age = 41)

# Using 90% confidence interval
predict(trainReg, newTrain4, se.fit = T, interval = "confidence", level = .90)

# Prediction interval is: 17% to 27% which means survival is still 0.

# We see survival chance increase if we change the Pclass and keep rest of the
# variable constant, but it doesn't have a great impact as gender.