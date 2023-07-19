library("dplyr")
library("tidyverse")
library("stringr")
library("janitor")
library(caret)

getwd()
setwd("/Users/shriya/Desktop/heartfailure")
data <- read.csv("heart_2020_cleaned.csv")

#view amount of people by race
groups <- data %>% group_by(Race) %>% summarise(count = n())

#data$HeartDisease <- ifelse(data$HeartDisease == "Yes", 1, 0)

#exploratory analysis with ggplot
ggplot(data, aes(
  y = BMI,
  group = Race,
  color = Race
)) + geom_boxplot() + facet_wrap(~as.factor(HeartDisease))


ggplot(data, aes(
  y = BMI,
  x = Race,
  group = Race,
  color = Race
)) + geom_violin()

ggplot(data, aes(
  y = SleepTime,
  x = Race,
  group = Race,
  color = Race
)) + geom_violin()

ggplot(data, aes(
  x = HeartDisease,
  group = Race,
  fill = Race
)) + geom_bar()

#clean data
str(data)

#convert to factor
data <- data %>%
  mutate(
    HeartDisease = as.factor(HeartDisease), #specify levels 0 and 1
    Smoking = as.factor(Smoking),
    AlcoholDrinking = as.factor(AlcoholDrinking),
    Stroke = as.factor(Stroke),
    DiffWalking = as.factor(DiffWalking),
    Sex = as.factor(Sex),
    Race = as.factor(Race),
    Diabetic = as.factor(Diabetic),
    PhysicalActivity = as.factor(PhysicalActivity),
    GenHealth = as.factor(GenHealth),
    Asthma = as.factor(Asthma),
    KidneyDisease = as.factor(KidneyDisease),
    SkinCancer = as.factor(SkinCancer),
    AgeCategory = as.factor(AgeCategory)
  )


#set seed
set.seed(123)

#new column with each row #
data$rownum <- 1:nrow(data)

#split dataset randomforest
install.packages("randomForest")
library(randomForest)

train <- data %>% dplyr::sample_frac(0.80)
test  <- dplyr::anti_join(data, train, by = 'rownum')

train_x <- train %>% select(-HeartDisease)
test_x <- test %>% select(-HeartDisease)

train_y <- train %>% select(HeartDisease)
test_y <- test %>% select(HeartDisease)

#convert into vector
test_y <- unlist(test_y)
train_y <- unlist(train_y)

#train model
rfm1 <- randomForest(x = train_x, y = train_y, xtest = test_x, ytest = test_y, importance = TRUE, ntree = 500)

# find the best value for the mtry hyperparameter. Set the x, y, xtest, ytest as before. Set the ntreeTry value to 500 (it will build 500 trees per try), stepFactor to 1.5, improve = 0.01, trace = TRUE, and plot = TRUE 

mtry <- tuneRF(x = train_x, y = train_y, xtest = test_x, ytest = test_y, ntreeTry = 300, stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE)

# The code below will save the best value for the mtry and print it out
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#tune number of trees
rf_res_df <-
  data.frame(
    TRAINING_ERROR = rfm1$err.rate[,1],
    ITERATION = c(1:500)
  ) %>%
  mutate(MIN = TRAINING_ERROR == min(TRAINING_ERROR))

best_nrounds <- rf_res_df %>%
  filter(MIN) %>%
  pull(ITERATION)

print(best_nrounds)

#model
rf_final_model <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 2,
    importance = TRUE,
    ntree = 2
  )
rf_final_model


# #split into train and test logreg
# train <- data %>% dplyr::sample_frac(0.80)
# test  <- dplyr::anti_join(data, train, by = 'rownum')
# 
# #fit
# model1 <-
#   glm(
#     HeartDisease ~ Smoking + PhysicalHealth + AlcoholDrinking + GenHealth + KidneyDisease + SleepTime + Stroke + Diabetic + PhysicalActivity + AgeCategory + DiffWalking,
#     data = train,
#     family = binomial()
#   )
# summary(model1)
# 
# model2 <-
#   glm(
#     HeartDisease ~ Smoking + AlcoholDrinking + Stroke + Asthma,
#     data = train,
#     family = binomial()
#   )
# summary(model2)
# 
# fit_model <- model1
# 
# test$y_pred <- predict(fit_model, test, type = "response")
# test$y_pred <- ifelse(test$y_pred > 0.5, 1, 0)
# test$y_pred <- as.factor(test$y_pred) # make sure levels are no = 0 and 1 = yes
# 
# confusionMatrix(test$HeartDisease, test$y_pred)
