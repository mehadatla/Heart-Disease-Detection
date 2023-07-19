library("dplyr")
library("tidyverse")
library("stringr")
library("janitor")
library(caret)

getwd()
setwd("/Users/shriya/Desktop/data_mining")
data <- read.csv("heart_2020_cleaned.csv")

#view amount of people by race
groups <- data %>% group_by(Race) %>% summarise(count = n())

# data$HeartDisease <- ifelse(data$HeartDisease == "Yes", 1, 0)
data$HeartDisease <- as.factor(data$HeartDisease)

#exploratory analysis with ggplot
ggplot(data, aes(y= BMI, x= HeartDisease, group = Race, color = Race)) + geom_boxplot()
ggplot(data, aes(y= BMI, x= Race, group = Race, color = Race)) + geom_violin()
ggplot(data, aes(y= SleepTime, x= Race, group = Race, color = Race)) + geom_violin()

#clean data
str(data)

#convert to factor
data <- data %>%
  mutate(HeartDisease = as.factor(HeartDisease),
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
         AgeCategory = as.factor(AgeCategory))


#set seed
set.seed(5)

#new column with each row #
data$rownum <- 1:nrow(data)

#split into train and test
train <- data %>% dplyr::sample_frac(0.80)
test  <- dplyr::anti_join(data, train, by = 'rownum')

#fit
model1 <- glm(HeartDisease ~ Smoking + PhysicalHealth + AlcoholDrinking + GenHealth + KidneyDisease + SleepTime + Stroke + Diabetic + PhysicalActivity + AgeCategory + DiffWalking, data = train, family = binomial())
summary(model1)

model2 <- glm(HeartDisease ~ Smoking + AlcoholDrinking + Stroke + Asthma, data = train, family = binomial())
summary(model2)

fit_model <- model2
test$y_pred<- predict(fit_model, test, type = "response")
test$y_pred <-ifelse(test$y_pred>0.5, 1, 0)
test$y_pred <- as.factor(test$y_pred)

confusionMatrix(test$HeartDisease, test$y_pred)
