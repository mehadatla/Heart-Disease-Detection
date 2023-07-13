library("dplyr")
library("tidyverse")
library("stringr")
library("janitor")

getwd()
setwd("/Users/shriya/Desktop/data_mining")
data <- read.csv("heart_2020_cleaned.csv")

# of rows and columns
dim(data)

#view amount of people by race
groups <- data %>% group_by(Race) %>% summarise(count = n())

#change heard disease y/n to 1/0
# data$HeartDisease <- ifelse(data$HeartDisease == "Yes", 1, 0)
data$HeartDisease <- as.factor(data$HeartDisease)


ggplot(data, aes(y= BMI, x= HeartDisease, group = Race, color = Race)) + geom_boxplot()
ggplot(data, aes(y= BMI, x= Race, group = Race, color = Race)) + geom_violin()
