library("dplyr")
library("tidyverse")
library("stringr")
library("janitor")
library("extrafont")
library(caret)
library(RColorBrewer)
library(ggplot2)

font_import(pattern = "Abhaya Libre") # Import the Abhaya Libre font

getwd()
setwd("/Users/shriya/Desktop/heartfailure")
data <- read.csv("heart_2020_cleaned.csv")

#view amount of people by race
groups <- data %>% group_by(Race) %>% summarise(count = n())
# calculate total people with heart disease per race
heart_disease_counts <- aggregate(HeartDisease ~ Race, data = data, FUN = function(x) sum(x == "Yes"))
# merge with the groups dataframe
groups <- merge(groups, heart_disease_counts, by = "Race", all.x = TRUE)
#calculate percentage
groups$percentdisease <- groups$HeartDisease / groups$count  # Division operation
groups$percentdisease <- groups$percentdisease*100

#BAR GRAPH
ggplot(groups, aes(x = Race, y = percentdisease, group = Race, fill = Race)) +
  geom_bar(stat = "identity") +
  labs(x = "Race", y = "Percentage of People with Heart Disease") +
  ggtitle("Percentage of People with Heart Disease by Race") +
  theme_minimal() + scale_fill_brewer(palette = "Set2")   #+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

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

#create dataset with only the features
train_x <- train %>% select(-HeartDisease)
test_x <- test %>% select(-HeartDisease)

#create dataset with only yes/no from heart disease column
train_y <- train %>% select(HeartDisease)
test_y <- test %>% select(HeartDisease)

#convert into vector
test_y <- unlist(test_y)
train_y <- unlist(train_y)

#train model
rfm1 <- randomForest(x = train_x, y = train_y, xtest = test_x, ytest = test_y, importance = TRUE, ntree = 5)

# find the best value for the mtry hyperparameter. Set the x, y, xtest, ytest as before. Set the ntreeTry value to 500 (it will build 500 trees per try), stepFactor to 1.5, improve = 0.01, trace = TRUE, and plot = TRUE 

mtry <- tuneRF(x = train_x, y = train_y, xtest = test_x, ytest = test_y, ntreeTry = 600, stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE)

# The code below will save the best value for the mtry and print it out
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#tune number of trees
rf_res_df <-
  data.frame(
    TRAINING_ERROR = rfm1$err.rate[,1],
    ITERATION = c(1:5)
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
    ntree = 5
  )
rf_final_model

#making predictions
test$y_pred<- predict(rf_final_model, test)

rf_features
#Feature Analysis
rf_features <- as.data.frame(varImp(rf_final_model))

#feature analysis
## Rename the column name to rf_imp
colnames(rf_features) <- "rf_imp"

## convert rownames to column
rf_features$feature <- rownames(rf_features)

## Selecting only relevant columns for mapping
rf_features <- rf_features[-nrow(rf_features), ]

features <- rf_features %>% dplyr::select(c(feature, rf_imp)) %>% arrange(desc(rf_imp))

### Plot the feature importance

feature_plot <- features %>%
  ggplot(aes(x = rf_imp, y = feature, color = rf_imp)) +  # Updated color mapping to rf_imp
  geom_point(position = position_dodge(0.5), size = 3) +   # Increased point size for better visibility
  labs(x = "Feature Importance", y = "Feature", color = "Importance") +  # Added axis labels and color legend label
  theme_minimal() +  # Using a minimal theme for a cleaner look
  theme(axis.text = element_text(size = 12),               # Increased axis text size
        axis.title = element_text(size = 14, face = "bold"), # Increased axis title size and made it bold
        legend.title = element_text(size = 12, face = "bold"))  # Increased legend title size and made it bold

print(feature_plot)

# Connecting line between 0 and the feature
feature_plot+ geom_linerange(aes(xmin = 0, xmax = rf_imp),
               linetype = "solid",
               position = position_dodge(.5)) +
  # Vertical line at 0
  geom_vline(xintercept = 0,
             linetype = "solid",
             color = "grey70") +
  # Adjust the scale if you need to based on your importance
  scale_x_continuous(limits = c(-0.3, 5)) +
  # Label the x and y axes
  labs(x = "Importance", y = "Feature") +
  # Make the theme pretty
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif")) +
  guides(color = guide_legend(title = NULL)) +
  # Plot them in order of importance
  scale_y_discrete(limits = features$feature[order(features$rf_imp, decreasing = FALSE)])

#KIDNEY DISEASE GRAPH
kidney_disease_counts <- aggregate(KidneyDisease ~ Race, data = data, FUN = function(x) sum(x == "Yes"))
# merge with the groups dataframe
groups <- merge(groups, kidney_disease_counts, by = "Race", all.x = TRUE)
#calculate percentage
groups$percentk <- groups$KidneyDisease / groups$count  # Division operation
groups$percentk <- groups$percentk*100

ggplot(groups, aes(x = Race, y = percentk, group = Race, fill = Race)) +
  geom_bar(stat = "identity") +
  labs(x = "Race", y = "Percentage of People with Kidney Disease") +
  ggtitle("Percentage of People with Kidney Disease by Race") +
  theme_minimal() + theme(
    # Increase the font size of the title
    plot.title = element_text(size = 16),
    # Increase the font size of axis labels
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 15)) +
  scale_fill_brewer(palette = "Set2") + labs(x=expression('American Indians/Alaskan'[Natives])   #+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

