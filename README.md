# Heart Disease

![](https://github.com/mehadatla/Heart-Disease-Detection/assets/137425663/c136ab07-1cd1-4adc-998a-b54d8c50dadd)

## Abstract
Heart Disease refers to numerous conditions and is considered to be the leading cause of death in America. In order to prevent such fatalities, it's important to take certain preventative measures, such as screening. Screening for Heart Disease often involves the use of an Electrocardiogram (ECG) which measures the heart's electrical activity. However, this process can cost the average American up to $3000, making it highly unattainable for certain individuals. Additionally, the mortality rate for heart disease is significantly higher in Black Indigenous People of Color (BIPOC), as certain communities don't have access to preventative methods. Using AI, we were able to build a Random Forests model that analyzes common indicators of heart disease to predict the likelihood of having it. 

The Random Forests model used the following factors: Sex, Age, Skin Cancer, Kidney Disease, Stroke, Physical Health, Diabetes, General Health, Difficulty Walking, Smoking, BMI, Mental Health, Race, Asthma, Physical Activity, Alcohol Drinking, and Sleep Time. These factors had a strong correlation with instances of heart disease for the patients within the data set. Furthermore, these factors allowed us to gain insight into the extent to which the factors are associated with heart disease. Using the most prominent factors, we were able to train a Random Forests model with an accuracy of 91.55%.  We hope that this model can serve as a basis to alert people of heart disease and act as a tool for those who don't have access to professional screening. 

![Feature Importance](https://github.com/mehadatla/Heart-Disease-Detection/assets/137425663/e0258fd2-44ce-4818-9ab8-8e960ab77340)

---
## Project Structure/Methodology: 

### 1) Exploratory Data Analysis 
In order to prevent bias within our model we assessed the distribution of racial groups within our data set 
    
    groups <- data %>% group_by(Race) %>% summarise(count = n())
Calculate total number of people with heart disease per race
   
    heart_disease_counts <- aggregate(HeartDisease ~ Race, data = data, FUN = function(x) sum(x == "Yes"))
Merge with the group data frame
    
    groups <- merge(groups, heart_disease_counts, by = "Race", all.x = TRUE)
Calculate the percentages
   
    groups$percentdisease <- groups$HeartDisease / groups$count  # Division operation
    groups$percentdisease <- groups$percentdisease*100

### 2) Splitting the Data
By separating the data we were able to test it using existing data and train with new data to further enhance its accuracy

    train <- data %>% dplyr::sample_frac(0.80)
    test  <- dplyr::anti_join(data, train, by = 'rownum')
Create dataset with only the features

    train_x <- train %>% select(-HeartDisease)
    test_x <- test %>% select(-HeartDisease)

Create dataset with only yes/no from heart disease column

    train_y <- train %>% select(HeartDisease)
    test_y <- test %>% select(HeartDisease)

 ### 3) Random Forests
We created a Random Forests Model that trained/tuned to increase model accuracy
Our final model

    rf_final_model <- randomForest(x = train_x, y = train_y, mtry = 2,importance = TRUE, ntree = 5)
    rf_final_model

Making Predictions
    
    test$y_pred<- predict(rf_final_model, test)
---
## Results and Possibilities: 
With an accuracy score of 91.55% we were able to conclude that the factors that most likely cause heart disease include: 
1. Sex
2. Age
3. Skin Cancer
4. Kidney Disease
5. Stroke
6. Physical Health
7. Diabetes
8. General Health
9. Difficulty Walking
10. Smoking
11. BMI
12. Mental Health
13. Race
14. Asthma
15. Physical Activity
16. Alcohol Drinking
17. Sleep Time
---
##### Our original data set allowed us to conclude that certain races have more access to screening facilities, as their race is more represented in the dataset. We hope to use our model to provide accessible screening for those without healthcare facilities. This specifically includes BIPOC      communities, which face much higher death rates from heart disease than other races and don't have access to healthcare resources. In the future, we hope to build our model to consider more factors that affect heart disease and further enhance the accuracy of our model. 
---



