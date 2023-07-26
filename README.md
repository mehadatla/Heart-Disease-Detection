# Heart Disease

![]("Percent With Heart Disease by Race (bar graph).png")

## Abstract
Heart Disease refers to numerous conditions and is considered to be the leading cause of death in America. In order to prevent such fatalities, it's important to take certain preventative measures, such as screening. Screening for Heart Disease often involves the use of an Electrocardiogram (ECG) which measures the heart's electrical activity. However, this process can cost the average American up to $3000, making it highly unattainable for certain individuals. Additionally, the mortality rate for heart disease is significantly higher in Black Indigenous People of Color (BIPOC), as certain communities don't have access to preventative methods. Using AI, we were able to build a Random Forests model that analyzes common indicators of heart disease to predict the likelihood of having it. 

The Random Forests model used the following factors: Sex, Age, Skin Cancer, Kidney Disease, Stroke, Physical Health, Diabetes, General Health, Difficulty Walking, Smoking, BMI, Mental Health, Race, Asthma, Physical Activity, Alcohol Drinking, and Sleep Time. These factors had a strong correlation with instances of heart disease for the patients within the data set. Furthermore, these factors allowed us to gain insight into the extent to which the factors are associated with heart disease. Using the most prominent factors, we were able to train a Random Forests model with an accuracy of 91.55%.  We hope that this model can serve as a basis to alert people of heart disease and act as a tool for those who don't have access to professional screening. 

**put factors graph here

---
## Project Structure: 

#view amount of people by race
    groups <- data %>% group_by(Race) %>% summarise(count = n())
#calculate total people with heart disease per race
    heart_disease_counts <- aggregate(HeartDisease ~ Race, data = data, FUN = function(x) sum(x == "Yes"))
 #merge with the groups dataframe
    groups <- merge(groups, heart_disease_counts, by = "Race", all.x = TRUE)
 #calculate percentage
    groups$percentdisease <- groups$HeartDisease / groups$count  # Division operation
    groups$percentdisease <- groups$percentdisease*100

---
## Possibilities: 

---
## Methodology: 
---


