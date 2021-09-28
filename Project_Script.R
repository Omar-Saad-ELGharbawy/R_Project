setwd("C:/Users/Lenovo/Documents/57357R/Final_Project/R_Project")
library(tidyverse)
df=read.csv("census_income_original_2.csv")

df[df$ID==5,]
df[32562,]
#Selecting non repeated IDs
df=df[1:32561,]
df=df%>% mutate(income_more50K= income==">50K")
str(df)
summary(df)
subset(df)

#Question 1 
unique(df$income)
unique(df$income_c)
unique(df$age)

age=df %>%group_by(age)%>% summarise(sum_big_income=sum(income_more50K)) %>% arrange(-sum_big_income)
summary(age)
str(age)
plot(age$age,age$sum_big_income,pch=19)
x=age[1:15,"age"] %>% arrange(age)
x
y=age[age$sum_big_income==0,"age"]
y
paste("so from the graph we notice that ages:",x,"has the largest opportunity to have income more than 50k while ages",y,"always have income less than 50")

#Question 2

df[df$Gender=="M"|df$Gender=="m"|df$Gender=="malee"|df$Gender=="male"|df$Gender=="m ","Gender"]="Male"
df[df$Gender=="female"|df$Gender=="f","Gender"]="Female"
unique(df$Gender)

hours=df %>% group_by(hours.per.week) %>% summarise(cnt=n())
df %>% filter(Gender=="Male") %>% group_by(hours.per.week) %>% summarise(cnt=n()) %>% arrange(-cnt) %>% head(1)
df%>% filter(Gender=="Female")%>% group_by(hours.per.week) %>% summarise(cnt=n()) %>% arrange(-cnt) %>% head(1)
gender_equality=df %>% filter(hours.per.week==40) %>%group_by(Gender) %>% summarise(cnt=n(),sum_income=sum(income_more50K),percent_high=sum_income/cnt*100)
 
male_percent=gender_equality[gender_equality$Gender=="Male","percent_high"]
female_percent=gender_equality[gender_equality$Gender=="Female","percent_high"]
paste("No we didin't achieve gender equality because for people who works 40 hours per week the male percentage of having income more than 50 k is:",round(male_percent,3),"while the female percentage is:",round(female_percent,3))

#Question 3
unique(df$workclass)
workclass=df %>%group_by(workclass) %>% summarise(cnt=n(),income=sum(income_more50K),percent=income/cnt*100 )
print("Workclass could be an income predictor as percentage of having income more than 50 k changes from workclass to another")
unique(df$race)
race=df %>%group_by(race) %>% summarise(cnt=n(),income=sum(income_more50K),percent=income/cnt*100 )
print("race could be an income predictor as percentage of having income more than 50 k changes from race to another")

unique(df$education)
education=df %>%group_by(education) %>% summarise(cnt=n(),income=sum(income_more50K),percent=income/cnt*100)%>%arrange(-percent)
print("Education has high effect on the percent of having high income as high education degrees has more percentage of having high income")
unique(df$education.num)
education_number=df %>%group_by(education.num) %>% summarise(cnt=n(),income=sum(income_more50K),percent=income/cnt*100)%>%arrange(-percent)
print("Number of years of education has high effect on the percent of having high income as by increasing the years of education the percentage increases and it seems like linear relation in this graph")
plot(education_number$education.num,education_number$percent)

print("So Workclass,race and education can be also an income predictors")
