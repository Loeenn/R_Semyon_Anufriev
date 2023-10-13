df <- read.csv("ДЗ3_superstore_data.csv")
head(df)
library(dplyr)
#Задание 1
df <- filter(df, Income > 30000)
print(df)
#Задание 2
df <- select(df, Id, Year_Birth, Education, Marital_Status, Income, Response)
print(df)
#Задание 3
df <- mutate(df, Age = 2023 - Year_Birth, Rich_flag = Income > 80000)
print(df)
#Задание 4
df2 <- df %>% 
  group_by(Education) %>%
  summarize(Average_Income = mean(Income))
print(df2)

#Задание 5
df <- df %>%
  left_join(df2, by = "Education")
print(df)
