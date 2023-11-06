df <- read.csv("~/projects/R_Semyon_Anufriev/r_projects/lesson_4/ДЗ3_superstore_data.csv")
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


############
# Задание 5
library(tidyverse)
df <- df %>%
  unite(Education_Marital_Status, Education, Marital_Status, sep = "_")


df$Rich_flag <- as.numeric(df$Rich_flag)

for (col in names(df)) {
  df[is.nan(df[, col])] <- 0
}

library(ggplot2)
ggplot(df, aes(x = factor(Response), y = Age)) +
  geom_boxplot() +
  labs(
    title = "ящик с усами",
    x = "Response",
    y = "Age"
  )

library(stringr)
rows_with_9 <- str_which(df$Age, pattern = "9")
df[rows_with_9, ]
