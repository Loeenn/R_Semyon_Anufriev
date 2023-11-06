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

################
#Задание 4

View(df)
#мне показалась очень удобной, намного проще чем в пандасе без аддонов для vsc, так эти таблички полезны для дата инженеринга и препроцессинга, чтобы лучше понимать структуру данных, etc бинарность признака.
#install.packages("pivottabler")
library(pivottabler)
pt <- PivotTable$new()
pt$addData(df)
pt$addColumnDataGroups("Marital_Status")
pt$addRowDataGroups("Education")
pt$defineCalculation(calculationName="Total count", summariseExpression="n()")
pt$evaluatePivot()
pt


#install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = Education, fill = Rich_flag)) +
  geom_bar() +
  labs(title = "Number of peope by type of education",
       x = "Education",
       y = "Count") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))+ coord_flip()


ggplot(df, aes(x = Year_Birth)) +
  geom_line(stat = "count",color = 'red') +
  labs(title = "Distribution by birth",
       x = "Year of birth",
       y = "Count")