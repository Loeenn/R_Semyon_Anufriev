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
# Задание 6

library(plotly)
scatter_plot <- plot_ly(df, x = ~Age, y = ~Income, mode = "markers", type = "scatter", marker = list(size = 8))
scatter_plot

heatmap_plot <- plot_ly(df, x = ~Education, y = ~Marital_Status, z = ~Average_Income, type = "heatmap")
heatmap_plot

data <- data.frame(
  label = c("Сотрудник 1", "Реклама 1", "Реклама 2", "Сотрудник 2", "Реклама 3"),
  parent = c("", "Сотрудник 1", "Сотрудник 1", "", "Сотрудник 2"),
  value = c(1, 1, 1, 1, 1)
)
tree_map <- plot_ly(
  data,
  ids = ~label,
  labels = ~label,
  parents = ~parent,
  type = "treemap"
)
tree_map


library(leaflet)
latitude <- 36
longitude <- 138

map <- leaflet() %>%
  setView(lng = longitude, lat = latitude, zoom = 8) %>%
  addTiles() %>%
  addMarkers(lng = longitude, lat = latitude, popup = "Япония")

map

library(DT)
datatable(df)

library(rpivotTable)
rpivotTable(df, rows = "Education", cols = "Marital_Status")