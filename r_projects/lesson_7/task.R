library(shiny)
library(shinydashboard)
library(ggplot2)

df <- read.csv("~/projects/R_Semyon_Anufriev/r_projects/lesson_7/ДЗ2_vgsales.csv")

df$Year <- as.numeric(df$Year)
# Удаление строк с NA в столбце "Year"
df <- df[!is.na(df$Year), ]


ui <- dashboardPage(
  dashboardHeader(title = "Аналитическое веб-приложение"),
  dashboardSidebar(
    selectInput("platform_filter", "Фильтр по платформе", unique(df$Platform)),
    sliderInput("year_filter", "Фильтр по году", min(df$Year), max(df$Year), value = c(min(df$Year), max(df$Year)))
  ),
  dashboardBody(
    fluidRow(
      infoBoxOutput("na_sales_kpi"),
      infoBoxOutput("eu_sales_kpi"),
      infoBoxOutput("jp_sales_kpi"),
      infoBoxOutput("other_sales_kpi")
    ),
    fluidRow(
      box(
        title = "Динамика продаж",
        plotOutput("sales")
      ),
      box(
        title = "Рейтинг по жанрам",
        plotOutput("genre_sales")
      )
    ),
    fluidRow(
      box(
        title = "Таблица с данными",
        tableOutput("df_table")
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_df <- reactive({
    df %>% filter(Platform %in% input$platform_filter,
                    Year >= input$year_filter[1],
                    Year <= input$year_filter[2])
  })
  output$na_sales_kpi <- renderInfoBox({
    na_sales_mean <- mean(filtered_df()$NA_Sales)
    infoBox("NA Sales mean", na_sales_mean)
  })
  
  output$eu_sales_kpi <- renderInfoBox({
    eu_sales_mean <- mean(filtered_df()$EU_Sales)
    infoBox("EU Sales mean", eu_sales_mean)
  })
  
  output$jp_sales_kpi <- renderInfoBox({
    jp_sales_mean <- mean(filtered_df()$JP_Sales)
    infoBox("JP Sales mean", jp_sales_mean)
  })
  
  output$other_sales_kpi <- renderInfoBox({
    other_sales_mean <- mean(filtered_df()$Other_Sales)
    infoBox("Other Sales mean", other_sales_mean)
  })
  
  output$sales <- renderPlot({
    ggplot(filtered_df(), aes(x = Year)) +
      geom_line(aes(y = NA_Sales, color = "NA Sales")) +
      geom_line(aes(y = EU_Sales, color = "EU Sales")) +
      geom_line(aes(y = JP_Sales, color = "JP Sales")) +
      geom_line(aes(y = Other_Sales, color = "Other Sales")) +
      labs(title = "Динамика продаж",
           x = "Год",
           y = "Продажи") +
      scale_color_manual(values = c("NA Sales" = "blue", "EU Sales" = "green", "JP Sales" = "red", "Other Sales" = "purple"))
  })
  
  output$df_table <- renderTable({
    filtered_df()
  })
  
  
  output$genre_sales <- renderPlot({
    genre_df <- filtered_df() %>% group_by(Genre) %>% summarise(NA_Sales = sum(NA_Sales),
                                                                    EU_Sales = sum(EU_Sales),
                                                                    JP_Sales = sum(JP_Sales),
                                                                    Other_Sales = sum(Other_Sales))
    
    genre_df1 <- pivot_longer(genre_df, cols = -Genre, names_to = "Region", values_to = "Sales")
    
    ggplot(genre_df1, aes(x = Genre, y = Sales, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(title = "Рейтинг продаж по жанрам",
           x = "Жанр",
           y = "Продажи",
           fill = "Регион")
  })
  
}

# Запуск приложения
shinyApp(ui, server)