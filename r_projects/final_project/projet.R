library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)
library(tidyr)
campaign_desc <- read_csv("~/projects/data/R_project/final/campaign_desc.csv")
campaign_table <- read_csv("~/projects/data/R_project/final/campaign_table.csv")
causal_data <- read_csv("~/projects/data/R_project/final/causal_data.csv")
coupon <- read_csv("~/projects/data/R_project/final/coupon.csv")
coupon_redempt <- read_csv("~/projects/data/R_project/final/coupon_redempt.csv")
hh_demographic <- read_csv("~/projects/data/R_project/final/hh_demographic.csv")
transaction_data <- read_csv("~/projects/data/R_project/final/transaction_data.csv")
transaction_data$PRODUCT_ID <- as.character(transaction_data$PRODUCT_ID)
transaction_data$STORE_ID <- as.character(transaction_data$STORE_ID)
transaction_data$time_of_day <- cut(transaction_data$TRANS_TIME, breaks = c(0, 800, 1600, 2400), labels = c("утро", "обед", "ужин"), right = FALSE)

transaction_data <- left_join(transaction_data,hh_demographic,by='household_key')
transaction_data <-transaction_data %>% drop_na(HOUSEHOLD_SIZE_DESC)
transaction_data$HOUSEHOLD_SIZE_DESC <- replace(transaction_data$HOUSEHOLD_SIZE_DESC, transaction_data$HOUSEHOLD_SIZE_DESC == "5+", 5)
transaction_data$HOUSEHOLD_SIZE_DESC <- as.integer(transaction_data$HOUSEHOLD_SIZE_DESC)


df_summary <- transaction_data %>% group_by(time_of_day) %>% summarise(count = n())
transaction_data$time_of_day2 <- cut(transaction_data$TRANS_TIME, breaks = c(0, 1400, 2400), labels = c("до обеда", "после обеда"), right = FALSE)
df_summary2 <- transaction_data %>% group_by(time_of_day2) %>% summarise(count = n())
df_summary3 <- transaction_data %>% group_by(PRODUCT_ID) %>% summarise(count = n())
df_summary4 <- transaction_data %>% group_by(PRODUCT_ID) %>% summarise(total_sales = sum(SALES_VALUE))
df_top10_quantity <- df_summary3[order(-df_summary3$count), ][1:10, ]
df_top10_sales <- df_summary4[order(-df_summary4$total_sales), ][1:10, ]

df_summary4_no_top10 <- transaction_data %>% filter(!PRODUCT_ID %in% c(df_top10_sales$PRODUCT_ID))
df_summary4_no_top10 <- df_summary4_no_top10 %>% group_by(STORE_ID) %>% summarise(total_sales = sum(SALES_VALUE))
df_summary4_no_top10 <- df_summary4_no_top10[order(-df_summary4_no_top10$total_sales), ][1:10, ]
View(transaction_data)
campaign_desc$START_DAY <- as.integer(campaign_desc$START_DAY)
campaign_desc$END_DAY <- as.integer(campaign_desc$END_DAY)
transaction_data$WEEK_NO <- as.integer(transaction_data$WEEK_NO)

ui <- dashboardPage(
  dashboardHeader(title = "НЕРОГА И НЕКОПЫТА ИНКОРПОРЕЙТЕД"),
  dashboardSidebar(
    selectInput("week_filter", label = "Выбор недели",choices = seq(1, 120, by=1),selected = 1),
    sidebarMenu(
      menuItem("Продажи по времени дня", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Магазины и продукты", tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("Показатели бизнеса", tabName = "dashboard3", icon = icon("dashboard"))
    )),
  dashboardBody(
    tabItems(
      tabItem("dashboard1",
              fluidRow(
                box(
                  title = "Число продаж в разные части дня",
                  plotOutput("df1")
                ),
                box(
                  title = "Число продаж до обеда и после обеда",
                  plotOutput("df2")
                )
              )),
      tabItem("dashboard2",
              fluidRow(
                box(
                  title = "Топ продуктов по продажам (количество)",
                  plotOutput("df3")
                ),
                box(
                  title = "Топ продуктов по продажам (сумма)",
                  plotOutput("df4")
                )
              ),
              fluidRow(
                box(
                  title = "Топ магазинов, где нет товаров из топ 10",
                  plotOutput("df5")
                ))
      ),
      tabItem("dashboard3",
              
              fluidRow(
                infoBoxOutput("box1"),
                infoBoxOutput("box2"),
                infoBoxOutput("box3"),
                infoBoxOutput("box4")
              ),
      )   
      
    )
  )
)
# создать серверную часть с функцией renderTable
server <- function(input, output) {
  fdf <- reactive({
    transaction_data %>% dplyr::filter(WEEK_NO >= 0 & WEEK_NO <= as.numeric(input$week_filter))})
  fdf1 <- reactive({campaign_desc %>% dplyr::filter(START_DAY<= as.numeric(input$week_filter)*7 & END_DAY >= as.numeric(input$week_filter)*7 )})
  unique_users <- reactive({
    transaction_data %>% dplyr::filter(WEEK_NO >= 0 & WEEK_NO <= as.numeric(input$week_filter))  %>% distinct(household_key, .keep_all = TRUE)})
  
  fdf2 <-reactive({
    transaction_data %>% dplyr::filter(WEEK_NO >= 0 & WEEK_NO <= as.numeric(input$week_filter)) %>% group_by(household_key) %>% summarise(summa = sum(SALES_VALUE))
  })
  output$df1 <- renderPlot({
    ggplot(df_summary, aes(x = time_of_day, y = count)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Количество наблюдений по времени дня", x = "Время дня", y = "Количество")
  })
  
  output$df2 <- renderPlot({
    ggplot(df_summary2, aes(x = time_of_day2, y = count)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Количество наблюдений по времени дня", x = "Время дня", y = "Количество")
  })
  output$df3 <- renderPlot({
    ggplot(df_top10_quantity, aes(x = PRODUCT_ID, y = count)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Топ продуктов по продажам (количество)", x = "Номер продукта", y = "Количество")
  })
  
  output$df4 <- renderPlot({
    ggplot(df_top10_sales, aes(x = PRODUCT_ID, y = total_sales)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Топ продуктов по продажам (сумма)", x = "Номер продука", y = "Сумма продаж")
  })
  output$df5 <- renderPlot({
    ggplot(df_summary4_no_top10, aes(x = STORE_ID, y = total_sales)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Топ магазинов, где нет товаров из топ 10", x = "Номер магазина", y = "Сумма продаж")
  })
  output$box1 <- renderInfoBox({
    infoBox("Общее число запущенных компаний", nrow(fdf1()))
  })
  output$box2 <- renderInfoBox({
    infoBox("Количество клиентов, которые покупают товары в наших магазинах", sum(unique_users()$HOUSEHOLD_SIZE_DESC))
  })
  output$box3 <- renderInfoBox({
    infoBox("Средняя сумма покупок на одну семью", mean(fdf2()$summa))
  })
  output$box4 <- renderInfoBox({
    infoBox("общее количество проданных товаров", sum(fdf()$QUANTITY))
  })
  
  
}

# запустить приложение
shinyApp(ui, server)
