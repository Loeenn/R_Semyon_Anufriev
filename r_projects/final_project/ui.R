library(shiny)
library(shinydashboard)
library(formattable)
ui <- dashboardPage(
  dashboardHeader(title = "НЕРОГА И НЕКОПЫТА ИНКОРПОРЕЙТЕД"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Продажи по времени дня", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Магазины и продукты", tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("Показатели бизнеса", tabName = "dashboard3", icon = icon("dashboard")),
      menuItem("Детализированные цифры", tabName = "dashboard4", icon = icon("dashboard"))
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
                valueBoxOutput("box1"),
                valueBoxOutput("box2"),
                valueBoxOutput("box3"),
                valueBoxOutput("box4")
              ),
              fluidRow(
                plotOutput("plot_users1"),
                plotOutput("plot_users2")
              ),
      ),
      tabItem("dashboard4",
              fluidRow(
                box(
                  title = "Продажи по времени дня",
                  formattableOutput('table1')),
                box(
                  title = "Топ продуктов по продажам (количество)",
                  formattableOutput("table2"))
              ),
              fluidRow(
                box(
                  title = "Топ продуктов по продажам (сумма)",
                  formattableOutput('table3')),
                box(
                  title = "Топ магазинов, где нет товаров из топ 10",
                  formattableOutput("table4"))
              ),
              fluidRow(
                box(
                  title = "Топ семей с наименьшей суммой купленных товаров",
                  formattableOutput('table5')),
                box(
                  title = "Топ семей с наибольшим числом купленных товаров",
                  formattableOutput("table6"))
              )
      )
      
    )
  )
)