library(ggplot2)

server <- function(input, output) {
  output$df1 <- renderPlot({
    ggplot(df_summary, aes(x = time_of_day, y = count)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Количество наблюдений по времени дня", x = "Время дня", y = "Количество")
  })
  
  output$df2 <- renderPlot({
    ggplot(df_summary2, aes(x = time_of_day2, y = count)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Количество наблюдений по времени дня", x = "Время дня", y = "Количество")
  })
  output$df3 <- renderPlot({
    ggplot(df_top10_quantity, aes(x = PRODUCT_ID, y = count)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Топ продуктов по продажам (количество)", x = "Номер продукта", y = "Количество")
  })
  
  output$df4 <- renderPlot({
    ggplot(df_top10_sales, aes(x = PRODUCT_ID, y = total_sales)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Топ продуктов по продажам (сумма)", x = "Номер продука", y = "Сумма продаж")
  })
  output$df5 <- renderPlot({
    ggplot(df_summary4_no_top10, aes(x = STORE_ID, y = total_sales)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Топ магазинов, где нет товаров из топ 10", x = "Номер магазина", y = "Сумма продаж")
  })
  output$box1 <- renderValueBox({
    valueBox(nrow(campaign_desc),"Общее число запущенных компаний",icon = icon("list"),color = "aqua")
  })
  output$box2 <- renderValueBox({
    valueBox(sum((transaction_data  %>% distinct(household_key, .keep_all = TRUE))$HOUSEHOLD_SIZE_DESC),"Количество клиентов, которые покупают товары в наших магазинах", icon = icon("list"),color = "aqua" )
  })
  output$box3 <- renderValueBox({
    valueBox( round(mean((transaction_data %>%  group_by(household_key) %>% summarise(summa = sum(SALES_VALUE)))$summa),3),"Средняя сумма покупок на одну семью",icon = icon("list"),color = "aqua" )
  })
  output$box4 <- renderValueBox({
    valueBox(sum(transaction_data$QUANTITY),"общее количество проданных товаров", icon = icon("list"),color = "aqua")
  })
  
  output$plot_users1 <- renderPlot({
    ggplot(
      hh_demographic %>% drop_na() %>% group_by(AGE_DESC) %>% summarise(summa = sum(HOUSEHOLD_SIZE_DESC)), aes(x = AGE_DESC, y = summa)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Возраст целевой аудитории", x = "Возраст покупателя", y = "Количество")
  })
  output$plot_users2 <- renderPlot({
    ggplot(hh_demographic %>% drop_na() %>% group_by(HH_COMP_DESC) %>% summarise(summa = n()), aes(x = HH_COMP_DESC, y = summa)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Семья целевой аудитории", x = "Тип семьи", y = "Количество")
  })
  
  
  #Детализированные цифры
  
  
  output$table1 <- renderFormattable({
    
    formattable(
      df_summary[,c("time_of_day","count")],
      list(
        'count' = color_tile("skyblue", "lightpink")
        
        ## a coloured bar with length proportional to value
        #`buy_value` = color_bar("lightblue"),
      )
    )
  })
  
  
  output$table2 <- renderFormattable({
    
    formattable(
      df_top10_quantity[,c("PRODUCT_ID","count")],
      list(
        'count' = color_bar("pink")
      )
    )
  })
  
  output$table3 <- renderFormattable({
    
    formattable(
      df_summary4_no_top10[,c("STORE_ID","total_sales")],
      list(
        'total_sales' = color_tile("pink", "skyblue")
      )
    )
  })
  output$table4 <- renderFormattable({
    
    formattable(
      df_top10_sales[,c("PRODUCT_ID","total_sales")],
      list(
        'total_sales' = color_tile("pink", "skyblue")
      )
    )
  })
  output$table5 <- renderFormattable({
    
    formattable(
      
      head((transaction_data %>%  group_by(household_key) %>% summarise(summa = sum(SALES_VALUE)))[,c("household_key","summa")] %>%  arrange(summa),n=10),
      list(
        'summa' = color_bar("pink")
      )
    )
  })
  
  output$table6 <- renderFormattable({
    
    formattable(
      head((transaction_data %>%  group_by(household_key) %>% summarise(quantity = sum(QUANTITY)))[,c("household_key","quantity")] %>%  arrange(desc(quantity)),n=10),
      list(
        'quantity' = color_bar("pink")
      )
    )
  })
}