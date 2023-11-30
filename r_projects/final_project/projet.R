library(readr)
library(dplyr)
library(plotly)
library(tidyr)
campaign_desc <- read_csv("~/projects/data/R_project/final/campaign_desc.csv")
campaign_table <- read_csv("~/projects/data/R_project/final/campaign_table.csv")
hh_demographic <- read_csv("~/projects/data/R_project/final/hh_demographic.csv")
transaction_data <- read_csv("~/projects/data/R_project/final/transaction_data.csv")
transaction_data$PRODUCT_ID <- as.character(transaction_data$PRODUCT_ID)
transaction_data$STORE_ID <- as.character(transaction_data$STORE_ID)
transaction_data$TRANS_TIME <- as.integer(transaction_data$TRANS_TIME)
transaction_data$time_of_day <- cut(transaction_data$TRANS_TIME, breaks = c(0, 800, 1600, 2400), labels = c("утро", "обед", "ужин"), right = FALSE)

transaction_data <- left_join(transaction_data,hh_demographic,by='household_key')
transaction_data <-transaction_data %>% drop_na(HOUSEHOLD_SIZE_DESC)
transaction_data$HOUSEHOLD_SIZE_DESC <- replace(transaction_data$HOUSEHOLD_SIZE_DESC, transaction_data$HOUSEHOLD_SIZE_DESC == "5+", 5)
# замена nan на медиану
transaction_data <- transaction_data %>% mutate(across(HOUSEHOLD_SIZE_DESC, ~replace_na(., median(., na.rm=TRUE))))

transaction_data$HOUSEHOLD_SIZE_DESC <- as.integer(transaction_data$HOUSEHOLD_SIZE_DESC)



df_summary <- transaction_data %>% group_by(time_of_day) %>% summarise(count = n())
transaction_data$time_of_day2 <- cut(transaction_data$TRANS_TIME, breaks = c(0, 1400, 2400), labels = c("до обеда", "после обеда"), right = FALSE)
df_summary2 <- transaction_data %>% group_by(time_of_day2) %>% summarise(count = n())
df_summary3 <- transaction_data %>% group_by(PRODUCT_ID) %>% summarise(count = sum(QUANTITY))
df_summary4 <- transaction_data %>% group_by(PRODUCT_ID) %>% summarise(total_sales = sum(SALES_VALUE))
df_top10_quantity <- df_summary3[order(-df_summary3$count), ][1:10, ]
df_top10_sales <- df_summary4[order(-df_summary4$total_sales), ][1:10, ]

df_summary4_no_top10 <- transaction_data %>% filter(!PRODUCT_ID %in% c(df_top10_sales$PRODUCT_ID))
df_summary4_no_top10 <- df_summary4_no_top10 %>% group_by(STORE_ID) %>% summarise(total_sales = sum(SALES_VALUE))
df_summary4_no_top10 <- df_summary4_no_top10[order(-df_summary4_no_top10$total_sales), ][1:10, ]
campaign_desc$START_DAY <- as.integer(campaign_desc$START_DAY)
campaign_desc$END_DAY <- as.integer(campaign_desc$END_DAY)
transaction_data$WEEK_NO <- as.integer(transaction_data$WEEK_NO)
hh_demographic$HOUSEHOLD_SIZE_DESC <- replace(hh_demographic$HOUSEHOLD_SIZE_DESC, hh_demographic$HOUSEHOLD_SIZE_DESC == "5+", 5)
hh_demographic$HOUSEHOLD_SIZE_DESC <- as.integer(hh_demographic$HOUSEHOLD_SIZE_DESC)




# запустить приложение
shinyApp(ui, server)

