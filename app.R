# Загрузка библиотек
library(shiny)
library(bslib)
library(DT)

# Загрузка данных
df <- read.csv('data/Walmart_Sales.csv')
df$Date <- as.Date(df$Date, format = '%d-%m-%Y')
df$Temperature <- round((df$Temperature - 32) * 5/9, 1)

# UI
ui <- page_sidebar(
  title = div(
    style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
    div("Walmart Sales Dashboard", style = "font-size: 24px; font-weight: bold;"),
    div(
      class = "switch-buttons",
      actionButton("btn_charts", "Charts", class = "switch-btn"),
      actionButton("btn_data", "Raw Data", class = "switch-btn")
    )
  ),
  
  tags$head(
    tags$style(HTML("
      /* Уменьшаем отступы между элементами в sidebar */
      .sidebar .shiny-input-container {
        margin-bottom: 8px !important;
      }
      .sidebar hr {
        margin-top: 8px !important;
        margin-bottom: 8px !important;
      }
      .sidebar p {
        margin-bottom: 4px !important;
        margin-top: 4px !important;
      }
      .sidebar .help-text {
        margin-bottom: 4px !important;
        margin-top: 4px !important;
      }
      .shiny-select-container {
        margin-bottom: 8px !important;
      }
      .sidebar .form-group {
        margin-bottom: 8px !important;
      }
      .sidebar-content {
        gap: 0px !important;
      }
      
      /* Стиль для кнопок переключения */
      .switch-buttons {
        display: flex;
        gap: 10px;
      }
      .switch-btn {
        padding: 6px 16px;
        font-size: 14px;
        font-weight: bold;
        border: none;
        border-radius: 5px;
        cursor: pointer;
        transition: all 0.3s ease;
        background-color: #f0f0f0;
        color: #333;
      }
      .switch-btn.active {
        background-color: #007bff;
        color: white;
      }
      .switch-btn:hover {
        background-color: #0056b3;
        color: white;
      }
      
      /* Карточки */
      .card {
        margin-bottom: 20px;
      }
      
      /* Графики */
      .shiny-plot-output {
        width: 100%;
      }
    "))
  ),
  
  sidebar = sidebar(
    selectInput("store", "Select Store:", 
                choices = c("All", sort(unique(df$Store))),
                selected = "All"),
    hr(),
    helpText(HTML('<span style="font-size: 13px;">Walmart sales data from 2010-2012</span>')),
    helpText(HTML('<span style="font-size: 13px;">Data from 45 stores across the US</span>')),
    helpText(HTML('<span style="font-size: 13px;">Holiday weeks are marked with Holiday_Flag = 1</span>'))
  ),
  
  # Динамический контент
  uiOutput("dynamic_content")
)

server <- function(input, output, session) {
  
  # Реактивная переменная для отслеживания активной вкладки
  active_view <- reactiveVal("charts")
  
  # Обработчики нажатия на кнопки
  observeEvent(input$btn_charts, {
    active_view("charts")
  })
  
  observeEvent(input$btn_data, {
    active_view("data")
  })
  
  # Фильтрация данных
  filtered_data <- reactive({
    data <- df
    if(input$store != "All") {
      data <- data[data$Store == input$store, ]
    }
    data
  })
  
  # Динамическое содержимое
  output$dynamic_content <- renderUI({
    if(active_view() == "charts") {
      # Две карточки с графиками
      tagList(
        card(
          card_header(
            div(
              "Sales Trend Over Time",
              style = "font-size: 18px; font-weight: bold;"
            )
          ),
          plotOutput("salesPlot", height = "450px")  # ← добавил высоту
        ),
        
        card(
          card_header(
            div(
              "Holiday Impact on Sales",
              style = "font-size: 18px; font-weight: bold;"
            )
          ),
          plotOutput("holidayPlot", height = "450px")  # ← добавил высоту
        )
      )
    } else {
      # Одна карточка с таблицей
      card(
        card_header(
          div(
            "Walmart Sales Data (First 20 Rows)",
            style = "font-size: 18px; font-weight: bold;"
          )
        ),
        DTOutput("rawDataTable"),
        height = "800px"
      )
    }
  })
  
  output$salesPlot <- renderPlot({
    trend_data <- aggregate(Weekly_Sales ~ Date, 
                            data = filtered_data(), 
                            FUN = sum)
    
    plot(trend_data$Date, trend_data$Weekly_Sales,
         type = "l",
         col = "steelblue",
         lwd = 2,
         xlab = "Date",
         ylab = "Total Sales ($)",
         main = paste("Sales Trend -", input$store),
         cex.main = 1.3)
    grid()
  })
  
  output$holidayPlot <- renderPlot({
    holiday_data <- aggregate(Weekly_Sales ~ Holiday_Flag, 
                              data = filtered_data(), 
                              FUN = mean)
    
    if(nrow(holiday_data) == 2) {
      holiday_data$Holiday_Flag <- ifelse(holiday_data$Holiday_Flag == 1, 
                                          "Holiday Weeks", 
                                          "Non-Holiday Weeks")
      
      bp <- barplot(holiday_data$Weekly_Sales,
                    names.arg = holiday_data$Holiday_Flag,
                    col = c("lightblue", "orange"),
                    main = paste("Average Sales:", input$store),
                    ylab = "Average Sales ($)",
                    xlab = "Week Type",
                    ylim = c(0, max(holiday_data$Weekly_Sales) * 1.2),
                    cex.names = 1.1)
      
      text(x = bp, 
           y = holiday_data$Weekly_Sales + 0.10 * max(holiday_data$Weekly_Sales),
           labels = paste0("$", format(round(holiday_data$Weekly_Sales, 0), big.mark = ",")),
           cex = 1.1)
      
      grid()
      
      non_holiday <- holiday_data$Weekly_Sales[holiday_data$Holiday_Flag == "Non-Holiday Weeks"]
      holiday <- holiday_data$Weekly_Sales[holiday_data$Holiday_Flag == "Holiday Weeks"]
      
      if(holiday > non_holiday) {
        diff_percent <- (holiday - non_holiday) / non_holiday * 100
        mtext(side = 3, 
              text = paste0("Holiday weeks have ", round(diff_percent, 1), 
                            "% higher sales"),
              col = "darkgreen", cex = 1.0, line = 0.5)
      } else {
        diff_percent <- (non_holiday - holiday) / holiday * 100
        mtext(side = 3, 
              text = paste0("Non-holiday weeks have ", round(diff_percent, 1), 
                            "% higher sales"),
              col = "darkred", cex = 1.0, line = 0.5)
      }
    } else {
      week_type <- ifelse(unique(holiday_data$Holiday_Flag) == 1, 
                          "Holiday Weeks", "Non-Holiday Weeks")
      barplot(holiday_data$Weekly_Sales,
              names.arg = week_type,
              col = ifelse(week_type == "Holiday Weeks", "orange", "lightblue"),
              main = paste("Average Sales:", input$store),
              ylab = "Average Sales ($)",
              ylim = c(0, holiday_data$Weekly_Sales * 1.2))
      
      text(x = 0.7, 
           y = holiday_data$Weekly_Sales + 0.05 * holiday_data$Weekly_Sales,
           labels = paste0("$", format(round(holiday_data$Weekly_Sales, 0), big.mark = ",")),
           cex = 1.1)
      grid()
      
      mtext(side = 3, 
            text = paste("Only", week_type, "in selected data"),
            col = "gray40", cex = 0.9)
    }
  })
  
  output$rawDataTable <- renderDT({
    table_data <- filtered_data()[1:20, ]
    
    datatable(
      table_data,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Weekly_Sales", "$") %>%
      formatRound("Temperature", 1) %>%
      formatRound("Fuel_Price", 3) %>%
      formatRound("CPI", 2) %>%
      formatRound("Unemployment", 3)
  })
}

shinyApp(ui, server)