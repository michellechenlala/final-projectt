library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  
  
  
  dashboardHeader(title = "Stock Price Shiny App"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Trend of Stock Price", tabName = "StockPriceHistory")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
    ),
    tabItems(
  
      tabItem(tabName = "Introduction",
              h1(align = "center","Introduction"),
              pre("This shiny app shows the trend of Historical Stock Price with line chart. 
              The data is from Alpha Vantage API 'Time Series Daily' including 5 tickers (IBM, MSFT, VRTX, NSC, OMC).
              Click the tab 'Trend of Stock Price' on the left handside. 
              Click 'Go' button after you fill the ticker name which you would like to show, date range and price types e.g. open and close. 
              Thus, the line plot shows."),
              div(
                img(src = "example.png", height = 500, width = 1000)
              )
              
      ),
      
      tabItem(tabName = "StockPriceHistory",
              h1(align = "center",
                 "Stock Price History"),
              fluidRow(
                box(width = 3,align = "center",
                    textInput("TichkeInput", "Input Ticker:",value = "IBM"),
                    dateRangeInput("date", strong("Select Data Range:"), 
                                   start = "2000-06-01", end = "2020-06-30",
                                   min = "2000-06-08", max = "2020-06-09", startview = "year"),
                    checkboxGroupInput(inputId = "showfiger", label = "Show History For:",inline = TRUE,selected="open",choices=c("open","close","high","low")),
                    actionButton("go","Go")
                ),
                
                box(width = 9,
                    plotOutput("plot", height = 500, click='pl_click')
                )
              )
      )
      
      
    )
    
  )
  
  
  
))