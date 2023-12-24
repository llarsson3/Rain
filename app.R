##########################################
####   Main Libraries                 ####
##########################################

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggthemes)
library(plotly)
library(rsconnect)
library(shinythemes)
library(haven)
library(labelled)
library(foreign)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(doBy)

##########################################
####   User interface                 ####
##########################################

# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "Bois-de-Lessines Rainfall",
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Main",
    titlePanel(div(
      windowTitle = "GraduatEmploymentSG",
      img(src = "Snow.jpg", width = "100%", class = "bg"),
    )),
    
    fluidRow(
      style = "margin-bottom: 20px;",
      div(
        class = "custom-infobox",
        infoBox(
          "Today's date:  ",
          Sys.Date(),
          icon = icon("calendar"),
          width = 3
        )
      )
    ),
    
    tags$br(),
    
    dateRangeInput("dateRange", "Select Date Range", start = Sys.Date(), end = Sys.Date()),
    
    tags$br(),
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        sidebarLayout(
          sidebarPanel(
            h3("Rain average"),
            tags$br(),
            checkboxGroupInput(
              "checkGroup",
              label = "Select year",
              choices = list(
                "2023" = "2023",
                "2024" = "2024"
              ),
              selected = list(
                "2023" = "2023",
                "2024" = "2024"
              )
            ),
            tags$br(),
            textInput("newWater", "Enter Water (mm):"),
            textInput("newMonth", "Enter Month:"),
            numericInput("newYear", "Enter Year:", min = 1900, max = 2100, value = 2023),
            actionButton("addDataBtn", "Add Data")
          ),
          
          mainPanel(
            h3("Summary"),
            plotlyOutput(outputId = "boxPlot"),
            tags$br(),
            tags$br()
            )
        ),
        
        sidebarLayout(
          sidebarPanel(
            h4("Quantity of rain over time"),
            tags$br(),
            textOutput("latestDateInfo")
          ),
          
          mainPanel(
            plotOutput("timeSeriesPlot"),
            tags$hr()
          )
        ),
        
        sidebarLayout(
          sidebarPanel(
            h5("Table with data by date to help visualise the latest date entered"),
            tags$br(),
            radioButtons(
              "radio",
              label = "Select year",
              choices = list(
                "2023" = "2023",
                "2024" = "2024"
              ),
              selected = "2023" 
            ),
          ),
          
          mainPanel(
            dataTableOutput("PostWindowTable"),
            tags$hr()
          )
        ),
      )
    )
  )
)
##########################################
####   Attaching datasets             ####
##########################################

rain <- readRDS("RainForecast.Rds")
min_date <- readRDS("RainForecast_MinDate.Rds")
max_date <- readRDS("RainForecast_MaxDate.Rds")

## Setting data tables view

options(shiny.maxRequestSize = 160*1024^2)

opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(
    targets = c(1, 6), searchable = FALSE
  ))
)


##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {
  
  # ----------------
  # Summary section
  # ----------------
  
  ## Rain fall over time

  dent <-  reactive({
    return(rain[rain$Year %in% input$checkGroup, ])
    
  })
  
  # Box plot
  
  uniMedian <- reactive({
    rain_subset <- rain[rain$Year %in% input$checkGroup, ]
    return(rain_subset)
  })
  
  
  output$boxPlot <- renderPlotly({
    colmap <- c("#1A237E",
                "#283593",
                "#303F9F",
                "#3949AB",
                "#3F51B5",
                "#5C6BC0",
                "#7986CB",
                "#9FA8DA",
                "#C5CAE9",
                "#E8EAF6",
                "#BBDEFB",
                "#90CAF9")
    
    data_subset <- uniMedian()
    
    p <- ggplot(
      data = data_subset, 
      aes(
        x = Month, 
        y = `Water (mm)`, 
        fill = Month
      )) +
      geom_boxplot(size = 1, alpha = 0.75) +
      labs(x = "Month") +
      scale_fill_manual(values = colmap) +
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text",
      height = 500
    ) %>%
      layout(font = list(family = "sans-serif"))
  })
  
  # Adding more data
  
  observeEvent(input$addDataBtn, {
    newDataRow <- data.frame(
      `Water (mm)` = as.numeric(input$newWater),
      Month = as.character(input$newMonth),
      Year = as.numeric(input$newYear)
    )
    rain <<- rbind(rain, newDataRow)
  })
  
  # Data table
  observe({
    print(input$radio)
  })
  
  filtered_data <- reactive({
    req(input$radio)
    rain %>% filter(Year == input$radio)
  })
  
  output$PostWindowTable <- renderDataTable({
    data_to_display <- filtered_data()
    data_to_display$Date <- as.Date(data_to_display$Date)
    datatable(
      data_to_display[, c("Date", "Water (mm)")],
      rownames = FALSE,
      class = "table",
      options = list(pageLength = 10, scrollX = TRUE),
      colnames = c("Date", "Water (mm)")
    )
  })
  
    
    # Trend over time
  output$timeSeriesPlot <- renderPlot({
    
    rain_ma <- rain %>%
      arrange(as.Date(Date)) %>%
      mutate(MovingAverage = zoo::rollmean(`Water (mm)`, k = 7, fill = NA))
    
    ggplot(data = rain, aes(x = as.Date(Date), y = `Water (mm)`)) +
      geom_bar(stat = "identity", fill = "#303F9F") +
      geom_line(data = rain_ma, aes(x = as.Date(Date), y = MovingAverage), color = "#90CAF9", size = 1) +
      labs(x = "Date", y = "Water (mm)", title = "Water (mm) over time with moving average") +
      scale_x_date(breaks = seq(min(as.Date(rain$Date)), max(as.Date(rain$Date)), by = "1 month"),
                   labels = function(x) format(x, "%Y-%m")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        plot.margin = margin(50, 50, 50, 30)
      )
  })
    
    # Latest date display
    
    output$latestDateInfo <- renderText({
      req(input$dateRange)
      latest_date <- max(filtered_data()$Date)
      paste("Latest Date in Selected Range: ", format(latest_date, "%Y-%m-%d"))
    })
  
}

##########################################
####   Run app.                       ####
##########################################

shinyApp(ui, server)