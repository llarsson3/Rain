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
        )
      )
    )
  )
)
##########################################
####   Attaching datasets             ####
##########################################

rain <- readRDS("RainForecast.Rds")

## Setting data tables view

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
  
  # render density plot
  
  output$densityPlot <- renderPlotly({
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
    
    ggplotly(
      ggplot(data = dent(), aes_string(x = "`Water (mm)`")) +
        geom_density(aes(fill = Month), size = 1, alpha=0.75) +
        theme(legend.position = "bottom") + labs(x = "Water (mm)") +
        scale_fill_manual(values = colmap) +
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    ) %>% layout(legend = list(orientation = "h",
                               y = 0, x = 0))
    
  })
  
  
  # --------------------
  # box plot section
  # --------------------
  
  # filter the checkgroup input:
  
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
      scale_fill_manual(values = colmap)
    
    ggplotly(
      p,
      tooltip = "text",
      height = 500
    )
  })
  
  observeEvent(input$addDataBtn, {
    newDataRow <- data.frame(
      `Water (mm)` = as.numeric(input$newWater),
      Month = as.character(input$newMonth),
      Year = as.numeric(input$newYear)
    )
    rain <<- rbind(rain, newDataRow)
  })
  
}

##########################################
####   Run app.                       ####
##########################################

shinyApp(ui, server)