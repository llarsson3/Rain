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

##########################################
####   User interface                 ####
##########################################

# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "Rain forecasting",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "ERASE-TBSG",
      img(src = "image.png", width = "100%", class = "bg"),
    )),
    
    # Infobox with today's date
    
    fluidRow(
      style = "margin-bottom: 20px;",  # Add some margin to separate from other content
      div(
        class = "custom-infobox",  # Add a custom CSS class
        infoBox(
          "Last date collected:  ",
          "2023-10-31",
          icon = icon("calendar"),
          width = 3
        )
      )
    ),
    
    tags$br(),
    
    ##########################################
    ####  Panel: Main>Summary             ####
    ##########################################
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
      
        #### Rainfall over time  
        
        fluidRow(
          column(6, plotlyOutput("raintime"))
        ),
      ),
    )
  )
)

##########################################
####   Attaching datasets             ####
##########################################

rain <- readRDS("RainForecast.Rda")

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

  output$raintime <- renderPlotly({
    colmap <- c("#193300",
                "#4C9900",
                "#B2FF66")
    
    rain <- rain %>%
      arrange(Date) 
    
    ggplotly(
      ggplot(rain, aes(x = Date, y = Measurement)) +
        geom_line() +
        labs(
          title = "Rain fall over time",
          x = "Date",
          y = "Rain fall (mm)"
        ) +
        scale_color_manual(values = colmap)
    )
  })
  
}

##########################################
####   Run app.                       ####
##########################################

shinyApp(ui, server)