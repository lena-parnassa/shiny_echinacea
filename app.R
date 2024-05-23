library(shiny)
library(tidyverse)
library(DT)
library(here)
library(DataEditR)
library(shinyWidgets)

dailyData <- read_csv("data/datedDemo.csv")
errorData <- read_csv("data/errorData.csv")
errorData <- rbind(errorData, c(1, "none", "lol this is fake", NA))
errorData <- rbind(errorData, c(1, "none", "lol this is also fake", NA))

errorVals <- as.numeric(errorData$demo.id)
dailyData$error <- ifelse(dailyData$demo.id %in% errorData$demo.id, TRUE, FALSE)

ui <- fluidPage(
  titlePanel("Daily Data Check"),
  sidebarLayout(
    sidebarPanel(
      "Filter Observations By:", width = 3,
      fluidRow(
        column(10, selectInput("user_select", label = "Username", multiple = TRUE,
                               choices = c("All", unique(dailyData$UserName)),
                               selected = "All")),
        column(10, selectInput("site_select", label = "Site Name", multiple = TRUE,
                               choices = c("All", unique(dailyData$siteName)),
                               selected = "All")),
        column(10, selectInput("error_select", label = "Error Present", multiple = FALSE,
                               choices = c("None Selected", unique(dailyData$error)),
                               selected = "None Selected"))
      )
    ),
    mainPanel(
      fluidRow(
        column(8, dataTableOutput("data_editor")),
        column(4, uiOutput("infoBox"))
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(data = dailyData)
  user_selected <- reactive(input$user_select)
  site_selected <- reactive(input$site_select)
  error_selected <- reactive(input$error_select)
  
  observe({
    filtered_data <- rv$data |>
      filter(
        (user_selected() == "All" | UserName %in% user_selected()),
        (site_selected() == "All" | siteName %in% site_selected()),
        (error_selected() == "None Selected" | error %in% error_selected())
      )
    
    output$data_editor <- renderDataTable({
      data_edit(filtered_data, options = list(editable = TRUE))
    })
  })
  
  observeEvent(input$data_editor_rows_selected, {
    selected_row <- input$data_editor_rows_selected
    if (length(selected_row) > 0) {
      selected_demo_id <- rv$data$demo.id[selected_row]
      error_info <- errorData |> filter(demo.id == selected_demo_id)
      if (nrow(error_info) > 0) {
        output$infoBox <- renderUI({
          wellPanel(
            h4("Error Details"),
            verbatimTextOutput("errorDetails")
          )
        })
        output$errorDetails <- renderText({
          paste(capture.output(print(error_info)), collapse = "\n")
        })
      } else {
        output$infoBox <- renderUI({
          wellPanel(
            h4("No Error Details")
          )
        })
      }
    } else {
      output$infoBox <- renderUI({
        NULL
      })
    }
  })
}

shinyApp(ui = ui, server = server)