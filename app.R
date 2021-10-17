library(shiny)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(DT)

load("main.RData")

#header <- dashboardHeader(
#    title = "IFRA Standards"
#)

#sidebar <- dashboardSidebar(
#    selectInput('name', "Enter The Ingredient Name",
#                choices = names(jsons))
#)

#body <- dashboardBody(
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput('name', "Enter The Ingredient Name",
                        choices = names(jsons))
        ),
        mainPanel(
            fluidRow(
                column(width = 6,
                       tableOutput('cas_no'),
                       tableOutput("synonyms")
                ),
                column(width = 6,
                       tableOutput('restrictions')
                )
            )
        )
    )   
)
#)

#ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    rval_data <- reactive({
        rval <- jsons[input$name]
        fromJSON(minify(rval))
    })
    
    output$cas_no <- renderTable({
        data <- rval_data()[["cas_no"]]
        data.frame(data)
        data <- data.frame(data)
        names(data) <- "CAS Number"
        data
    }, bordered = T)
    
    output$synonyms <- renderTable({
        data <- rval_data()[["synonyms"]]
        data <- data.frame(data)
        names(data) <- "Synonyms"
        data
    }, bordered = T)
    
    output$restrictions <- renderTable({
        data <- rval_data()[["restrictions"]]
        data
    }, bordered = T)
}

shinyApp(ui = ui, server = server)
