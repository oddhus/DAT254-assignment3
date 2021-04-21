## app.R ##
library(shiny)
library(shinydashboard)
source("hapi.R")
source("heartRate.R")

client <- fhirClient$new("http://hapi.fhir.org/baseR4")


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      fluidRow(
        column(width = 12,
          box(plotOutput("plot1", height = 250)),
          box(
            title = "Controls",
            selectInput("patientId", "Choose Patient", getPatientList(client)),
          ),
        ),
      ),
      box(plotOutput("plot2", height = 250)),
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    patientData <- getPatientRespiatoryRate(input$patientId, client)
    if (is.null(patientData)){
      ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste('No data for patientId', input$patientId, sep = " "))) +
        xlab(NULL)
    }else{
      ggplot(data = patientData, aes(x = as.Date(DATE), y = RESP)) + 
        geom_point() + 
        geom_line() +
        labs(
          x = "Date", 
          y = "Breaths per minute", 
          title = paste("Respiratory rate (for patient ", input$patientId, ")", sep =""))
    }
  })

   output$plot2 <- renderPlot({
    patientData <- getHeartRate(input$patientId, client)
    if (is.null(patientData)){
      ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste('No data for patientId', input$patientId, sep = " "))) +
        xlab(NULL)
    }else{
      ggplot(data = patientData, aes(x = as.Date(DATE), y = RESP)) + 
        geom_point() + 
        geom_line() +
        labs(
          x = "Date", 
          y = "Beats per minute", 
          title = paste("Heart rate (for patient ", input$patientId, ")", sep =""))
    }
  })
}

shinyApp(ui, server)
