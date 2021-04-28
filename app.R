## app.R ##
library(shiny)
library(shinydashboard)
source("hapi.R")
source("heartRate.R")
source("bodyTemperature.R")

client <- fhirClient$new("http://hapi.fhir.org/baseR4")


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Browse Patients",
      tabName = "browse",
      icon = icon("dashboard")
    ),
    menuItem("All Patients", tabName = "all", icon = icon("th"))
  )),
  dashboardBody(tabItems(
    ##First tab
    tabItem(tabName = "browse",
            # Boxes need to be put in a row (or column)
            fluidRow(column(
              width = 12,
              box(plotOutput("plot1", height = 250)),
              box(
                title = "Controls",
                selectInput("patientId", "Choose Patient", getPatientList(client)),
              ),
            )),
            fluidRow(column(
              width = 12,
              box(plotOutput("plot2", height = 250)),
              box(plotOutput("plot3", height = 250)),
            ))),
    
    ## Second tab
    tabItem(tabName = "all",
            fluidRow(column(
              width = 24,
              box(plotOutput("plot4", height = 500)),
            )))
  ))
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
   
   output$plot3 <- renderPlot({
     patientData <- getPatientBodyTemperature(input$patientId, client)
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
           y = "Body Temperature", 
           title = paste("Body Temperature (for patient ", input$patientId, ")", sep =""))
     }
   })
   
   output$plot4 <- renderPlot({
     patientData <- getPatientRespiatoryRateAll(client)
     if (is.null(patientData)){
       ggplot() +
         theme_void() +
         geom_text(aes(0,0,label=paste('No data for patientId', input$patientId, sep = " "))) +
         xlab(NULL)
     }else{
       ggplot(data = allPatients, aes(x = as.Date(DATE), y = RESP, color=PATIENTID)) + 
         geom_point() + 
         geom_line() +
         scale_x_date(
           #limits = c(Sys.Date() - 200, NA),
           date_labels = "%d-%m-%Y")+
         labs(x = "Date", y = "Breaths per minute", title = "Respiratory rate")
     }
   })

}

shinyApp(ui, server)

