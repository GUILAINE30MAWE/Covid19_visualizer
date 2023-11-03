#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("ggplot2")


library(shiny)
library(ggplot2)


dataset <- read.csv("covid19.csv")
dataset$Date <- as.Date(dataset$Date)
dataset$AirportName <- as.factor(dataset$AirportName)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("COVID-19 Airport Vizualizer"),
      
      selectInput(inputId = "airport", "Aéroport(s)",
                  choices = levels(dataset$AirportName),
                  selected = "Kingsford Smith"),
      
      dateRangeInput(inputId = "date", "Intervalle de temps",
                     start = min(dataset$Date),
                     end   = max(dataset$Date)),
      
      downloadButton(outputId = "download_data", label = "Télécharger"),
    ),
    
    mainPanel(
      plotOutput(outputId = "plot"), br(),
      # em("Postive and negative percentages indicate an increase and decrease from the baseline period (median value between January 3 and February 6, 2020) respectively."),
      br(), br(), br(),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    subset(dataset,
           AirportName %in% input$airport &
           Date >= input$date[1] & Date <= input$date[2])})
  
  output$plot <- renderPlot({
     ggplot(filtered_data(), aes_string(x="Date", y="PercentOfBaseline")) +
            geom_point(alpha=0.5) + theme(legend.position = "none") +
            ylab("% PercentOfBaseline")
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)