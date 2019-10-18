library(shiny)
library(ggplot2)
library(DT)
require(rgdal)
require(leaflet)
require(leaflet.extras)
require(dplyr)
require(readxl)
require(stringr)
require(rgeos)
require(sp)
require(shiny)
require(shinydashboard)
require(reshape2)
require(plotly)
require(shinythemes)

#install.packages("ggalt")
#library(ggalt)


#getwd()

#Set working directory 
#setwd("C:/Users/shrey/Dropbox/rShiny/P2/shreyapr_Project2")

#Load Data into R
listing <- read.csv(file = 'listings.csv')
#head(comp)
View(listing)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select variable for y-axis
      selectInput(inputId = "Neighborhood", 
                  label = "Select your preferred Neighborhood in Seattle",
                  choices = sort(unique(listing$neighbourhood_group_cleansed)), 
                  selected = "University District"),
      
      # Select variable for x-axis
      selectInput(inputId = "Response", 
                  label = "How urgently do you need a response on your booking:",
                  choices = sort(unique(listing$host_response_time)), 
                  selected = "within a few hours"),
      
      #Select alpha level for plots
      sliderInput(inputId= "alpha", 
                  label = "Select alpha level:", 
                  min = 0, 
                  max = 1, 
                  value = 0.4), 
      
      # Show data table 
      checkboxInput(inputId = "show_data",
                    label = "Show data table :",
                    value = TRUE), 
      
      
      # Button
      downloadButton("downloadData", "Download")
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "plot1"),
      
      plotOutput(outputId = "plot2"),
      
      plotOutput("dot"),
      
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "listing")
      
    )
  )
)


# Define server function required to create the scatterplot
server <- function(input, output) {
  
 # output$plot1 <- renderPlot({
    #ggplot(data = listing, aes_string(x = input$x, y = input$y, col = input$x)) +
     # geom_point(alpha = input$alpha) + coord_flip()
    
  #  c <- listing %>% group_by(property_type,
   # bp<- ggplot(data =filter(listing, neighbourhood_group_cleansed == 'input$Neighborhood' & host_response_time == 'input$Response'), aes(x="", y= c) %>% tally(), fill=sort(unique(listing$property_type))))
  #  pie <- bp + coord_polar("y", start=0)
   # bp
  
    #})
   
  
  output$plot2 <- renderPlot({
    
    
    ggplot(data = listing, horiz = True, aes_string(x =, y =input$Response)) + geom_bar(stat = "identity", fill = "#FF6666")+coord_flip()
    
    
  })
  
  
  
  output$dot <- renderPlot({
    
    # Simple Dotplot
    
    ggplot(crime) +
      geom_point(aes_string(x = input$y, y = input$x), alpha = input$alpha, color='darkred')
    
  })
  
  
  
  # Print data table if checked -------------------------------------
  output$listing <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = listing[, 47:53], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
      
    }
  )
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(listing, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(listing, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
