#Load Libraries
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
getwd()

#Set working directory 
#setwd("C:/Users/shrey/Desktop/CMU/Sem3/rShiny/hw1-shreyapr")

#Load Data into R

states <- readOGR("./Datafiles/cb_2018_us_state_20m.shp")
crime <- read.csv("./Datafiles/data.csv")

#preprocessing data
StateCrime <- states[states$NAME %in% crime$State,]
StateCrime@data <- merge(StateCrime@data, crime, sort = FALSE, by.x = "NAME", by.y = "State")



# Define UI for application 
ui <- fluidPage(
  
  # Sidebar layout 
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("State", "homicideRate2017", "firearmDeathRate", "firearmDeaths", "Pop"),
                  selected = "Pop"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis",
                  choices = c("State", "homicideRate2017", "firearmDeathRate", "firearmDeaths", "Pop"),
                  selected = "State"),
      
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
      
      #Select input variable for the map
      selectInput("measure", "Measure on Map", c("firearmDeaths", "homicideRate2017"), 
                  selected="firearmDeaths"),
       
      # Download Button
      downloadButton("downloadData", "Download")
    ),
    
    # Output: Show scatterplot
    mainPanel(
      
      
      plotOutput(outputId = "bar"),
      
      plotOutput("dot"),
      
      leafletOutput("mymap", width = "100%", height = 500),

      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "crime")
      
    )
  )
)


# Define server function required to create the scatterplot
server <- function(input, output) {

  #plot bar graph
  output$bar <- renderPlot({
    ggplot(data = crime, horiz = True, aes_string(x =input$x, y =input$y)) + geom_bar(stat = "identity", fill = "#FF6666")+coord_flip()
  })
  
  
  #plot dot plot
  output$dot <- renderPlot({
    ggplot(crime) + geom_point(aes_string(x = input$y, y = input$x), alpha = input$alpha, color='darkred')
  })
  
  #Plot the map
  output$mymap <- renderLeaflet({

    if(input$measure=="firearmDeaths"){d <- StateCrime$firearmDeaths}
    if(input$measure=="homicideRate2017"){d <- StateCrime$homicideRate2017}
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = d # as.numeric(na.omit(x))
    )
    
    legend.title <- paste(paste0(input$measure, " ("), round(min(d, na.rm=T), 2), " - ", round(max(d, na.rm=T), 2), ")", sep="")
    
    leaflet(data = StateCrime) %>%
      
      addProviderTiles("Stamen.Toner", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~pal(d)) %>%
      addLegend("bottomright", pal = pal, values = ~d, title = legend.title, labFormat = labelFormat(suffix = ""), opacity = 0.3)
})
  
  # Print data table if checked -------------------------------------
  output$crime <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = crime[, 1:5], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
      
    }
  )
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(crime, Sys.Date(), ".csv", sep = '')
    },
    content = function(file) {
      write.csv(crime, file, row.names = FALSE)
    },
    contentType = "csv"
  )
}




# Run the application 
shinyApp(ui = ui, server = server)
