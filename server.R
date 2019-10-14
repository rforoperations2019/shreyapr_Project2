#Defining the server function

server <- function(input,output, session){
  
  data <- reactive({
    x <- lib
  })  
  
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet(data = lib) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude, 
                 lat = ~Latitude,
                 popup = paste("Name", lib$Location.Name, "<br>",
                               "City:", lib$City))
    m
  }) 
}