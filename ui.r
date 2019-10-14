
#Design and Define the UI

# Application header & title ----------------------------------------------



ui <- fluidPage(
  
  header <- dashboardHeader(title = "US Public Libraries")
  
  
  leafletOutput("mymap",height = 1000)
)