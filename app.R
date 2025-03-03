library(shiny)
library(bslib)  
library(leaflet)  
library(DT)  
library(ggplot2)  
library(rWind)  
library(gdistance) 
library(raster)  
library(fields)  
library(fields)
library(shape)
library(rworldmap)
library(shinycssloaders)

ui <- navbarPage(
  title = "Wind Farm Generator",
  theme = bs_theme(bootswatch = "cosmo"),
  tabPanel("Home",
           fluidPage(
             fluidRow(
               column(6,
                      h3("About"),
                      h5("By: Gerald Clark, Nicholas Leong, Ryan Stanley"),
                      p("As nations worldwide accelerate their transition to renewable energy to meet carbon reduction goals, 
                        the wind energy sector is experiencing unprecedented growth. In 2023 alone, 7.8% of the world's electricity was generated from wind turbines.
                        While shifts in administration may influence this progress, this powerful tool empowers users to identify optimal wind farm locations using 
                        real-time wind data from the National Oceanic and Atmospheric Administration (NOAA) Physical Sciences Laboratory. By leveraging cutting-edge technology,
                        users can evaluate the geographic and economic feasibility of future wind projects through an interactive map, wind farm design tools, 
                        expected megawatt output calculator, total cost, and increased data access, paving the way for a cleaner, more sustainable energy future.")),
               column(6,
                      img(src = "Wind_farm_highres.jpeg", height = "300px", width = "500px")) 
             )
           )
  ),
  tabPanel("Wind Data",
           sidebarLayout(
             sidebarPanel(
               h4("Wind Data Inputs"),
               numericInput("year", "Year:", 2024, min = 2000, max = 2030),
               numericInput("month", "Month:", 1, min = 1, max = 12),
               numericInput("day", "Day:", 1, min = 1, max = 31),
               numericInput("hour", "Hour:", 0, min = 0, max = 23),
               numericInput("lon_min", "Longitude Min:", -125),
               numericInput("lon_max", "Longitude Max:", -70),
               numericInput("lat_min", "Latitude Min:", 25),
               numericInput("lat_max", "Latitude Max:", 50),
               actionButton("load_data", "Load Wind Data")
             ),
             mainPanel(
               h4("Wind Data Table"),
               DTOutput("windTable") %>% withSpinner(color = "#0dc5c1"),
               plotOutput("windPlot") %>% withSpinner(color = "#0dc5c1")
             )
           )
  ),
  
  tabPanel("Wind Farm Design",
           sidebarLayout(
             sidebarPanel(
               h4("Wind Farm Customization"),
               sliderInput("num_turbines", "Number of Turbines:", min = 1, max = 100, value = 10),
               sliderInput("tower_height", "Tower Height (m):", min = 50, max = 150, value = 100),
               sliderInput("blade_length", "Blade Length (m):", min = 10, max = 100, value = 50),
               selectInput("material", "Blade Material:", choices = c("Fiberglass", "Carbon Fiber")),
               actionButton("calculate", "Calculate MW Output")
             ),
             mainPanel(
               h4("Wind Farm Performance"),
               tableOutput("wind_farm_table")
               #verbatimTextOutput("mw_output"),
               #verbatimTextOutput("cost_estimate")
             )
           )
  ),
  
  tabPanel("Interactive Map",
           fluidPage(
             h4("Wind Profile Map"),
             leafletOutput("windMap")
           )
  ),
  
  tabPanel("Export Report",
           fluidPage(
             h4("Generate Wind Farm Report"),
             actionButton("export_report", "Download Report"),
             p("Summary of the wind farm's expected MW output, costs, and selected location."),
             downloadButton("download", "Download Report")
           )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load wind data when button is clicked
  wind_data <- reactive({
    req(input$load_data)  # Ensure button is clicked
    isolate({
      w <- wind.dl(input$year, input$month, input$day, input$hour, 
                   input$lon_min, input$lon_max, input$lat_min, input$lat_max)
      return(w)
    })
  })
  
  # Display wind data table
  output$windTable <- DT::renderDataTable({
    w <- wind_data()
    req(w)
    
    datatable(w, options = list(
      scrollY = "400px",  # Enables vertical scrolling (adjust height as needed)
      scrollX = TRUE,     # Enables horizontal scrolling to prevent overflow
      autoWidth = TRUE,   # Adjusts column widths automatically
      pageLength = 10     # Controls how many rows are shown per page
    ))
  })
  # Plot wind speed
  output$windPlot <- renderPlot({
    w <- wind_data()
    req(w)
    
    wind_layer <- wind2raster(w)
    image.plot(wind_layer[["speed"]], main = "Wind Speed",
               col = terrain.colors(10), xlab = "Longitude", ylab = "Latitude", zlim = c(0, 7))
    lines(getMap(resolution = "low"), lwd=4)
  })
  
  # Leaflet Map for wind profile
  output$windMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95, lat = 40, zoom = 4)
  })
  
  # Calculate MW output based on user input
  output$wind_farm_table <- renderTable({
    num_turbines <- input$num_turbines
    tower_height <- input$tower_height
    blade_length <- input$blade_length
    
    #REPLACE WITH A REAL FORMULA
    power_output <- num_turbines * blade_length * tower_height  
    
    #REPLACE WITH A REAL FORMULA
    cost_estimate <- num_turbines * 2.5 
    
    # Create the output table
    data.frame(
      "Metric" = c("Power Output (MW)", "Estimated Cost (Million $)"),
      "Value" = c(round(power_output, 2), round(cost_estimate, 2))
    )
  })
  
  
  # Cost estimate
  output$cost_estimate <- renderText({
    num_turbines <- input$num_turbines
    material <- input$material
    
    cost_per_turbine <- ifelse(material == "Fiberglass", 1, 
                               ifelse(material == "Carbon Fiber", 2.0)) * 1e6  
    
    total_cost <- num_turbines * cost_per_turbine
    paste("Estimated Total Cost: $", format(round(total_cost, 2), big.mark = ","))
  })
  
  # Export report button NOT FUNCTIONAL
  output$download <- downloadHandler(
    filename = function() { "wind_farm_report.pdf" },
    content = function(file) {
      writeLines("Wind Farm Report", file)
    }
  )
}

# Run the app
shinyApp(ui, server)
