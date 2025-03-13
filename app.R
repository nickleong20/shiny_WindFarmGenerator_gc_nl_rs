library(shiny)
library(bslib)  
library(leaflet)  
library(DT)  
library(ggplot2)  
library(rWind)  
library(gdistance) 
library(raster)  
library(fields)  
library(rworldmap)
library(shinycssloaders)
library(rmarkdown)
library(tidyverse)

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
                        the wind energy sector is experiencing unprecedented growth. In 2023 alone, 7.8% of the world's electricity 
                        was generated from wind turbines. While shifts in administration may influence this progress, this powerful tool 
                        empowers users to identify optimal wind farm locations using real-time wind data from the 
                        National Oceanic and Atmospheric Administration (NOAA) Physical Sciences Laboratory. By leveraging cutting-edge technology,
                        users can evaluate the geographic and economic feasibility of future wind projects through an interactive map, wind farm design tools, 
                        expected megawatt output calculator, total cost, and increased data access, paving the way for a cleaner, more sustainable energy future.")),
               column(6, img(src = "Wind_farm_highres.jpeg", height = "300px", width = "500px")) 
             )
           )
  ),
  
  tabPanel("Wind Data",
           sidebarLayout(
             sidebarPanel(
               h4("Select Wind Data Location"),
               leafletOutput("selectMap", height = "300px"),  
               h5("Selected Coordinates:"),
               verbatimTextOutput("selectedCoords"),
               h5("Average Wind Speed (m/s):"),
               verbatimTextOutput("avgWindSpeed"),
               actionButton("load_data", "Load Climatology Wind Data")
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
             )
           )
  ),
  
  tabPanel("Export Report",
           fluidPage(
             h4("Generate Wind Farm Report"),
             actionButton("export_report", "Download Report"),
             downloadButton("download", "Download Report")
           )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store selected coordinates
  selected_location <- reactiveValues(lon = -95, lat = 40)
  wind_farm_results <- reactiveValues(total_power = NULL, total_cost = NULL)
  
  # Render interactive map with initial marker
  output$selectMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = selected_location$lon, lat = selected_location$lat, zoom = 4) %>%
      addMarkers(lng = selected_location$lon, lat = selected_location$lat, popup = "Selected Location")
  })
  
  # Capture user click on the map and update marker
  observeEvent(input$selectMap_click, {
    selected_location$lon <- input$selectMap_click$lng
    selected_location$lat <- input$selectMap_click$lat
    
    leafletProxy("selectMap") %>%
      clearMarkers() %>%
      addMarkers(lng = selected_location$lon, lat = selected_location$lat, popup = "Selected Location")
  })
  
  # Display selected coordinates
  output$selectedCoords <- renderText({
    paste("Longitude:", round(selected_location$lon, 2), 
          "Latitude:", round(selected_location$lat, 2))
  })
  
  output$avgWindSpeed <- renderText({
    w <- wind_data()
    req(w)
    
    avg_speed <- mean(w$avg_speed, na.rm = TRUE)
    paste(round(avg_speed, 2), "m/s")  # Display rounded wind speed
  })
  
  
  # Load wind data
  wind_data <- reactive({
    req(input$load_data, selected_location$lon, selected_location$lat)
    
    isolate({
      # Define range of years for the climatology (Modify as needed)
      years <- 2000:2020  
      
      # Fixed month, day, and hour for data retrieval (Can be adjusted)
      month <- 6  
      day <- 15  
      hour <- 12  
      
      # Download wind data for each year
      winds_list <- lapply(years, function(yr) {
        tryCatch(
          wind.dl(yr, month, day, hour, 
                  selected_location$lon - 1, selected_location$lon + 1, 
                  selected_location$lat - 1, selected_location$lat + 1),
          error = function(e) { NULL }
        )
      })
      
      # Remove any years where data failed to download
      winds_list <- Filter(Negate(is.null), winds_list)
      
      if(length(winds_list) == 0){
        return(NULL)
      }
      
      # Combine data frames (assuming each dataset has a column named "speed")
      combined <- do.call(rbind, winds_list)
      
      # Compute the average wind speed for the entire dataset
      combined$avg_speed <- mean(combined$speed, na.rm = TRUE)
      
      return(combined)  
    })
  })
  
  
  output$windTable <- DT::renderDataTable({
    w <- wind_data() %>%
      rename(Time = time,
             Latitude = lat, 
             Longitude = lon, 
             `Wind Speed (m/s)` = speed,
             `Wind Direction (º)` = dir,
             `U Component of Wind (m/s)` = ugrd10m,
             `V Component of Wind (m/s)` = vgrd10m,
             `Average Speed (m/s)` = avg_speed) %>%
      mutate(across(c(`Wind Speed (m/s)`, `Wind Direction (º)`, 
                      `U Component of Wind (m/s)`, `V Component of Wind (m/s)`, `Average Speed (m/s)`), 
                    \(x) round(x, 2)))
    req(w)
    
    datatable(w, options = list(scrollY = "400px", scrollX = TRUE, autoWidth = TRUE, pageLength = 10))
  })
  
  output$windPlot <- renderPlot({
    w <- wind_data()
    req(w)
    
    # Convert averaged wind data to a raster format
    wind_layer <- wind2raster(w)
    image.plot(wind_layer[["speed"]], main = "Climatological Average Wind Speed", 
               col = terrain.colors(10), xlab = "Longitude", ylab = "Latitude", zlim = c(0, max(w$speed, na.rm=TRUE)))
    lines(getMap(resolution = "low"), lwd = 4)
  })
  
  # Wind farm calculations
  output$wind_farm_table <- renderTable({
    req(input$num_turbines, input$tower_height, input$blade_length, input$material)
    
    num_turbines <- input$num_turbines
    tower_height <- input$tower_height
    blade_length <- input$blade_length
    material <- input$material
    wind_shear_exponent <- 0.143
    H0 <- 80
    C0_tower <- 400000 
    
    w <- wind_data()
    req(w)
    
    avg_wind_speed_80m <- mean(w$avg_speed, na.rm = TRUE)
    avg_wind_speed_H <- avg_wind_speed_80m * (tower_height / H0) ^ wind_shear_exponent
    
    if (material == "Fiberglass") {
      blade_weight <- 8000
      tower_weight <- 75000
      material_cost_per_kg <- 2
      efficiency_multiplier <- 1.0
    } else {
      blade_weight <- 6000
      tower_weight <- 58000
      material_cost_per_kg <- 30
      efficiency_multiplier <- 1.07
    }
    
    tower_weight_scaled <- tower_weight * (tower_height / H0)
    num_blades <- 3
    blade_cost_per_turbine <- num_blades * blade_weight * material_cost_per_kg  
    tower_cost_per_turbine <- C0_tower * (tower_height / H0)^2  
    
    total_cost <- num_turbines * (blade_cost_per_turbine + tower_cost_per_turbine)
    total_cost_millions <- round(total_cost / 1e6, 2)  
    
    Cp <- 0.35  
    rho <- 1.225  
    A <- pi * (2 * blade_length)^2 / 4  
    power_per_turbine_H <- Cp * 0.5 * rho * A * (avg_wind_speed_H^3) / 1e6
    total_power <- round(num_turbines * power_per_turbine_H * efficiency_multiplier, 2)
    
    # Store values in reactive variables (moved here to ensure consistency)
    wind_farm_results$total_power <- total_power
    wind_farm_results$total_cost <- paste("$", format(total_cost_millions, big.mark = ","))
    
    # Return table
    data.frame(
      "Metric" = c("Power Output (MW)", "Estimated Cost (Million $)"),
      "Value" = c(total_power, total_cost_millions)
    )
  })
  
  observeEvent(input$calculate, {
    req(wind_farm_results$total_power, wind_farm_results$total_cost)
    
    showModal(modalDialog(
      title = "Wind Farm Calculation Results",
      div(
        style = "font-size: 22px; font-weight: bold; color: #007BFF; text-align: center;",
        paste("Estimated Power Output:", wind_farm_results$total_power, "MW")
      ),
      br(),
      div(
        style = "font-size: 20px; font-weight: bold; color: #28a745; text-align: center;",
        paste("Estimated Cost:", wind_farm_results$total_cost)
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  
  # Generate PDF Report
  output$download <- downloadHandler(
    filename = function() { "wind_farm_report.pdf" },
    content = function(file) {
      req(wind_farm_results$total_power, wind_farm_results$total_cost)
      
      params <- list(
        title = "Wind Farm Report",
        image_path = "Wind_farm_highres.jpeg",
        location = paste("Longitude:", round(selected_location$lon, 2), 
                         "Latitude:", round(selected_location$lat, 2)),
        num_turbines = input$num_turbines,
        tower_height = input$tower_height,
        blade_length = input$blade_length,
        material = input$material,
        power_output = wind_farm_results$total_power,
        estimated_cost = wind_farm_results$total_cost
      )
      
      tempReport <- tempfile(fileext = ".Rmd")
      
      report_content <- "
      ---
      title: '`r params$title`'
      output: pdf_document
      ---

      ![Wind Farm]( `r params$image_path` )

      ## Wind Farm Details
      - **Location:** `r params$location`
      - **Number of Turbines:** `r params$num_turbines`
      - **Tower Height:** `r params$tower_height` m
      - **Blade Length:** `r params$blade_length` m
      - **Blade Material:** `r params$material`

      ## Performance & Cost Analysis
      - **Estimated Power Output:** `r params$power_output` MW
      - **Estimated Cost:** `r params$estimated_cost`
      "
      
      writeLines(report_content, tempReport)
      
      rmarkdown::render(tempReport, output_file = file)
    }
  )
}

# Run the app
shinyApp(ui, server)