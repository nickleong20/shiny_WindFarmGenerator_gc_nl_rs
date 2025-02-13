# Load required libraries
library(bs4Dash)
library(leaflet)
library(DT)
library(plotly)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidygeocoder)
library(sf)  
library(mapview)
library(mapdata)
library(data.table)
library(shiny)
library(ggdist)
library(ggthemes)
library(ggplot2)
library(rlang)
library(PupillometryR)
library(gridExtra)
library(networkD3)
library(tidyr)
library(One4All)
library(janitor)
library(jsonlite)
library(here)
library(httr)

devtools::install_github("jabiologo/rWind")
library(rWind)
library(raster)
library(gdistance)

w <- wind.dl(2024, 1, 1, 0, -125, -70, 25, 50)



wind_layer <- wind2raster(w)

Conductance<-flow.dispersion(wind_layer)

AtoB<- shortestPath(Conductance, 
                    c(-5.5, 37), c(-5.5, 35), output="SpatialLines")
BtoA<- shortestPath(Conductance, 
                    c(-5.5, 35), c(-5.5, 37), output="SpatialLines")

library(fields)
library(shape)
library(rworldmap)

image.plot(wind_layer[["speed"]], main="Wind speed", 
           col=terrain.colors(10), xlab="Longitude", ylab="Lattitude", zlim=c(0,7))

lines(getMap(resolution = "low"), lwd=4)

points(-5.5, 37, pch=19, cex=3.4, col="red")
points(-5.5, 35, pch=19, cex=3.4, col="blue")

lines(AtoB, col="red", lwd=4, lty=2)
lines(BtoA, col="blue", lwd=4, lty=2)

alpha <- arrowDir(w)
Arrowhead(w$lon, w$lat, angle=alpha, arr.length = 0.4, arr.type="curved")

text(-5.75, 37.25,labels="Spain", cex= 2.5, col="red", font=2)
text(-5.25, 34.75,labels="Morocco", cex= 2.5, col="blue", font=2)
legend("toprigh", legend = c("From Spain to Morocco", "From Morocco to Spain"),
       lwd=4 ,lty = 1, col=c("red","blue"), cex=0.9, bg="white")
library(terra)

r <- rast(wind_layer)







us_boundaries <- st_read(here("data/s_05mr24.shp"))


# Convert sewage spills to spatial data frame
sewage_spills_sf <- st_as_sf(sewage_spills, coords = c("longitude", "latitude"), crs = st_crs(watersheds))

# Reproject datasets to WGS84
sewage_spills_sf <- st_transform(sewage_spills_sf, crs = 4326)
watersheds <- st_transform(watersheds, crs = 4326)

# Perform spatial join to attribute sewage spills to watersheds
sewage_spills_watershed <- st_join(sewage_spills_sf, watersheds)

# Handle NA values by finding the nearest watershed
na_spills <- sewage_spills_watershed[is.na(sewage_spills_watershed$wuname), ]
non_na_spills <- sewage_spills_watershed[!is.na(sewage_spills_watershed$wuname), ]
nearest_watershed <- sf::st_nearest_feature(na_spills, non_na_spills)

# Replace NA values with the nearest watershed name
sewage_spills_watershed$wuname[is.na(sewage_spills_watershed$wuname)] <- non_na_spills$wuname[nearest_watershed]
# Convert sewage_spills_watershed to a non-spatial data frame
sewage_spills_watershed_df <- as.data.frame(sewage_spills_watershed)
sewage_spills_watershed_df <- sewage_spills_watershed_df %>%
  select(-objectid, -wuc, -huc, -swma, -area_sqmi, -area_m2, -name_hwn, -st_areasha, -st_perimet, -objectid.1, -geoid20, -name20, -namelsad20, -aland20, -awater20, -pop20, -st_areasha.1, -st_perimet.1)

sewage_spills_watershed_df <- sewage_spills_watershed_df %>%
  left_join(watersheds, by = c("wuname", "island" = "name20"))


# Convert back to a spatial data frame
sewage_spills_watershed <- st_as_sf(sewage_spills_watershed_df)

latitude <- sewage_spills$latitude
longitude <- sewage_spills$longitude

sewage_spills_watershed$latitude <- latitude
sewage_spills_watershed$longitude <- longitude

# Import dataset
#sewage_spills_watershed <- st_read("sewage_spills_watershed.shp")

################################################################################


ui <- bs4DashPage(
  bs4DashNavbar(
    title = "Wind Farm Generator",
    tags$style(
      HTML(".navbar { background-color: #87CEEB; }")
    )
  ),
  bs4DashSidebar(
    tags$style(
      HTML(".sidebar { background-color: #87CEEB; }")
    ),
    sidebarMenu(
      menuItem("Interactive Map", tabName = "mapTab", icon = icon("map"))
    )
  ),
  bs4DashBody(
    fluidRow(
      box(
        title = "Wind Farm Map",
        h3(
          tags$div(
            "Insert caption for this here",
            style = "font-size: 14px;"
          )
        ),
        width = 12
      )
    ),
    fluidRow(
      column(
        width = 12,
        plotOutput("windMap")
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Wind Data Table",
          style = "overflow-x: auto;",
          DT::dataTableOutput("windTable"),
          width = 12
        )
      )
    )
  )
)



server <- function(input, output, session) {
  output$windMap <- renderPlot({image.plot(wind_layer[["speed"]], main="Wind speed", 
                                           col=terrain.colors(10), xlab="Longitude", ylab="Lattitude", zlim=c(0,7))
    
    lines(getMap(resolution = "low"), lwd=4)
  })
  
  output$windTable <- DT::renderDataTable({
    w
  })
}
shinyApp(ui, server)
