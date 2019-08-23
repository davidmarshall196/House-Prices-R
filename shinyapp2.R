
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(rgdal)
library(tidyr)
library(stringr)


# import shapes
shape_data <- readOGR("Local_Authority_Districts_December_2017_Super_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp",
                      layer = "Local_Authority_Districts_December_2017_Super_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84",
                      GDAL1_integer64_policy = TRUE)

# Import Data
library(readr)
houses <- read_csv("average_prices.csv")
houses$Date <- as.character(houses$Date)
houses$year <- substr(houses$Date, 1, 4)
houses <- houses %>%
  filter(year >= "1995" & str_detect(Area_Code, "E06|E07|E08|E09|N09|S12|W06")) %>%
  group_by(Region_Name, Area_Code, year) %>%
  summarise(Average_Price = mean(Average_Price))
houses$year <- as.integer(houses$year)
houses$Average_Price <- round(ifelse(houses$Average_Price > 250000, 250000, 
                                     houses$Average_Price),2)

# Join data
shape_data <- subset(shape_data, is.element(shape_data$lad17cd, houses$Area_Code))
houses <- houses[order(match(houses$Area_Code, shape_data$lad17cd)),]

# Set palette
pal <- colorBin("RdYlBu", domain = c(0,250000), bins = 15, reverse = TRUE)

# define ui
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "House Prices Dashboard"),
  dashboardSidebar(
    sliderInput("date_range", label = "Date Range",
                min = min(houses$year),
                max = max(houses$year),
                value = c(min(houses$year)),
                sep = "",
                step = 1)
  ),
  dashboardBody(fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
                fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
  )
)

server <- function(input, output) {
  data_input <- reactive({
    houses %>%
      filter(year == input$date_range[1])
  })
  
  labels <- reactive({
    paste("<p>", data_input()$Region_Name, "</p>",
          "<p>", "Price: £", round(data_input()$Average_Price, digits = 2), 
          sep = "")
  })
  
  output$mymap <- renderLeaflet(
    leaflet(shape_data) %>%
      setView(lng = -1.709469, lat = 54.155908, zoom = 5) %>%
      addTiles() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.6, fillOpacity = 0.6,
                  fillColor = pal(data_input()$Average_Price),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = lapply(labels(), HTML),
                  labelOptions = c(interactive = TRUE, textsize = "14px")) %>%
      addLegend(pal = pal, values = data_input()$Average_Price,
                title = "House Prices Over Time (£)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1) %>%
      addMiniMap(position = "bottomleft")
  )
  
  output$summary_table <- renderDataTable(data_input())
  
}

shinyApp(ui, server)





























