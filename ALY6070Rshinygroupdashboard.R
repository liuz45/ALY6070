# Load necessary libraries
install.packages("lubridate")
install.packages("leaflet")
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)

# Load the data
data <- read.csv("C:/Users/l1761/Downloads/tmpvt1d9zu3.csv")

# Convert dates and extract necessary components
data$OCCURRED_ON_DATE <- ymd_hms(data$OCCURRED_ON_DATE)
data$Month_Year <- floor_date(data$OCCURRED_ON_DATE, "month")

# Bar Chart of Offense Count by District
bar_plot <- ggplot(data, aes(x = DISTRICT)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Offense Count by District", x = "District", y = "Count of Offenses")

# Heatmap of Offense Count by Day of the Week and Hour
heatmap_data <- data %>%
  mutate(DAY_OF_WEEK = wday(OCCURRED_ON_DATE, label = TRUE),
         HOUR = hour(OCCURRED_ON_DATE)) %>%
  count(DAY_OF_WEEK, HOUR)

heatmap_plot <- ggplot(heatmap_data, aes(x = HOUR, y = DAY_OF_WEEK, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Offense Count by Day of the Week and Hour", x = "Hour of the Day", y = "Day of the Week")

# Map of Incident Locations
map_plot <- leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(~Long, ~Lat, radius = 2, color = "blue", fillOpacity = 0.5)

# Trend Line of Offense Counts Over Time
trend_data <- data %>%
  count(Month_Year)

trend_plot <- ggplot(trend_data, aes(x = Month_Year, y = n)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  theme_minimal() +
  labs(title = "Trend of Offense Counts Over Time", x = "Month-Year", y = "Count of Offenses")



# Define UI
ui <- fluidPage(
  titlePanel("Crime Data Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Explore the data using the plots on the right.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("barChart")),
        tabPanel("Heatmap", plotOutput("heatmap")),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Trend Line", plotOutput("trendLine"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$barChart <- renderPlot({ bar_plot })
  output$heatmap <- renderPlot({ heatmap_plot })
  output$map <- renderLeaflet({ map_plot })
  output$trendLine <- renderPlot({ trend_plot })
  output$pieChart <- renderPlot({ pie_chart })
}

# Run the application 
shinyApp(ui = ui, server = server)
