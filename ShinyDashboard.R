###packages####
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(sf)
library(shiny)
library(shinydashboard)

theme_set(theme_bw())


#####################
####### prep #######
#####################

files <- list.files(pattern = "\\.csv$")

df <- files %>%
  lapply(read.csv) %>%
  bind_rows()

#turn off scientific notation for large values
options(scipen = 999)

# district groups 
dst <- df %>%
  group_by(year, dst) %>%
  summarise(count = n())
dst <- as.data.frame(dst)

# violent crime groups
vc_dst <- df %>%
  group_by(year, dst) %>%
  summarise(count = sum(violent_crime))
vc_dst <- as.data.frame(vc_dst)

# shootings groups
sh_dst <- df %>%
  group_by(year, dst) %>%
  summarise(count = sum(shooting))
sh_dst <- as.data.frame(sh_dst)
sh_dst <- sh_dst %>%
  filter(sh_dst$year >= 2019)

# Convert the year column to numeric or integer
dst$year <- as.numeric(as.character(dst$year))
vc_dst$year <- as.numeric(as.character(vc_dst$year))
sh_dst$year <- as.numeric(as.character(sh_dst$year))

# heatmap tables
heatmap_data <- df %>%
  mutate(day_of_week = wday(date, label = TRUE), 
         hour = hour) %>% 
  count(year, day_of_week, hour)
str(df)

shheatmap_data <- df %>%
  filter(!is.na(shooting) & shooting == 1) %>%  # Filter out NA and where shooting equals 1
  mutate(day_of_week = wday(date, label = TRUE), 
         hour = hour) %>% 
  count(year, day_of_week, hour)

vcheatmap_data <- df %>%
  filter(!is.na(violent_crime) & violent_crime == 1) %>%  # Filter out NA and where shooting equals 1
  mutate(day_of_week = wday(date, label = TRUE), 
         hour = hour) %>% 
  count(year, day_of_week, hour)


max_count_dst <- max(dst$count, na.rm = TRUE)
max_count_vc <- max(vc_dst$count, na.rm = TRUE)
max_count_sh <- max(sh_dst$count, na.rm = TRUE)


#####################
##### dashboard #####
#####################

### UI
ui <- fluidPage(
  titlePanel("Boston Crime Analysis"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        sliderInput("year",
                    "Choose Year:",
                    min = min(dst$year), 
                    max = max(dst$year), 
                    value = 2019, 
                    step = 1,
                    animate = TRUE,
                    sep = "")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Total Incidents", plotOutput("barPlot")),
          tabPanel("Violent Crime", plotOutput("vcPlot")),
          tabPanel("Shootings", plotOutput("shPlot"))
        )
      )
    )
  ),
  fluidRow(  
    tabsetPanel(
      tabPanel("Total Incidents Heatmap", plotOutput("heatPlot")),
      tabPanel("Violent Crime Heatmap", plotOutput("vcheatPlot")),
      tabPanel("Shooting Heatmap", plotOutput("shheatPlot"))
    )
  )
)

### server

server <- function(input, output) {
  output$barPlot <- renderPlot({
    filtered_data <- dst[dst$year == input$year, ]
    ggplot(filtered_data, aes(x = dst, y = count, fill = count)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Incident Counts by District in", input$year),
           x = "",
           y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_continuous(low = "lightblue", high = "darkblue") +
      ylim(0, max_count_dst)
  })
  
  output$vcPlot <- renderPlot({
    filter_data2 <- vc_dst[vc_dst$year == input$year, ]
    ggplot(filter_data2, aes(x = dst, y = count, fill = count)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Violent Crime Counts by District in", input$year),
           x = "",
           y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_continuous(low = "lightblue", high = "darkblue") +
      ylim(0, max_count_vc)
  })
  
  output$shPlot <- renderPlot({
    filter_data3 <- sh_dst[sh_dst$year == input$year, ]
    ggplot(filter_data3, aes(x = dst, y = count, fill = count)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Shootings by District in", input$year),
           subtitle = "Data Unavailable Before 2019",
           x = "",
           y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_continuous(low = "lightblue", high = "darkblue") +
      ylim(0, max_count_sh)
  })
  ## all incidents heatmap
  output$heatPlot <- renderPlot({
    filtered_heatmap_data <- heatmap_data[heatmap_data$year == input$year, ]  # Filter data based on year
    ggplot(filtered_heatmap_data, aes(x = hour, y = day_of_week, fill = n)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      labs(title = paste("All Offenses Count by Day of the Week and Hour in", input$year), 
           x = "", 
           y = "",
           fill = "")
  })
  ## violent crime heatmap
  output$vcheatPlot <- renderPlot({
    filtered_heatmap_data2 <- vcheatmap_data[vcheatmap_data$year == input$year, ]  # Filter data based on year
    ggplot(filtered_heatmap_data2, aes(x = hour, y = day_of_week, fill = n)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      labs(title = paste("Violent Crime Count by Day of the Week and Hour in", input$year), 
           x = "", 
           y = "",
           fill = "")
  })
  ## shooting heatmap 
  output$shheatPlot <- renderPlot({
    filtered_heatmap_data1 <- shheatmap_data[shheatmap_data$year == input$year, ]  # Filter data based on year
    ggplot(filtered_heatmap_data1, aes(x = hour, y = day_of_week, fill = n)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      labs(title = paste("Shooting Count by Day of the Week and Hour in", input$year), 
           x = "", 
           y = "",
           fill = "")
  })
}

# CALL THE SHINY APP #######################################

shinyApp(ui = ui, server = server)
