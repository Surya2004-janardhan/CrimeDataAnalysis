# Load necessary libraries (install if not installed)
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)

# Load the dataset
crime_data <- read.csv("crime.csv", stringsAsFactors = FALSE)

# Convert Date column to Date format (adjust format if needed)
crime_data$Date <- as.Date(crime_data$Date, format="%d-%m-%Y")

# View dataset structure
str(crime_data)

# Filter for 2023 data
crime_2023 <- crime_data %>% filter(year(Date) == 2023)

# Remove rows with missing location data for mapping
crime_2023 <- crime_2023 %>% filter(!is.na(Longitude) & !is.na(Latitude))

# Monthly crime count
monthly_crimes <- crime_2023 %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month) %>%
  summarise(Total_Crimes = n())

# Arrest distribution data
arrest_dist <- crime_2023 %>%
  group_by(Arrest) %>%
  summarise(Total = n())

# Crime type count (top 10)
crime_types <- crime_2023 %>%
  group_by(Primary.Type) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  head(10)

# UI
ui <- fluidPage(
  titlePanel("Crime Data Dashboard - 2023"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(crime_data$Date, na.rm = TRUE),
                     end = max(crime_data$Date, na.rm = TRUE)),
      selectInput("crimeType", "Select Crime Type:", 
                  choices = c("ALL", unique(crime_data$Primary.Type)), 
                  selected = "ALL", multiple = TRUE),
      selectInput("location", "Select Location:", 
                  choices = c("ALL", unique(crime_data$Location.Description)), 
                  selected = "ALL", multiple = TRUE),
      checkboxGroupInput("visualizations", "Choose Visualizations to Display:",
                         choices = list("Monthly Crimes" = "monthly", 
                                        "Top Crime Types" = "top",
                                        "Crime Map" = "map",
                                        "Arrest Distribution" = "arrest"),
                         selected = c("monthly", "top", "map", "arrest")),
      actionButton("update", "Update View")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Monthly Crimes", plotOutput("monthlyCrimesPlot")),
        tabPanel("Top Crime Types", plotOutput("topCrimeTypesPlot")),
        tabPanel("Crime Map", leafletOutput("crimeMap")),
        tabPanel("Arrest Distribution", plotOutput("arrestDistPlot"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  # Reactive filter function
  filtered_data <- reactive({
    data <- crime_2023 %>% 
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    
    if (!("ALL" %in% input$crimeType)) {
      data <- data %>% filter(Primary.Type %in% input$crimeType)
    }
    
    if (!("ALL" %in% input$location)) {
      data <- data %>% filter(Location.Description %in% input$location)
    }
    
    data
  })
  
  # Monthly Crimes Plot
  output$monthlyCrimesPlot <- renderPlot({
    if (!"monthly" %in% input$visualizations) return(NULL)
    
    monthly_data <- filtered_data() %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month) %>%
      summarise(Total_Crimes = n())
    
    ggplot(monthly_data, aes(x = Month, y = Total_Crimes)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Total Crimes per Month in 2023", x = "Month", y = "Total Crimes") +
      theme_minimal()
  })
  
  # Top Crime Types
  output$topCrimeTypesPlot <- renderPlot({
    if (!"top" %in% input$visualizations) return(NULL)
    
    crime_data_filtered <- filtered_data() %>%
      group_by(Primary.Type) %>%
      summarise(Total = n()) %>%
      arrange(desc(Total)) %>%
      head(10)
    
    ggplot(crime_data_filtered, aes(x = reorder(Primary.Type, -Total), y = Total)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Crime Types in 2023", x = "Crime Type", y = "Total Count") +
      theme_minimal()
  })
  
  # Crime Map
  output$crimeMap <- renderLeaflet({
    if (!"map" %in% input$visualizations) return(NULL)
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       popup = ~paste0("<b>Type:</b> ", Primary.Type, 
                                       "<br><b>Date:</b> ", Date, 
                                       "<br><b>Location:</b> ", Location.Description),
                       radius = 3, color = "red", fillOpacity = 0.7)
  })
  
  # Arrest Distribution Plot
  output$arrestDistPlot <- renderPlot({
    if (!"arrest" %in% input$visualizations) return(NULL)
    
    arrest_data <- filtered_data() %>%
      group_by(Arrest) %>%
      summarise(Total = n()) %>%
      filter(!is.na(Arrest)) # Remove NA values
    
    ggplot(arrest_data, aes(x = "", y = Total, fill = factor(Arrest))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Arrest Distribution in 2023", x = NULL, y = "Total Arrests") +
      theme_void() +
      scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"))
  })
}

# Run the app
shinyApp(ui, server)

#rsconnect::setAccountInfo(name='suryar', token='4278DD5E27F58D59EAB79B3934468994', secret='5pGexwjrENieic/7/3loyC9Ee0AKv4SCGrMJ8aTf')