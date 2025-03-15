# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(data.table)
library(shinycssloaders)
# Load the dataset (handle errors)
crime_data <- tryCatch({
  fread("crime.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  showNotification("Error loading crime dataset!", type = "error")
  return(NULL)
})

# Ensure data is valid
if (!is.null(crime_data) && "Date" %in% colnames(crime_data)) {
  crime_data$Date <- as.Date(crime_data$Date, format="%d-%m-%Y")
} else {
  crime_data <- NULL
}

# Filter for 2023 data (handle missing Date column)
if (!is.null(crime_data)) {
  crime_2023 <- crime_data %>% filter(year(Date) == 2023)
  crime_2023 <- crime_2023 %>% filter(!is.na(Longitude) & !is.na(Latitude))
} else {
  crime_2023 <- NULL
}

# UI
ui <- fluidPage(
  titlePanel("Crime Data Dashboard - 2023"),
  
  sidebarLayout(
    sidebarPanel(
      # Dynamic Date Range
      uiOutput("dateRangeUI"),
      
      # Crime Type Filter
      selectInput("crimeType", "Select Crime Type:", 
                  choices = c("ALL", if (!is.null(crime_2023)) unique(crime_2023$Primary.Type)), 
                  selected = "ALL", multiple = TRUE),
      
      # Location Filter
      selectInput("location", "Select Location:", 
                  choices = c("ALL", if (!is.null(crime_2023)) unique(crime_2023$Location.Description)), 
                  selected = "ALL", multiple = TRUE),
      
      # Visualization Selection
      checkboxGroupInput("visualizations", "Choose Visualizations to Display:",
                         choices = list("Monthly Crimes" = "monthly", 
                                        "Top Crime Types" = "top",
                                        "Arrest Distribution" = "arrest"),
                         selected = c("monthly", "top", "map", "arrest")),
      
      actionButton("update", "Update View", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Monthly Crimes", withSpinner(plotOutput("monthlyCrimesPlot"))),
        tabPanel("Top Crime Types", withSpinner(plotOutput("topCrimeTypesPlot"))),
        tabPanel("Arrest Distribution", withSpinner(plotOutput("arrestDistPlot"))),
        tabPanel("Data Table", DTOutput("dataTable"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Dynamic Date Range Input
  output$dateRangeUI <- renderUI({
    if (is.null(crime_2023)) return(NULL)
    
    dateRangeInput("dateRange", "Select Date Range:",
                   start = min(crime_2023$Date, na.rm = TRUE),
                   end = max(crime_2023$Date, na.rm = TRUE))
  })
  
  # Reactive Filtered Data
  filtered_data <- reactive({
    if (is.null(crime_2023)) return(NULL)
    
    data <- crime_2023 %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    
    if (!("ALL" %in% input$crimeType)) {
      data <- data %>% filter(Primary.Type %in% input$crimeType)
    }
    
    if (!("ALL" %in% input$location)) {
      data <- data %>% filter(Location.Description %in% input$location)
    }
    
    return(data)
  })
  
  # Monthly Crimes Plot
  output$monthlyCrimesPlot <- renderPlot({
    if (!"monthly" %in% input$visualizations || is.null(filtered_data())) return(NULL)
    
    monthly_data <- filtered_data() %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month) %>%
      summarise(Total_Crimes = n())
    
    ggplot(monthly_data, aes(x = Month, y = Total_Crimes)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "red", size = 3) +
      labs(title = "Total Crimes per Month in 2023", x = "Month", y = "Total Crimes") +
      theme_minimal()
  })
  
  # Top Crime Types Plot
  output$topCrimeTypesPlot <- renderPlot({
    if (!"top" %in% input$visualizations || is.null(filtered_data())) return(NULL)
    
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
  
  # Arrest Distribution Plot
  output$arrestDistPlot <- renderPlot({
    if (!"arrest" %in% input$visualizations || is.null(filtered_data())) return(NULL)
    
    arrest_data <- filtered_data() %>%
      group_by(Arrest) %>%
      summarise(Total = n()) %>%
      filter(!is.na(Arrest))
    
    ggplot(arrest_data, aes(x = "", y = Total, fill = factor(Arrest))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Arrest Distribution in 2023", x = NULL, y = "Total Arrests") +
      theme_void() +
      scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"))
  })
  
  # Data Table Output
  output$dataTable <- renderDT({
    if (is.null(filtered_data())) return(NULL)
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui, server)
