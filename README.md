# 🔒 Crime Data Dashboard - 2023

## Live Link: https://suryar.shinyapps.io/crime_dashboard/
## 📄 Overview
This project is a Shiny-based web application for visualizing crime data from 2023. It allows users to analyze crime trends, explore different types of crimes, visualize their locations on an interactive map, and understand arrest distributions.

## ✨ Features
- **⏰ Date Range Filtering**: Select a specific range within 2023 to analyze crime data.
- **🔎 Crime Type & Location Filtering**: Choose specific crime types and locations for analysis.
- **📊 Multiple Visualizations**:
  - 🔄 Monthly Crime Trends
  - 🎯 Top 10 Crime Types
  - 🌍 Interactive Crime Map
  - 🔒 Arrest Distribution
- **📝 Interactive UI**: Users can customize displayed visualizations and filter data dynamically.

## 👨‍💻 Technologies Used
- **R and Shiny**: For building the interactive web application.
- **ggplot2**: For data visualization.
- **dplyr**: For data manipulation.
- **lubridate**: For date processing.
- **leaflet**: For interactive map visualization.

## 🛠️ Installation
1. Install R and RStudio if not already installed.
2. Install required R packages by running:
   ```r
   install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "leaflet"))
   ```
3. Clone or download this repository and place the `crime.csv` dataset in the working directory.

## 💡 Running the Application
1. Open RStudio and set the working directory to the project folder.
2. Run the following command:
   ```r
   shiny::runApp()
   ```
3. The dashboard will open in your browser.

## 📃 Data Requirements
- The dataset should be a CSV file named `crime.csv` with at least the following columns:
  - **📅 Date**: Crime occurrence date (format: dd-mm-yyyy)
  - **📝 Primary.Type**: Type of crime
  - **🏢 Location.Description**: Description of crime location
  - **🌍 Longitude** & **Latitude**: Coordinates for mapping
  - **🔒 Arrest**: Boolean indicating whether an arrest was made

## 🌐 Application Structure
- **UI Components**:
  - `dateRangeInput`: Select date range
  - `selectInput`: Filter by crime type and location
  - `checkboxGroupInput`: Choose visualizations to display
  - `actionButton`: Update the dashboard
- **Visualizations**:
  - 🔄 Line chart for monthly crime trends
  - 🌟 Bar chart for top 10 crime types
  - 🌍 Interactive leaflet map for crime locations
  - 💧 Pie chart for arrest distribution

## 📡 Deployment
To deploy on **ShinyApps.io**, use:
```r
rsconnect::setAccountInfo(name='YOUR_NAME', token='YOUR_TOKEN', secret='YOUR_SECRET')
deployApp()
```

## 📚 License
This project is licensed under the MIT License.

## 👤 Author
- **Surya Janardhan**
- **chintalajanardhan2004@gmail.com** 

## 🎯 Future Improvements
- Add crime severity ranking
- Include additional filtering options
- Improve visualization aesthetics

