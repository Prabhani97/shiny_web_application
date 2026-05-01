# This web app is built using the Shiny framework to visualize and analyze the Global Climate–Health Impact Tracker dataset, whihc was extracted from
# Kaggle Datasets (Link to the dataset is th ine README file) The dataset tracks the relationship between climate events and health outcomes across 25 countries from 2015 to 2025,
# combining climate indicators, air quality measures, health outcomes, socioeconomic variables, and wellbeing indicators.
# The app has three main sections: a map view to explore spatial patterns of climate-health impacts, a dataset view for filtering and downloading the data,
# and an analysis section for temporal trends, country rankings, regional comparisons, and variable relationships.


# First, Loading the required packages

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(rnaturalearth)
library(bslib)
library(DT)
library(plotly)

# Preparing data

d <- read_csv("global_climate_health_impact_tracker_2015_2025.csv")

# I wanted to convert the date column to Date format and extract the month name for easier filtering and display in the app.
# My idea is to keep the month_name column available to users to filter by month using a dropdown menu with month names instead of numeric values.

d <- d %>%
  mutate(
    date = as.Date(date),
    month_name = month.name[month]
  )

# Since I am going to display various climate and health indicators on the map and in the analysis section,
# I created a named vector called impact_choices to map user-friendly names to the actual column names in the dataset.

impact_choices <- c(
  "PM2.5 Air Pollution" = "pm25_ugm3",
  "Air Quality Index" = "air_quality_index",
  "Temperature" = "temperature_celsius",
  "Heat Wave Days" = "heat_wave_days",
  "Drought Indicator" = "drought_indicator",
  "Flood Indicator" = "flood_indicator",
  "Respiratory Disease Rate" = "respiratory_disease_rate",
  "Cardio Mortality Rate" = "cardio_mortality_rate",
  "Vector Disease Risk" = "vector_disease_risk_score",
  "Waterborne Disease Incidents" = "waterborne_disease_incidents",
  "Heat Related Admissions" = "heat_related_admissions"
)

# My map view is going to show the average value of the selected impact variable for each country in a given year
# Therefore, I needed to define the appropriate units for each variable to display in the map pop ups and legends.

impact_units <- c(
  pm25_ugm3 = "µg/m³",
  air_quality_index = "AQI",
  temperature_celsius = "°C",
  heat_wave_days = "days",
  drought_indicator = "index",
  flood_indicator = "index",
  respiratory_disease_rate = "rate",
  cardio_mortality_rate = "rate",
  vector_disease_risk_score = "risk score",
  waterborne_disease_incidents = "incidents",
  heat_related_admissions = "admissions"
)


# Since the user can change the indicator or the risk category in the map view,
# I created this function called get_palette to return the appropriate color palette for the selected variable.
# I assigned colors that would be meaningful to each type of indicator
# (For examlpe: purple for air pollution, red for heat-related variables, blue for water-related variables) to enhance the visual interpretation of the map.

get_palette <- function(var) {
  if (var %in% c("pm25_ugm3", "air_quality_index", "respiratory_disease_rate")) {
    return("Purples")
  } else if (var %in% c("temperature_celsius", "heat_wave_days",
                        "heat_related_admissions", "drought_indicator")) {
    return("YlOrRd")
  } else if (var %in% c("flood_indicator",
                        "waterborne_disease_incidents",
                        "vector_disease_risk_score")) {
    return("Blues")
  } else {
    return("YlOrRd")
  }
}

# Loading the world shapefile using the rnaturalearth package to get the country boundaries for the map view.
# I selected the medium scale for a good balance between detail and performance,
# and I kept only the iso_a3 code, country name, and geometry for efficient merging with the dataset.

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry)

# Okay, now we are coming to the UI part.

# One strategy I mainly foloowed in designing the UI is inspecting the basic structure of a dashboard I initially built opening it in the browser and then
# trying to improving the design by adding custom CSS styles, adjusting the layout, and enhancing the visual elements to create a more
# user-friendly interface.

# This idea i got from my husband who is a QA engineer about the user experience and design!!


# I designed a navbarPage layout with three main tabs: Home, Map View, Dataset, and Analysis.

# Now I am going to define stylings needed for the naigation bar in the app and UI inside each tab
# I used custom CSS styles to create a modern and clean look for the dashboard, with a light background, a blue navigation bar, and white content cards with shadows.

ui <- navbarPage(

  # The title of the app is "Global Climate–Health Impact Tracker (2015–2025)".
  # I also applied a Bootstrap theme called "flatly" for a professional appearance

  title = "Global Climate–Health Impact Tracker (2015–2025)",
  theme = bslib::bs_theme(bootswatch = "flatly"),

  # I added custom CSS styles in the header to enhance the visual design of the app

  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f9fc; # a clean canvas for the content
      }

       # Custom styles for the navbar and content cards

      .navbar {
        background-color: #2c7fb8 !important;
        border-bottom: none;
        box-shadow: 0 2px 8px rgba(0,0,0,0.12);
      }

      .navbar-brand {
        color: white !important;
        font-weight: 700;
      }

      .navbar-nav > li > a {
        color: white !important;
        font-weight: 500;
      }

      .navbar-nav {
  margin-left: auto !important;
}

      .navbar-nav > li.active > a,
      .navbar-nav > li.active > a:focus,
      .navbar-nav > li.active > a:hover {
        background-color: #1f5f8b !important;
        color: white !important;
        font-weight: 700;
      }


       # Card styles for map, dataset, and analysis sections

      .sidebar-card {
        background: white;
        border: 1px solid #dce3ea;
        border-radius: 14px;
        padding: 18px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      }

      .map-card {
        background: white;
        border: 1px solid #dce3ea;
        border-radius: 14px;
        padding: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      }

      .metric-card-blue {
        background-color: #2c7fb8;
        color: white;
        border-radius: 12px;
        padding: 18px;
        text-align: center;
        box-shadow: 0 2px 10px rgba(0,0,0,0.12);
        min-height: 105px;
      }

      .metric-title {
        color: white;
        font-weight: bold;
        font-size: 15px;
        margin-bottom: 10px;
      }

      .metric-value {
        color: white;
        font-weight: bold;
        font-size: 22px;
      }

      .btn-danger-custom {
        background-color: #c0392b;
        color: white;
        font-weight: bold;
        border: none;
        border-radius: 8px;
        width: 100%;
      }
    "))
  ),

  # Now I am going to define the content for each tab.
  # The Home tab provides an overview of the dataset and the app's features,
  # while the Map View, Dataset, and Analysis tabs contain interactive elements for exploring the data in different ways.

  tabPanel(
    "Home",

    # In the Home tab, I included a title, a brief description of the dataset and its contents, and an overview of what users can do in the app.

    fluidPage(
      br(),

      fluidRow(
        column(
          12,
          div(
            class = "map-card",
            style = "padding: 30px;",
            h1("Global Climate–Health Impact Tracker")

          ),

          br(),

          p("This dashboard is built from the Global Climate–Health Impact Tracker dataset, which tracks the relationship between climate events and health outcomes across 25 countries from 2015 to 2025."),

          p("The dataset combines climate indicators, air quality measures, health outcomes, socioeconomic variables, and wellbeing indicators. It is useful for climate-health research, predictive modeling, time series analysis, policy impact assessment, and regional comparative studies."),

          br(),

        # This is for the four key metrics at the top of the home page, which show the total number of records, countries, regions, and years covered in the dataset.

          fluidRow(
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Records"),
                       div(class = "metric-value", textOutput("home_records", inline = TRUE)))
            ),
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Countries"),
                       div(class = "metric-value", textOutput("home_countries", inline = TRUE)))
            ),
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Regions"),
                       div(class = "metric-value", textOutput("home_regions", inline = TRUE)))
            ),
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Years"),
                       div(class = "metric-value", "2015–2025"))
            )
          ),

          br(),
          br(),

         # the below section provides a detailed list of what the dataset includes and what users can do in the app.

          h3("What this dataset includes"),

          tags$ul(
            tags$li(strong("Climate indicators: "), "temperature, precipitation, heat wave days, drought, floods, and extreme weather events."),
            tags$li(strong("Air quality: "), "PM2.5 concentration and air quality index."),
            tags$li(strong("Health outcomes: "), "respiratory disease, cardiovascular mortality, vector-borne disease risk, waterborne disease incidents, and heat-related admissions."),
            tags$li(strong("Socioeconomic factors: "), "healthcare access, GDP per capita, income level, and population."),
            tags$li(strong("Wellbeing indicators: "), "mental health index and food security index.")
          ),

          br(),

          h3("What you can do in this web app"),

          tags$ul(
            tags$li(strong("Map View: "), "visualize country-level climate-health impacts across the world."),
            tags$li(strong("Dataset: "), "filter, search, select columns, and download the data."),
            tags$li(strong("Analysis: "), "explore temporal trends, country rankings, regional comparisons, and impact relationships.")
          ),

          br(),

          p(strong("License: "), "Public Domain"),
          p(strong("Dataset source: "), "Kaggle Datasets")
        )
      )
    )
  )
  ,

  # In the Map View tab, I created a sidebar with filters for selecting the impact variable, year, month, and region.
  # The main area displays an interactive Leaflet map showing the average value of the selected variable for each country,
  # along with key metrics summarizing the global average, most affected country, number of countries shown, and highest value.

  tabPanel(
    "Map View",

    fluidPage(
      br(),

      fluidRow(
        column(
          width = 3,

          div(
            class = "sidebar-card",

            # The selectInput is for "map_var" allows users to choose which climate-health impact variable they want to visualize on the map.
            # The choices are defined in the impact_choices vector,which i defined veryearly in the code and the default selection is "PM2.5 Air Pollution".\

            selectInput(
              "map_var",
              "Impact Type",
              choices = impact_choices,
              selected = "pm25_ugm3"
            ),

            # I wanted to add a slider for the user to change the year and see how the impacts change over time.
            # The slider ranges from the minimum to the maximum year in the dataset, with a default value of 2023 initially.

            sliderInput(
              "year",
              "Year",
              min = min(d$year),
              max = max(d$year),
              value = 2023,
              step = 1,
              sep = ""
            ),

            # Since the dataset also includes monthly data, I added a dropdown menu for users to filter the map by month
            # The choices include "All" for no filtering and the names of the months

            selectInput(
              "month",
              "Month",
              choices = c("All", month.name),
              selected = "All"
            ),

            # I also added a dropdown for users to filter the map by region, with "All" as the default option and the unique regions
            # from the dataset as choices

            selectInput(
              "region",
              "Region",
              choices = c("All", sort(unique(d$region))),
              selected = "All"
            ),

             # Finally, I included a reset button that allows users to quickly clear all filters and return to the
             # default map view showing PM2.5 levels for all countries in 2023 without any month or region filtering.

            actionButton(
              "reset",
              "Reset Filters",
              class = "btn-danger-custom"
            )
          )
        ),

        column(
          width = 9,

          # This view also contains few cards at the bottom of it to show key metrics related to the selected variable on the map,
          # such as the global average value, the most affected country, the number of countries shown, and the highest value observed.
          # These are updating dynamically based on the filters applied to the map by the user

          div(
            class = "map-card",
            leafletOutput("map", height = 620)
          ),

          br(),

          fluidRow(
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Global Average"),
                       div(class = "metric-value", textOutput("avg_value", inline = TRUE)))
            ),
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Most Affected"),
                       div(class = "metric-value", textOutput("max_country", inline = TRUE)))
            ),
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Countries Shown"),
                       div(class = "metric-value", textOutput("n_country", inline = TRUE)))
            ),
            column(3,
                   div(class = "metric-card-blue",
                       div(class = "metric-title", "Highest Value"),
                       div(class = "metric-value", textOutput("max_value", inline = TRUE)))
            )
          )
        )
      )
    )
  ),

  # In the Dataset tab, I created a sidebar with filters for region, country, year range, month, and columns to show.
  # The main area displays a searchable and paginated data table showing the filtered dataset based on the user's selections.
  # I also included a download button that allows users to download the currently filtered dataset as a CSV file for offline analysis.

  tabPanel(
    "Dataset",


    fluidPage(
      br(),

      fluidRow(
        column(
          width = 3,

          div(
            class = "sidebar-card",

            selectInput(
              "data_region",
              "Filter by Region",
              choices = c("All", sort(unique(d$region))),
              selected = "All"
            ),

            selectInput(
              "data_country",
              "Filter by Country",
              choices = c("All", sort(unique(d$country_name))),
              selected = "All"
            ),

            sliderInput(
              "data_year",
              "Year Range",
              min = min(d$year),
              max = max(d$year),
              value = c(min(d$year), max(d$year)),
              step = 1,
              sep = ""
            ),

            selectInput(
              "data_month",
              "Filter by Month",
              choices = c("All", month.name),
              selected = "All"
            ),

            # The selectInput for "data_cols" allows users to choose which columns they want to see in the data table.
            # The choices are the column names from the dataset, and I set some default columns that are most relevant for initial exploration.

            selectInput(
              "data_cols",
              "Columns to Show",
              choices = names(d),
              selected = c(
                "country_name", "country_code", "region", "year", "month_name",
                "pm25_ugm3", "air_quality_index", "temperature_celsius",
                "heat_wave_days", "flood_indicator", "drought_indicator",
                "respiratory_disease_rate"
              ),
              multiple = TRUE
            ),

            downloadButton(
              "download_dataset",
              "Download Filtered Data",
              class = "btn-danger-custom"
            )
          )
        ),

        column(
          width = 9,

          # The main area of the Dataset tab contains a DTOutput called "dataset_table" which will render an interactive data table
          # showing the filtered dataset based on the user's selections in the sidebar.

          div(
            class = "map-card",
            h3("Filtered Climate–Health Dataset"),
            p("Use the filters on the left to explore selected countries, regions, years, months, and variables."),
            DTOutput("dataset_table")
          )
        )
      )
    )
  )
  ,

  # In the Analysis tab, I created a sidebar with buttons for different types of analyses: temporal trends, country rankings, regional comparisons, and impact relationships.
  # When a user clicks on one of these buttons, the main area updates to show the corresponding plot based on the selected
  # analysis type and additional filters that appear dynamically in the sidebar.

  tabPanel(
    "Analysis",

    fluidPage(
      br(),

      fluidRow(
        column(
          width = 3,

          # Just like I explained in the begining the side bar includes buttons for different types of analyses, temporal trends, country rankings,
          # regional comparisons, and impact relationships.

          div(
            class = "sidebar-card",

            fluidRow(
              column(
                6,
                actionButton("show_trend", "Temporal Trends",
                             class = "btn-danger", style = "width:100%; margin-bottom:10px;")
              ),
              column(
                6,
                actionButton("show_top10", "Country Rankings",
                             class = "btn-danger", style = "width:100%; margin-bottom:10px;")
              )
            ),

            fluidRow(
              column(
                6,
                actionButton("show_region", "Regional Comparison",
                             class = "btn-danger", style = "width:100%;")
              ),
              column(
                6,
                actionButton("show_relation", "Impact Relationships",
                             class = "btn-danger", style = "width:100%;")
              )
            ),

            hr(),

            uiOutput("analysis_controls")
          )
        ),

        # The main area of the Analysis tab contains a plotlyOutput the analysis_plot which will render different types of plots based on the selected analysis type and filters.
        # I also included a textOutput called "analysis_title" to display the title of the current analysis, and a description box that
        # explains what the current view shows and how to interpret it

        column(
          width = 9,

          div(
            class = "analysis-card",
            h3(textOutput("analysis_title", inline = TRUE)),
            plotlyOutput("analysis_plot", height = 560),
            br(),
            div(
              class = "description-box",
              h4("What this view shows"),
              textOutput("analysis_description")
            )
          )
        )
      )
    )
  )
)

# Now I am going to define the server logic for the app.

server <- function(input, output, session) {

  # The first part of the server function handles the Map View tab.
  # It includes an observeEvent for the reset button to clear all filters and return to the default map view.

  observeEvent(input$reset, {
    updateSelectInput(session, "map_var", selected = "pm25_ugm3")
    updateSliderInput(session, "year", value = 2023)
    updateSelectInput(session, "month", selected = "All")
    updateSelectInput(session, "region", selected = "All")
  })

   # The map_data reactive expression filters the dataset based on the user's selections for year, month, and region, and then calculates the average value of the selected variable for each country.
   # This filtered and summarized data is then used to create the Leaflet map and update the key metrics displayed below the map.

  map_data <- reactive({
    df <- d %>%
      filter(year == input$year)

    if (input$month != "All") {
      df <- df %>% filter(month_name == input$month)
    }

    if (input$region != "All") {
      df <- df %>% filter(region == input$region)
    }

    df %>%
      group_by(country_code, country_name, region) %>%
      summarise(
        value = mean(.data[[input$map_var]], na.rm = TRUE),
        .groups = "drop"
      )
  })

  # The renderLeaflet function creates an interactive map using the Leaflet package. It merges the world shapefile with the filtered dataset to get the geometry for each country, and then uses color coding to visualize the average value of the selected variable for each country.
  # The map includes interactive features such as tooltips and popups that show detailed information about each country when hovered or clicked, and a legend to interpret the color scale.

  output$map <- renderLeaflet({
    df <- map_data()

    map_sf <- world %>%
      left_join(df, by = c("iso_a3" = "country_code"))

    # This is where I use the get_palette function to assign the appropriate color palette based on the selected variable.

    pal <- colorNumeric(
      palette = get_palette(input$map_var),
      domain = df$value,
      na.color = "#eeeeee"
    )

    # I also retrieve the unit for the selected variable from the impact_units vector to display in the popups and legend.

    unit <- impact_units[[input$map_var]]

   # This is the code for creating the Leaflet map.
    # It uses the world shapefile merged with the filtered dataset to create polygons for each country, colored by the average value of the selected variable.
    # The map includes interactive features such as tooltips and popups that show detailed information about each country when clicked
    # there is a color scale too.


    # In order to do this I also saw other these kind of map views online and I wanted to highlight the regions in a country specially and color
    # them according to the impact.

    leaflet(map_sf, options = leafletOptions(worldCopyJump = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = 20, lat = 15, zoom = 2) %>%

      addPolygons(
        fillColor = ~pal(value),
        fillOpacity = 0.82,
        color = "black",
        weight = 1.2,
        opacity = 1,

        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          fillOpacity = 0.95,
          bringToFront = TRUE
        ),

        label = ~lapply(
          paste0(
            "<span style='color:black; font-weight:bold;'>",
            ifelse(is.na(country_name), name, country_name),
            "</span>"
          ),
          HTML
        ),

        labelOptions = labelOptions(
          textsize = "13px",
          direction = "auto",
          style = list(
            "font-weight" = "bold",
            "color" = "black"
          )
        ),

        # The popup shows detailed information about the country, including the name, region, impact value with units, year, and month.
        # It uses HTML styling to make the information clear and visually appealing.

        popup = ~paste0(
          "<div style='color:black; font-weight:bold;'>",
          "<b>", ifelse(is.na(country_name), name, country_name), "</b><br>",
          "Region: ", ifelse(is.na(region), "No data", region), "<br>",
          "Impact value: ",
          ifelse(is.na(value), "No data", paste0(round(value, 2), " ", unit)), "<br>",
          "Year: ", input$year, "<br>",
          "Month: ", input$month,
          "</div>"
        )
      ) %>%

      # color scale

      addLegend(
        position = "topright",
        pal = pal,
        values = df$value,
        title = paste0(
          names(impact_choices)[impact_choices == input$map_var],
          " (", unit, ")"
        ),
        opacity = 0.9
      )
  })

  # This is how I show the key metrices values i wanted to insert cards for in the UI view.

  output$avg_value <- renderText({
    df <- map_data()
    unit <- impact_units[[input$map_var]]

    if (nrow(df) == 0 || all(is.na(df$value))) {
      return("No data")
    }

    paste0(round(mean(df$value, na.rm = TRUE), 2), " ", unit)
  })

  output$max_country <- renderText({
    df <- map_data()

    if (nrow(df) == 0 || all(is.na(df$value))) {
      return("No data")
    }

    df %>%
      filter(!is.na(value)) %>%
      slice_max(value, n = 1, with_ties = FALSE) %>%
      pull(country_name)
  })

  output$n_country <- renderText({
    df <- map_data()

    if (nrow(df) == 0) {
      return("0")
    }

    as.character(n_distinct(df$country_name))
  })

  output$max_value <- renderText({
    df <- map_data()
    unit <- impact_units[[input$map_var]]

    if (nrow(df) == 0 || all(is.na(df$value))) {
      return("No data")
    }

    paste0(round(max(df$value, na.rm = TRUE), 2), " ", unit)
  })

  # This is the function for the dataset view.
  # The dataset_data reactive expression filters the dataset based on the user's selections.


  dataset_data <- reactive({

    df <- d

    if (input$data_region != "All") {
      df <- df %>% filter(region == input$data_region)
    }

    if (input$data_country != "All") {
      df <- df %>% filter(country_name == input$data_country)
    }

    df <- df %>%
      filter(
        year >= input$data_year[1],
        year <= input$data_year[2]
      )

    if (input$data_month != "All") {
      df <- df %>% filter(month_name == input$data_month)
    }

    if (length(input$data_cols) > 0) {
      df <- df %>% select(all_of(input$data_cols))
    }

    df
  })

 # This DT library, I found to be creating interactive data table using the DT package,
  # I am showing the filtered dataset based on the user's selections in the sidebar using this.
  #The table includes features such as searching, pagination, column selection, and export buttons for copying, downloading as CSV or Excel.

  output$dataset_table <- renderDT({
    datatable(
      dataset_data(),
      filter = "top",
      rownames = FALSE,
      extensions = c("Buttons"),
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  })

  # The downloadHandler function allows users to download the currently filtered dataset as a CSV file when they click the "Download Filtered Data" button.


  output$download_dataset <- downloadHandler(
    filename = function() {
      "filtered_climate_health_dataset.csv"
    },
    content = function(file) {
      write_csv(dataset_data(), file) # This writes the filtered dataset to a CSV file that the user can download.
    }
  )

  # Now I am going to define the server logic for the Analysis tab.

  # I created a reactiveVal called selected_analysis to keep track of which analysis type is currently selected by the user.
  # When the user clicks on one of the analysis buttons, the corresponding observeEvent updates the value of selected_analysis.

  selected_analysis <- reactiveVal("trend")

  observeEvent(input$show_trend, {
    selected_analysis("trend")
  })

  observeEvent(input$show_top10, {
    selected_analysis("top10")
  })

  observeEvent(input$show_region, {
    selected_analysis("region")
  })

  observeEvent(input$show_relation, {
    selected_analysis("relation")
  })

  # This function is for analysis_title updates the title displayed in the Analysis tab based on the currently selected analysis type,
  # providing context for the user about what they are viewing, i mean for a whole new user of the web app.

  output$analysis_title <- renderText({
    if (selected_analysis() == "trend") {
      "Trend Over Time"
    } else if (selected_analysis() == "top10") {
      "Top 10 Countries"
    } else if (selected_analysis() == "region") {
      "Regional Comparison"
    } else {
      "Relationship Plot"
    }
  })


  # This is the function for analysis_description, which provides a dynamic description of the current analysis view based on the selected analysis type.
  # It helps users understand what the current plot shows and how to interpret it.

  output$analysis_controls <- renderUI({

    # Based on the value of selected_analysis, this function generates different sets of input controls for the user

    if (selected_analysis() == "trend") {

      tagList(
        selectInput("trend_var", "Select variable", impact_choices),
        selectInput(
          "trend_countries",
          "Select countries",
          choices = sort(unique(d$country_name)),
          selected = c("United States", "India", "China"),
          multiple = TRUE
        ),
        selectInput(
          "trend_month",
          "Month",
          choices = c("All", month.name),
          selected = "All"
        )
      )

    } else if (selected_analysis() == "top10") {

      tagList(
        selectInput("top_var", "Select variable", impact_choices),
        sliderInput(
          "top_year",
          "Select year",
          min = min(d$year),
          max = max(d$year),
          value = 2023,
          step = 1,
          sep = ""
        ),
        selectInput(
          "top_region",
          "Region",
          choices = c("All", sort(unique(d$region))),
          selected = "All"
        )
      )

    } else if (selected_analysis() == "region") {

      tagList(
        selectInput("region_var", "Select variable", impact_choices),
        sliderInput(
          "region_year",
          "Select year",
          min = min(d$year),
          max = max(d$year),
          value = 2023,
          step = 1,
          sep = ""
        )
      )

    } else {

      tagList(
        selectInput("x_var", "X variable", impact_choices, selected = "pm25_ugm3"),
        selectInput("y_var", "Y variable", impact_choices, selected = "respiratory_disease_rate"),
        sliderInput(
          "relation_year",
          "Select year",
          min = min(d$year),
          max = max(d$year),
          value = 2023,
          step = 1,
          sep = ""
        ),
        selectInput(
          "relation_region",
          "Region",
          choices = c("All", sort(unique(d$region))),
          selected = "All"
        )
      )
    }
  })

   # This is where the plots coming.

  # The renderPlotly function generates different types of plots based on the selected analysis type and the user's filter selections.
  # It uses ggplot2 to create the plots and ggplotly

  # This is where the plots coming.

  # The renderPlotly function generates different types of plots based on the selected analysis type and the user's filter selections.
  # It uses ggplot2 to create the plots and ggplotly

  # This is where the plots coming.

  # The renderPlotly function generates different types of plots based on the selected analysis type and the user's filter selections.
  # It uses ggplot2 to create the plots and ggplotly

  output$analysis_plot <- renderPlotly({

  if (selected_analysis() == "trend") {

    df <- d

    if (input$trend_month != "All") {
      df <- df %>% filter(month_name == input$trend_month)
    }

    # Just like we do in the class, we take the dataset, I filter it based on the selected countries and month,
    # then I group it by year and country name, and calculate the average value of the selected variable for each group.

    df <- df %>%
      filter(country_name %in% input$trend_countries) %>%
      group_by(year, country_name) %>%
      summarise(value = mean(.data[[input$trend_var]], na.rm = TRUE), .groups = "drop")

    # Now I can create a line plot using ggplot2 to show the trend of the selected variable over time for the selected countries, using what we already know
    # about plotting! 4 types of plots I have.

    # This is a simple line plot with points.

    p <- ggplot(df, aes(x = year, y = value, color = country_name)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Selected Impact Trend Over Time",
        x = "Year",
        y = names(impact_choices)[impact_choices == input$trend_var],
        color = "Country"
      ) +
      theme_minimal()

    ggplotly(p)

    # Now, when I need to visualizae the top 10 countries affected by that impact type , I use a
    # bar chart to show the average value of the selected variable for the top 10 countries in the selected year and region.

  } else if (selected_analysis() == "top10") {

    df <- d %>%
      filter(year == input$top_year)

    if (input$top_region != "All") {
      df <- df %>% filter(region == input$top_region)
    }

    # Filtering
    df <- df %>%
      group_by(country_name) %>%
      summarise(value = mean(.data[[input$top_var]], na.rm = TRUE), .groups = "drop") %>%
      slice_max(value, n = 10, with_ties = FALSE)

    # Bar chart showing the top 10 countries with the highest average value of the selected variable, ordered from highest to lowest
    p <- ggplot(df, aes(x = reorder(country_name, value), y = value)) +
      geom_col(fill = "#c0392b") +
      coord_flip() +
      labs(
        title = "Top 10 Countries with Highest Selected Impact",
        x = "Country",
        y = names(impact_choices)[impact_choices == input$top_var]
      ) +
      theme_minimal()

    ggplotly(p)

    # For the regional comparison, I use a boxplot to compare the distribution of the selected variable across different regions
    # in the selected year, which helps to identify regional disparities and variability in the impact.

  } else if (selected_analysis() == "region") {

    df <- d %>%
      filter(year == input$region_year)

    # Plotting the boxplot to compare the distribution of the selected variable across regions.

    p <- ggplot(df, aes(x = region, y = .data[[input$region_var]], fill = region)) +
      geom_boxplot(alpha = 0.75) +
      labs(
        title = "Regional Distribution of Selected Impact",
        x = "Region",
        y = names(impact_choices)[impact_choices == input$region_var]
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "none")

    ggplotly(p)

    # Now I further want to inspect the relationship between two selected variables,
    # I use a simple scatter plot to show the relationship between the selected x and y variables for the observations in the selected year

  } else {

    df <- d %>%
      filter(year == input$relation_year)

    if (input$relation_region != "All") {
      df <- df %>% filter(region == input$relation_region)
    }

    p <- ggplot(df, aes(
      x = .data[[input$x_var]],
      y = .data[[input$y_var]],
      color = region,
      text = country_name
    )) +
      geom_point(alpha = 0.7, size = 2.5) +
      labs(
        title = "Relationship Between Selected Variables",
        x = names(impact_choices)[impact_choices == input$x_var],
        y = names(impact_choices)[impact_choices == input$y_var],
        color = "Region"
      ) +
      theme_minimal()

    ggplotly(p, tooltip = c("text", "x", "y"))
  }
})
output$home_records <- renderText({
  format(nrow(d), big.mark = ",")
})

output$home_countries <- renderText({
  n_distinct(d$country_name)
})

output$home_regions <- renderText({
  n_distinct(d$region)
})

# Plot descriptions

output$analysis_description <- renderText({

  if (selected_analysis() == "trend") {
    paste0(
      "This view shows how ",
      names(impact_choices)[impact_choices == input$trend_var],
      " changes over time for the selected countries. It helps identify long-term increases, decreases, or stable patterns across years."
    )

  } else if (selected_analysis() == "top10") {
    paste0(
      "This view ranks countries by the highest average ",
      names(impact_choices)[impact_choices == input$top_var],
      " in ",
      input$top_year,
      ". It helps quickly identify the countries most affected by the selected climate-health indicator."
    )

  } else if (selected_analysis() == "region") {
    paste0(
      "This view compares the distribution of ",
      names(impact_choices)[impact_choices == input$region_var],
      " across regions in ",
      input$region_year,
      ". Wider boxes indicate more variability within a region, while higher medians suggest greater overall impact."
    )

  } else {
    paste0(
      "This view explores the relationship between ",
      names(impact_choices)[impact_choices == input$x_var],
      " and ",
      names(impact_choices)[impact_choices == input$y_var],
      " in ",
      input$relation_year,
      ". Each point represents an observation, colored by region, helping reveal possible associations between environmental exposure and health outcomes."
    )
  }
})
}

# Finally, I call the shinyApp function to run the application, passing in the ui and server components that I defined above.

shinyApp(ui, server)
