#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(sf)
library(tidygeocoder)
library(fs)

source("../fun/read_ndac_directory.R")

# Define UI for slider demo app ----
ui <- page_sidebar(
  # App title ----
  title = h1(
    "Create spatial data for 'ND Aeronautics Commission Applicator Directory'"
  ),

  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Select a file ----
    h3("Step 1"),
    fileInput(
      "file1",
      "Choose MS Excel File",
      multiple = FALSE,
      accept = c(
        ".xls",
        ".xlsx"
      )
    ),

    # Horizontal line ----
    tags$hr(),

    # Geocode button ----
    h3("Step 2"),
    actionButton(
      "geocode",
      "Geocode Input Data"
    ),

    # Horizontal line ----
    tags$hr(),

    # Download button ----
    h3("Step 3"),
    downloadButton(
      "downloadData",
      "Download Geocoded Data"
    ),

    # Horizontal line ----
    tags$hr(),

    # Documentation link ----
    h3("Need Help?"),
    tags$a(
      class = "btn btn-default",
      href = "https://docs.google.com/document/d/1Zu9q-l1z0-i5aqFSYzfCfgnPrChbF70Hs1BB_jOPsf8/preview?tab=t.0",
      target = "_blank",
      "View Documentation"
    )
  ),

  # Output: Data file ----
  h2("Input Data Preview"),
  fluidRow(
    style = "height: 40%; overflow-y: auto;",
    column(12, tableOutput("datainput"))
  ),

  # Output: Geo data to fix ----
  h2(
    "Geocoding Errors (make sure CITY and STATE are correct in input file, then run again)"
  ),
  fluidRow(
    style = "height: 40%; overflow-y: auto;",
    column(12, tableOutput("geodata"))
  ),

  tags$footer(
    "Maintained by Rob Proulx, NDSU Extension (rob.proulx@ndsu.edu | 701-231-5389)",
    style = "position:absolute; bottom:0; width:100%; height:5%; text-align:left;"
  )
)

# Define server logic to read and process selected file ----
server <- function(input, output) {
  df_rv <- reactiveValues(data = NULL)
  dat_geo_rv <- reactiveValues(data = NULL)

  # Load and render input table ----
  observeEvent(input$file1, {
    # This code block will execute only when input$file1 changes (i.e., when uploaded)
    df <- read_ndac_directory(
      input = input$file1$datapath
    )
    df_rv$data <- df

    output$datainput <- renderTable(striped = TRUE, {
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.

      req(input$file1)

      return(df)
    })
  })

  # Geocode data and display any errors ----
  observeEvent(input$geocode, {
    # This code block will execute only when input$geocode changes (i.e., when clicked)
    req(df_rv$data)

    withProgress(
      message = 'Geocoding in progress',
      detail = 'This may take a while...',
      value = 0,
      {
        # Update progress bar
        incProgress(0.3, detail = "Sending data to API")

        # Perform geocoding
        dat_geo <- df_rv$data |>
          geocode(city = CITY, state = STATE, method = "osm")
        dat_geo_rv$data <- dat_geo

        # Final progress update
        incProgress(0.7, detail = "Done!")
      }
    )

    output$geodata <- renderTable(striped = TRUE, {
      dat_geo |>
        dplyr::filter(is.na(long) | is.na(lat))
    })
  })

  # Downloadable csv of geocoded dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(path_ext_remove(input$file1), "_geocoded.csv", sep = "")
    },
    content = function(file) {
      dat_geo_rv$data |>
        st_as_sf(coords = c("long", "lat"), crs = 4326) |>
        st_jitter(amount = 0.01) |>
        st_write(
          file,
          layer_options = "GEOMETRY=AS_WKT"
        )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
