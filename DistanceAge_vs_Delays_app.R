#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Load data
aircrafts_ages_raw <- read.csv("SKYS_THE_LIMIT.csv")
aircrafts_ages_since_2003 <- aircrafts_ages_raw |>
  filter(AGE <= 22)


# UI
ui <- fluidPage(
  titlePanel("Aircraft (airtime) Age vs Carrier Delays"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Select Year:",
                  min = 2003,
                  max = 2025,
                  value = 2003,
                  sep = "")  # no commas in year
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
)

# Define server logic required to draw a boxplot
server <- function(input, output) {
  
  # Filtered dataset based on selected year
  filtered_data <- reactive({
    aircrafts_ages_since_2003 |>
      filter(YEAR == input$year, !is.na(AIR_TIME), !is.na(TOT_CARRIER_DELAY)) |>
      mutate(
        distance_bin = cut(
          DISTANCE,
          breaks = c(0, 1e5, 5e5, 1e6, 5e6, 1e7, 2e7, 3e7, 4e7),
          labels = c(
            "0–100K", "100K–500K", "500K–1M", "1M–5M",
            "5M–10M", "10M–20M", "20M–30M", "30M–40M"
          ),
          include.lowest = TRUE
        )
      )
  })
  
  output$Plot <- renderPlot({
    ggplot(filtered_data(), aes(x = airtime_bin, y = (TOT_CARRIER_DELAY / 60))) +
      geom_boxplot(fill = "thistle3", color = "darkblue", outlier.alpha = 0.2) +
      labs(
        title = "Aircraft Total Airtime vs Carrier Delays",
        x = "Aircraft Age (in time spent flying)",
        y = "Total Carrier Delay"
      ) +
      scale_y_log10(
        breaks = c(.5, 1, 5, 24, 48, 72, 168),
        labels = c("30 mins", "1 hr", "5 hrs", "1 day", "2 days", "3 days", "1 week")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)