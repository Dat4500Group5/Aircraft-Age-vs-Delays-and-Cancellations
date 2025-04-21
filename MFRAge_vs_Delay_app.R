library(shiny)
library(ggplot2)
library(dplyr)

# Load data
aircrafts_ages_raw <- read.csv("SKYS_THE_LIMIT.csv")
aircrafts_ages_since_2003 <- aircrafts_ages_raw |>
  filter(AGE <= 22)

# UI
ui <- fluidPage(
  titlePanel("Aircraft Age vs Carrier Delays"),
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

# Server
server <- function(input, output) {
  
  # Filtered dataset based on selected year
  filtered_data <- reactive({
    aircrafts_ages_since_2003 |>
      filter(YEAR == input$year)
  })
  
  # Render boxplot
  output$Plot <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(AGE), y = (TOT_CARRIER_DELAY / 60))) +
      geom_boxplot(fill = "thistle3", color = "darkblue", outlier.alpha = 0.2) +
      labs(
        title = paste("Aircraft Age vs Carrier Delays -", input$year),
        x = "Aircraft Age (years)",
        y = "Total Carrier Delay"
      ) +
      scale_y_log10(
        breaks = c(.5, 1, 5, 24, 48, 72, 168, 336),
        labels = c("30 mins", "1 hr", "5 hrs", "1 day", "2 days", "3 days", "1 week", "2 weeks")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
