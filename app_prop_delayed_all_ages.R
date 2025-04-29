library(shiny)
library(ggplot2)
library(dplyr)

# Load data
aircrafts_ages_raw <- read.csv("SKYS_THE_LIMIT.csv")

# Process data

#delay count/ flight count
aircraft_with_prop <- aircrafts_ages_raw |>
  carrier_delay_prop <- COUNT_CARRIER_DELAY/FLIGHTS|> 
  filter(!is.na(AIR_TIME), !is.na(airtime_bin), !is.na(TOT_CARRIER_DELAY))

aircrafts_ages_since_2003 <- aircrafts_ages_raw |>
  filter(AGE <= 22) |>
  mutate(
    distance_bin = cut(
      DISTANCE,
      breaks = c(0, 1e5, 5e5, 1e6, 5e6, 1e7, 2e7, 3e7, 4e7),
      labels = c("0–100K", "100K–500K", "500K–1M", "1M–5M", "5M–10M", "10M–20M", "20M–30M", "30M–40M"),
      include.lowest = TRUE
    )
  ) |>
  mutate(
    flight_bin = cut(
      FLIGHTS,
      breaks = c(0, 100, 500, 1000, 5000, 10000, 25000, 50000, 75000),
      labels = c("0–100", "101–500", "501–1K", "1K–5K", "5K–10K", "10K–25K", "25K–50K", "50K–75K"),
      include.lowest = TRUE
    )
  ) |>
  mutate(
    airtime_bin = cut(
      AIR_TIME,
      breaks = c(0, 60, 300, 720, 1440, 2160, 4320, 7200, 14400, 28800),
      labels = c(
        "<1 hr",
        "1–5 hrs",
        "6–12 hrs",
        "13 hrs–1 day",
        "1.5 days",
        "3 days",
        "2–5 days",
        "5–10 days",
        "10–20 days"
      )
    )
  ) |> 
  filter(!is.na(AIR_TIME), !is.na(airtime_bin), !is.na(TOT_CARRIER_DELAY))

# UI
ui <- fluidPage(
  titlePanel("Aircraft Age vs Carrier Delays"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:", min = 2003, max = 2024, value = 2003, sep = ""),
      selectInput("age_var", "Select Age Variable:",
                  choices = c("Age in Years Since Manufacture" = "AGE",
                              "Distance Flown" = "distance_bin",
                              "Airtime Hours" = "airtime_bin",
                              "Number of Flights" = "flight_bin"))
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
)

# Function to generate plot
generate_plot <- function(data, age_var) {
  if (age_var == "AGE") {
    x_label <- "Aircraft Age (years since manufacture)"
    plot_title <- "Aircraft Age vs Carrier Delays"
  } else if (age_var == "distance_bin") {
    x_label <- "Total Distance Flown"
    plot_title <- "Aircraft Total Distance vs Carrier Delays"
  } else if (age_var == "airtime_bin") {
    x_label <- "Aircraft Age (by time spent flying)"
    plot_title <- "Aircraft Airtime Age vs Carrier Delays"
  } else if (age_var == "flight_bin") {
    x_label <- "Aircraft Age (by total flights)"
    plot_title <- "Aircraft Flight Cycle Age vs Carrier Delays"
  }
  
  ggplot(data, aes(x = .data[[age_var]], y = proportion_delayed * 100, group = 1)) +
    geom_point(color = "thistle3", size = 3) +
    geom_line(color = "darkblue") +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = plot_title,
      x = x_label,
      y = "Percentage Delayed"
    )
}

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    aircrafts_ages_since_2003 |>
      filter(YEAR == input$year)
  })
  
  delay_by_age <- reactive({
    filtered_data() |>
      group_by(.data[[input$age_var]]) |>
      summarize(
        total_aircraft = n(),
        num_delayed = sum(TOT_CARRIER_DELAY > 30, na.rm = TRUE),
        proportion_delayed = num_delayed / total_aircraft
      ) |>
      filter(!is.na(.data[[input$age_var]]))
  })
  
  output$Plot <- renderPlot({
    generate_plot(delay_by_age(), input$age_var)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
