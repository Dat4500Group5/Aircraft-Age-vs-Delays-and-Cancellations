filtered_data <- reactive({
    aircrafts_ages_since_2003 |>
      filter(YEAR == input$year, !is.na(AIR_TIME), !is.na(TOT_CARRIER_DELAY)) |>
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
      )
  })
