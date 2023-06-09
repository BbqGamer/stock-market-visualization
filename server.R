library(tidyr)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

server <- function(input, output) {

  # all separate csv for each company
  csv_directory <- "data/stocks"
  csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)
  data_companies <- list()
  for (csv_file in csv_files) {
    key <- tools::file_path_sans_ext(basename(csv_file))
    data_companies[[key]] <- read.csv(csv_file)
  }

  companies <- data.frame(read.csv("data/companies.csv"))
  sector_counts <- table(companies$sector)

  output$sector_plot <- renderPlotly({
    plot_ly(
      x = names(sector_counts),
      y = sector_counts,
      type = "bar"
    ) %>%
      layout(
        title = "Number of companies in each sector (click on bar to see companies)",
        xaxis = list(title = "Sector", categoryorder = "total descending"),
        yaxis = list(title = "Count"),
        bargap = 0.1
      )
  })

  output$sector_table <- DT::renderDataTable({
    # Select only the rows with the selected sector
    selected_sector <- toString(event_data("plotly_click")["x"])
    selected_companies <- companies[companies$sector == selected_sector, ]

    selected_companies[, c("company_name",
                  "market_cap",
                  "stock",
                  "sector",
                  "industry")]
  })

  output$num_companies <- renderValueBox({
    n_companies <- companies %>% nrow()

    valueBox(
      value = n_companies,
      subtitle = "Number of companies in the dataset",
      icon = icon("users"),
      color = "green"
    )
  })

  output$total_market_cap <- renderValueBox({
    # Remove sollar sign from the market cap column
    market_cap <- sapply(companies$market_cap, function(x) {
      # Remove dollar sign and extract numeric value
      value <- as.numeric(gsub("[^0-9.]", "", x))
      if (grepl("M", x)) {
        value <- value * 1e6
      }
      if (grepl("B", x)) {
        value <- value * 1e9
      }
      if (grepl("T", x)) {
        value <- value * 1e12
      }
      return(value)
    })

    # Round total to two decimal places
    total <- round(sum(market_cap) / 1e12, 2)

    valueBox(
      value = paste0(total, "T"),
      subtitle = "Total market capitalization of companies in dataset",
      icon = icon("money-bill-wave"),
      color = "blue"
    )
  })

  output$number_of_sectors <- renderValueBox({
    n_sectors <- companies %>% distinct(sector) %>% nrow()

    valueBox(
      value = n_sectors,
      subtitle = "Number of sectors in the dataset",
      icon = icon("building"),
      color = "yellow"
    )
  })
}