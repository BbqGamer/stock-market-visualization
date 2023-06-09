library(tidyr)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(DT)

# functions

transformFinancialData <- function(value) {
  # Remove non-numeric characters and convert to numeric format
  letter <- substr(value, nchar(value), nchar(value))
  value <- substr(value, 2, nchar(value) - 3)
  value <- as.numeric(value)
  
  # Check if the original value had "B" (billion) or "T" (trillion)
  if (grepl("B", letter)) {
    value <- value * 1e9
  } else if (grepl("T", letter)) {
    value <- value * 1e12
  }
  
  return(value)
}

server <- function(input, output, session) {
  
  

  # all separate csv for each company
  csv_directory <- "data/stocks"
  csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)
  data_companies <- list()
  for (csv_file in csv_files) {
    key <- tools::file_path_sans_ext(basename(csv_file))
    data_companies[[key]] <- read.csv(csv_file)
  }
  
  # widget z wyborem firmy 2page
  companies <- data.frame(read.csv("data/companies.csv"))
  companies$market_capitalization <- companies$market_cap
  companies$market_cap <- sapply(companies$market_cap, transformFinancialData)
  companies$market_cap <- as.numeric(companies$market_cap)
  
  updateSelectInput(session, "ticker", choices = companies$stock)
  
  # table #3page
  output$ChoosingCompanytable <- DT::renderDataTable({
    # dataset <- get(companies, "package:datasets")
    companies[, c("index", "company_name", "market_capitalization", "stock", "sector", "industry")]
    
    DT::datatable(
      companies[, c("index", "company_name", "market_capitalization", "stock", "sector", "industry")],
      style = "bootstrap",
      filter = "top",
      # callback = JS("limitSelectionTable"),
      rownames = FALSE,
      selection = list(mode = 'single', selected = c(1), target = 'row'), # selectable = c(-2, -3)),
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5  # Set the number of rows per 
      )
    )
  })
  
  # table #3page
  output$CompTable <- DT::renderDataTable({
    companies
    
    DT::datatable(
      companies[, c("index", "company_name", "market_capitalization", "stock", "sector", "industry")],
      style = "bootstrap",
      filter = "top",
      # callback = JS("limitSelectionTable"),
      rownames = FALSE,
      selection = list(mode = 'multiple', selected = c(1), target = 'row'), # selectable = c(-2, -3)),
      extensions = list("Buttons","Select"),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5  # Set the number of rows per 
      )
    )
  })

  # Events 
  
  observeEvent(input$CompTable_rows_selected, {
    selected_indices <- input$CompTable_rows_selected
    selected_rows <- companies[selected_indices, ]
    
    output$selectedRows <- renderText({
      paste("Selected Rows: ", paste(selected_rows$stock, collapse = ", "))
    })
    
    output$compBarChart <- renderPlotly({
      plot_ly(
        data = selected_rows,
        x = ~stock,
        y = ~market_cap,
        type = "bar"
      )
    })
  })
}
