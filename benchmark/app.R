library(shiny)
library(plotly)
# D:/Studia/AI/AI4sem/DataViz/stockViz/app.R
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

companies <- data.frame(read.csv("StocksViz/data/companies.csv")) # index,company_name,market_cap,stock,country,sector,industry,datafile
nasdaq100 <- data.frame(read.csv("StocksViz/data/ndx_d.csv")) # Date,Open,High,Low,Close,Volume
spx500    <- data.frame(read.csv("StocksViz/data/spx_d.csv")) # Date,Open,High,Low,Close,Volume

nasdaq100$Date = as.Date(nasdaq100$Date)
spx500$Date = as.Date(spx500$Date)

csv_directory <- "D:/Studia/AI/AI4sem/DataViz/stockViz/StocksViz/data/stocks"

# Get the list of CSV files in the directory
csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)
data_dict <- list()
already_plotted <- list()

# Loop through the CSV files and load them into data frames
for (csv_file in csv_files) {
  key <- tools::file_path_sans_ext(basename(csv_file))# Extract the file name without extension as the key for the data frame
  data_dict[[key]] <- read.csv(csv_file) # Date,Open,High,Low,Close,Volume,OpenInt
  # Load the CSV file into a data frame and store it in the dictionary
  data_dict[[key]]$Date = as.Date(data_dict[[key]]$Date)
}

# old code

# Get the unique dates from each data frame
spx_dates <- unique(spx500$Date)
nasdaq_dates <- unique(nasdaq100$Date)
aapl_dates <- unique(data_dict[['aapl']]$Date)

# Find the common dates in all three data frames
common_dates <- intersect(intersect(spx_dates, nasdaq_dates), aapl_dates)

# Filter the spx500 data frame to keep only the rows with common dates
spx500 <- spx500[spx500$Date %in% common_dates, ]

# Filter the nasdaq100 data frame to keep only the rows with common dates
nasdaq100 <- nasdaq100[nasdaq100$Date %in% common_dates, ]

# Filter the aapl data frame to keep only the rows with common dates
data_dict$aapl <- data_dict$aapl[data_dict$aapl$Date %in% common_dates, ]




length(data_dict$aapl$Open)
length(spx500$Open)
length(nasdaq100$Open)

# Create initial data for three sample companies
company_data <- list(
  'stock' = data_dict$aapl$Open,
  'S&P500' = spx500$Open,
  'NASDAQ100' = nasdaq100$Open
)

# colnames(company_data)[colnames(company_data) == "stock"] <- "some other name"
# company_data

# company_data <- list(
#   data_dict$aapl = list(c('10', '15', '18', '12', '14')),
#   S_P500 = list(c('12', '9', '10', '11', '13')),
#   NASDAQ100 = list(c('8', '10', '12', '11', '9'))
# )


company_data2 <- company_data

ui <- fluidPage(
  titlePanel("Stock Prices"),
  sidebarLayout(
    sidebarPanel(
      selectInput("company_name", "Select to add/delete", choices = c("", names(company_data))),
      actionButton("add_button", "Add"),
      actionButton("delete_button", "Delete"),
      hr(),
      helpText("Note: Use the dropdown to select a company to add/delete.")
    ),
    mainPanel(
      plotlyOutput("stock_plot")
    )
  )
)

server <- function(input, output, session) {
  # Initialize the plot with the initial data
  output$stock_plot <- renderPlotly({
    plot_data <- plot_ly()

    # Add traces for each company
    for (company in names(company_data)) {
      plot_data <- add_trace(plot_data,
                             x = spx500$Date,# seq_along(company_data[[company]]),
                             y = company_data[[company]],
                             type = 'scatter',
                             mode = 'lines',
                             name = company)
    }

    plot_data
  })

  # Function to update the plot
  update_plot <- function() {
    plot_data <- plot_ly()

    # Add traces for each company
    for (company in names(company_data)) {
      plot_data <- add_trace(plot_data,
                             x = spx500$Date,# seq_along(company_data[[company]]),
                             y = company_data[[company]],
                             type = 'scatter',
                             mode = 'lines',
                             name = company)
    }

    # Update the stock plot
    output$stock_plot <- renderPlotly({ plot_data })
  }

  # Add button event
  observeEvent(input$add_button, {
    company_name <- input$company_name
    # stock_data <- input$stock_data

    if (company_name != "" ) { # && !is.na(stock_data)
      company_data[[company_name]] <<- company_data2[[company_name]]
      update_plot()

      # Reset input values
      updateSelectInput(session, 'company_name', selected = "")
      # updateNumericInput(session, 'stock_data', value = NA)
    }
  })

  # Delete button event
  observeEvent(input$delete_button, {
    company_name <- input$company_name

    if (company_name != "") {
      company_data[[company_name]] <- NULL
      company_data <<- company_data  # Use <<- to update the global variable
      update_plot()

      # Reset input value
      updateSelectInput(session, 'company_name', selected = "")
    }
  })
}

shinyApp(ui, server)

# old code
# 
# # Define a helper function to generate the layout configuration
# generate_layout <- function(yaxis_title, yaxis2_title) {
#   layout_data <- list(
#     yaxis = list(title = yaxis_title),
#     yaxis2 = list(
#       title = yaxis2_title,
#       overlaying = "y",
#       side = "right"
#     )
#   )
#   
#   return(layout_data)
# }
# 
# ui <- fluidPage(
#   titlePanel("Stock Prices"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("company_name", "Select Company", choices = c("", companies$company_name)),
#       selectInput("price_type", "Price Type", choices = c("Open", "Close", "High", "Low")),
#       selectInput("benchmark", "Benchmark", choices = c("", "NASDAQ100", "SPX500")),
#       actionButton("add_button", "Add Company"),
#       actionButton("delete_button", "Delete Company"),
#       actionButton("add_button_bench", "Add Benchmark"),
#       actionButton("delete_button_bench", "Delete Benchmark"),
#       actionButton("update_price", "Update Price Type"),
#       hr(),
#       helpText("Note: Use the dropdowns to select a company, price type, and benchmark.")
#     ),
#     mainPanel(
#       plotlyOutput("stock_plot")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
# 
#   
#   # Initialize the plot with the initial data
#   output$stock_plot <- renderPlotly({
#     plot_data <- plot_ly()
#     
#     # Add traces for each company
#     for (company in names(already_plotted)) {
#       plot_data <- add_trace(plot_data,
#                              x = seq_along(already_plotted[[company]]),
#                              y = already_plotted[[company]],
#                              type = 'scatter',
#                              mode = 'lines',
#                              name = company)
#     }
#     
#     layout_data <- generate_layout("Stock Price", "Benchmark")
#     plot_data <- layout(plot_data, yaxis = layout_data$yaxis, yaxis2 = layout_data$yaxis2)
#     
#     plot_data
#   })
#   
#   # Function to update the plot
#   update_plot <- function() {
#     plot_data <- plot_ly()
#     
#     # Add traces for each company
#     for (company in names(already_plotted)) {
#       plot_data <- add_trace(plot_data,
#                              x = seq_along(already_plotted[[company]]),
#                              y = already_plotted[[company]],
#                              type = 'scatter',
#                              mode = 'lines',
#                              name = company)
#     }
#     
#     # Update the stock plot
#     output$stock_plot <- renderPlotly({ plot_data })
#   }
#   
#   # Delete button event
#   observeEvent(input$delete_button, {
#     company_name <- input$company_name
#     
#     if (company_name != "") {
#       already_plotted[[company_name]] <- NULL
#       already_plotted <<- already_plotted  # Use <<- to update the global variable
#       update_plot()
#       
#       # Reset input value
#       updateSelectInput(session, 'company_name', selected = "")
#     }
#   })
#   
#   # Add button event
#   observeEvent(input$add_button, {
#     company_name <- input$company_name
#     
#     if (company_name != "") {
#       already_plotted[[company_name]] <- data_dict[[company_name]]
#       update_plot()
#       
#       # Reset input values
#       updateSelectInput(session, 'company_name', selected = "")
#       updateNumericInput(session, 'stock_data', value = NA)
#     }
#   })
#   
#   # # Delete button event
#   # observeEvent(input$delete_button_bench, {
#   #   benchmark_name <- input$benchmark
#   #   
#   #   if (benchmark_name != "") {
#   #     already_plotted[[benchmark_name]] <- NULL
#   #     already_plotted <<- already_plotted  # Use <<- to update the global variable
#   #     update_plot()
#   #     
#   #     # Reset input value
#   #     updateSelectInput(session, 'benchmark_name', selected = "")
#   #   }
#   # })
#   # 
#   # # Add button event
#   # observeEvent(input$add_button_bench, {
#   #   benchmark_name <- input$benchmark
#   #   
#   #   if (benchmark_name != "") {
#   #     already_plotted[[benchmark_name]] <- data_dict[[benchmark_name]]
#   #     update_plot()
#   #     
#   #     # Reset input values
#   #     updateSelectInput(session, 'benchmark_name', selected = "")
#   #   }
#   # })
#   
#   # Updating price type button event
#   observeEvent(input$update_price, {
#     price_type <- input$price_type
#     update_plot()
#     
#     # Reset input values
#     updateSelectInput(session, 'company_name', selected = "")
#     
#   })
# }
# 
# shinyApp(ui, server)
# 
# 
#                              