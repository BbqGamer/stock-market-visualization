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

# funkcja do tego, żeby benchmarki mialy te same wiersze co akcje

filter_common_dates <- function(df1, df2, df3) {
  # Get the unique dates from each data frame
  dates_df1 <- unique(df1$Date)
  dates_df2 <- unique(df2$Date)
  dates_df3 <- unique(df3$Date)
  
  # Find the common dates between df1 and df2
  common_dates <- intersect(dates_df1, dates_df2)
  common_dates <- intersect(common_dates, dates_df3)
  
  # Filter df3 to keep only the rows with common dates
  df3_filtered <- df3[df3$Date %in% common_dates, ]
  
  # Return the filtered df3
  return(df3_filtered)
}

server <- function(input, output, session) {

  # all separate csv for each company
  csv_directory <- "data/stocks"
  csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)
  data_companies <- list()
  for (csv_file in csv_files) {
    key <- tools::file_path_sans_ext(basename(csv_file))
    data_companies[[key]] <- read.csv(csv_file)
    data_companies[[key]]$Date = as.Date(data_companies[[key]]$Date)
    
    for (i in 1:length(data_companies[[key]][, 1])) {
      if (data_companies[[key]]$Close[i] >= data_companies[[key]]$Open[i]) {
        data_companies[[key]]$direction[i] = "Increasing"
      } else {
        data_companies[[key]]$direction[i] = "Decreasing"
      }
    }
  }
  
  # widget z wyborem firmy 2page
  companies <- data.frame(read.csv("data/companies.csv"))
  companies$market_capitalization <- companies$market_cap
  companies$market_cap <- sapply(companies$market_cap, transformFinancialData)
  companies$market_cap <- as.numeric(companies$market_cap)
  
  nasdaq100 <- data.frame(read.csv("data/ndx_d.csv"))
  spx500    <- data.frame(read.csv("data/spx_d.csv")) 
  nasdaq100$Date <- as.Date(nasdaq100$Date)
  spx500$Date <- as.Date(spx500$Date)

  
  updateSelectInput(session, "ticker", choices = companies$stock)

  # Events 
  
  # -----------------------------candleStick---------------------------------------
  
  # table
  output$ChoosingCompanytable <- DT::renderDataTable({
    companies
    
    DT::datatable(
      companies[, c("index","company_name")],
      style = "bootstrap",
      # filter = "top",
      # callback = JS("limitSelectionTable"),
      rownames = FALSE,
      selection = list(mode = 'single', selected = c(1), target = 'row'), # selectable = c(-2, -3)),
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        buttons = list(),
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,  # Set the number of rows per 
        lengthMenu = list(c(5, 10, 15), c('5', '10', '15'))
      )
    )
  })
  
  observeEvent(input$ChoosingCompanytable_rows_selected, {
    selected_indices <- input$ChoosingCompanytable_rows_selected
    selected_row <- companies[selected_indices, ]
    # print(selected_row)
    if (selected_row$stock == "BRK.B") {
      selected_row$stock <- gsub("\\.", "-", selected_row$stock)
    }
    selected_company <- data_companies[[tolower(selected_row$stock)]] # to juz jest .csv ze Stocks
    
    output$infoTable <- DT::renderDataTable({
      selected_row
      
      DT::datatable(
        selected_row,
        style = "bootstrap",
        rownames = TRUE,
        selection = list(mode = 'none')
      )
      })
    
    output$candlePlot <- renderPlotly({
      i <- list(line = list(color = '#17BECF'))
      d <- list(line = list(color = '#7F7F7F'))
      
      
      # print(selected_company)
      # Plot candlestick chart
      fig <- selected_company %>%
        plot_ly(x = ~Date, type = "candlestick",
                open = ~Open, close = ~Close,
                high = ~High, low = ~Low, name = selected_company$company_name,
                increasing = i, decreasing = d) 
      fig <- fig %>% layout(yaxis = list(title = "Price"))
      #fig <- fig %>% add_lines(x = ~Date, y = ~Open, line = list(color = 'black', width = 0.75), inherit = F)
      # Create rangeselector buttons
      rs <- list(visible = TRUE, x = 0.5, y = -0.1,
                 xanchor = "center", yref = "paper",
                 font = list(size = 9),
                 buttons = list(
                   list(count = 1,
                        label = "RESET",
                        step = "all"),
                   list(count = 1,
                        label = "1 YR",
                        step = "year",
                        stepmode = "backward"),
                   list(count = 3,
                        label = "3 MO",
                        step = "month",
                        stepmode = "backward"),
                   list(count = 1,
                        label = "1 MO",
                        step = "month",
                        stepmode = "backward")
                 ))
      
      min_candle <- min(selected_company$Low) * 0.9  # Adjust the multiplier as desired
      max_candle <- max(selected_company$High) * 1.1  # Adjust the multiplier as desired
      
      fig <- fig %>% layout(title = paste(selected_company$company_name), # , Sys.Date()),
                            xaxis = list(rangeselector = rs, rangeslider = list(visible = F), 
                                         title = list(
                                           # text = "Date",
                                           standoff = 4,  # Adjust the standoff value to move the caption down
                                           font = list(size = 12)
                                         )
                            ), # list(showticklabels = FALSE), 
                            yaxis = list(
                              range = c(min_candle, max_candle),
                              title = "Price"
                            ),
                            legend = list(orientation = "h", x = 0.5, y = 1,
                                          xanchor = "center", yref = "paper",
                                          font = list(size = 10),
                                          bgcolor = "transparent"))
      fig
    })
    
    
    
    output$description <- renderPrint({
      cat(paste("Company name:", selected_row$company_name, "\n",
                "Market capitalization: ", selected_row$market_capitalization, "\n",
                "Stock ticker: ", selected_row$stock, "\n",
                "Sector: ", selected_row$sector, "\n",
                "Industry: ", selected_row$industry))
    })
    
  })
  
  # -----------------------------candleStick---------------------------------------
  
  # Multiselecting table
  
  output$CompTable <- DT::renderDataTable({
    companies
    
    DT::datatable(
      companies[, c("index", "company_name", "market_capitalization", "stock", "sector", "industry")],
      style = "bootstrap",
      filter = "top",
      # callback = JS("limitSelectionTable"),
      rownames = FALSE,
      selection = list(mode = 'multiple', selected = c(1,2,3,4), target = 'row'), # selectable = c(-2, -3)),
      extensions = list("Buttons","Select"),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5  # Set the number of rows per 
      )
    )
  })
  
  # Bar Plot - with selecting companies
  
  observeEvent(input$CompTable_rows_selected, {
    selected_indices <- input$CompTable_rows_selected
    selected_rows <- companies[selected_indices, ]
    
    output$selectedRows <- renderText({
      paste("Selected Stocks: ", paste(selected_rows$stock, collapse = ", "))
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
  
  # -----------------------benchmark scatter plot -----------------------
  
  company_data <- list(
    'Stock' = c(),
    'S&P500' = spx500$Open,
    'NASDAQ100' = nasdaq100$Open
  )
  
  event_trigger <- reactive({
    list(input$ChoosingCompanytable_rows_selected, input$price_type)
  })

  observeEvent(ignoreInit = TRUE, event_trigger(),{
    selected_indices <- input$ChoosingCompanytable_rows_selected
    selected_row <- companies[selected_indices, ]
    
    if (selected_row$stock == "BRK.B") {
      selected_row$stock <- gsub("\\.", "-", selected_row$stock)
    }
    
    selected_company <- data_companies[[tolower(selected_row$stock)]] # to juz jest .csv ze Stocks
    
    # Filter the data frames to keep only the rows with common dates
    spx500 <- filter_common_dates(nasdaq100, selected_company, spx500)
    nasdaq100 <- filter_common_dates(spx500, selected_company, nasdaq100)
    filtered_date_company <- filter_common_dates(spx500, nasdaq100, selected_company)

  
    company_data$Stock = filtered_date_company[[input$price_type]]
    company_data$`S&P500`= spx500[[input$price_type]]
    company_data$NASDAQ100 = nasdaq100[[input$price_type]]

    
    output$stock_plot <- renderPlotly({
      plot_data <- plot_ly()
      
      plot_data <- add_trace(plot_data,
                             x = filtered_date_company$Date, # seq_along(company_data[[company]]), # spx500$Date,# 
                             y = company_data[['Stock']],
                             type = 'scatter',
                             mode = 'lines',
                             yaxis = "y1",
                             name = selected_row$company_name)
      plot_data <- add_trace(plot_data,
                             x = filtered_date_company$Date, # seq_along(company_data[[company]]), # spx500$Date,# 
                             y = company_data[['S&P500']],
                             type = 'scatter',
                             mode = 'lines',
                             yaxis = "y2",
                             name = "S&P500")
      plot_data <- add_trace(plot_data,
                             x = filtered_date_company$Date, # seq_along(company_data[[company]]), # spx500$Date,# 
                             y = company_data[['NASDAQ100']],
                             type = 'scatter',
                             mode = 'lines',
                             yaxis = "y2",
                             name = "NASDAQ100")
      
      plot_data <- layout(plot_data,
                          yaxis = list(title = "Stock Price"),
                          yaxis2 = list(title = "Benchmark Price", overlaying = "y", side = "right"))
      
      plot_data
    })
    
  })
  
  observeEvent(input$company_name, {
    selected_company <- input$company_name
    filtered_date_company <- filter_common_dates(spx500, nasdaq100, data_companies$aapl)
    company_data$Stock = filtered_date_company$Open
    
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
  })
  
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
  # -----------------------benchmark scatter plot -----------------------
  
  # Comparing sectors/industries
  
  output$stacked_bar_plot <- renderPlotly({
    plot_ly(data(), x = ~companies[[input$group_by]], y = ~companies$market_cap, color = ~companies$company_name, type = "bar", #  marker = list(color = ~companies[[input$group_by]], colorscale = "Viridis", autocolorscale = FALSE))
      hovertemplate = ~companies$company_name) %>%
      layout(
        title = "",
        xaxis = list(title = input$group_by),
        yaxis = list(title = "Market cap sum"),
        barmode = "stack"
      )
  })
  
  data <- reactive({
    if (input$group_by == "sector") {
      df <- companies %>%
        group_by(sector) %>%
        summarize(n = n()) %>%
        mutate(prop = n / sum(n))
      
      companies <- companies %>% arrange(sector)
    } else {
      df <- companies %>% 
        group_by(industry) %>%
        summarize(n = n()) %>%
        mutate(prop = n / sum(n)) 
      
      companies <- companies %>% arrange(industry)
    }
    df
  })
  
}
