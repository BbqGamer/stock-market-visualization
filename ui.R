library(shiny)
library(shinydashboard)
library(plotly)
library(flexdashboard)

ui <- dashboardPage(
  skin = "black",
  # Create dashboard header with image
  dashboardHeader(title = "Stock market Analysis",
    tags$li(a(href = "https://put.poznan.pl",
              img(src = "logo.png",
                  title = "PUT", height = "30px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown")),

  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Stock Market",
        tabName = "general_dashboard", icon = icon("money-bill-trend-up")),

      menuItem("Company Analysis",
        tabName = "company_dashboard", icon = icon("building-user")),

      menuItem("Stock Comparison",
        tabName = "stock_comparison", icon = icon("code-compare")),

      menuItem("Help",
        tabName = "help", icon = icon("circle-info"))
    )
  ),

  # ---------------------------------------
  ## Body content
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
      # First tab content
      tabItem(
        tabName = "general_dashboard",
        fluidRow(
          flexdashboard::valueBoxOutput("num_companies"),
          flexdashboard::valueBoxOutput("total_market_cap"),
          flexdashboard::valueBoxOutput("number_of_sectors"),
        ),
        fluidRow(
          column(
            width = 10,
            plotlyOutput("sector_plot")
          ),
          column(
            width = 2,
            align = "center",
            div(style = "display: flex; 
                         flex-direction: column;",
                list(
                  h4("Capitalization of companies in the selected sector"),
                  flexdashboard::gaugeOutput("gauge", height = 200),
                  h3("Selected sector:"),
                  textOutput("sector_name")
                )
            )
          )
        ),
        fluidRow(
          DT::dataTableOutput("sector_table")
        ),
      ),
      # Second tab content
      tabItem(
        tabName = "company_dashboard",
        h2("Widgets tab content"),
        fluidRow(
          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        ),
      ),

      # third tab content
      tabItem(
        tabName = "stock_comparison"
      ),

      # fourth tab content
      tabItem(
        tabName = "help",
        fluidPage(
          h1("Help"),
          h2("How to use this app"),
          p("This app is divided into three section:"),
          h3("General Stock Market"),
          p("This tab contains general information about the stock market."),
          p("The first row contains three value boxes with information about the number of companies in the dataset, total market capitalization of companies in the dataset and number of sectors in the dataset."),
          p("The second row contains a plot with the number of companies in each sector. The plot is interactive, so you can click on a bar to see the companies in the selected sector in the table on the right."),
          p("The third row contains a table with companies in the selected sector."),
          h3("Company Analysis"),
          p("This tab contains information about companies."),
          h3("Stock Comparison"),
          p("This tab contains information about stock comparison."),
          h3("Data sources"),
          a(href = "https://www.kaggle.com/datasets/borismarjanovic/price-volume-data-for-all-us-stocks-etfs",
            "Kaggle Huge Stock Market Dataset"),
          br(),
          a(href = "https://disfold.com/united-states/companies/",
            "Information about sectors and market cap scraped from Disfold"),
        ),
        img(src = "dataset-cover.jpg"),
      )
    ),
  )
)
