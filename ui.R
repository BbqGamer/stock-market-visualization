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
      )
    ),
  )
)
