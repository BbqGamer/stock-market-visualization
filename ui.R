library(shiny)
library(shinydashboard)

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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # First tab content
      tabItem(
        tabName = "general_dashboard",
        fluidRow(
          valueBoxOutput("num_companies"),
          valueBoxOutput("total_market_cap"),
          valueBoxOutput("number_of_sectors"),
        ),
        fluidRow(
          plotlyOutput("sector_plot"),
          DT::dataTableOutput("sector_table"),
        )
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
