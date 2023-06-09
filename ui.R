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
          box(plotOutput("plot1", height = 250)),
          box(
            title = "Controls",
            sliderInput(
              inputId = "slider",
              label = "Number of observations:",
              min = 1,
              max = 5,
              value = 5
            )
          ),
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
        fluidRow(
          column(width = 1),
          column(width = 3,
                 selectInput(
                   "ticker",
                   "Select an option:",
                   choices = NULL
                 )
          )
        ),
        fluidRow(
          # Time range
          radioButtons(
            inputId = "period", 
            label   = h4("Period"),
            choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12      months" = 4, "YTD" = 5), 
            selected = 4
          ),
          # Benchmark
          radioButtons(
            inputId  = "benchmark", 
            label    = h4("Benchmark"),
            choices  = list("SP500" = 1, "Nasdaq100" = 2,"None" = 3),
            selected = 3)
        ),
        fluidRow(
          DT::dataTableOutput("ChoosingCompanytable")
        )

      ),

      # third tab content
      tabItem(
        tabName = "stock_comparison",
        fluidRow(
          DT::dataTableOutput("CompTable")
        ),
        verbatimTextOutput("selectedRows"),
        fluidRow(
          plotlyOutput("compBarChart")
        )
      ),

      # fourth tab content
      tabItem(
        tabName = "help",
      )
    ),
  )
)
