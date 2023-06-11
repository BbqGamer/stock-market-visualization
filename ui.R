library(shiny)
library(shinydashboard)
library(plotly)

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


        
        #---------------beuzyteczne---------------------
        
        # h2("Widgets tab content"),
        # fluidRow(
        #   box(
        #     title = "Controls",
        #     sliderInput("slider", "Number of observations:", 1, 100, 50)
        #   )
        # ),
        # fluidRow(
        #   column(width = 1),
        #   column(width = 3,
        #          selectInput(
        #            "ticker",
        #            "Select an option:",
        #            choices = NULL
        #          )
        #   )
        # ),
        
        #---------------beuzyteczne---------------------
        
        #---------------moze zyteczne---------------------
        
        # fluidRow(
        #   # Time range
        #   radioButtons(
        #     inputId = "period", 
        #     label   = h4("Period"),
        #     choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
        #     selected = 4
        #   ),
        #   # Benchmark
        
        #---------------moze zyteczne---------------------
      
      tabItem(
        tabName = "company_dashboard",
            fluidRow(
              column(
                width = 12,
                h2("Stock Price over time")
              )
            ),
            fluidRow(
              column(
                width = 4,
                DT::dataTableOutput("ChoosingCompanytable")
              ),
              column(
                width = 8,
                plotlyOutput("candlePlot")
              )
            ),
            fluidRow(
              column(
                width = 8,
                h2("Stock Price with benchmarks"),
                plotlyOutput("stock_plot")
              ),
              column(
                width = 4,
                h3("Stock description"),
                verbatimTextOutput("description"),
                radioButtons(
                  inputId  = "price_type",
                  label    = h4("Price type:"),
                  choices  = list("Open", "High","Low", "Close"),
                  selected = "Open")
                )
              )
      ),
      
      # fluidRow(
      #   column(
      #     width = 8,
      # 
      #   ),
      #   column(
      #     width = 4,
      #     dateRangeInput("xaxisRange", "X-axis range"),
      #     plotlyOutput("infoTable")
      #   )
      # )
 
      # Second tab content
      # tabItem(tabName = "company_dashboard",
      #   fluidPage(
      #     fluidRow( # it sums to 12
      #       column(width = 3,
      #         DT::dataTableOutput("ChoosingCompanytable")
      #       ),
      #       column(width = 9,
      #         plotlyOutput("candlePlot")     
      #       )
      #     ),
      #   # fluidRow(
      #     # column(width = 3,
      #     #    # box(
      #     #    #   # textOutput("desc")
      #     #    # )
      #     # ),
      #     # column(width = 6,
      #     #        dateRangeInput("xaxisRange", "X-axis range"),
      #     #        # plotlyOutput("infoTable") 
      #     #        ),
      #     #  column(width = 4,
      #     #         box(
      #     #           title = "Company description:",
      #     #           textOutput("desc")
      #     #           # plotlyOutput("infoTable")
      #     #         )
      #     #  )
      #   # ),
      #   sidebarLayout(
      #     sidebarPanel(
      #       plotlyOutput("infoTable")
      #     ),
      #     mainPanel(
      #       textOutput("Company description:")
      #     )
      #   )
      # )
      # ),

      # third tab content
      tabItem(
        tabName = "stock_comparison",
        fluidRow(
          DT::dataTableOutput("CompTable")
        ),
        fluidRow(
          verbatimTextOutput("selectedRows"),
          plotlyOutput("compBarChart")
        )
      ),

      # fourth tab content
      tabItem(
        tabName = "help",
        selectInput("company_name", "Select a company:", choices = c("")),
        # sidebarLayout(
        #   sidebarPanel(
        #     selectInput("company_name", "Select to add/delete", choices = c("")),
        #     actionButton("add_button", "Add"),
        #     actionButton("delete_button", "Delete"),
        #     hr(),
        #     helpText("Note: Use the dropdown to select a company to add/delete.")
        #   ),
        #   mainPanel(
        #     plotlyOutput("stock_plot")
        #   )
        # )
      )
    )
  )
)
