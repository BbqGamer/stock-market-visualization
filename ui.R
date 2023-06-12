library(shiny)
library(shinydashboard)
library(plotly)
library(flexdashboard)

ui <- dashboardPage(
  skin = "black",
  # Create dashboard header with image
  dashboardHeader(
    title = "Stock market Analysis",
    tags$li(
      a(
        href = "https://put.poznan.pl",
        img(
          src = "logo.png",
          title = "PUT", height = "30px"
        ),
        style = "padding-top:10px; padding-bottom:10px;"
      ),
      class = "dropdown"
    )
  ),

  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Stock Market",
        tabName = "general_dashboard", icon = icon("money-bill-trend-up")
      ),
      menuItem("Company Analysis",
        tabName = "company_dashboard", icon = icon("building-user")
      ),
      menuItem("Stock Comparison",
        tabName = "stock_comparison", icon = icon("code-compare")
      ),
      menuItem("Help",
        tabName = "help", icon = icon("circle-info")
      )
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
            div(
              style = "display: flex;
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
              choices  = list("Open", "High", "Low", "Close"),
              selected = "Open"
            )
          )
        )
      ),
      # third tab content
      tabItem(
        tabName = "stock_comparison",
        fluidRow(
          offset = 1,
          h2("Compare market capitalization"),
        ),
        fluidRow(
          width = 12,
          box(
            width = 12,
            DT::dataTableOutput("CompTable")
          ),
        ),
        fluidRow(
          width = 12,
          box(
            width = 12,
            verbatimTextOutput("selectedRows"),
            plotlyOutput("compBarChart")
          )
        ),
        fluidRow(
          offset = 1,
          h2("Compare industries/sectors")
        ),
        fluidRow(
          column(
            width = 2,
            selectInput("group_by", "Group By", choices = c("sector", "industry"), selected = "sector")
          ),
          column(
            width = 10,
            box(
              width = 12,
              plotlyOutput("stacked_bar_plot")
            )
          )
        )
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
          p("First plot shows you CandleStick plot of selected company from the table. You can set period of time that you wan to see by clicking at the buttons below. Except that you can snip and zoom in interesting part of the plot.(and see the candles:)).(However after snipping some part of the plot, I advise you to click autoscale button that would show you when you hover mouse over plot next to the small house)"),
          p("By hovering mouse over candles, you can see the information about prices in the given day and if the stock decreased/increased compared to the previous day."),
          p("Second plot shows you how our stock behaves comparing him with popular market benchamarks."),
          p("Plot shows you company that you have chosen in the table. It's linear plot of prices (open/close/low/high -> which you can choose on the right) of stocks or benchmark. (Keep in mind that y-axis for benchmarks is on the right and for stock is on the left)"),
          p("If you would like to compare stock price with only one benchmark, you can make one benchmark disappear by clicking at its name in the legend."),
          h3("Stock Comparison"),
          p("This tab contains information about stock comparison."),
          p("First row contains table from which you can compare market caps of selected companies. There you can select/unselect them by clicking at them"),
          p("For clarity, you can also see a list of compared companies below."),
          p("Below at the last plot you can compare sectors and industries by sum of the market caps of chosen companies by us and you can easily see how big part of them have some specific companies."),
          p("You can also make some companies disappear what would decrease the stack. e.g. Technology sector without apple and microsoft is smaller by market cap by most of the sectors"),
          p("Except of that you can hover stacks with mouse to see which companies created it."),
          p("Of course, you can change the plot to compare industries/sectors by choosing one of them from dropdown list on the left."),
          h3("Data sources"),
          a(
            href = "https://www.kaggle.com/datasets/borismarjanovic/price-volume-data-for-all-us-stocks-etfs",
            "Kaggle Huge Stock Market Dataset"
          ),
          br(),
          a(
            href = "https://disfold.com/united-states/companies/",
            "Information about sectors and market cap scraped from Disfold"
          ),
          br(),
          a(href = "https://stooq.com/q/d/?s=^ndx", "NASDAQ100"),
          br(),
          a(href = "https://stooq.com/q/d/?s=^spx", "S&P500"),
          br(),
        ),
        img(src = "dataset-cover.jpg"),
      )
    )
  )
)
