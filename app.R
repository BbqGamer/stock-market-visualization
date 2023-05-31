## app.R ##
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(png)
library(dplyr)

# list of companies
companies <- data.frame(read.csv("data/companies.csv"))

# all separate csv for each company
csv_directory <- "D:/Studia/AI/AI4sem/DataViz/stockViz/StocksViz/data/stocks"
csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)
dataCompanies <- list()
for (csv_file in csv_files) {
  key <- tools::file_path_sans_ext(basename(csv_file))
  dataCompanies[[key]] <- read.csv(csv_file)
}


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  # --------------------------------------- 
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Sector data", tabName = "sectors", icon = icon("th")),
      menuItem("Help", tabName = "help", icon = icon("th"))
    )
  ),
  
  # ---------------------------------------
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
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
                         choices = companies$stock
                       )
                )
              ),
              fluidRow(
                box(plotOutput("plot2", height = 250))
              ),
              fluidRow(
                # box(plotlyOutput("myPlot"))
              )
      ),
      
      # third tab content
      tabItem(tabName = "sectors",
              fluidRow(
                plotlyOutput("sectorPlot")
              )
      ),
      
      # fourth tab content
      tabItem(tabName = "help",
              plotlyOutput("myPlot"),
              fluidRow(
                imageOutput("imagePUT")
              ),
      )
    ),
    
  )
)

# -----------------------------Server=-----------------------------------------

server <- function(input, output) {
  # randomowe dane dla histogramu
  set.seed(122) 
  histdata <- rnorm(500)
  
  companies <- data.frame(read.csv("data/companies.csv"))
  market_caps <- substr(companies$market_cap,2,nchar(companies$market_cap) -2)
  market_caps <- as.numeric(market_caps) # uwaga są źle przefiltrowane
  
  
  #---------------------PLOTS--------------------------------------
  
  output$plot1 <- renderPlot({
    data <- market_caps[seq_len(input$slider)]
    
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    
    company <- input$ticker
    data <- dataCompanies[[tolower(gsub("\\.", "-", company))]]$Open
    hist(data)
    
  })
  
  output$myPlot <- renderPlotly({
    p <- ggplot(
      diamonds[sample(nrow(diamonds), 2000), ], 
      aes(x=carat, y=price, color=cut)
    ) + 
      geom_point(alpha=0.5) + 
      # scale_color_jco() + # scale_color_npg()
      theme_bw() +
      theme(legend.title = element_blank())
    
      ggplotly(p)
  })
  
  output$sectorPlot <- renderPlotly({
    
    transformMarketCap <- function(value) {
      # Remove non-numeric characters and convert to numeric format
      letter <- substr(value,nchar(value),nchar(value))
      value <- substr(value,2,nchar(value)-3)
      value <- as.numeric(value)
      
      # Check if the original value had "B" (billion) or "T" (trillion)
      if (grepl("B", letter)) {
        value <- value * 1e9
      } else if (grepl("T", letter)) {
        value <- value * 1e12
      }
      
      return(value)
    }
    
    comp_by_sector <- companies %>% select(sector, market_cap)
    comp_by_sector$market_cap <- sapply(comp_by_sector$market_cap, transformMarketCap)
    comp_by_sector <- comp_by_sector %>% group_by(sector) %>%
      summarize(total_market_cap = sum(comp_by_sector$market_cap)) %>%
      arrange(desc(total_market_cap))
    
    p <- plot_ly(
      data = comp_by_sector,
      labels = ~sector,
      values = ~total_market_cap,
      type = "pie"
    )
    
    ggplotly(p)
  })
  
  #--------------------------IMAGES---------------------------
  output$imagePUT <- renderImage({
    img <- readPNG("data/PP_logotyp_ANG_RGB.png")
    
    # Get the dimensions of the image in pixels
    w <- dim(img)[2]
    h <- dim(img)[1]
    
    filename <- normalizePath(file.path('./data',
                                        paste('PP_logotyp_ANG_RGB', input$n, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = w*0.2,
         height = h*0.2,
         alt = paste("Image number", input$n))
    
          
  }, deleteFile = FALSE)
}

shinyApp(ui, server)


