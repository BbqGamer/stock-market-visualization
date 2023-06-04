library(tidyr)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)

server <- function(input, output) {

  # all separate csv for each company
  csv_directory <- "data/stocks"
  csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)
  data_companies <- list()
  for (csv_file in csv_files) {
    key <- tools::file_path_sans_ext(basename(csv_file))
    data_companies[[key]] <- read.csv(csv_file)
  }
}
