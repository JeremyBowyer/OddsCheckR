
library(shiny)
library(ggvis)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  textInput("url", label = NULL, value = "https://www.oddschecker.com/politics/us-politics/donald-trump-specials/trump-specials", placeholder = "URL to OddsChecker Page"),
  actionButton("run", "Run"),
  conditionalPanel(
    condition = "output.urlProvided",
    dateRangeInput("oddsDates", "Date range:",
                   start = as.Date("2017-01-01","%Y-%m-%d"),
                   end = as.Date("2017-08-01","%Y-%m-%d")),
    tableOutput("dataPreview")
  )
))
