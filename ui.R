
library(shiny)
library(XLConnect)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  textInput("url", label = NULL, value = "https://www.oddschecker.com/politics/us-politics/donald-trump-specials/trump-exit-date", placeholder = "URL to OddsChecker Page"),
  checkboxInput("scale", "Scale down so sum of probs = 1.0"),
  actionButton("run", "Run"),
  conditionalPanel(
    condition = "output.urlProvided",
    dateRangeInput("oddsDates", "Date range:",
                   start = as.Date("2017-01-01","%Y-%m-%d"),
                   end = as.Date("2017-01-02","%Y-%m-%d")),
    plotOutput("oddsLine"),
    downloadButton('downloadData', 'Download Odds Data')
  )
))
