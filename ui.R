
library(rvest)
library(zoo)
library(reshape2)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  textInput("url", label = NULL, value = "https://www.oddschecker.com/politics/us-politics/donald-trump-specials/trump-specials", placeholder = "URL to OddsChecker Page"),
  actionButton("run", "Run"),
  tableOutput("dataPreview")
))
