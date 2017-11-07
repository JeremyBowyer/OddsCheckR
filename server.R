
library(rvest)
library(curl)
library(XML)
library(zoo)
library(reshape2)
library(shiny)
library(XLConnect)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
    ###################
    # Reactive Values #
    ###################
    vals <- reactiveValues(originalDF = data.frame(),
                           oddsDF = data.frame())  

    ###########################
    # Option Panel Conditions #
    ###########################
  
    output$urlProvided <- reactive({
      return(nrow(vals$originalDF) > 0)
    })
    outputOptions(output, 'urlProvided', suspendWhenHidden=FALSE)
  
 
    observeEvent(input$run, {
    
    withProgress(message = 'Loading market data...', value = 0, {
      
      url <- input$url
      
      # create list of scenarios
      mainhtml <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
      scenarios <- html_text(html_nodes(mainhtml,".nm"))
      
      # progress bar information
      n <- length(scenarios) * 2

      # Loop through scenarios, save HTML text and build list of all dates
      scenariosHtml <- list()
      scenariosDates <- character()
      for(scenario in scenarios){
        incProgress(1/n, message = "Gathering data...")

        # Read in scenario data
        scenariourl <- paste0(url,"/bet-history/",gsub(" ","-",scenario),"/all-history")
        scenariotext <- readLines(scenariourl)
        scenariotext <- paste(scenariotext, collapse = " ")
        scenariosHtml[scenario] <- scenariotext
        
        scenariodoc <- read_html(scenariotext)
        
        # Scenario dates
        datedoc <- html_nodes(scenariodoc, xpath = '//*[@id="all-history"]/table/tbody/tr/td[1]')
        scenarioDates <- html_text(datedoc)
        scenarioDates <- scenarioDates[scenarioDates != "Date"]
        # Check for valid data
        if (length(scenarioDates) == 0) next
        # Append dates to vector for all dates
        scenariosDates <- c(scenariosDates, scenarioDates)
      }
  
      # construct main dataframe that will hold all probabilities
      # Extract start and end date for market
      scenariosDates <- as.Date(scenariosDates)
      startDate <- min(scenariosDates, na.rm = TRUE)
      endDate <- max(scenariosDates, na.rm = TRUE)
      # generate date list
      maindates <- seq.Date(from = startDate, to = endDate, by = "day")
      
      # Create main DF
      maindf <- setNames(as.data.frame(matrix(nrow=length(maindates), ncol=length(scenarios)), row.names=as.character(maindates)), scenarios)
      rownames(maindf) <- rownames(maindf)[order(row.names(maindf), decreasing=TRUE)]
      
      for (scenario in scenarios){
        incProgress(1/n, message = paste0("Calculating odds for ", scenario, "..."))

        # Read in scenario data
        scenariotext <- scenariosHtml[[scenario]]
        scenariodoc <- read_html(scenariotext)
        
        # Scenario dates
        datedoc <- html_nodes(scenariodoc, xpath = '//*[@id="all-history"]/table/tbody/tr/td[1]')
        scenariodates <- html_text(datedoc)
        scenariodates <- scenariodates[scenariodates != "Date"]
        # Check for valid data
        if (length(scenariodates) == 0) next
        
        # create list of sites
        sites <- character()
        sitesrow <- html_nodes(scenariodoc, xpath = '//*[@id="all-history"]/table/thead/tr[@class="eventTableHeader"]/td[position()>1]/span')
        for(row in sitesrow){
          site <- html_attr(row, name = "data-bk")
          sites[length(sites)+1] <- site
        }
        
        convertOdds = function(node, encoding) {
          
          if (!is.null(xmlChildren(node)$div)) { 
            
            rawodd <- xmlValue(xmlChildren(node)$div)
            
            if(length(rawodd) == 0) {
              return(NA) 
            } else if (length(grep("/",rawodd)) != 0) {
              # split character fractions by "/" then divide numerator by denominator
              odd <- as.numeric(unlist(strsplit(rawodd, split = "/"))[1]) / as.numeric(unlist(strsplit(rawodd, split = "/"))[2])
              odd <- 1/(odd+1)
            } else if (rawodd == "SUSP") {
              odd <- "SUSP"
            } else {
              odd <- as.numeric(rawodd)
              odd <- 1/(odd+1)
            }
            
            return(odd)
            
          } else {
            return(NA)
          }
          
        }
        
        scenariodf <- readHTMLTable(scenariotext, which = 2, elFun = convertOdds, stringsAsFactors=FALSE)
        scenariodf <- scenariodf[1:length(scenariodates), -1]
        names(scenariodf) <- sites
        rownames(scenariodf) <- scenariodates
        
        ## create full daily scenario df
        dailydf <- setNames(as.data.frame(matrix(nrow=length(maindates), ncol=length(sites)), row.names=as.character(maindates)), sites)
        dailydf <- dailydf[order(rownames(dailydf), decreasing = TRUE),]
        
        ## merge candidate df and daily candidate df
        dailydf[match(rownames(scenariodf), rownames(dailydf)),] <- scenariodf
        
        ## fill in leading NAs with prior data
        dailydf <- apply(dailydf, 2, function(x) na.locf(x, na.rm=FALSE, fromLast=TRUE))
        
        ## convert daily candidate df to numeric
        suppressWarnings(class(dailydf) <- "numeric")
        
        ## calculate row means and insert into main df
        maindf[,scenario] <- replace(rowMeans(dailydf, na.rm = TRUE),is.nan(rowMeans(dailydf, na.rm = TRUE)),NA)
      }

      if (length(scenarios) > 1 && input$scale) {
        
        maindf[] <- t(apply(maindf, 1, function(x) x / sum(x, na.rm = TRUE)))
        
      }

      cols <- names(maindf)
      maindf$Date <- row.names(maindf)
      maindf <- maindf[, c("Date", cols)]
      
      longdf <- melt(maindf, id.vars = "Date", value.name = "Probability", variable.name = "Scenario")
      
      updateDateRangeInput(session, "oddsDates",
                           label = "Date range:",
                           start = startDate,
                           end = endDate,
                           min = startDate,
                           max = endDate
      )
      
      names(maindf) <- gsub(" ", "", names(maindf))
      
      vals$originalDF <- maindf
      vals$oddsDF <- maindf
      vals$longDF <- longdf
      
    })
    
    observeEvent(input$oddsDates, {
      df <- vals$longDF
      df$Date <- as.Date(df$Date)
      df <- df[df$Date <= input$oddsDates[2] & df$Date >= input$oddsDates[1], ]
      df$Date <- as.character(df$Date)
      
      vals$oddsDF <- df
      
    })
    
    output$oddsLine <- renderPlotly({
      
      df <- vals$originalDF
      df$Date <- as.Date(df$Date)
      
      scenarioList <- names(df)[-1]
      
      yform <- as.formula(paste0("~",scenarioList[1]))
      p <- plot_ly(data = df, x = ~Date, y = yform, name = scenarioList[1], type = "scatter", mode = "lines")

      for (scenario in scenarioList[-1]) {
        yform <- as.formula(paste0("~", scenario))
        p <- add_trace(p, data = df, x = ~Date, y = yform, name = scenario)
      }
      
      p
      
    })
    
    output$downloadData <- downloadHandler(
      filename = function(){"odds_data.xlsx"},
      content = function(file) {
        fname <- paste(file,"xlsx",sep=".")
        wb <- loadWorkbook(fname,create = TRUE)
        createSheet(wb,"data")
        writeWorksheet(wb, data = vals$originalDF, sheet = "data")
        saveWorkbook(wb)
        file.rename(fname,file)
      },
      contentType="application/xlsx" 
    )
    
  })
  
})
