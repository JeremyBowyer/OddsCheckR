
library(rvest)
library(zoo)
library(reshape2)
library(shiny)
library(curl)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
    observeEvent(input$run, {
    
    withProgress(message = 'Loading market data...', value = 0, {
      
      url <- input$url
      
      # create list of scenarios
      mainhtml <- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
      scenarios <- html_text(html_nodes(mainhtml,".nm"))
      
      # progress bar information
      n <- length(scenarios) * 2

      scenariosHtml <- list()
      scenariosDates <- character()
      for(scenario in scenarios){
        incProgress(1/n, message = "Gathering dates...")

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
        
        # Create scenario data frame
        scenariodf <- setNames(as.data.frame(matrix(nrow=length(scenariodates), ncol=length(sites))), sites)
        rownames(scenariodf) <- scenariodates
        
        # Add data to DF
        for (r in 1:length(scenariodates)) {
          for (c in 1:length(sites)) {
            
            path <- paste0('//*[@id="all-history"]/table/tbody/tr[',r,']/td[',c+1,']/div[1]')
            rawodd <- html_nodes(scenariodoc, xpath = path)
            rawodd <- html_text(rawodd)
            
            scenariodf[r,c] <- 
              if(length(rawodd) == 0) {
                NA 
              } else if (length(grep("/",rawodd)) != 0) {
                # split character fractions by "/" then divide numerator by denominator
                odd <- as.numeric(unlist(strsplit(rawodd, split = "/"))[1]) / as.numeric(unlist(strsplit(rawodd, split = "/"))[2])
                odd <- 1/(odd+1)
              } else if (rawodd == "SUSP") {
                "SUSP"
              } else {
                odd <- as.numeric(rawodd)
                odd <- 1/(odd+1)
              }
          }
        }
        
        ## create full daily scenario df
        dailydf <- setNames(as.data.frame(matrix(nrow=length(maindates), ncol=length(sites)), row.names=as.character(maindates)), sites)
        dailydf <- dailydf[order(rownames(dailydf), decreasing = TRUE),]
        
        ## merge candidate df and daily candidate df
        dailydf[match(rownames(scenariodf), rownames(dailydf)),] <- scenariodf
        
        ## fill in leading NAs with prior data
        dailydf <- apply(dailydf, 2, function(x) na.locf(x, na.rm=FALSE, fromLast=TRUE))
        
        ## convert daily candidate df to numeric
        suppressWarnings(class(dailydf) <- "numeric")
        
        ## calculate row means and insert into UKConservative df
        maindf[,scenario] <- replace(rowMeans(dailydf, na.rm = TRUE),is.nan(rowMeans(dailydf, na.rm = TRUE)),NA)
      }
      
      maindf[] <- t(apply(maindf, 1, function(x) x / sum(x, na.rm = TRUE)))
      cols <- names(maindf)
      maindf$Date <- row.names(maindf)
      maindf <- maindf[, c("Date", cols)]
      
    })
    
      
    output$dataPreview <- renderTable({
      
      return(maindf)
      
    })
    
  })
  
})