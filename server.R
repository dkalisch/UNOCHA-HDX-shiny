# Define background calculations for shiny application
shinyServer(function(input, output, session) {

  # Get the URL variable which file to be analyzed 
  ## If no variable is given, create the drop down menu
  output$queryText <- renderText({
    if(session$clientData$url_search == '') { # Create the drop down menu
      query <- input$selectFile
    } else {
      query <- parseQueryString(session$clientData$url_search) # Parse the GET query string
    }
    
    # Return a string with key-value pairs
    paste(query)
  })

  ## Get the file that should be analyzed
  query.file <- renderText({
    if(session$clientData$url_search == '') {
      query <- paste0("data/", input$selectFile) # Use drop down input for selection
    } else {
      query <- paste0("data/", parseQueryString(session$clientData$url_search)) # Use GET variable for selection
    }
    
    # Return a string with key-value pairs
    paste(query)
  })

  # Load the data sheet of the xls file
  hdx.df <- reactive({
    
    # Give the user status feedback
    withProgress(session, min = 0, max = 7, { 
      setProgress(message = "Loading dataset",
                  detail = "This may take a few moments...")
        
        # Read data sheet of the xls file
        hdx.df.tmp <- read.xls(xls = query.file(), sheet = 3, stringsAsFactors = FALSE)
        setProgress(value = 1) # Give the user status feedback
        
        # Clean up data set
        ## Set all letters to lower cases
        names(hdx.df.tmp) <- tolower(names(hdx.df.tmp))
        setProgress(value = 2) # Give the user status feedback
        
        # Calculate the overall average
        all.avg <- c("ALL", "COUNTRY AVERAGE", round(colSums(as.data.frame(hdx.df.tmp[,3:ncol(hdx.df.tmp)]), na.rm = TRUE)/nrow(hdx.df.tmp),1))
        setProgress(value = 3) # Give the user status feedback
        
        ## Add the overall average to the data set
        hdx.df.tmp <- rbind(hdx.df.tmp, all.avg)
        setProgress(value = 4) # Give the user status feedback
        
        ## Transform dataset to long format
        hdx.df.tmp <- melt(hdx.df.tmp, id.vars = c("country.code", "country.name"), variable.name = "year")
        setProgress(value = 5) # Give the user status feedback
        
        hdx.df.tmp <- transform(hdx.df.tmp, value = as.numeric(value))
        setProgress(value = 6) # Give the user status feedback
        
        ## Remove x from year
        hdx.df.tmp$year <- substring(hdx.df.tmp$year, 2, 5)
        setProgress(value = 7) # Give the user status feedback
        
        ## Send back sorted list by year and contry name
        arrange(hdx.df.tmp, year, country.name)
    })
  })

  # Load description sheet of the xls file
  hdx.desc <- reactive({
    
    # Give the user status feedback
    withProgress(session, min = 0, max = 2, {
      setProgress(message = "Loading description",
                  detail = "This may take a few moments...")
      
        # Read description sheet of the xls file 
        hdx.desc.tmp <- read.xls(xls = query.file(), sheet = 2, stringsAsFactors=FALSE)
        setProgress(value = 1) # Give the user status feedback

        # Clean up data set
        colnames(hdx.desc.tmp) <- c("Attribute", "Description")
        setProgress(value = 2) # Give the user status feedback
        
        # Return description
        hdx.desc.tmp
    })
  })
  
  # Create a menue to select the country to be plottet
  output$selectVariable <- renderUI({
    hdx.vars <- as.character(unique(hdx.df()$country.name))
    selectInput("var", "Please select a variable", hdx.vars, multiple = TRUE, selected = "COUNTRY AVERAGE")
  })
  
  # Create a menu to select the file to be loaded
  output$selectFile <- renderUI({
    if(session$clientData$url_search == '') {
      selectInput("selectFile", "Choose file:", list.files(path = 'data/', pattern = '*.xlsx'))
    }
  })
  
  # Get a subset of the data set according to the selected countries
  dat <- reactive({
    dist <- subset(hdx.df(), country.name %in% input$var)
  })
  
  # Create a time series plot of the selected data
  output$plot <- renderPlot({
    # Define variables and meta indicators
    max <- max(hdx.df()$value, na.rm = TRUE) # Get the maximum value of all values of the df
    y.axis.desc <- subset(hdx.desc()$Description, hdx.desc()$Attribute == 'Units') # Get the unit definition
    title.desc <- paste(subset(hdx.desc()$Description, hdx.desc()$Attribute == 'Indicator name')) # Get the data set definition
    
    # Create plot
    p <- ggplot(dat(), aes(x = year, y = value, colour = factor(country.name), group = country.name)) + # Plot definition
      geom_point(size = 3) + # Add points
      geom_line() + # Add line
      labs(x = NULL, y = y.axis.desc, fill = NULL) + # Define axis
      labs(title = title.desc) + # Define title
      scale_y_continuous(limits=c(0,max)) + # Set limits
      
      # Design the plot
      guides(colour = guide_legend(title = 'Country', ncol = 3)) +
      theme(plot.title = element_text(size = rel(1.5), vjust=3),
            axis.title = element_text(size = rel(1.2)),
            axis.text.x = element_text(size = rel(1.2), angle = 90, hjust = 1),
            axis.text.y = element_text(size = rel(1.2)),
            legend.position = "bottom",
            legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.2))
      )
    
    # Return plot
    print(p)
  })
  
  # Create the data set summary of the selected countries
  output$summary <- renderTable({
    ddply(dat(), .(country.name), summarise,
          n = length(which(!is.na(value))), # Number of data points
          min = min(value, na.rm = TRUE), # Minimum valus
          q1 = quantile(value, na.rm = TRUE, names = FALSE)[2], # First quntiel
          median = round(median(value, na.rm = TRUE),1), # Median
          q3 = quantile(value, na.rm = TRUE, names = FALSE)[4], # Third quntiel
          max = max(value, na.rm = TRUE), # Maximum value
          difference = (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)), # Difference between min and max
          mean = round(mean(value, na.rm = TRUE),1), # Mean
          trimmed = round(mean(value, na.rm = TRUE, trim = 1),1) # Trimed mean - removes the fraction (0 to 0.5) from each end
          )
  })
  
  # Create a data table of the selected contries
  output$table <- renderDataTable({
    
    # Data set to be displayed
    dat()},
    
    # Define the functionality of the data table
    options = list(bSearchable = TRUE, # Is searchable
                   bSortClasses = TRUE, # Can set classes
                   aLengthMenu = list(c(10, 25, -1), c('10', '25', 'All')), # Define the possible length of the table
                   iDisplayLength = 25) # Set standard lenght
  )
  
  # Create the description output
  ## Render as table for a better layout
  output$desc <- renderTable({
    x <- hdx.desc() # Get the description information
    x[5,2] <- as.character(tags$a(id = 'myId', href=x[5,2], target='_blank', x[5,2])) # Set the variables as characters
    
    # Return information
    x},
    include.rownames = FALSE, sanitize.text.function = function(x)
    
    # Return description information
    x
  )
  
  # Create download handler
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste('HDX_Data', input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {

    # Excel is not supported yet
      # if(input$filetype == "xls"){
      #   write.xlsx(dat(), file, sheetName = "Data", row.names = FALSE)
      #     } else {
          sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
          # Write to a file specified by the 'file' argument
          write.table(dat(), file, sep = sep, row.names = FALSE)
    #      }
    }
  )
})
