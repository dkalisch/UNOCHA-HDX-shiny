# Define UI for shiny application
shinyUI(fluidPage(
  progressInit(),
  
  # Application title
  titlePanel(HTML("Humanitarian Data Exchange (HDX) Explorer <small>(beta version)</small>")),
  fluidRow(
    # Side panel definition
    column(3,
           wellPanel(
             # Short description of the application
             helpText(HTML("This Webapplication loads a datafile from the <a href='http://data.hdx.rwlabs.org' target = '_blank'>HDX CKAN Webdatabase</a> and display the content of it.")),
             br(),
             
             # Drop down menu, defined in server.R, to select local file. Only shown if no file variable were given in URL
             uiOutput("selectFile"),
             
             # Variable selector, defined in server.R, to select the countries that should be analyzed
             helpText("Please select one or more countries that should be displayed."),
             uiOutput("selectVariable"),
             br(),
             
             # Download handler to download the data from the selected countries
             helpText("You can download the selected subset of the data set. For this choose the desired file extension and press the download button."),
             selectInput("filetype", "File type:",
                         c("CSV" = "csv",
                           "TSV" = "tsv"
#                           "Excel" = "xls" # Excel doesn't work yet
                           )
                         ),
             downloadButton('downloadData', 'Download')
           )
    ),
    
    # Main panel definition
    column(9,
           h3("Loaded File"),
           
           # Display the xls file that is beeing analyzed
           verbatimTextOutput("queryText"),
           tabsetPanel(
             
             # Tab with data description according to the description in the xls file
             tabPanel("Data Description", tableOutput("desc"),
                      
                      # Style the description and summary table
                      tags$head(tags$style(type="text/css",
                                            "#desc table{
                                              border: 0px !important;
                                            }
                                            #desc th {
                                              font-size:1.1em;
                                              text-align:left;
                                              padding-top:5px;
                                              padding-bottom:4px;
                                            }
                                            #desc table, th, tr, td {
                                              border: 0px !important;
                                            }
                                            #desc tr td:first-child {
                                              width: 10em;
                                              font-weight:bold
                                            }
                                           #summary table{
                                              border: 0px !important;
                                            }
                                           #summary th {
                                              font-size:1.1em;
                                              text-align:right;
                                              padding-top:5px;
                                              padding-bottom:4px;
                                            }
                                            #summary table, th, tr, td {
                                              border: 0px !important;
                                            }
                                            #summary tr td:first-child {
                                              font-weight:bold
                                            }"
                                          )
                      )
             ),
             
             # Tab with the graphical representation of the selected countries in the xls file
             tabPanel("Graphic", plotOutput("plot")),
             
             # Tab with basic descriptive statistics of the selected contries
             tabPanel("Statistical Summary", tableOutput("summary"), div("Notice: For the calculations, the NA cells were removed.")),
             
             # Tab with the data of the selected contries
             tabPanel("Data Table", dataTableOutput("table"),
                      tags$head(tags$style(type="text/css",
                                           "tfoot {
                                              display: table-header-group;
                                           }
                                           .dataTables_filter {
                                              display: none;
                                           }"
                                           )
                                )
                      )
             )
    )
  ),

  # Copyright
  fluidRow(
    HTML("This Webapplication was developed by <a href='http://www.kalisch.biz'>Dominik Kalisch</a>.
  				The source code can be found on github:
					<a xmlns:cc='http://creativecommons.org/ns#' href='https://github.com/dkalisch/UNOCHA-HDX-shiny' property='cc:attributionName' rel='cc:attributionURL' >https://github.com/dkalisch/UNOCHA-HDX-shiny</a> <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en'><img alt='Creative Commons License' style='border-width:0; margin-right:5px; margin-left:1px vertical-align:top' src='http://i.creativecommons.org/l/by-nc-sa/3.0/80x15.png' /></a>")
  )
))
