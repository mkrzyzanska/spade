library("SHELF")
library("ggplot2")
library("shinyjs")
library("hdrcde")
limits<-"0, 100"
startingMethod<-"roulette"
nbins=20
gridheight=20
startingPanel <- "Roulette"
startDate<-1800
endDate<-1950

source("extra_functions/checkJudgementsValid.r")
source("extra_functions/feedback.r")
source("extra_functions/feedbacksingle.r")
source("extra_functions/qhist.r")
source("extra_functions/phist.r")
source("extra_functions/fitdist2.r")
source("extra_functions/normal.error.r")
source("extra_functions/logt.error.r")
source("extra_functions/gamma.error.r")
source("extra_functions/tError.R")
source("extra_functions/exponential.error.R")
source("extra_functions/lognormal.error.R")
source("extra_functions/plotfit.R")
source("extra_functions/makeSingleExpertPlot.R")
source("extra_functions/dhist.R")
source("extra_functions/fitBspline.r")
source("extra_functions/makeCDFPlot.R")
source("extra_functions/cdfHelperfunctions.R")
source("extra_functions/hdrhist.r")




source("extra_functions/beta.error.R")



 ui <- shinyUI(fluidPage(

     useShinyjs(),  # Initialize shinyjs
  tags$head(
    # Include Quill.js for rich text editing
    tags$script(src = "https://cdn.quilljs.com/1.3.6/quill.js"),
    tags$link(rel = "stylesheet", href = "https://cdn.quilljs.com/1.3.6/quill.snow.css")
  ),

    
    # Application title
    titlePanel("QUEADE Artefact Dates Elicitation Software"),
    
    sidebarLayout(
      sidebarPanel(
          textInput("QoI", label = "Quantity of Interest", value = ""),
        # Create a div for the Quill editor
  # Create a div for the Quill editor
       # Create a div for the Quill editor
      textAreaInput("PoE", 
                          label = "Portofolio of evidence", 
                          rows = 5,  # Adjust height
                          cols = 40, # Adjust width
                          placeholder = "Summarise the evidence about the QoI here..."
            ),
            actionButton("submitPoE", "Save Portfolio of Evidence"),
    
  verbatimTextOutput("text"),
        wellPanel(
          h5("Fitting and feedback"),
          checkboxInput("showFittedPDF", label = "show fitted PDF"),
          checkboxInput("showFittedCDF", label = "show fitted CDF"),
          checkboxInput("selectDistribution", label = "Select Custom Distribution"),
           conditionalPanel(
            condition = "input.selectDistribution == true",
          
          selectInput("dist", label = "Distribution", 
                      choices =  list('Best fitting' = "best",
                                      Histogram = "hist",
                                      Normal = "normal", 
                                      'Student-t' = "t",
                                      'Skew normal' = "skewnormal",
                                      Gamma = "gamma",
                                      'Log normal' = "lognormal",
                                      'Log Student-t' = "logt",
                                      Beta = "beta",
                                      'Mirror gamma' = "mirrorgamma",
                                      'Mirror log normal' = "mirrorlognormal",
                                      'Mirror log Student-t' = "mirrorlogt",
                                      'Natural Cubic Spline' = "NS",
                                      'Monotonic P-spline' = "MP")
                     ),

               conditionalPanel(
          condition = "input.dist == 'NS'",
          numericInput("bs_degree", label = h5("Degress of freedom (cubic spline)"),
                     value = 5),
          ),
        conditionalPanel(
        condition = "input.dist == 'NS' || input.dist == 'MP'",
               checkboxInput("exponential_tails", label = "Model exponential decay on tails"),
            conditionalPanel(
                condition = "input.exponential_tails == true",
                   numericInput("ltp", label = h5("Left tail points"),
                     value = 4),
                   numericInput("rtp", label = h5("Right tail points"),
                     value = 4),
                   numericInput("edr", label = h5("Tails exponential decay rate"),
                     value = 0.1))
        ),
        conditionalPanel(
          condition = "input.dist == 't' || input.dist == 'logt' || input.dist == 'mirrorlogt'",
          numericInput("tdf", label = h5("Student-t degrees of freedom"),
                     value = 10)
          ),
      #  checkboxInput("showCustomFeedback", label = "Custom Feedback"),
      #  conditionalPanel(condition = "input.showCustomFeedback == true",
      #  textInput("fq", label = h5("Feedback quantiles"), 
      #            value = "0.1, 0.9"),
      #   tableOutput("valuesPDF"),
      #  uiOutput("feedbackProbabilities"),
      #  tableOutput("fittedProbsPDF")
        ),
             checkboxInput("showCustomFeedback", label = "Custom Feedback"),
        conditionalPanel(condition = "input.showCustomFeedback == true",
        textInput("fq", label = h5("Feedback quantiles"), 
                  value = "0.1, 0.9"),
         tableOutput("valuesPDF"),
        uiOutput("feedbackProbabilities"),
        tableOutput("fittedProbsPDF"),
        uiOutput("hdrProbability"),
        tableOutput("fittedHDR")
          )
        )
     
      ),
            mainPanel(
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              
              
              wellPanel(
                fluidRow(
                column(2, 
                       numericInput("startDate", label = h5("Earliest Plausible Date"),
                       value = 8000,min = .Machine$double.eps),     
                      ),

                column(2, selectInput("sdate", label = h5("\u00A0"), width = "100px",
                      choices =  list(BCE = "bce",
                          CE = "ce"
                                      
                                      )),
                      ),
               
                column(2,    
                       numericInput("endDate", label = h5("Latest Plausible Date  "), 
                       value = 2025,min = .Machine$double.eps),
                      ),

                column(2, selectInput("edate", label = h5("\u00A0"), width = "100px",
                      choices =  list(CE = "ce",
                                      BCE = "bce"
                                      )),
                      ),
                column(2,offset=1,
                numericInput("gridHeight", 
                         label = h5(tags$b("Grid height")), value = 10, min  = 1)
                ),
                ),

               h5(tags$b("Input the number of tokens to get the probability per token. Input the token's probability to calculate the number of tokens needed.")),

                fluidRow(
                    column(2,
                           numericInput("nTokens", h5("Number of tokens:"), value=NULL, min = 1,step = 1),
                           ),
                    column(2,
                        numericInput("tokenProb", h5("Token's probability:"), value=NULL, min = 0,max=1,step=0.001)),
                    column(2, 
                         numericInput("fs", label = h5("Font size (plots)"), value = 16)
                      ),
                     column(2, selectInput("outFormat", label = h5("Report format"),
                                        choices = list('html' = "html_document",
                                                       'pdf' = "pdf_document",
                                                       'Word' = "word_document"))),
                    
                    column(2, downloadButton("report", "Download report"),
                          style = "margin-top: 25px;"),

                    column(2, downloadButton("download_rds", "Download R Object"))
              ),
              checkboxInput("customiseGraph", label = "Customise the number of bins"),

              conditionalPanel(condition = "input.customiseGraph == true",
                  fluidRow(
                      column(2, 
                      numericInput("nBins", label = h5("Number of bins"),
                         value = NULL, min = 3)
                        ),
                      #,
                      # column(1, offset = 1, actionButton("exit", "Quit"))
                  )   
              )
            ),
              hr(),
              
              tabsetPanel(
                  tabPanel("Roulette", 
                           plotOutput("roulette",
                                                click = "location"),
                           helpText("Click directly in the plot to allocate tokens to time intervals. 
                                    Click just below the line at 0 on the vertical axis to clear an interval. 
                                    Note that in the empty intervals are not assumed to have a 0 chance of containing the quantity of interest: probabilities from adjacent non-empty intervals will be smoothed out over the empty intervals."),
                           fluidRow(
                             column(4,
                                    downloadButton('downloadRoulette',
                                                   "Download plot")),
                             column(4,
                                    downloadButton('downloadRouletteCSV',
                                                   "Download allocation (csv)"))),
                            fluidRow(
                                conditionalPanel(condition = "false",
                                column(1, offset = 1, actionButton("exit", "Quit"))
                            )
                                )
                          ),
                tabPanel("PDF", 
                         plotOutput("distPlot"),
                         conditionalPanel(
                           condition = "input.showFittedPDF == true",
                           wellPanel(
                            h5(tags$b("Feedback:")),
                            htmlOutput("quantileValuesText"),
                           ),
                           fluidRow(
                             column(4,
                                    downloadButton('downloadDensities',
                                                   "Download plot")),
                            conditionalPanel(
                                condition="true",
                             column(3,  
                             uiOutput("setPDFxaxisLimits")
                             )
                           )
                           )
                         )),
                tabPanel("CDF", plotOutput("cdf"),
                         conditionalPanel(
                           condition = "input.showFittedCDF == true",
                           fluidRow(
                             column(4,
                                    downloadButton('downloadCDF',
                                                   "Download plot")),
                             column(3,  
                                    uiOutput("setCDFxaxisLimits")
                             )
                           )
                         )),
                
                tabPanel("Help", 
                         includeHTML(system.file("shinyAppFiles", "help.html",
                                                 package="SHELF"))
                         ),
                selected = startingPanel
              )
      )
    )
  ))
  
  # Server ----
   
  server <- function(input, output,session) {


   # Initialize the Quill editor when the app starts
  observe({
    runjs("
      var quill = new Quill('#editor', {
        theme: 'snow'
      });
    ")
  })
  
  # When the 'save' button is pressed, capture the text from Quill editor and save it
  observeEvent(input$save, {
    
    # Capture content from Quill editor (as plain text)
    runjs("Shiny.setInputValue('editor_content', quill.root.innerHTML);")
    
    # Get the content from the Quill editor
    content <- input$editor_content
    
    # If there is content, save it to a .txt file
    if (!is.null(content) && content != "") {
      
      # Clean HTML content to get plain text
      plain_text <- gsub("<[^>]+>", "", content)  # Strip out HTML tags
      
      # Write the plain text to a text file
      writeLines(plain_text, "user_input.txt")
      
      # Trigger file download
      download.file("user_input.txt", "user_input.txt")
    }
  })
      




     observeEvent(input$nTokens, {
        req(input$nTokens)  # Ensure input is not NULL
          if (input$nTokens < 1) {
            updateNumericInput(session, "nTokens", value = input$nTokens*-1)  # Reset to positive number
        }

        
        updateNumericInput(session, "tokenProb", value = round(1 / input$nTokens,3)) } # Maintain sum = 1
    , ignoreInit = TRUE)

     observeEvent(input$tokenProb, {
        req(input$tokenProb, input$tokenProb > 0)  # Ensure input is valid
         # Find closest valid 1/n value
        closest_n <- round(1 / input$tokenProb)  # Get closest integer n
        valid_prob <- round(1 / closest_n, 3)    # Compute valid 1/n probability

        updateNumericInput(session, "tokenProb", value = valid_prob)  # Update input box
        updateNumericInput(session, "nTokens", value = closest_n)  # Sync nTokens
    }, ignoreInit = TRUE)
    
    # Parameter limits ----

    ### Define the distribution range to figure out optimal bin number & placement
    distributionRange <- reactive({
        req(input$startDate, input$endDate, input$sdate,input$edate)

        # Get the right start and end dates depending on whether we have BCE or CE
        if(input$sdate=="bce"){
            start <- as.integer(-input$startDate)+1
        }else{
            start <- as.integer(input$startDate)
        }

        if(input$edate=="bce"){
            end <- as.integer(-input$endDate)+1
        }else{
        end <- as.numeric(input$endDate)}
        
      # Return distribution range
        if (!is.na(start) && !is.na(end)) {
            return(end - start)
        } else {
            return(NULL)
        }
    })

    trueStart <- reactive({
        req(input$startDate,input$sdate)

        # Define the right start date depending on whether the date is CE or BCE
        if(input$sdate=="bce"){
            start <- as.integer(-input$startDate)+1
        }else{
            start <- as.integer(input$startDate)
        }})

     trueEnd <- reactive({
        req(input$endDate,input$edate)

        # Hande bce/ce
        if(input$edate=="bce"){
            end <- as.integer(-input$endDate)+1
        }else{
            end <- as.numeric(input$endDate)
        }})

    startDate <- reactive({
        req(input$startDate,input$sdate, distributionRange())

        # Define the right start date depending on whether the date is CE or BCE
        if(input$sdate=="bce"){
            start <- as.integer(-input$startDate)+1
        }else{
            start <- as.integer(input$startDate)
        }

        # If checkbox is checked, return the raw start date (skip adjustments)
        if (input$customiseGraph) {
            return(start)
        }
        range <- distributionRange()

        if (!is.na(start)) {

            #### Start always the same, when needs to be divisible by 2
            if (range <= 50) {
                return(start)
            } else if (range > 50 && range <= 100 && range %% 5 != 0) {
                limLow <- floor(start / 5) * 5
                return(limLow)
            } else if (range > 100 && range <= 200 && range %% 10 != 0) {
                limLow <- floor(start / 10) * 10
                return(limLow)
            } else if (range > 200 && range <= 500 && range %% 20 != 0) {
                limLow <- floor(start / 20) * 20
                return(limLow)
            } else if (range > 500 && range <= 1000 && range %% 50 != 0) {
                limLow <- floor(start / 50) * 50
                return(limLow)
            }else if (range > 1000 && range <= 2000 && range %% 100 != 0) {
                limLow <- floor(start / 100) * 100
                return(limLow)
            }else if (range > 2000 && range <= 5000 && range %% 200 != 0) {
                limLow <- floor(start / 200) * 200
                return(limLow)
            } else if (range > 5000 && range <= 10000 && range %% 500 != 0) {
                limLow <- floor(start / 500) * 500
                return(limLow)
            }  else if (range > 10000 && range <= 50000 && range %% 1000 != 0) {
                limLow <- floor(start / 1000) * 1000
                return(limLow)
            }  else if (range > 50000) {
                limLow <- floor(start / 10000) * 10000
                return(limLow)
            }else{
                return(start)}
        } else {
            return(NULL)
        }
        
       # return(start)
    })

    endDate <- reactive({
        req(input$endDate,input$edate,distributionRange())

        # Hande bce/ce
        if(input$edate=="bce"){
            end <- as.integer(-input$endDate)+1
        }else{
            end <- as.numeric(input$endDate)
        }

         if (input$customiseGraph) {
            return(end)
        }

        # 
        range <- distributionRange()

        if (!is.na(end)) {
            if (range > 20 && range <= 50 && range %% 2 != 0) {
                limHigh <- end + 1
                return(limHigh)
            } else if (range > 50 && range <= 100 && range %% 5 != 0) {
                limHigh <- ceiling(end / 5) * 5
                return(limHigh)
            }   else if (range > 100 && range <= 200 && range %% 10 != 0) {
                limHigh <- ceiling(end / 10) * 10
                return(limHigh)
            }  else if (range > 200 && range <= 500 && range %% 20 != 0) {
                limHigh <- ceiling(end / 20) * 20
                return(limHigh)
            } else if (range > 500 && range <= 1000 && range %% 50 != 0) {
                limHigh <- ceiling(end / 50) * 50
                return(limHigh)
            } else if (range > 1000 && range <= 2000) {
                limHigh <- ceiling(end / 100) * 100
                return(limHigh)
            }else if (range > 2000 && range <= 5000) {
                limHigh <- ceiling(end / 200) * 200
                return(limHigh)
            } else if (range > 5000 && range <= 10000) {
                limHigh <- ceiling(end / 500) * 500
                return(limHigh)
            } else if (range > 10000 && range <= 50000) {
                limHigh <- ceiling(end / 1000) * 1000
                return(limHigh)
            }else if (range > 50000) {
                limHigh <- ceiling(end / 10000) * 10000
                return(limHigh)
            }else{
                return(end)
            } 
        } else {
        return(NULL)
    }
       
   # return(end)
    })
    
    # Feedback quantiles ----
    fq <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$fq, ")"))),
               error = function(e){NULL})
      
    })
    
    # Feedback probabilities. Needs to know parameter limits ----
    output$feedbackProbabilities <- renderUI({
      textInput("fp", label = h5("Feedback probabilities"), 
                paste(c(startDate(),endDate()), collapse = ", "))
    }) 
    output$hdrProbability <- renderUI({
      textInput("hdr", label = h5("Highest Probability Dates"), "10")
    }) 
    fp <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$fp, ")"))),
               error = function(e){NULL})
      
    }) 

    hdr <- reactive({
      tryCatch(eval(parse(text = paste(input$hdr))),
               error = function(e){NULL})
      
    }) 
    
    # Axes limits for pdf/cdf plots. Needs to know parameter limits ----
    output$setPDFxaxisLimits <- renderUI({
      textInput("xlimPDF", label = h5("x-axis limits"), 
                paste(c(startDate(),endDate()), collapse = ", "))
    }) 
    xlimPDF <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xlimPDF, ")"))),
               error = function(e){NULL})
      
    })
    output$setCDFxaxisLimits <- renderUI({
      textInput("xlimCDF", label = h5("x-axis limits"), 
                paste(c(startDate(),endDate()), collapse = ", "))
    }) 
    xlimCDF <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xlimCDF, ")"))),
               error = function(e){NULL})
      
    })
    
    # Elicited probabilities and values ----
    p <- reactive({
      myp <- rl$allBinsPr[rl$nonEmpty]
      myp
    })
    v <- reactive({
       myv <- bin.right()[rl$nonEmpty]
    })

    cn <-  reactive({
      myc <- rl$chips[rl$chips>0]
      myc})
    
    # Roulette ----

    # Extract number of bins and grid height, 
    # and grid positions for roulette method

    # Alternatively get the maximu number of interval for optimal interval finding
     nIntervals <- reactive({
        req(endDate(), startDate())
        if(endDate()>startDate()){
            return(endDate() - startDate())}
            else{return(NULL)}
        })

    # nBins <- reactive({

     #     req(nIntervals(),distributionRange())

         ### Here we need to add a check mark if we want the automatic bin_width adjustment

     #   if (input$customiseGraph) {
     #     return(input$nBins)
     #   }else{    
     #       if(is.numeric(nIntervals()) & is.numeric(distributionRange())){
     #         if(distributionRange()>10&distributionRange()<=20){
     #             return(nIntervals()/2)    
     #         }else if(distributionRange()>20&distributionRange()<=60){
     #             return(nIntervals()/5)  
     #         }else if(distributionRange()>60){
     #             return(nIntervals()/10)  
     #         }else{ 
     #             return(nIntervals())}}
     #       else{
     #         return(NULL)
     #   } 
     #}       
    #})

nBins <- reactiveVal()  # Reactive value to store nBins

# When checkbox is checked, use input$nBins
observeEvent(input$customiseGraph, {
  if (input$customiseGraph) {
    updateNumericInput(session, "nBins", value = nBins())  
    nBins(input$nBins)  # Set nBins to user input when the checkbox is checked
  } else {
    # Reset to auto-calculated bins when checkbox is unchecked
    req(nIntervals(), distributionRange())
    updateBins()
  }
})

# Ensure nBins updates when input$nBins changes while checkbox is checked
observeEvent(input$nBins, {
  if (input$customiseGraph&& !is.null(input$nBins)) {
    nBins(input$nBins)  # Keep updating nBins immediately when input$nBins changes
  }
})

# Automatically update nBins when the checkbox is unchecked
observe({
  if (!input$customiseGraph) {  # Only run when unchecked
    req(nIntervals(), distributionRange(),startDate(),endDate())  # Ensure required values exist
    updateBins()  # Call function to update bins dynamically
    rl$chips <- rep(0, nBins())
  }
})

# Helper function to update bins dynamically when in auto mode
updateBins <- function() {
  if (is.numeric(nIntervals()) & is.numeric(distributionRange())) {
    if (distributionRange() > 20 & distributionRange() <= 50) {
      nBins(nIntervals() / 2)    
    } else if (distributionRange() > 50 & distributionRange() <= 100) {
      nBins(nIntervals() / 5)  
    } else if (distributionRange() > 100 & distributionRange() <= 200) {
      nBins(nIntervals() / 10)        
    } else if (distributionRange() > 200 & distributionRange() <= 500) {
      nBins(nIntervals() / 20)        
    }  else if (distributionRange() > 500 & distributionRange() <= 1000) {
      nBins(nIntervals() / 50)        
    } else if (distributionRange() > 1000 & distributionRange() <= 2000) {
      nBins(nIntervals() / 100) 
    } else if (distributionRange() > 2000 & distributionRange() <= 5000) {
      nBins(nIntervals() / 200)  
    } else if (distributionRange() > 5000 & distributionRange() <= 10000) {
      nBins(nIntervals() / 500)  
    } else if (distributionRange() > 10000 & distributionRange() <= 50000) {
      nBins(nIntervals() / 1000) 
    } else if (distributionRange() > 50000) {
      nBins(nIntervals() / 10000)  
    } else { 
      nBins(nIntervals()) 
    }
  } else {
    nBins(NULL)  # Set to NULL if values are invalid
  }
}


      
    # nBins <- reactive({
    #  req(input$nBins)
    #  if(is.integer(input$nBins) & input$nBins > 0){
    #    return(input$nBins)}else{
    #      return(NULL)}
    #    
    #})
    gridHeight <- reactive({
      req(input$gridHeight)
      if(is.integer(input$gridHeight) & input$gridHeight > 0){
        return(input$gridHeight)}else{
          return(NULL)}
      
    })
    bin.width <- reactive({
      req(startDate(),endDate(), nBins())
      print(startDate())
      print(endDate())
      abs((endDate()-startDate())/ nBins())
    })
    bin.left <- reactive({
      req(startDate(),endDate(), nBins(), bin.width())
      seq(from = startDate(),
          to = endDate() - bin.width(),
          length=nBins())
    })
    bin.right <- reactive({
      req(startDate(),endDate(), nBins(), bin.width())
      seq(from = startDate() + bin.width(),
          to = endDate(),
          length = nBins())
    })
    
    # Initial allocation of chips to bins
    rl <- reactiveValues(x=-1, y=-1,
                          chips = rep(0, 1003),
                         allBinsPr = NULL,
                         nonempty = NULL
    )

      # Watch for changes in nBins() and update rl$chips accordingly
observe({
  # Ensure nBins is not NULL or invalid before updating rl$chips
  req(nBins())
  
  # Update rl$chips based on the current value of nBins
  rl$chips <- rep(0, nBins())  # Update rl$chips with the new number of bins
})


 #  observeEvent(nBins(), {
 #       req(nBins()) # Ensure nBins() has a valid value
 #       if(is.numeric(nBins()) & nBins() > 0){
 #           rl$chips <- rep(0, nBins())}  # Set chips to the length of nBins() with all elements initialized to 0
 #   })
    
    # Update allocation as bins are clicked on
    observeEvent(input$location, {
      rl$x <-input$location$x
      rl$y <-input$location$y
      
      plotHeight <- max(gridHeight(), max(rl$chips) + 1)
      
      
      if(rl$x > startDate() & rl$x < endDate() & rl$y < plotHeight){
        index <- which(rl$x >= bin.left() & rl$x < bin.right())
        rl$chips[index]<-ceiling(max(rl$y, 0))
        rl$allBinsPr <- cumsum(rl$chips)/sum(rl$chips)
        rl$nonEmpty <- rl$allBinsPr > 0 & rl$allBinsPr < 1
      }
      
      
    })
    
    # Reset chip allocation if number of bins or limits change
   # observeEvent(input$nBins,{
   #   req(input$nBins)
   #   if(is.integer(input$nBins) & input$nBins > 0){
   #     rl$chips <- rep(0, input$nBins)}
   # })
    observeEvent(input$limits,{
      req(nBins())
      if(is.integer(nBins()) & nBins() > 0){
        rl$chips <- rep(0, nBins())}
    })

      observeEvent(input$startDate,{
      req(nBins())
      if(is.integer(nBins()) & nBins() > 0){
        rl$chips <- rep(0, nBins())}
    })

      observeEvent(input$endDate,{
      req(nBins())
      if(is.integer(nBins()) & nBins() > 0){
        rl$chips <- rep(0, nBins())}
    })
    # Fit distributions to elicited judgements ----
    myfit <- reactive({
      req(startDate(),endDate(), v(), p(), input$tdf,input$ltp,input$rtp,input$edr,trueStart(),trueEnd())
      print(input$ltp)
      check <- checkJudgementsValid(probs = p(), vals = v(),
                           tdf = input$tdf,
                           lower = startDate(),
                           upper= endDate()
                                   )
      if(check$valid == TRUE){
      fitdist2(vals = v(), probs = p(),cn=cn(), lower = trueStart(),
              upper = trueEnd(), width=bin.width(),
              tdf = input$tdf,bs_degree=input$bs_degree,exponential_tails=input$exponential_tails,rtp=input$rtp,ltp=input$ltp,edr=input$edr)
      }
    })
    
    # All plots have separate functions, so can be called from 
    # both renderPlot, and from ggsave() for downloading
    
    plotPDF <- function(){
      req(myfit(), xlimPDF(), fq(), input$fs, quantileValues(),gridHeight())
      
      if(input$showFittedPDF){
    
        dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best","NS","MP")

        if(input$selectDistribution){  
        suppressWarnings(plotfit(myfit(), d = input$dist,
                                  ql = fq()[1], qu = fq()[2],
                                 xl = xlimPDF()[1], xu = xlimPDF()[2], 
                                 fs = input$fs,
                                 xlab = input$xLabel,
                                 startDate=startDate(),
                                 endDate=endDate(),
                                 nBins=nBins(),
                                 ybreaks=max(rl$chips),
                                 chips=rl$chips))
            }else{
            suppressWarnings(plotfit(myfit(), d = "best",
                                  ql = 0.1, qu = 0.9,
                                 xl = xlimPDF()[1], xu = xlimPDF()[2], 
                                 fs = input$fs,
                                 xlab = input$xLabel,
                                 startDate=startDate(),
                                 endDate=endDate(),
                                 nBins=nBins(),
                                 ybreaks=max(rl$chips),
                                 chips=rl$chips))}
      }
        
      
    }
    output$distPlot <- renderPlot({
      plotPDF()
    })
    
    plotCDF <- function(){
      req(myfit(), xlimCDF(), startDate(),endDate(), fq())
      
      if(input$dist == "best"){
        mydist <- as.character(myfit()$best.fitting[1, 1])
      }else{
        mydist <- input$dist
      }
      
      if(is.null(xlimCDF())){
        xL <- startDate()
        xU <- endDate()
      }else{
        xL <- xlimCDF()[1]
        xU <- xlimCDF()[2]
      }
      
      
      makeCDFPlot(lower = startDate(),
                  v = v(),
                  p = p(),endDate(),
                  input$fs,
                  fit = myfit(),
                  dist = mydist,
                  showFittedCDF = input$showFittedCDF,
                  showQuantiles = TRUE,
                  ql = fq()[1],
                  qu = fq()[2],
                  xaxisLower = xL,
                  xaxisUpper = xU,
                  xlab = input$xLabel,min_val=trueStart(),max_val=trueEnd())
      
    }
    output$cdf <- renderPlot({
      plotCDF()
    })
    
    plotRoulette <- function(){
      
      req(startDate(),endDate(), input$fs, nBins(), gridHeight(), bin.left(),
          bin.right(),rl)
      print(v())
      print(p())  
      print(nBins()) 
      print(paste("rl_chips:",rl$chips))

      xticks<-seq(startDate(),endDate(), length.out=nBins()+1)

     # print(paste("xticks",xticks))
      
      convert_year <- function(year) {
          if (year < 1) year-1 else year
      }

     new_ticks<-sapply(xticks,convert_year)
     #print(paste("new_ticks",new_ticks))
     if (all(new_ticks < 0)) {
        x_lab = "YEAR BCE"
     } else if (all(new_ticks > 0)) {
        x_lab = "YEAR CE"
     } else {
         x_lab= "YEAR"
     }
      
      plotHeight <-  max(gridHeight(), 
                         max(rl$chips) + 1)
      
      par(ps = input$fs)
      plot(c(startDate(),endDate()), c(0, 0),
           xlim=c(startDate(),endDate()),
           ylim=c(-(plotHeight/5), plotHeight),
           type="l",
           ylab="",
           xaxp=c(startDate(),endDate(), nBins()), 
           main = paste("Total tokens placed:", sum(rl$chips), "Probability per token:", round(1/sum(rl$chips),3)),
           xlab = x_lab,
           xaxt="n") # Supress axis drawing
        axis(1, at=xticks, labels=round(abs(new_ticks),digits=2))
        segments(x0 = 1, y0 = 0, x1 = 1, y1 = gridHeight(), col = "red", lwd = 2, lty = 2)
        # Add BCE to the left dynamically
        text(x = 1 - (diff(c(startDate(),endDate()))*0.01), y = gridHeight()*0.95, labels = "BCE", col = "red", cex = 1, adj = 1)
        # Add CE to the right dynamically
        text(x = 1 + (diff(c(startDate(),endDate())) * 0.01), y = gridHeight()*0.95, labels = "CE", col = "red", cex = 1, adj = 0)
      for(i in 1:nBins()){
        lines(c(bin.left()[i],bin.left()[i]),
              c(0, plotHeight),lty=3,col=8)
      }
      lines(c(bin.right()[nBins()],bin.right()[nBins()]),
            c(0, plotHeight),lty=3,col=8)
      
      for(i in 1:plotHeight){
        lines(c(startDate(),endDate()),
              c(i,i), lty=3,col=8)
      }
      
      for(i in 1:nBins()){
          print(i)
        if(rl$chips[i]>0){
          rect(rep(bin.left()[i],rl$chips[i]),c(0:(rl$chips[i]-1)),
               rep(bin.right()[i],rl$chips[i]),c(1:rl$chips[i]),col=2)
        }
      }
      
    }
    output$roulette <- renderPlot({
      plotRoulette()
    })



    
    # Feedback - get fitted quantiles/probabilities ----
    
    quantileValues <- reactive({
      req(fq(), myfit())
      
      if(min(fq())<=0 | max(fq())>=1){
        return(NULL)
      }else{
        
        FB <- feedback(myfit(), 
                       quantiles = fq(),
                       ex = 1)

          
        if(input$selectDistribution&&input$dist != "best"){
            values <- FB$fitted.quantiles[, input$dist]
          
        }else{
            values <- FB$fitted.quantiles[, 
                                        as.character(myfit()$best.fitting[1,
                                                                          1])]
          
        }
        
        return(data.frame(quantiles=fq(), values=values))
      }
      
    })

    customQuantileValues <- reactive({
      req(myfit())
      fqc<-c(0.1,0.9,0.5)
      
      if(min(fqc)<=0 | max(fqc>=1)){
        return(NULL)
      }else{
        
      FB <- feedback(myfit(), 
                       quantiles = fqc,
                       ex = 1)
        
        if(input$selectDistribution&&input$dist != "best"){
           values <- FB$fitted.quantiles[, input$dist]
        }else{
         
            values <- FB$fitted.quantiles[, 
                                        as.character(myfit()$best.fitting[1,
                                                                          1])]
          
        }
        print(values)
        return(values=values)
      }
      
    }) 

    customProbabilityValues <- reactive({
      req(myfit(),bin.left(),bin.right(),rl$chips)
      left_bin <- bin.left()
      left_bin<-left_bin[min(which(rl$chips!=0))]
      right_bin <- bin.right()
      right_bin<-right_bin[max(which(rl$chips!=0))]
      fpc<-c(left_bin,right_bin)
      FB <- feedback(myfit(), 
                       values = fpc,
                       ex = 1)
        print(rl$chips)
        print(bin.left())
         print(fpc)
        print(input$dist)
        if(input$selectDistribution&&input$dist != "best"){
             probs <- FB$fitted.probabilities[, input$dist]
          
        }else{
          probs <- FB$fitted.probabilities[, 
                                        as.character(myfit()$best.fitting[1,
                                                                          1])]
        }
      print(probs)
      df<-data.frame(probs=probs,vals=c(left_bin,right_bin))
      print(df)
      return(df)
      
      
    }) 

    customHDR<- reactive({
      req(myfit())


          HDR <- feedback(myfit(), 
                       quantiles = c(0.1,0.9),
                       ex = 1,hdr.prob=10)
     
        print(input$dist)
        if(input$selectDistribution&&input$dist != "best"){
             hdr <- HDR$fitted.hdr[[input$dist]]         
        }else{
          hdr <- HDR$fitted.hdr[[as.character(myfit()$best.fitting[1,1])]]
        }
      df<-hdr$hdr
      df2<-as.data.frame(df)
      colnames(df2)<-c("lower","upper")
      print("hdr")
     
      return(df)
      
      
    }) 


    updatedQuantileValues <- reactive({
         req(customQuantileValues(),customProbabilityValues)  # Only proceed if triggerUpdate has been incremented

        # Retrieve the latest values
        list(values = isolate(customQuantileValues()), probs = isolate(customProbabilityValues()$probs),pvalues=isolate(customProbabilityValues()$vals))
    })


    output$quantileValuesText <- renderText({ 
    values <- updatedQuantileValues()$values
    probs<-updatedQuantileValues()$probs
    pvals<-updatedQuantileValues()$pvalues
    hdrval <- customHDR()    

    
    # Check if values are NULL and handle that case
    if (is.null(values)) {
        return("Quantiles are out of bounds or not available.")
    }
    
    # Create a formatted text output

        

    values <- sapply(values,function(x){
        if(x<1){
           text<- paste(abs(x-1),"BCE")}
       else{text <- paste(x,"CE")}
        return(text)
        })

      pvals <- sapply(pvals,function(x){
        if(x<1){
           text<- paste(round(abs(x-1),0),"BCE")}
       else{text <- paste(round(x,0),"CE")}
        return(text)
        })

    hdrval <- apply(hdrval,c(1, 2),function(x){
        if(x<1){
           text<- paste(round(abs(x-1),0),"BCE")}
       else{text <- paste(round(x,0),"CE")}
        return(text)
        })
        
    s1<- paste("There is about 10% probability that the artefact was deposited before", values[1], sep=" ")
    s2<-paste("There is about 10% probability that the artefact was deposited after", values[2],sep=" ")
    s3<-paste("It is equally likely that the artefact was deposited before and after", values[3],sep=" ")
    s4<-paste("There is about ",round(probs[1] * 100,2), "% probabiliy that the artefact was deposited before ", pvals[1] ,sep="")
    s5<-paste("There is about ",round((1-probs[2]) * 100,2), "% probabiliy that the artefact was deposited after ", pvals[2],sep="")
    s6<-paste("The most likely date range for the artefact deposion is between ", hdrval[1,1]," and ", hdrval[1,2], " (10% probability)", sep="" )    
    HTML(paste0("<ul>",
              "<li>", s1, "</li>",
              "<li>", s2, "</li>",
              "<li>", s3, "</li>",
             "<li>", s4, "</li>",
             "<li>", s5, "</li>",
            "<li>", s6, "</li>",
              "</ul>"))
})
    probabilityValues <- reactive({
      req(fp(), myfit())
      
      FB <- feedback(myfit(), 
                     values = fp(),
                     ex = 1)
      
      if(input$selectDistribution&&input$dist != "best"){
      probs <- FB$fitted.probabilities[, input$dist]
      }else{
          probs <- FB$fitted.probabilities[, 
                                         as.character(myfit()$best.fitting[1,
                                                                           1])]
        
      }
      
      return(data.frame(values=fp(), probabilities = probs))
      
      
    }) 

    hdrValues <- reactive({
      req(hdr(), myfit())
      #print("hdr")
      #print(hdr())
      
      HDR <- feedback(myfit(), quantiles = c(0.1,0.9),ex = 1,hdr.prob=hdr())
     
       # print(input$dist)
        if(input$selectDistribution&&input$dist != "best"){
             hdreg <- HDR$fitted.hdr[[input$dist]]         
        }else{
          hdreg <- HDR$fitted.hdr[[as.character(myfit()$best.fitting[1,1])]]
        }
      df<-hdreg$hdr
      df<-as.data.frame(df)
      colnames(df)<-c("earlier","later")
        
      return(df)
      
    }) 
    
    # ...and display on the PDF tab...
    output$valuesPDF <- renderTable({
      req(quantileValues())
      quantileValues()
    })
    output$fittedProbsPDF <- renderTable({
      req(probabilityValues())
      probabilityValues()
    })
    output$fittedHDR <- renderTable({
      print("rendering")
      req(hdrValues())
      hdrValues()
    })
    
    # ...and display on the CDF tab
    output$fittedProbsCDF <- renderTable({
      req(probabilityValues())
      probabilityValues()
    })
    output$valuesCDF <- renderTable({
      req(quantileValues())
      quantileValues()
    })
    
    # Compare individual elicited judgements with RIO ----
    
    groupFit <- reactive({
      file <- input$loadCSV
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      readSHELFcsv(file$datapath)
    })
    
    output$compareRIO <- renderPlot({
      req(groupFit(), myfit())
      compareGroupRIO(groupFit(), myfit(), type = input$comparePlotType,
                      fs = input$fs,
                      xlab = input$xLabel)
    })
    
    # Download individual plots ----
    output$downloadDensities = downloadHandler(
      filename = 'fittedPDF.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotPDF(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadCDF = downloadHandler(
      filename = 'fittedCDF.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotCDF(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadTertiles = downloadHandler(
      filename = 'tertiles.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotTertileJudgements(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadQuartiles = downloadHandler(
      filename = 'quartiles.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotQuartileJudgements(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadRoulette = downloadHandler(
      filename = 'roulette.png',
      content = function(file) {
       # device <- function(..., width, height) {
        #  grDevices::png(..., width = 5, height = 3,
        #                 res = 300, units = "in")
        #}
        #ggsave(file, plot = plotRoulette(),
         #      device = device, width = 5,
          #     height = 3, units = "in")
        grDevices::png(file)
        plotRoulette()
        grDevices::dev.off()
      })
    
    # Download roulette allocation as csv
    
    output$downloadRouletteCSV <- downloadHandler(
      filename = function() {
        paste('roulette-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        rouletteCSV <- data.frame(bins = paste0("(",bin.left(),
                                               ", ",bin.right(),"]"),
                                 probs = rl$chips)
                                 
        utils::write.csv(rouletteCSV, file)
      }
    )

    output$download_rds <- downloadHandler(
        filename = function() {
      paste0("my_object_", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(myfit(), file)
    }
  )
      
 observeEvent(input$submitPoE, {
        req(input$PoE)  # Ensure the input is not empty
        writeLines(input$PoE, "Portfolio_of_Evidence.txt")
        showNotification("Portfolio of Evidence saved!", type = "message")
    })
    
    # Download handler for exporting the PoE as a text file
    output$downloadPoE <- downloadHandler(
        filename = function() { "Portfolio_of_Evidence.txt" },
        content = function(file) {
            writeLines(input$PoE, file)
        }
    )

   
    
    # Download R Markdown report
    output$report <- downloadHandler(
      filename = function(){switch(input$outFormat,
                                   html_document = "distributions-report.html",
                                   pdf_document = "distributions-report.pdf",
                                   word_document = "distributions-report.docx")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "elicitationShinySummary.Rmd")
        file.copy("elicitationShinySummary.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        #if(input$method==1){
        #  params <- list(fit = myfit(), roulette = FALSE)
        #}
        
        
        # Include roulette allocation
        QoI<- input$QoI
        print(QoI)

       # Check selected distribution
 
        if(input$selectDistribution){
            selected_distribution<-input$dist}
        else{
            selected_distribution<-"best"}

      print(selected_distribution)
      
       # if(input$method==2){
          params <- list(fit = myfit(),
                         ql = fq()[1], 
                         qu = fq()[2],
                         xl = xlimPDF()[1],
                         xu = xlimPDF()[2], 
                         fs = input$fs,
                         xlab = input$xLabel,
                         startDate=startDate(),
                         endDate=endDate(),
                         nBins=nBins(),
                         ybreaks=max(rl$chips),
                         bin.left = bin.left(),
                         bin.right = bin.right(),
                         chips = rl$chips,
                         roulette = TRUE,
                         QoI=QoI,
                         TS=trueStart(),
                         TE=trueEnd(),
                         selected_distribution=selected_distribution,
                         fq=fq(),
                         fp=fp(),
                         PoE=input$PoE)
        #}


                    
                               
                              
                                
                                
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          output_format = input$outFormat,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    # Quit app button
    observeEvent(input$exit, {
      stopApp(myfit())
    }) 
    
  }
  #), launch.browser = TRUE)
  shinyApp(ui, server, options = list(launch.browser = TRUE))
