# UI ----
ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("SPADE: Sheffield Probabilistic Artefact Dates Elicitation Software"),

    sidebarLayout(
      sidebarPanel(

        ##### METADATE #####
        wellPanel(
          tags$details(
            open = TRUE,  # <-- opened by default
            tags$summary(
              style = "font-size: 1.1em; cursor: pointer;margin-bottom: 20px;",
              tags$strong("Metadata (click here to collapse / expand)\n")
            ),
          textInput("Date", label = "Date of Elicitation", value = format(Sys.time(), '%d-%m-%Y')),
          textInput("Expert", label = "Expert", value = ""),
          textInput("Facilitator", label = "Facilitator", value = ""),
          textInput("FindType", label = "Find Type (e.g. coin, pottery sherd)", value = ""),
          textInput("EoI", label = "Event of Interest (e.g. production, deposition)", value = ""),
          textInput("UFI", label = "Unique Find Identifier", value = ""),
          textInput("ULI", label = "Unique Location Identifier", value = ""),
          textInput("USI", label = "Unique Site Identifier", value = ""),
          textInput("PoE", label = "Portofolio of Evidence Location (e.g. DOI, link, reference, owner)", value = "")
        )),

        ##### PROBABILITY CALCULATOR #####
        wellPanel(
          tags$details(
            collapsed = TRUE,  # <-- opened by default
            tags$summary(
              style = "font-size: 1.1em; cursor: pointer;margin-bottom: 20px;",
              tags$strong("Probability calculator (click here to collapse / expand)\n")
            ),
          helpText("Input the number of tokens to get the probability per token. Input the token's probability to calculate the number of tokens needed."),

          fluidRow(
          column(4,
                 numericInput("nTokens", h5("Number of tokens:"), value=NULL, min = 1,step = 1),
          ),
          column(4,
                 numericInput("tokenProb", h5("Token's probability:"), value=NULL, min = 0,max=1,step=0.001))
        ))),

        ##### CUSTOM FEEDBACK #####
        wellPanel(
          tags$details(
            collapsed = TRUE,  # <-- opened by default
            tags$summary(
              style = "font-size: 1.1em; cursor: pointer;margin-bottom: 20px;",
              tags$strong("Show Custom Feedback (click here to collapse /expand)\n")
            ),
             #checkboxInput("showCustomFeedback", label = "Show Custom Feedback"),
        #conditionalPanel(condition = "tag$details$open == TRUE",
        textInput("fq", label = h5("Feedback quantiles"),
                  value = "0.1, 0.9"),
         tableOutput("valuesPDF"),
        uiOutput("feedbackProbabilities"),
        tableOutput("fittedProbsPDF"),
        uiOutput("hdrProbability"),
        tableOutput("fittedHDR")
         # )
        ))

      ),

        ##### MAIN PANEL #####
            mainPanel(
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),

              wellPanel(

                fluidRow(
                  column(8, textInput("QoI", label = "Quantity of Interest (fill in the metadata to generate automatically)", value = "")),
                  column(4,fileInput("load_rds", "Import date", accept = c(".rds",".json",".csv")))
                ),

                fluidRow(

                    column(2,uiOutput("start_date_container")
                           #numericInput("startDate", label = "Earliest Date",
                           #value = 8000,min = .Machine$double.eps),
                           #checkboxInput("earlyHard", label = "hard boundary")
                    ),

                    column(2,
                          uiOutput("sdate_container")
                          #selectInput("sdate", label = "\u00A0", width = "100px",
                          #choices =  list(BCE = "bce", CE = "ce")),
                    ),

                    column(2,
                           uiOutput("end_date_container")
                           #numericInput("endDate", label = "Latest Date  ",
                           #value = 2025,min = .Machine$double.eps),
                           #checkboxInput("earlyHard", label = "hard boundary")
                    ),

                    column(2,
                          uiOutput("edate_container")
                          #selectInput("edate", label = "\u00A0", width = "100px",
                          #choices =  list(CE = "ce",  BCE = "bce")),
                    ),

                    column(2, selectInput("outFormat", label = "Report format",
                                          choices = list('pdf' = "pdf_document",
                                                          'html' = "html_document",
                                                         'Word' = "word_document")),
                              downloadButton("report", "Download report")),

                    column(2, selectInput("exportFormat", label = "Export format",
                                          choices = list( 'json' = "json_file",
                                                          'csv' = "csv_file",
                                                          'R' = "r_file")),
                              downloadButton("download_rds", "Export Results"))
                ),

                fluidRow(

                    column(4, uiOutput("checkbox_container")
                           #checkboxInput("customiseGraph", label = "Customise the number of bins")
                           ),
                    column(2, actionButton("reset_tokens", "Clear token allocation", icon = icon("refresh"), class = "btn-danger"))
                ),

              conditionalPanel(condition = "input.customiseGraph == true",
                fluidRow(
                    column(2, uiOutput("nBins_ui"))
                    #numericInput("nBins", label = "Number of bins",value = NULL, min = 3) ),
                    # column(1, offset = 1, actionButton("exit", "Quit"))
                  )
              )
            ),

            hr(),

            tabsetPanel(id = "tabs",

              tabPanel("Token Allocation",value="tokens",

                  wellPanel(style="background: white;border:0", tags$details(
                              collapsed = TRUE,  # opened by default
                              tags$summary(
                                style = "font-size: 1.1em; cursor: pointer;margin-bottom: 1px;",
                                tags$strong("Click here to customise the plot\n")
                              ),

                      fluidRow(

                          column(2,numericInput("gridHeight",label = h5("Grid height"), value = 10, min  = 1)),
                          column(2,numericInput("fs", label = h5("Font size (plots)"), value = 16)),
                          column(2, selectInput("colour", label = h5("Colour"),
                                                 choices =  list(Blue = "slateblue",
                                                   Green = "darkseagreen",
                                                   Red= "red",
                                                   Orange="orange",
                                                   Purple="purple",
                                                   Pink="pink",
                                                   Brown="brown",
                                                   Grey="grey"
                          )))


                  ))),

                  plotOutput("roulette", click = "location"),
                  helpText("Click directly in the plot to allocate tokens to time intervals.
                            Click just below the line at 0 on the vertical axis to clear an interval.
                            Note that in the empty intervals are not assumed to have a 0 chance of containing the quantity of interest: probabilities from adjacent non-empty intervals will be smoothed out over the empty intervals."),
                  fluidRow(
                          column(4, downloadButton('downloadRoulette',"Download plot")),
                          column(4, downloadButton('downloadRouletteCSV',"Download allocation (csv)"))),
                  fluidRow(
                          conditionalPanel(condition = "false",
                            column(1, offset = 1, actionButton("exit", "Quit"))
                          )
                  )
              ),

              tabPanel("Distribution Fitting and Feedback", value="PDF",

                  checkboxInput("selectDistribution", label = "Select Custom Distribution"),
                  fluidRow(
                    conditionalPanel(condition = "input.selectDistribution == true",
                      column(4, selectInput("dist", label = "Distribution",
                                choices =  list('Best fitting' = "best",
                                                # Histogram = "hist",
                                                Normal = "normal",
                                                'Student-t' = "t",
                                                'Skew normal' = "skewnormal",
                                                Gamma = "gamma",
                                                'Log normal' = "lognormal",
                                                'Log Student-t' = "logt",
                                                Beta = "beta",
                                                'Mirror gamma' = "mirrorgamma",
                                                'Mirror log normal' = "mirrorlognormal",
                                                'Mirror log Student-t' = "mirrorlogt"#,
                                                #'Natural Cubic Spline' = "NS",
                                                #'Monotonic P-spline' = "MP"
                      ))),

                    conditionalPanel(condition = "input.dist == 't' || input.dist == 'logt' || input.dist == 'mirrorlogt'",
                      column(4,numericInput("tdf", label = "Student-t degrees of freedom", value = 10))
                  ))),

                plotOutput("distPlot"),
                  wellPanel(

                    fluidRow(

                      column(width = 7,
                        h5(tags$b("Feedback:")),
                        htmlOutput("quantileValuesText"),
                      ),
                      column(width = 5,
                        h5(tags$b("Post-elicitation notes:")),
                        textAreaInput( inputId = "user_notes", label   = NULL, width   = "100%", height  = "200px", placeholder = "Add notes on how the information from the portfolio of evidence informed your judgments and distribution fit here…")
                    ))),

                    fluidRow(

                      column(4, downloadButton('downloadDensities',"Download plot")),
                    #  conditionalPanel(condition="false",
                    #    column(4, uiOutput("setPDFxaxisLimits"))
                    #  ),

                      column(4, downloadButton(outputId = "download_notes", label = "Save notes (.txt)"))
              )),
              tabPanel("Cumulative Distibution Function", value="CDF",plotOutput("cdf"),
                  fluidRow(
                    column(4, downloadButton('downloadCDF', "Download plot")),
                    column(3,uiOutput("setCDFxaxisLimits")) )

              ),

             #tabPanel("Help", includeHTML(system.file("shinyAppFiles", "help.html", package="SHELF"))

          selected = "tokens"
        )
      )
    )
  )
)

  # Server ----

server <- function(input, output,session) {

    is_loading <- reactiveVal(FALSE)
    pending_dat <- reactiveVal(NULL)

    observeEvent(input$load_rds, {

      req(input$load_rds)

      # Reset conditions
      #rl$chips <- rl$chips <- rep(0, nBins())
      is_loading(TRUE)

      path <- input$load_rds$datapath

      # Assign data based on file extension
      dat <- if (grepl("\\.json$", path, ignore.case = TRUE)) {
        jsonlite::fromJSON(path)
      } else if (grepl("\\.rds$", path, ignore.case = TRUE)) {
        readRDS(path)
      } else{
        NULL
      }

      if(is.null(dat)){
        csv_file<-read.csv(path,row.names=1)
        csv_file<-as.list(as.data.frame(t(csv_file)))
        csv_file$chips<-as.numeric(unlist(strsplit(csv_file$chips, ",")))
        csv_file$startDate<- as.numeric(csv_file$startDate)
        csv_file$endDate<- as.numeric(csv_file$endDate)
        dat<-csv_file
      }

      # --- basic validation ---
      validate(
        need(is.list(dat), "Invalid file"),
      )

      existing_bins<-isolate(nBins())#
      pending_dat(dat)
      rl$chips <- rep(0, dat$nBins)

      updateCheckboxInput(session, "customiseGraph", value = TRUE)
      updateNumericInput(session, "nBins", value = as.numeric(dat$nBins))

      # --- restore metadata inputs ---
      updateTextInput(session, "Expert",      value = dat$expert      %||% "")
      updateTextInput(session, "Facilitator", value = dat$facilitator %||% "")
      updateTextInput(session, "FindType",    value = dat$findtype    %||% "")
      updateTextInput(session, "EoI",         value = dat$EoI         %||% "")
      updateTextInput(session, "UFI",         value = dat$UFI         %||% "")
      updateTextInput(session, "ULI",         value = dat$ULI         %||% "")
      updateTextInput(session, "USI",         value = dat$USI         %||% "")
      updateTextInput(session, "PoE",         value = dat$PoE         %||% "")
      updateTextAreaInput(session, "user_notes", value = dat$notes %||% "")

      if (!is.null(dat$startDate)){
          updateNumericInput(session, "startDate", value = abs(dat$startDate))
          updateSelectInput(session, "sdate", selected = ifelse(dat$startDate<1,"bce","ce"))

      }

      if (!is.null(dat$endDate)){
          updateNumericInput(session, "endDate", value = abs(dat$endDate))
          updateSelectInput(session, "edate", selected = ifelse(dat$endDate<1,"bce","ce"))

      }

      updateNumericInput(session, "nBins", value = as.numeric(dat$nBins))

      if(dat$selected_distribution!="best"){
        updateCheckboxInput(session, "selectDistribution", value = TRUE)
        updateSelectInput(session, "dist",
                                             selected = dat$selected_distribution)
      }else{
        updateCheckboxInput(session, "selectDistribution", value = FALSE)
        updateSelectInput(session, "dist",
                          selected = "best")
      }


      session$onFlushed(function() {
        x  <- dat$chips
        nb <- dat$nBins

        length(x) <- nb
        x[is.na(x)] <- 0

        rl$chips <- x
        rl$allBinsPr <- cumsum(x)/sum(x)
        rl$nonEmpty <- cumsum(x)/sum(x) > 0 & cumsum(x)/sum(x) < 1


        if(existing_bins!= nb){pause(TRUE)}
        is_loading(FALSE)
      }, once = TRUE
      )
    })

    output$start_date_container <- renderUI({
      current_val <- if (!is.null(input$startDate)) input$startDate else 8000
      if (sum(rl$chips) == 0) {
        # If chips are gone, show the ACTUAL working date input
        numericInput("startDate", "Earliest Date", value = current_val)
      } else {
        # If chips are present, show a "fake" version that doesn't work
        div(
          style = "position: relative; display: inline-block;",
          div( style = "opacity: 0.5; cursor: not-allowed;", # Make it look grey/locked
          numericInput("startDate_disabled", "Earliest Date", value = current_val)
        ),
        actionLink("date_click_trap", "",
                    style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 10;")
        )
      }
    })

    output$end_date_container <- renderUI({

      current_val <- if (!is.null(input$endDate)) input$endDate else 2025
      if (sum(rl$chips) == 0) {
        # If chips are gone, show the ACTUAL working date input
        numericInput("endDate", "Latest Date", value = current_val)

      } else {
        # If chips are present, show a "fake" version that doesn't work
        div(
          style = "position: relative; display: inline-block;",
          title = "Reset tokens to edit this field",
          div( style = "opacity: 0.5; cursor: not-allowed;", # Make it look grey/locked
               numericInput("endDate_disabled", "Latest Date", value = current_val)
          ),
          actionLink("date_click_trap", "",
                     style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 10;")
        )

      }
    })

    output$sdate_container <- renderUI({

      current_val <- if (!is.null(input$sdate)) input$sdate else "bce"
      if (sum(rl$chips) == 0) {
        # If chips are gone, show the ACTUAL working date input
        selectInput("sdate", label = "\u00A0", width = "100px",
                     choices = list(BCE = "bce", CE = "ce"),
                     selected = current_val)

      } else {
        # If chips are present, show a "fake" version that doesn't work
        div(
          style = "position: relative; display: inline-block; width: 100px;",
          div(style = "pointer-events: none; opacity: 0.5;",
              selectInput("sdate_disabled", label = "\u00A0", width = "100px",
                          choices = list(BCE = "bce", CE = "ce"),
                          selected = current_val)
          ),
          actionLink("sdate_click_trap", "",
                     style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 10;")
        )

      }
    })

    output$edate_container <- renderUI({

      current_val <- if (!is.null(input$edate)) input$edate else "ce"
      if (sum(rl$chips) == 0) {
        # If chips are gone, show the ACTUAL working date input
        selectInput("edate", label = "\u00A0", width = "100px",
                    choices = list(BCE = "bce", CE = "ce"),
                    selected = current_val)

      } else {
        # If chips are present, show a "fake" version that doesn't work
        div(
          style = "position: relative; display: inline-block; width: 100px;",
          div(style = "pointer-events: none; opacity: 0.5;",
              selectInput("edate_disabled", label = "\u00A0", width = "100px",
                          choices = list(BCE = "bce", CE = "ce"),
                          selected = current_val)
          ),
          actionLink("sdate_click_trap", "",
                     style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 10;")
        )

      }
    })

    output$checkbox_container <- renderUI({

        current_check <- isolate({
            if (!is.null(input$customiseGraph)) input$customiseGraph else FALSE
        })

        if (sum(rl$chips) == 0) {
            # --- ACTIVE STATE ---
            checkboxInput("customiseGraph", label = "Customise the number of bins",
                          value = current_check)

        } else {
            # --- INACTIVE STATE ---
            div(
              style = "position: relative; display: inline-block;",
              # The Visual Fake
              div(style = "pointer-events: none; opacity: 0.5;",
                  checkboxInput("customiseGraph_disabled", label = "Customise the number of bins",
                                value = current_check)
              ),
              # The Click Trap (covers both the box and the text label)
              actionLink("check_click_trap", "",
                         style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 10;")
            )
          }
    })

    output$nBins_ui <- renderUI({

      # 1. Capture the current value so it doesn't disappear when locking/unlocking
      # We use isolate so the numericInput doesn't trigger its own re-render
      current_bins <- isolate({
        if (!is.null(input$nBins)) input$nBins else 10 # 10 is a fallback default
      })

      if (sum(rl$chips) == 0) {
        # --- ACTIVE STATE ---
        # This is the real input your code already uses
        numericInput("nBins", label = "Number of bins", value = current_bins, min = 3)

      } else {
        # --- INACTIVE STATE ---
        # We use a different ID here ("nBins_locked") to avoid the crash loop
        div(
          style = "opacity: 0.5; cursor: not-allowed;",
          title = "Reset tokens to change the number of bins", # Tooltip
          div(style = "pointer-events: none;",
              numericInput("nBins_locked", label = "Number of bins", value = current_bins, min = 3)
          )
        )
      }
    })

    observe({
      updateTextInput(session,"QoI",value = build_qoi(input$EoI, input$FindType, input$UFI, input$ULI,input$USI)
      )
    })

    observeEvent(input$nTokens, {
          req(input$nTokens)  # Ensure input is not NULL
          if (input$nTokens < 1) {
            updateNumericInput(session, "nTokens", value = input$nTokens*-1)  # Reset to positive number
          }

          updateNumericInput(session, "tokenProb", value = round(1 / input$nTokens,3)) }, ignoreInit = TRUE)

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

    prevStartDate <- reactiveVal()

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
    })

    # Feedback quantiles ----
    fq <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$fq, ")"))),
               error = function(e){NULL})

    })

    # Feedback probabilities. Needs to know parameter limits ----
    output$feedbackProbabilities <- renderUI({
      textInput("fp", label = h5("Feedback probabilities (use negative values for BCE)"),
                paste(c(startDate(),endDate()), collapse = ", "))
    })

    output$hdrProbability <- renderUI({
      textInput("hdr", label = h5("Highest Probability Dates"), "10")
    })
    fp <- reactive({
      tryCatch(
        eval(parse(text = paste("c(", input$fp, ")"))
             ),
               error = function(e){NULL})
    })

    hdr <- reactive({
      tryCatch(eval(parse(text = paste(input$hdr))),
               error = function(e){NULL})

    })

    # Axes limits for pdf/cdf plots. Needs to know parameter limits ----
    #output$setPDFxaxisLimits <- renderUI({
    #  textInput("xlimPDF", label = h5("x-axis limits"),
    #            paste(c(startDate(),endDate()), collapse = ", "))
    #})

    #xlimPDF <- reactive({
    #  tryCatch(eval(parse(text = paste("c(", input$xlimPDF, ")"))),
    #           error = function(e){NULL})
    #})

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

    nBins <- reactiveVal()  # Reactive value to store nBins
    pause <-reactiveVal(FALSE)
    unpause <-reactiveVal(FALSE)
    pause_override<-reactiveVal(FALSE)

    # When checkbox is checked, use input$nBins
    observeEvent(input$customiseGraph, {
      if (input$customiseGraph==TRUE) {
        if(is.null(input$nBins)|is.na(input$nBins)){
          updateNumericInput(session, "nBins", value = nBins())
        }
        if(pause()!=TRUE){
          nBins(input$nBins)  # Set nBins to user input when the checkbox is checked
        }
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
    }, ignoreInit = TRUE)

    observeEvent(input$reset_tokens, {
      # Assign your specific values here
      rl$chips <- rl$chips <- rep(0, nBins())

    })

    # Automatically update nBins when the checkbox is unchecked
    observe({
      if (!isTRUE(input$customiseGraph)) {  # Only run when unchecked
        req(nIntervals(), distributionRange(),startDate(),endDate())  # Ensure required values exist
        updateBins()  # Call function to update bins dynamically
        rl$chips <- rep(0, nBins())
      }
    })

    # Helper function to update bins dynamically when in auto mode
    updateBins <- function() {
      if (input$customiseGraph==TRUE) {
        return()}else{# Skip if in custom mode
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
    }

    gridHeight <- reactive({
      req(input$gridHeight)
      if(is.integer(input$gridHeight) & input$gridHeight > 0){
        return(input$gridHeight)}else{
          return(NULL)}

    })

    bin.width <- reactive({
      req(startDate(),endDate(), nBins())
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
      pa <- isolate(pause())
      up <-isolate(unpause())
      po <- isolate(pause_override())

      if(pa!=TRUE & up!=TRUE){
      rl$chips <- rep(0, nBins())  # Update rl$chips with the new number of bins
      }
      else if(pa==TRUE & up==TRUE){
        isolate({
        pause(FALSE)
        unpause(FALSE)
        })
      }
    })

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

    observeEvent(input$limits,{
      req(nBins())
      if(is.integer(nBins()) & nBins() > 0){
        rl$chips <- rep(0, nBins())}
    })

    observeEvent(input$startDate,{
      req(nBins(),rl$chip==0)
      if(is.integer(nBins()) & nBins() > 0 & pause()!=TRUE&pause_override()!=TRUE){
        rl$chips <- rep(0, nBins())}
    })

    observeEvent(input$endDate,{
      req(nBins())
      if(is.integer(nBins()) & nBins() > 0 & pause()!=TRUE&pause_override()!=TRUE){
        rl$chips <- rep(0, nBins())}
      if(pause_override()==TRUE){
          pause_override(FALSE)
        }else if(pause()==TRUE){
         unpause(TRUE)
      }
    })

    # Fit distributions to elicited judgements ----
    myfit <- reactive({
      req(startDate(),endDate(), v(), p(), input$tdf,trueStart(),trueEnd())
      check <- checkJudgementsValid(probs = p(), vals = v(),
                           tdf = input$tdf,
                           lower = startDate(),
                           upper= endDate()
                                   )
      if(check$valid == TRUE & is_loading()==FALSE & pause()==FALSE){
       return(fitdist(vals = v(), probs = p(), lower = trueStart(), upper = trueEnd(),tdf = input$tdf))
      }else{
        return(check$error)}
    })

    # All plots have separate functions, so can be called from
    # both renderPlot, and from ggsave() for downloading

    plotPDF <- function(){
     req(myfit(), fq(), input$fs,gridHeight(),input$tabs)#,quantileValues())
      if(input$tabs == "PDF"){
        dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best","NS","MP")
        if(input$selectDistribution){
          suppressWarnings(plotfit(myfit(), d = input$dist,
                                    ql = fq()[1], qu = fq()[2],
                                   xl = startDate(), xu = endDate(),
                                   # xl = xlimPDF()[1], xu = xlimPDF()[2],
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
                                  xl = startDate(), xu = endDate(),
                                 #xl = xlimPDF()[1], xu = xlimPDF()[2],
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
                  showFittedCDF = ifelse(input$tabs == "CDF", TRUE, FALSE),
                  showQuantiles = TRUE,
                  ql = fq()[1],
                  qu = fq()[2],
                  xaxisLower = xL,
                  xaxisUpper = xU,
                  xlab = input$xLabel,min_val=trueStart(),max_val=trueEnd())

    }

    output$cdf <- renderPlot({
      if(class(myfit())=="character"){
        plotPDF()
      }else{
      plotCDF()
      }
    })

    plotRoulette <- function(){


      req(startDate(),endDate(), input$fs, nBins(), gridHeight(), bin.left(),
          bin.right(),rl,input$colour)


      xticks<-seq(startDate(),endDate(), length.out=nBins()+1)

      convert_year <- function(year) {
          if (year < 1) year-1 else year
      }

       new_ticks<-sapply(xticks,convert_year)
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

        lines(c(bin.right()[nBins()],bin.right()[nBins()]), c(0, plotHeight),lty=3,col=8)

        for(i in 1:plotHeight){
          lines(c(startDate(),endDate()),
              c(i,i), lty=3,col=8)
        }


        #if(length(rl$chip)==nBins()){
          for(i in 1:nBins()){
            if(rl$chips[i]>0){
              rect(rep(bin.left()[i],rl$chips[i]),c(0:(rl$chips[i]-1)),
                 rep(bin.right()[i],rl$chips[i]),c(1:rl$chips[i]),col=input$colour)
            }
          }
        #}
    }

    output$roulette <- renderPlot({
      plotRoulette()
    })

    # Feedback - get fitted quantiles/probabilities ----

    quantileValues <- reactive({
      req(fq(), myfit())

      if(min(fq())<=0 | max(fq())>=1|class(myfit())=="character"){
        return(NULL)
      }else{

        FB <- feedback(myfit(), quantiles = fq(), ex = 1)


        if(input$selectDistribution&&input$dist != "best"){
            values <- FB$fitted.quantiles[, input$dist]

        }else{
            values <- FB$fitted.quantiles[, as.character(myfit()$best.fitting[1,1])]

        }

        return(data.frame(quantile=fq(), year=to.BCECE(values,toText=TRUE)))
      }

    })

    customQuantileValues <- reactive({
      req(myfit())
      fqc<-c(0.1,0.9,0.5)

      if(min(fqc)<=0 | max(fqc>=1)|class(myfit())=="character"){
        return(NULL)
      }else{

      FB <- feedback(myfit(),
                       quantiles = fqc,
                       ex = 1)

        if(input$selectDistribution&&input$dist != "best"){
           values <- FB$fitted.quantiles[, input$dist]
        }else{

            values <- FB$fitted.quantiles[, as.character(myfit()$best.fitting[1,1])]

        }
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
        if(input$selectDistribution&&input$dist != "best"){
             probs <- FB$fitted.probabilities[, input$dist]

        }else{
          probs <- FB$fitted.probabilities[,
                                        as.character(myfit()$best.fitting[1,
                                                                          1])]
        }
      df<-data.frame(probs=probs,vals=c(left_bin,right_bin))
      return(df)


    })

    customHDR<- reactive({
      req(myfit())


          HDR <- feedback(myfit(),
                       quantiles = c(0.1,0.9),
                       ex = 1,hdr.prob=10)

        if(input$selectDistribution&&input$dist != "best"){
             hdr <- HDR$fitted.hdr[[input$dist]]
        }else{
          hdr <- HDR$fitted.hdr[[as.character(myfit()$best.fitting[1,1])]]
        }

      df<-t(as.data.frame(hdr))

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

      fp_tmp<-to.BCECE(fp()[fp()!=0],reverse=TRUE)

      if(class(myfit())=="character"){
        return(NULL)
      }else{
        FB <- feedback(myfit(),
                       values =fp_tmp,
                       ex = 1)

        if(input$selectDistribution&&input$dist != "best"){
        probs <- FB$fitted.probabilities[, input$dist]
        }else{
            probs <- FB$fitted.probabilities[,
                                           as.character(myfit()$best.fitting[1,
                                                                             1])]

        }

        return(data.frame(year=to.BCECE(fp_tmp, toText=TRUE), probability = probs))

      }
    })

    hdrValues <- reactive({
      req(hdr(), myfit())

      if(class(myfit())=="character"){
        return((NULL))}else{
        HDR <- feedback(myfit(), quantiles = c(0.1,0.9),ex = 1,hdr.prob=hdr())

          if(input$selectDistribution&&input$dist != "best"){
               hdreg <- HDR$fitted.hdr[[input$dist]]
          }else{
            hdreg <- HDR$fitted.hdr[[as.character(myfit()$best.fitting[1,1])]]
          }
        df<-t(as.data.frame(hdreg))
        df<-t(as.data.frame(apply(df,2,to.BCECE,toText=TRUE)))

        return(df)
        }
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
        grDevices::png(file)
        plotPDF()
        grDevices::dev.off()
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

          inputs <- c(input$USI, input$ULI, input$UFI, input$Expert, input$Date)
          valid_inputs <- inputs[!is.na(inputs) & inputs != "" & !is.null(inputs)]
          name <- if (length(valid_inputs) > 0) {
            paste(valid_inputs, collapse = "_")
          } else {
            paste0("elicited_date_",Sys.Date())
          }
        switch(input$exportFormat,
               r_file = paste0(name, ".rds"),
              json_file = paste0(name, ".json"),
              csv_file = paste0(name, ".csv"))
    },

    content = function(file) {

      fit <- myfit()
      distr <- ifelse(input$dist=="best",fit$best.fitting$best.fit,input$dist)
      dist_params <- fit[[which(colnames(fit$ssq)==distr)]]

      param_string<-paste(paste(paste('"',colnames(dist_params),'"',sep=""),paste('"',dist_params,'"',sep=""),sep=":"),collapse=",\n")

      if(grepl(".rds",file)==TRUE){


      elicited_date <- list(date = input$Date,
                            expert = input$Expert,
                            facilitator = input$Facilitator,
                            findtype= input$FindType,
                            EoI= input$EoI,
                            UFI = input$UFI,
                            ULI = input$ULI,
                            USI = input$USI,
                            PoE = input$PoE,
                            startDate=startDate(),
                            endDate=endDate(),
                            nBins=ifelse(input$customiseGraph && !is.null(input$nBins),input$nBins,nBins()),
                           customisedBins=input$customiseGraph,
                           chips = rl$chips,
                           selected_distribution=distr,
                           notes = input$user_notes
      )
      elicited_date<-c(elicited_date,dist_params)
      saveRDS(elicited_date, file)
      }else if(grepl(".json",file)==TRUE){
        writeLines(paste0('{"date":"', input$Date,
                          '",\n"expert":"', input$Expert,
                          '",\n"facilitator":"', input$Facilitator,
                          '",\n"findtype":"', input$FindType,
                          '",\n"EoI":"', input$EoI,
                          '",\n"UFI":"', input$UFI,
                          '",\n"ULI":"', input$ULI,
                          '",\n"USI":"', input$USI,
                          '",\n"PoE":"', input$PoE,
                          '",\n"startDate":', startDate(),
                          ',\n"endDate":', endDate(),
                          ',\n"nBins":', ifelse(input$customiseGraph && !is.null(input$nBins),input$nBins,nBins()),
                          ',\n"chips":[', paste(rl$chips,collapse=","),
                          '],\n"selected_distribution":"', distr,
                          '",\n"notes":"', input$user_notes,
                          '",\n',param_string,
                          '}'
                          )
          ,file)

      }else if(grepl(".csv",file)==TRUE){
        df<-data.frame(date = input$Date,
        expert = input$Expert,
        facilitator = input$Facilitator,
        findtype= input$FindType,
        EoI= input$EoI,
        UFI = input$UFI,
        ULI = input$ULI,
        USI = input$USI,
        PoE = input$PoE,
        notes = input$user_notes,
        startDate=startDate(),
        endDate=endDate(),
        nBins=ifelse(input$customiseGraph && !is.null(input$nBins),input$nBins,nBins()),
        customisedBins=input$customiseGraph,
        chips = paste(rl$chips,collapse=","),
        selected_distribution=distr)
        df<-as.data.frame(t(cbind(df,dist_params)))
        colnames(df)<-c("elicited_date")
        write.csv(df,file)

      }
      }
  )

    output$download_notes <- downloadHandler(
      filename = function() {
        paste0("notes_", Sys.Date(), ".txt")
      },
      content = function(file) {
        writeLines(
          text = input$user_notes %||% "",
          con  = file,
          useBytes = TRUE
        )
      }
    )



    # Download R Markdown report
    output$report <- downloadHandler(
      filename = function(){
        inputs <- c(input$USI, input$ULI, input$UFI, input$Expert, input$Date)
        valid_inputs <- inputs[!is.na(inputs) & inputs != "" & !is.null(inputs)]
        name <- if (length(valid_inputs) > 0) {
          paste(valid_inputs, collapse = "_")
        } else {
          paste0("elicited_date_",Sys.Date())
        }
        switch(input$outFormat,
                                   pdf_document = paste0(name,".pdf"),
                                   html_document = paste0(name,".html"),
                                   word_document = paste0(name,".docx"))},
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

       # Check selected distribution
        if(input$selectDistribution){
            selected_distribution<-input$dist}
        else{
            selected_distribution<-"best"}
          params <- list(fit = myfit(),
                         ql = fq()[1],
                         qu = fq()[2],
                         xl = trueStart(),
                         xu = trueEnd(),
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
                         user_notes = input$user_notes,
                         expert = input$Expert,
                         date = input$Date,
                         facilitator = input$Facilitator,
                         findtype= input$FindType,
                         EoI= input$EoI,
                         UFI = input$UFI,
                         ULI = input$ULI,
                         USI = input$USI,
                         PoE=input$PoE)
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
