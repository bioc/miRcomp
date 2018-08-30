library(BiocManager)
library(miRcomp)
library(shiny)

server <- shinyServer(function(input, output, session) {
  data(list = data(package = "miRcomp")$results[, "Item"],
       package = "miRcomp")
  data_info <- data(package = "miRcomp")
  makeCustom <- reactive({
    inFileCT <- input$ctElements
    if (is.null(inFileCT))
      return(NULL)
    tmp <- read.csv(inFileCT$datapath, header = TRUE)
    ct <- as.matrix(tmp[, -1])
    rownames(ct) <- tmp[, 1]
    colnames(ct) <- gsub(".", ":", colnames(ct), fixed = TRUE)
    
    inFileQC <- input$qcElements
    if (is.null(inFileQC))
      return(NULL)
    tmp <- read.csv(inFileQC$datapath, header = TRUE)
    qc <- as.matrix(tmp[, -1])
    rownames(qc) <- tmp[, 1]
    colnames(qc) <- gsub(".", ":", colnames(qc), fixed = TRUE)
    
    return(list(ct = ct, qc = qc))
  })
  
  datasetInput <- reactive({
    object = get(input$datasets)
  })
  
  setCustom <- reactive({
    if (input$chooseFirstMethod == "custom" ||
        input$chooseSecondMethod == "custom") {
      custom <<- makeCustom()
    }
  })
  
  createNone <- reactive({
    if (input$chooseSecondMethod == "none") {
      none <<- NULL
    }
  })
  
  plotLoD <- function() {
    setCustom()
    limitOfDetection(
      object = get(input$chooseFirstMethod),
      qcThreshold = quantile(
        get(input$chooseFirstMethod)$qc,
        input$qcThresholdA,
        na.rm = TRUE
      ),
      plotType = (input$plotTypes)
    )
  }
  
  plotAccuracy <- function() {
    setCustom()
    createNone()
    if (!is.null(get(input$chooseSecondMethod))) {
      qcThresh2 <-
        quantile(get(input$chooseSecondMethod)$qc,
                 input$qcThreshold2B,
                 na.rm = TRUE)
      validate(
        need(qcThresh2 < 0.90, "Not enough good quality data")
      )
    } else
      qcThresh2 <- NULL
    accuracy(
      object1 = get(input$chooseFirstMethod),
      qcThreshold1 = quantile(
        get(input$chooseFirstMethod)$qc,
        input$qcThresholdB,
        na.rm = TRUE
      ),
      object2 = get(input$chooseSecondMethod),
      qcThreshold2 = qcThresh2,
      commonFeatures = input$commonFeaturesB,
      label1 = input$chooseFirstMethod,
      label2 = input$chooseSecondMethod
    )
  }
  
  plotPrecision <- function() {
    setCustom()
    createNone()
    if (!is.null(get(input$chooseSecondMethod))) {
      qcThresh2 <-
        quantile(get(input$chooseSecondMethod)$qc,
                 input$qcThreshold2C,
                 na.rm = TRUE)
    } else
      qcThresh2 <- NULL
    precision(
      object1 = get(input$chooseFirstMethod),
      qcThreshold1 = quantile(
        get(input$chooseFirstMethod)$qc,
        input$qcThresholdC,
        na.rm = TRUE
      ),
      object2 = get(input$chooseSecondMethod),
      qcThreshold2 = qcThresh2,
      commonFeatures = input$commonFeaturesC,
      statistic = input$statistic,
      scale = input$scale,
      label1 = input$chooseFirstMethod,
      label2 = input$chooseSecondMethod
    )
  }
  
  plotTitrationResponse <- function() {
    setCustom()
    createNone()
    if (!is.null(get(input$chooseSecondMethod))) {
      qcThresh2 <-
        quantile(get(input$chooseSecondMethod)$qc,
                 input$qcThreshold2E,
                 na.rm = TRUE)
    } else
      qcThresh2 <- NULL
    titrationResponse(
      object1 = get(input$chooseFirstMethod),
      qcThreshold1 = quantile(
        get(input$chooseFirstMethod)$qc,
        input$qcThresholdE,
        na.rm = TRUE
      ),
      object2 = get(input$chooseSecondMethod),
      qcThreshold2 = qcThresh2,
      commonFeatures = input$commonFeaturesE
    )
  }
  
  plotQualityAssessment <- function() {
    setCustom()
    createNone()
    qualityAssessment(
      object1 = get(input$chooseFirstMethod),
      object2 = get(input$chooseSecondMethod),
      plotType = input$qPlotType,
      label1 = input$chooseFirstMethod,
      label2 = input$chooseSecondMethod
    )
  }
  
  
  output$LoD <- renderPlot({
    plotLoD()
  })
  
  output$A <- renderPlot({
    plotAccuracy()
  })
  
  output$P <- renderPlot({
    plotPrecision()
  })
  
  output$Qa <- renderPlot({
    plotQualityAssessment()
  })
  
  output$Tr <- renderPlot({
    plotTitrationResponse()
  })
  
  output$text1 <- renderText({
    paste("This corresponds to a qcThreshold of:",
          round(
            quantile(
              get(input$chooseFirstMethod)$qc,
              input$qcThresholdA,
              na.rm = TRUE
            ),
            digits = 4
          ))
  })
  
  output$text2 <- renderText({
    paste("This corresponds to a qcThreshold of:",
          round(
            quantile(
              get(input$chooseFirstMethod)$qc,
              input$qcThresholdB,
              na.rm = TRUE
            ),
            digits = 4
          ))
  })
  
  output$text3 <- renderText({
    paste("This corresponds to a qcThreshold of:",
          round(
            quantile(
              get(input$chooseFirstMethod)$qc,
              input$qcThresholdC,
              na.rm = TRUE
            ),
            digits = 4
          ))
  })
  
  output$text4 <- renderText({
    paste("This corresponds to a qcThreshold of:",
          round(
            quantile(
              get(input$chooseFirstMethod)$qc,
              input$qcThresholdE,
              na.rm = TRUE
            ),
            digits = 4
          ))
  })
  
  output$text5 <- renderText({
    if (!is.null(get(input$chooseSecondMethod))) {
      paste("This corresponds to a qcThreshold of:",
            round(
              quantile(
                get(input$chooseSecondMethod)$qc,
                input$qcThreshold2B,
                na.rm = TRUE
              ),
              digits = 4
            ))
    } else
      paste("")
  })
  
  output$text6 <- renderText({
    if (!is.null(get(input$chooseSecondMethod))) {
      paste("This corresponds to a qcThreshold of:",
            round(
              quantile(
                get(input$chooseSecondMethod)$qc,
                input$qcThreshold2C,
                na.rm = TRUE
              ),
              digits = 4
            ))
    } else
      paste("")
  })
  
  output$text7 <- renderText({
    if (!is.null(get(input$chooseSecondMethod))) {
      paste("This corresponds to a qcThreshold of:",
            round(
              quantile(
                get(input$chooseSecondMethod)$qc,
                input$qcThreshold2E,
                na.rm = TRUE
              ),
              digits = 4
            ))
    } else
      paste("")
  })
  
  output$figcaption <- renderText({
    paste(
      "Limit of Detection: 
      These boxplots show the average observed expression
      stratified by the proportion of poor quality data points.
      Below each box, the number of unique feature/sample
      type combinations each box contains is reported."
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$datasets, ".csv", sep = "")
    },
    content = function(file) {
      sep <- switch(input$fileFormat, ".csv" = ",", ".tsv" = "\t")
      write.table(datasetInput(), file, sep = sep, row.names = FALSE)
    }
  )
  
  output$accuracyCaption <- renderText({
    paste(
      "Accuracy: To assess accuracy, we calculate the signal detect slope:
      the slope of the regression line of observed expression on
      expected expression for both the selected algorithms.
      The ideal signal detect slope is one, representing agreement between
      observed and expected expression. The signal detect slopes are stratified
      by pure sample expression. Each point represents an miRNA. Points in the
      figures below are grey if the signal detect slope is not statistically
      signficantly different from zero."
    )
  })
  
  output$precisionCaption <- renderText({
    paste(
      "Precision: we calculate the within-replicate coefficient of variation, 
      calculated as the within-replicate standard deviation divided by the 
      within-replicate mean, for both the algorithms selected.
      These are calculated for each set of replicates (unique feature/sample type combination)
      that are of good quality and stratified by the observed expression"
    )
  })
  
  output$qAcaption <- renderText({
    "A direct comparison between the quality scores: Life Technologies (LifeTech)
    AmpScore and qpcR R-squared. The vertical dashed line represents the recommended
    AmpScore threshold of 1.25. The horizontal dashed line represents a potential
    R-squared threshold chosen by examination of this figure. Each point represents
    the quality values for a unique miRNA/sample combination. Two-dimensional scatter-plot
    smoothing is used to avoid over-plotting and convey the distribution of points across
    the plotting region"
  })
  
  output$tRcaption <- renderText({
    "Titration Response. To examine the titration response, we plot
    the proportion of features that show monotone increasing expression
    as the amount of input RNA increases stratified by the difference
    in expression between the sample being titrated and the sample being
    held constant."
  })
  
  output$lodText <- renderText({
    paste(
      "You are currently plotting this method:",
      input$chooseFirstMethod,
      ". To change this, select the method you would like to plot as your first method."
    )
  })

})

ui <- shinyUI(fluidPage(navbarPage(
  "miRcomp-Shiny",
  tabPanel("Main",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "chooseFirstMethod",
                 label = ("Choose first method to compare:"),
                 choices = c(data(package = "miRcomp")$results[, "Item"], "custom")
               ),
               selectInput(
                 "chooseSecondMethod",
                 label = ("Choose second method to compare:"),
                 choices = c(data(package = "miRcomp")$results[, "Item"], "custom", "none")
               ),
               conditionalPanel(
                 condition = "input.chooseSecondMethod=='custom'||input.chooseFirstMethod=='custom'",
                 fileInput(
                   'qcElements',
                   'Upload qc elements',
                   accept = c('text/csv, values-, text/plain')
                 ),
                 fileInput(
                   'ctElements',
                   'Upload ct elements',
                   accept = c('text/csv, values-, text/plain')
                 )
               )
             ),
             mainPanel(
               h3(
                 "Interactive assessment of microRNA quantification and quality control algorithms"
               ),
               align = "center",
               tabsetPanel(
                 id = "tabs",
                 tabPanel("Limit of Detection", value =
                            "A", plotOutput("LoD")),
                 tabPanel("Accuracy", value = "B", plotOutput("A")),
                 tabPanel("Precision", value = "C", plotOutput("P")),
                 tabPanel("Quality Assessment", value =
                            "D", plotOutput("Qa")),
                 tabPanel("Titration Response", value =
                            "E", plotOutput("Tr"))
               ),
               conditionalPanel("input.tabs=='A'",
                                fluidRow(
                                  tags$br(),
                                  textOutput("lodText"),
                                  tags$br(),
                                  textOutput("figcaption"),
                                  tags$br(),
                                  column(
                                    4,
                                    sliderInput(
                                      "qcThresholdA",
                                      label = "Proportion of poor quality data to exclude:",
                                      min = 0.02,
                                      max = 1.0,
                                      value = c(0.00)
                                    ),
                                    textOutput("text1")
                                  ),
                                  column(4, offset = 1,
                                         radioButtons(
                                           "plotTypes",
                                           "Select plot type:",
                                           c(
                                             "boxplot" = "boxplot",
                                             "scatterplot" = "scatterplot",
                                             "MAplot" = "MAplot"
                                           )
                                         ))
                                )),
               conditionalPanel(condition = "input.tabs=='B'",
                                textOutput("accuracyCaption"),
                                tags$br(),
                                fluidRow(
                                  column(
                                    4,
                                    sliderInput(
                                      "qcThresholdB",
                                      label =
                                        "Proportion of poor quality data to exclude from first method:",
                                      min = 0.02,
                                      max = 1.0,
                                      value = c(0.00)
                                    ),
                                    textOutput("text2")
                                  ),
                                  column(
                                    4,
                                    offset = 1,
                                    conditionalPanel(
                                      condition = "input.chooseSecondMethod!='none'",
                                      sliderInput(
                                        "qcThreshold2B",
                                        label =
                                          "Proportion of poor quality data to exclude from second method:",
                                        min =
                                          0.02,
                                        max = 1.0,
                                        value = c(0.00)
                                      ),
                                      textOutput("text5")
                                    )
                                  ),
                                  column(
                                    3,
                                    radioButtons(
                                      "commonFeaturesB",
                                      "Select common features preference",
                                      c("True" =
                                          "TRUE", "False" = "FALSE")
                                    )
                                  )
                                )),
               conditionalPanel(condition = "input.tabs=='C'",
                                textOutput("precisionCaption"), 
                                tags$br(),
                                fluidRow(
                                  column(
                                    4,
                                    sliderInput(
                                      "qcThresholdC",
                                      label =
                                        "Proportion of poor quality data to exclude from first method:",
                                      min = 0.02,
                                      max = 1.0,
                                      value = c(0.02)
                                    ),
                                    textOutput("text3")
                                  ),
                                  column(
                                    4,
                                    offset = 1,
                                    conditionalPanel(
                                      condition = "input.chooseSecondMethod!='none'",
                                      sliderInput(
                                        "qcThreshold2C",
                                        label =
                                          "Proportion of poor quality data to exclude from second method:",
                                        min =
                                          0.02,
                                        max = 1.0,
                                        value = c(0.02)
                                      ),
                                      textOutput("text6"),
                                      radioButtons(
                                        "commonFeaturesC",
                                        "Select common features preference",
                                        c("True" = "TRUE", "False" = "FALSE")
                                      )
                                    )
                                  ),
                                  column(
                                    3,
                                    radioButtons(
                                      "statistic",
                                      "Select which statistic you would like to compute:",
                                      c(
                                        "standard deviation" = "sd",
                                        "coefficient of variation" =
                                          "cv"
                                      )
                                    ),
                                    br(),
                                    radioButtons(
                                      "scale",
                                      "Select which scale you would like to use (if any)",
                                      c(
                                        "none" = "none",
                                        "log" =
                                          "log",
                                        "log10" = "log10"
                                      )
                                    )
                                  )
                                )),
               conditionalPanel(condition = "input.tabs=='D'",
                                textOutput("qAcaption"),
                                tags$br(),
                                fluidRow(column(
                                  4, offset = 1,
                                  radioButtons(
                                    "qPlotType",
                                    "Select plot type:",
                                    c("boxplot" = "boxplot",
                                      "scatterplot" =
                                        "scatterplot")
                                  )
                                ))),
               conditionalPanel(condition = "input.tabs=='E'",
                                textOutput("tRcaption"), 
                                tags$br(),
                                fluidRow(
                                  column(
                                    3,
                                    sliderInput(
                                      "qcThresholdE",
                                      label =
                                        "Proportion of poor quality data to exclude from first method:",
                                      min = 0.02,
                                      max = 1.0,
                                      value = c(0.02)
                                    ),
                                    textOutput("text4")
                                  ),
                                  column(
                                    4,
                                    offset = 1,
                                    conditionalPanel(
                                      condition = "input.chooseSecondMethod!='none'",
                                      sliderInput(
                                        "qcThreshold2E",
                                        label =
                                          "Proportion of poor quality data to exclude from second method:",
                                        min =
                                          0.02,
                                        max = 1.0,
                                        value = c(0.02)
                                      ),
                                      textOutput("text7")
                                    )
                                  ),
                                  column(
                                    4,
                                    radioButtons(
                                      "commonFeaturesE",
                                      "Select common features preference",
                                      c("True" =
                                          "TRUE", "False" = "FALSE")
                                    )
                                  )
                                ))
             )
             
           )),
  tabPanel("Intended Users", 
           h3("Methodology and quality threshold selection"),
           p("A user who is selecting an existing miRNA expression estimation methodology to analyze a data set can use miRcomp-Shiny to evaluate the performance of different methods based on the benchmark dataset."),
           h3("Comparison of novel algorithms with current methods"), 
           p("A user who has developed a new method for miRNA expression estimation can utilize the repository of existing methods in miRcomp-Shiny to compare the performance based on the assessments that are most relevant to their research.")
           
  ),
  
  tabPanel("Dataset Descriptions/ Download", 
           selectInput(
             "datasets",
             label = 'Select Dataset to download',
             choices = data(package = "miRcomp")$results[, "Item"]
           ),
           radioButtons(
             "fileFormat", 
             label= 'Select file format to download data', 
             choices=c(".csv", ".tsv")
           ),
           downloadButton("downloadData", "Download"),
           h3("Lifetech"),
           p("The processed data generated using the LifeTech software."),
           tags$hr(style="border: 0; height: 1px;background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));"),
           
           h3("qpcRb4"),
           p("The processed data generated using the 4 parameter sigmoidal method from the qpcR software."),
           tags$hr(style="border: 0; height: 1px;background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));"),
           
           h3("qpcRb5"),
           p("The processed data generated using the 5 parameter sigmodial method from the qpcR software."),
           tags$hr(style="border: 0; height: 1px;background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));"),
           
           h3("qpcRdefault"),
           p("The processed data generated using the default method (4 parameter log-logistic) implemented in the qpcR software package"),
           tags$hr(style="border: 0; height: 1px;background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));"),
           
           h3("qpcRl5"),
           p("The processed data generated using the 5 parameter log-logistic method from the qpcR software."),
           tags$hr(style="border: 0; height: 1px;background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));"),
           
           h3("qpcRlinexp"),
           p("The processed data generated using the linear-exponential method implemented in the qpcR software package.")
           
    ),
    tabPanel("miRcomp Data",
             h2("Data used in the miRcomp package"),
             tags$hr(style="border: 0; height: 1px;background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));"),
             p("The miRcomp package uses raw amplification data from a large microRNA mixture / dilution study to
               assess the performance of methods from amplification curves. Therefore, it will be necessary to download
              this data to assess the performance of a new method to compare to existing methods hosted on this application. 
               The data can be found at : ", tags$a(href = "https://bioconductor.org/packages/release/data/experiment/html/miRcompData.html","miRcomp Data"))
             ))))

shinyApp(ui = ui, server = server)