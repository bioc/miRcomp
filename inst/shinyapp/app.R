server <- shinyServer(function(input, output, session) {

  data(list=data(package="miRcomp")$results[,"Item"], package="miRcomp")
  
  makeCustom <- reactive({
    inFileCT <- input$ctElements
    if (is.null(inFileCT)) return(NULL)
    tmp <- read.csv(inFileCT$datapath, header=TRUE)
    ct <- as.matrix(tmp[,-1])
    rownames(ct) <- tmp[,1]
    colnames(ct) <- gsub(".", ":", colnames(ct), fixed=TRUE)
    
    inFileQC <- input$qcElements
    if (is.null(inFileQC)) return(NULL)
    tmp <- read.csv(inFileQC$datapath, header=TRUE)
    qc <- as.matrix(tmp[,-1])
    rownames(qc) <- tmp[,1]
    colnames(qc) <- gsub(".", ":", colnames(qc), fixed=TRUE)
    
    return(list(ct=ct, qc=qc))
  })
  
  setCustom<-reactive({
    if(input$chooseFirstMethod=="custom"||input$chooseSecondMethod=="custom"){
      custom<<-makeCustom()
    }
  })
  
  createNone<-reactive({
    if(input$chooseSecondMethod=="none"){
      none<<-NULL
    }
  })
  
  plotLoD<-function(){
    setCustom()
    limitOfDetection(object=get(input$chooseFirstMethod), 
                     qcThreshold=quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdA, na.rm=TRUE), 
                     plotType=(input$plotTypes))
  }
  
  plotAccuracy<-function(){
    setCustom()
    createNone()
    if(!is.null(get(input$chooseSecondMethod))){
      qcThresh2 <- quantile(get(input$chooseSecondMethod)$qc, input$qcThreshold2B, na.rm=TRUE)
    } else qcThresh2 <- NULL
    accuracy(object1=get(input$chooseFirstMethod), 
             qcThreshold1=quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdB, na.rm=TRUE),
             object2=get(input$chooseSecondMethod), 
             qcThreshold2=qcThresh2,
             commonFeatures=input$commonFeaturesB, label1=input$chooseFirstMethod, label2=input$chooseSecondMethod)
  }
  
  plotPrecision<-function(){
    setCustom()
    createNone()
    if(!is.null(get(input$chooseSecondMethod))){
      qcThresh2 <- quantile(get(input$chooseSecondMethod)$qc, input$qcThreshold2C, na.rm=TRUE)
    } else qcThresh2 <- NULL
    precision(object1=get(input$chooseFirstMethod),
              qcThreshold1=quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdC, na.rm=TRUE),
              object2=get(input$chooseSecondMethod), 
              qcThreshold2=qcThresh2,
              commonFeatures=input$commonFeaturesC,
              statistic=input$statistic, scale=input$scale, label1=input$chooseFirstMethod, label2=input$chooseSecondMethod)
  }
  
  plotTitrationResponse<-function(){
    setCustom()
    createNone()
    if(!is.null(get(input$chooseSecondMethod))){
      qcThresh2 <- quantile(get(input$chooseSecondMethod)$qc, input$qcThreshold2E, na.rm=TRUE)
    } else qcThresh2 <- NULL
    titrationResponse(object1=get(input$chooseFirstMethod), 
                      qcThreshold1=quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdE, na.rm=TRUE),
                      object2=get(input$chooseSecondMethod), 
                      qcThreshold2=qcThresh2, 
                      commonFeatures=input$commonFeaturesE)
  }
  
  plotQualityAssessment<-function(){
    setCustom()
    createNone()
    qualityAssessment(object1=get(input$chooseFirstMethod),
                      object2=get(input$chooseSecondMethod), plotType=input$qPlotType,
                      label1=input$chooseFirstMethod, label2=input$chooseSecondMethod)
  }
  
  output$LoD<-renderPlot({
    plotLoD()
  })
  
  output$A<-renderPlot({
    plotAccuracy()
  })
  
  output$P<-renderPlot({
    plotPrecision()
  })
  
  output$Qa<-renderPlot({
    plotQualityAssessment()
  })
  
  output$Tr<-renderPlot({
    plotTitrationResponse()
  })
  
  output$text1<-renderText({
    paste("This corresponds to a qcThreshold of:", round(quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdA, na.rm=TRUE), digits=4))
  })
  
  output$text2<-renderText({
    paste("This corresponds to a qcThreshold of:", round(quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdB, na.rm=TRUE), digits=4))
  })
  
  output$text3<-renderText({
    paste("This corresponds to a qcThreshold of:", round(quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdC, na.rm=TRUE), digits=4))
  })
  
  output$text4<-renderText({
    paste("This corresponds to a qcThreshold of:", round(quantile(get(input$chooseFirstMethod)$qc, input$qcThresholdE, na.rm=TRUE), digits=4))
  })
  
  output$text5<-renderText({
    if(!is.null(get(input$chooseSecondMethod))){
      paste("This corresponds to a qcThreshold of:", round(quantile(get(input$chooseSecondMethod)$qc, input$qcThreshold2B, na.rm=TRUE), digits=4))
    } else paste("")
  })
  
  output$text6<-renderText({
    if(!is.null(get(input$chooseSecondMethod))){
      paste("This corresponds to a qcThreshold of:", round(quantile(get(input$chooseSecondMethod)$qc, input$qcThreshold2C, na.rm=TRUE), digits=4))
    } else paste("")
  })
  
  output$text7<-renderText({
    if(!is.null(get(input$chooseSecondMethod))){
      paste("This corresponds to a qcThreshold of:", round(quantile(get(input$chooseSecondMethod)$qc, input$qcThreshold2E, na.rm=TRUE), digits=4))
    } else paste("")
  })
  
  output$lodText<-renderText({
    paste("You are currently plotting this method:", input$chooseFirstMethod, ". To change this, select the method you would like to plot as your first method.")
  })
  
})

ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("miRcomp"),
  
  # Sidebar with a slider input for qcThreshold
  sidebarLayout(
    
    sidebarPanel(
      #       #Choices for first method for comparison 
      #       selectInput("chooseFirstMethod", label=("Choose first method to compare:"),
      #                   choices=list("qpcRDefault"="qpcRdefault", "Lifetech"="lifetech", "Custom"="custom"), selected=1),
      #       
      #       #Choices for next method for comparison 
      #       selectInput("chooseSecondMethod", label=("Choose second method to compare:"),
      #                   choices=list("None"="none", "qpcRDefault"="qpcRdefault", "Lifetech"="lifetech", "Custom"="custom"
      #                                ), selected=1),
      
      selectInput("chooseFirstMethod", label=("Choose first method to compare:"),
                  choices=c(data(package="miRcomp")$results[,"Item"],"custom")),
      
      selectInput("chooseSecondMethod", label=("Choose second method to compare:"),
                  choices=c(data(package="miRcomp")$results[,"Item"],"custom","none")),
      
      conditionalPanel(condition="input.chooseSecondMethod=='custom'||input.chooseFirstMethod=='custom'",
                       
                       fileInput('qcElements', 'Upload qc elements',
                                 accept=c('text/csv, values-, text/plain')),
                       fileInput('ctElements', 'Upload ct elements',
                                 accept=c('text/csv, values-, text/plain'))
      )
    ),
    
    
    mainPanel(align="center",
              
              tabsetPanel(id="tabs",
                          tabPanel("Limit of Detection", value="A", plotOutput("LoD")),
                          tabPanel("Accuracy", value="B", plotOutput("A")),
                          tabPanel("Precision", value="C", plotOutput("P")),
                          tabPanel("Quality Assessment", value="D", plotOutput("Qa")),
                          tabPanel("Titration Response", value="E", plotOutput("Tr"))
                          
              ),
              
              conditionalPanel("input.tabs=='A'",
                               fluidRow(
                                 textOutput("lodText"),
                                 column(4,
                                        sliderInput("qcThresholdA",
                                                    label="Percentage of data to exclude from method:",
                                                    min=0, max=1.0, value=c(0.00)),
                                        textOutput("text1")
                                 ),
                                 column(4, offset = 1,
                                        radioButtons("plotTypes", "Select plot type:",
                                                     c("boxplot"="boxplot",
                                                       "scatterplot"="scatterplot",
                                                       "MAplot"="MAplot"
                                                     ))  )
                               )
              ),
              
              conditionalPanel(condition="input.tabs=='B'",
                               fluidRow(
                                 column(4,
                                        sliderInput("qcThresholdB",
                                                    label="Percentage of data to exclude from first method:",
                                                    min=0, max=1.0, value=c(0.00)),
                                        textOutput("text2")
                                 ),
                                 column(4, offset = 1,
                                        conditionalPanel(condition="input.chooseSecondMethod!='none'",
                                                         sliderInput("qcThreshold2B",
                                                                     label="Percentage of data to exclude from second method:",
                                                                     min=0, max=1.0, value=c(0.00)),
                                                         textOutput("text5")
                                        )
                                 ),
                                 column(3,
                                        radioButtons("commonFeaturesB", "Select common features preference",
                                                     c("True"="TRUE", "False"="FALSE"))
                                 )
                               )
                               
                               
              ),
              conditionalPanel(condition="input.tabs=='C'",
                               fluidRow(
                                 column(4,
                                        sliderInput("qcThresholdC",
                                                    label="Percentage of data to exclude from first method:",
                                                    min=0, max=1.0, value=c(0.00)),
                                        textOutput("text3")
                                 ),
                                 column(4, offset = 1,
                                        conditionalPanel(condition="input.chooseSecondMethod!='none'",
                                                         sliderInput("qcThreshold2C",
                                                                     label="Percentage of data to exclude from second method:",
                                                                     min=0, max=1.0, value=c(0.00)),
                                                         textOutput("text6"),
                                                         radioButtons("commonFeaturesC", "Select common features preference",
                                                                      c("True"="TRUE", "False"="FALSE")))
                                        
                                 ),
                                 column(3,
                                        radioButtons("statistic", "Select which statistic you would like to compute:",
                                                     c("standard deviation"="sd",
                                                       "coefficient of variation"="cv")
                                        ),
                                        br(),
                                        radioButtons("scale", "Select which scale you would like to use (if any)",
                                                     c("none"="none",
                                                       "log"="log",
                                                       "log10"="log10"))
                                 )
                               )),
              
              
              conditionalPanel(condition="input.tabs=='D'",
                               fluidRow(
                                 column(4, offset=1,
                                        radioButtons("qPlotType", "Select plot type:",
                                                     c("boxplot"="boxplot",
                                                       "scatterplot"="scatterplot"))
                                 )
                               )),
              
              conditionalPanel(condition="input.tabs=='E'",
                               
                               fluidRow(
                                 column(3,
                                        sliderInput("qcThresholdE",
                                                    label="Percentage of data to exclude from first method:",
                                                    min=0, max=1.0, value=c(0.00)),
                                        textOutput("text4")
                                 ),
                                 
                                 column(4, offset = 1,
                                        conditionalPanel(condition="input.chooseSecondMethod!='none'",
                                                         sliderInput("qcThreshold2E",
                                                                     label="Percentage of data to exclude from second method:",
                                                                     min=0, max=1.0, value=c(0.00)),
                                                         textOutput("text7")
                                        )
                                 ),
                                 column(4,
                                        radioButtons("commonFeaturesE", "Select common features preference",
                                                     c("True"="TRUE", "False"="FALSE"))
                                 )
                               )
                               
              )))))

shinyApp(ui = ui, server = server)


