library(shiny)
library(stm)

options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output, session) {
  
  # reactive object that stores intermediate results
  # not perfect but unsure how else to do this in R
  inFile <- NULL
  storedData <- reactiveValues()
  
  storedData$storeconfirm <- ""
  storedData$data <- NULL
  storedData$textprocess <- NULL
  storedData$prepdocs <- NULL
  storedData$stmresult <- NULL
  
  ##### Input Data #####
  # if input file is null, then return nothing
  # otherwise, display data as a table (csv, tsv only)
  # **TODO**: Expand file types that can be handled, decide various cases for different output
  observeEvent(input$filesettings, ({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    
    storedData$data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    output$data <- renderDataTable({
      storedData$data
    }, options=list(pageLength=10))
  }))
  
  ##### Plot Removed #####
  observeEvent(input$plotPlotRemove, ({
    if (is.null(storedData$textprocess)) {
      output$plotRemoveNoTP <- renderPrint({ "You must successfully run Text Processor!"})
      return(NULL)
    }
    
    plotRevDocs <- storedData$textprocess$documents
    
    output$plotRemovePlot <- renderPlot(
      plotRemoved(plotRevDocs, lower.thresh = seq(from = input$plotLowThresh,
        to = input$plotUpThresh, by = input$plotInterval)
      )
    )
  }))
  
  ##### Text Processor #####
  # processes input to the Text Processor
  # selects gadarian or data that was input by the user
  # **TODO**: clear output before every use  
  
  observe(({
    input$tpTextprocess
    output$textprocresult <- renderPrint(print(" "))
    }), priority = 10)
  
  observeEvent(input$tpTextprocess, ({
    
    if (input$tpGadarian == 1) {
      output$textprocresult <- renderPrint({
        storedData$textprocess <- textProcessor(documents=gadarian$open.ended.response, metadata=gadarian,
          lowercase=input$tpLowercase, removestopwords=input$tpRemovestop,
          removenumbers=input$tpRemovenum, removepunctuation=input$tpRemovepunc,
          stem=input$tpStem, sparselevel=input$tpSparselevel,
          language=input$tpLang, verbose=input$tpVerbose,
          onlycharacter=input$tpOnlychar, striphtml=input$tpStriphtml,
          customstopwords=NULL)
        print("Done processing Gadarian dataset!")
      })
    }
    else {
      if (is.null(storedData$data)){
        output$textprocresult <- renderText({"Enter data!"})
        return(NULL)
      }
      
      docs <- input$tpDocs
      textDocs <- storedData$data[[docs]]
      
      if (!is.null(textDocs)) {
        output$textprocresult <- renderPrint({
          storedData$textprocess <- textProcessor(textDocs, metadata=NULL,
            lowercase=input$tpLowercase, removestopwords=input$tpRemovestop,
            removenumbers=input$tpRemovenum, removepunctuation=input$tpRemovepunc,
            stem=input$tpStem, sparselevel=input$tpSparselevel,
            language=input$tpLang, verbose=input$tpVerbose,
            onlycharacter=input$tpOnlychar, striphtml=input$tpStriphtml,
            customstopwords=NULL)
          if (length(storedData$textprocess$vocab) > 0)
            print("Done processing user data!")
          else {
            print("Text processor did not complete correctly!")
          }
        })
      }
      else {
        output$textprocresult <- renderText({"Try again with column name of document vector!"})
      }
    }
  }), priority = 0)
  
  ##### Prep Documents #####
  # calculates on the output from text processor
  # or calculates on the file uploaded by the user
  # **TODO**: expand functionality for case of user-input file
  observeEvent(input$pdPrepdocs, ({
    if (input$pdChoice == 1) {
      if (is.null(storedData$textprocess)) {
        output$prepdocresults <- renderPrint({ "You must successfully run Text Processor!"})
        return(NULL)
      }

      tpres <- storedData$textprocess
      output$prepdocresults <- renderPrint({
        storedData$prepdocs <- prepDocuments(documents=tpres$documents,
          vocab=tpres$vocab, meta=tpres$meta, lower.thresh=input$pdLowThresh,
          upper.thresh=Inf, verbose=TRUE)
      })
    }
    else {
      if (is.null(storedData$data)){
        output$prepdocresults <- renderText({"You must enter data!"})
        return(NULL)
      }
      
      docs <- input$pdDocs
      textDocs <- storedData$data[[docs]]
      
      if (!is.null(textDocs)) {
        output$prepdocresults <- renderPrint({
          storedData$prepdocs <- prepDocuments(documents=tpres$documents,
            vocab=tpres$vocab, meta=tpres$meta, lower.thresh=input$pdLowThresh,
            upper.thresh=Inf, verbose=TRUE)
          if (length(storedData$textprocess$vocab) > 0)
            print("Done processing user data!")
          else {
            print("Text processor did not complete correctly!")
          }
        })
      }
      else {
        output$prepdocresults <- renderText({"Try again with column name of document vector!"})
      }
    }
  }))
  
  observeEvent(input$stmRun, ({
    if (input$stmChoice == 1) {
#       initParse <- "LDA"
#       prevParse <- NULL
#       contentParse <- NULL
#       emtolParse <- as.double(input$stmEmTol)
#       
#       if (!is.null(input$stmPrev))
#         prevParse <- formula(input$stmPrev)
#       if (!is.null(input$stmContent))
#         contentParse <- formula(input$stmContent)
#       
#       if (input$stmInitType == 2)
#         initParse <- "Random"
#       if (input$stmInitType == 3)
#         initParse <- "Spectral"
      
      pdres <- storedData$prepdocs
      output$stmprocresult <- renderPrint({
        storedData$stmresult <- stm(documents=pdres$documents,
          vocab=pdres$vocab, K=3, prevalence=~treatment + s(pid_rep),
          data=pdres$meta, init.type="LDA",
          seed=94107, max.em.its=10, emtol=0.00001,
          verbose=TRUE)
      })
    }
  }))
  
})