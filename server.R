##### stmGUI ####
#
# Server-side logic for handling stm workflows


library(shiny)
library(stm)
# library(data.table)
source("dataUtils.R")

#### TEST CODE HERE ####
# source("shinyUtils.R")
####

options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output, session) {

  #### TEST CODE HERE ####
  #  load("stmResult.RData")
  ####

  # reactive object that stores intermediate results
  storedData <- reactiveValues()

  storedData$data <- NULL
  storedData$textprocess <- NULL
  storedData$prepdocs <- NULL
  storedData$stmresult <- NULL

  ##### Input Data #####
  # if input file is null, then return nothing
  # otherwise, display data as a table (csv, tsv only)
  # **TODO**: Expand file types that can be handled
  observeEvent(input$filesettings, ({
    userData <- input$upDatafile
    if (is.null(userData)) {
      output$dataOutputText <-
        renderPrint({ "You must select a file to upload!"})
      return(NULL)
    }

    # fread or read.csv
    storedData$data <- read.csv(userData$datapath,
                                header=input$header,
                                sep=input$sep,
                                quote=input$quote)
    # fread(userData$datapath)

    output$data <- DT::renderDataTable({
      DT::datatable(storedData$data)
    })
  }))

  ##### Text Processor #####
  # processes input to the Text Processor
  # selects gadarian or data that was input by the user
  # **TODO**: clear output without needing a button

  observeEvent(input$tpClearout, ({
    output$tpResult <- renderPrint({ invisible(NULL) })
  }))

  #### TEST ####
  #  clearInputGivenEvent(input$tpClearout, output$tpResult)
  ####

  observeEvent(input$tpTextprocess, ({

    docsForTp <- NULL
    metadataForTp <- NULL
    dataType <- NULL

    if (input$tpGadarian == 1) {
      docsForTp <- gadarian$open.ended.response
      metadataForTp <- gadarian
      dataType <- "Gadarian"
    }
    else {
      if (is.null(storedData$data)){
        output$tpResult <- renderText({"You must upload data in the Data tab to use this option!"})
        return(NULL)
      }

      docs <- input$tpDocs
      docsForTp <- storedData$data[[docs]]

      metadataForTp <- storedData$data
      dataType <- "user"
    }

    if (!is.null(docsForTp)) {
      # **TODO**: Implement wordLengths, customstopwords, onlytxtfiles
      shinyjs::text("tpResult", "Text Processor is running...")
      withProgress( message = "Running Text Processor...", {
        setProgress(0.5)
        tpOutputRaw <-
          paste(
            capture.output(
              storedData$textprocess <- isolate(
                textProcessor(documents=docsForTp,
                              metadata=metadataForTp,
                              lowercase=input$tpLowercase,
                              removestopwords=input$tpRemovestop,
                              removenumbers=input$tpRemovenum,
                              removepunctuation=input$tpRemovepunc,
                              stem=input$tpStem,
                              wordLengths=c(3, Inf),
                              sparselevel=input$tpSparselevel,
                              language=input$tpLang,
                              verbose=input$tpVerbose,
                              onlycharacter=input$tpOnlychar,
                              striphtml=input$tpStriphtml,
                              customstopwords=NULL,
                              onlytxtfiles=TRUE)
              )
            ), collapse = "\n")
        setProgress(1)
      })
      shinyjs::text("tpResult", tpOutputRaw)
      if (length(storedData$textprocess$vocab) > 0) {
        doneAlert = sprintf("\nDone processing %s data!", dataType)
        shinyjs::text("tpResult", doneAlert, TRUE)
      } else {
        shinyjs::text("tpResult", "Text processor did not complete correctly!")
      }
    } else {
      shinyjs::text("tpResult", "Try again with column name of the document vector!")
    }
  }))

  ##### Plot Removed #####
  # **TODO**: Decide of more edge/bad cases for input?

  # **TODO**: Clearing should be one command? Two?
  observeEvent(input$prClearout, ({
    output$prOutput <- renderPrint({ invisible(NULL) })
    output$prPlot <- renderPlot({ invisible(NULL) })
  }))

  observeEvent(input$prRun, ({
    if (is.null(storedData$textprocess)) {
      shinyjs::text("prOutput", "You must successfully run Text Processor!")
      return(NULL)
    }

    plotRevDocs <- storedData$textprocess$documents

    output$prPlot <- renderPlot(
      isolate(
        plotRemoved(plotRevDocs, lower.thresh = seq(from = input$plotLowThresh,
                                                    to = input$plotUpThresh,
                                                    by = input$plotInterval)
        )
      )
    )
  }))

  ##### Prep Documents #####
  # calculates on the output from text processor
  # or calculates on the file uploaded by the user
  # **TODO**: expand functionality for case of user-input file

  # Change Upper Thresh
  observe({
    pdUpThreshType <- input$pdUpThreshChoice

    if (pdUpThreshType == 1) {
      updateNumericInput(session, 'pdUpThresh', value = Inf )
    } else {
      updateNumericInput(session, 'pdUpThresh', value = 10000 )
    }
  })

  observeEvent(input$pdClearout, ({
    output$prepdocresults <- renderPrint({ invisible(NULL) })
  }))

  observeEvent(input$pdPrepdocs, ({

    if (is.null(storedData$textprocess)) {
      output$prepdocresults <- renderPrint({ "You must successfully run Text Processor!"})
      return(NULL)
    }

    upperThresh <- Inf

    if (input$pdUpThreshChoice > 1) {
      upperThresh <- input$pdUpThresh
    }

    tpres <- storedData$textprocess

    shinyjs::text("prepdocresults", "Running Prep Documents...")
    withProgress( message = "Preparing Documents...", {
      setProgress(0.5)
      pdOutputRaw <-
        paste(
          capture.output(
            storedData$prepdocs <- isolate(
              prepDocuments(documents=tpres$documents,
                            vocab=tpres$vocab,
                            meta=tpres$meta,
                            lower.thresh=input$pdLowThresh,
                            upper.thresh=upperThresh,
                            subsample=NULL,
                            verbose=input$pdVerbose)
            )
          ), collapse = "\n")
      setProgress(1)
    })
    shinyjs::text("prepdocresults", pdOutputRaw)
  }))

  ##### STM #####

  observeEvent(input$stmRun, ({

    pdres <- storedData$prepdocs

    if(is.null(pdres)) {
      output$stmprocresult <-
        renderPrint({ "You must successfully run Prep Documents!" })
      return(NULL)
    }

    shinyjs::text("stmprocresult", "Generating STM model...")
    withProgress( message = "Running STM model generation...", {
      setProgress(0.5)
      stmOutputRaw <-
        paste(
          capture.output(
            # TODO: Implement content
            storedData$stmresult <- isolate(
              stm(documents=pdres$documents,
                  vocab=pdres$vocab,
                  K=input$stmK,
                  prevalence=formula(input$stmPrev),
                  data=pdres$meta,
                  init.type=input$stmInitType,
                  seed=input$stmSeed,
                  max.em.its=input$stmMaxEm,
                  emtol=input$stmEmTol,
                  verbose=changeStringToLogical(input$stmVerbose),
                  reportevery=input$stmReportEvr,
                  LDAbeta=changeStringToLogical(input$stmLdaBeta),
                  interactions=changeStringToLogical(input$stmInteractions),
                  ngroups = input$stmNgroups,
                  gamma.prior=input$stmGamma,
                  sigma.prior=input$stmSigma,
                  kappa.prior=input$stmKappa)
            )
          ), collapse = "\n")
      setProgress(1)
    })
    shinyjs::text("stmprocresult", stmOutputRaw)
  }))

  observeEvent(input$exportStm, ({
    stmObj <- storedData$stmresult
    if (is.null(stmObj)) {
      output$stmprocresult <- renderPrint({ "You must successfully run STM before saving!" })
      return(NULL)
    }

    save(stmObj, file="stmResult.RData")
  }))

  ##### plot STM #####
  # currently leaving out as inputs until future input:
  #   family, width, covarlevels, plabels, text.cex
  #   custom.labels, topic.names

  # change default for n automatically
  observe({
    plotType <- input$plotStmType

    if (plotType == "labels") {
      updateNumericInput(session, 'plotStmN', value = 20 )
    } else if (plotType == "perspectives") {
      updateNumericInput(session, 'plotStmN', value = 25 )
    } else {
      updateNumericInput(session, 'plotStmN', value = 3 )
    }
  })

  observeEvent(input$plotStmClearout, ({
    output$plotStmOut <- renderPrint({ invisible(NULL) })
    output$plotStmPlot <- renderPlot({ invisible(NULL) })
  }))

  observeEvent(input$plotStm, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      output$plotStmOut <- renderPrint({ "You must successfully run STM before plotting!" })
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$plotStmTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$plotStmXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$plotStmYLim)

    output$plotStmPlot <- renderPlot({
      isolate(
        plot.STM(stmObj,
                 type=input$plotStmType,
                 n=input$plotStmN,
                 topics=tops,
                 labeltype=input$plotStmLabelType,
                 frexw=input$plotStmFrexw,
                 main=input$plotStmMain,
                 xlim=plotXLim,
                 ylim=plotYLim)
      )
    })
  }))

  observeEvent(input$labelTopics, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      output$labelTopicsOut <- renderPrint({ "You must successfully run STM before running Label Topics" })
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$labelTopicsTopics)

    output$labelTopicsOut <- renderPrint({
      isolate(
        labelTopics(stmObj,
                    n=input$labelTopicsN,
                    topics=tops,
                    frexweight=input$labelTopicsFrexw)
      )
    })
  }))
})