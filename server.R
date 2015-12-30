##### stmGUI ####
#
# Server-side logic for handling stm workflows


library(shiny)
library(stm)
library(shinyjs)
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

  ##### Data Upload #####
  # Allow file upload and submission to be read by read.csv
  # as the raw data in shiny
  # **TODO**: Expand file types that can be handled

  onclick("toggleAdvDataUpload", toggle(id = "advUploadOptions", anim = TRUE))
  observe({
    toggleState("submitDataForUpload", !is.null(input$dataFileToUpload))
  })
  observe({
    toggleState("moveFromDataToProc", !is.null(storedData$data))
  })
  observeEvent(input$moveFromDataToProc, ({
    updateTabsetPanel(session, "navBar", selected = "Processing")
  }))

  observeEvent(input$submitDataForUpload, ({
    shinyjs::text("dataInputTextResult", "")

    userData <- input$dataFileToUpload

    withProgress( message = "Loading data, please wait...", {
      setProgress(0.5)

      readDataArgs <-
        list(userData$datapath,
          header=input$headerPresent,
          sep=input$columnSeparator,
          quote=input$quoteAroundData)

      tryCatch({
        disable("moveFromStep1To2")
        storedData$data <- do.call(read.csv, readDataArgs)
      }, error=function(e) {
        funName <- deparse(substitute(read.csv))
        shinyjs::text("dataInputTextResult",
          paste("ERROR: Error while running '", funName, "':\n", e, sep = ""))
        storedData$data <- NULL
        return(NULL)
      }, warning=function(w) {
        shinyjs::text("dataInputTextResult",
          paste("WARNING: Warning while reading data:\n", w, sep = "\n"))
        storedData$data <- NULL
        return(NULL)
      }, finally={})

      setProgress(1)
    })
    output$data <- DT::renderDataTable({
      DT::datatable(storedData$data)
    })
  }))

  ##### Text Processor #####
  # processes input to textProcessor
  # includes option to choose data from preloaded gadarian data
  # or data uploaded by the user
  onclick("toggleAdvTextProc", toggle(id = "advTextProcOptions", anim = TRUE))
  observeEvent(input$tpClearout, ({
    shinyjs::text("tpTextResult", "")
  }))
  observe({
    toggleState("moveFromProcToStm", !is.null(storedData$prepdocs))
  })
  observeEvent(input$moveFromProcToStm, ({
    updateTabsetPanel(session, "navBar", selected = "Model")
  }))

  # Only activate the submit button if either the preloaded gadarian data
  # is chosen OR the user has uploaded data
  observe({
    toggleState("tpProcessText",
      input$tpGadarian == "gadar" || !is.null(storedData$data))
  })

  # Update document options based on column names of uploaded data
  observe({
    userData <- storedData$data
    if (!is.null(userData)) {
      enable("tpDocs")
      dataColumnNames <- colnames(userData)
      updateSelectInput(session, "tpDocs", choices = dataColumnNames)
    } else {
      disable("tpDocs")
    }
  })

  observe({
    if (is.null(storedData$data)) {
      updateRadioButtons(session,
        "tpGadarian",
        choices = list("Preloaded Gadarian Dataset" = "gadar", "Uploaded Data" = "upload"),
        selected = "gadar" )
    } else {
      updateRadioButtons(session,
        "tpGadarian",
        choices = list("Preloaded Gadarian Dataset" = "gadar", "Uploaded Data" = "upload"),
        selected = "upload" )
    }
  })

  observeEvent(input$tpProcessText, ({

    docsForTp <- NULL
    metadataForTp <- NULL
    dataType <- NULL

    if (input$tpGadarian == "gadar") {
      docsForTp <- gadarian$open.ended.response
      metadataForTp <- gadarian
      dataType <- "Gadarian"
    }
    else {
      docs <- input$tpDocs
      docsForTp <- storedData$data[[docs]]
      metadataForTp <- storedData$data
      dataType <- "user"
    }

    if (!is.null(docsForTp)) {
      # **TODO**: Implement wordLengths, customstopwords, onlytxtfiles
      shinyjs::text("tpTextResult", "textProcessor is running...")

      argsForTp <-
        list(documents=docsForTp,
          metadata=metadataForTp,
          lowercase=input$tpLowercase,
          removestopwords=input$tpRemovestop,
          removenumbers=input$tpRemovenum,
          removepunctuation=input$tpRemovepunc,
          stem=input$tpStem,
          wordLengths=c(3, Inf),
          sparselevel=input$tpSparselevel,
          language=input$tpLang,
          onlycharacter=input$tpOnlychar,
          striphtml=input$tpStriphtml,
          customstopwords=NULL,
          onlytxtfiles=TRUE)

      withProgress( message = "Running textProcessor, please wait...", {
        setProgress(0.5)
        tpOutputRaw <-
          tryCatch({
            paste(
              capture.output(
                isolate(
                storedData$textprocess <- do.call(textProcessor, argsForTp)
                )
              ), collapse = "\n")
          }, error=function(e) {
            errorMessage <-
              paste(errorForUi("Error while running textProcessor:"), e, sep = "\n")
            storedData$textprocess <- NULL
            return(errorMessage)
          }, warning=function(w) {
            warningMessage <-
              paste("WARNING: Warning while running textProcessor:", w, sep = "\n")
            storedData$textprocess <- NULL
            return(warningMessage)
          }, finally={
            setProgress(1)
          })
      })
      if (!is.null(storedData$textprocess)) {
        if (length(storedData$textprocess$vocab) > 0) {
          doneAlert <- sprintf("Done processing %s data!", dataType)
          completeMessage <- paste(tpOutputRaw, doneAlert, sep = "\n")
          shinyjs::text("tpTextResult", completeMessage)
        } else {
          shinyjs::text("tpTextResult", errorForUi(
            paste("textProcessor did not complete correctly! This occurred because",
              "the output of textProcessor contained a vocab size of 0! Perhaps you entered a",
              "nontextual data column as the choice for documents?")))
        }
      } else {
        shinyjs::text("tpTextResult", tpOutputRaw)
      }
    } else {
      shinyjs::text("tpTextResult", errorForUi("Try again with column name of the document vector!"))
    }
  }))

  ##### Plot Removed #####

  observeEvent(input$prClearout, ({
    shinyjs::text("prTextResult", "")
    output$prPlotOutput <- renderPlot({ invisible(NULL) })
  }))

  observeEvent(input$prRun, ({
    if (is.null(storedData$textprocess)) {
      shinyjs::text("prTextResult", "You must successfully run textProcessor!")
      return(NULL)
    }

    plotRevDocs <- storedData$textprocess$documents

    output$prPlotOutput <- renderPlot(
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

    if (pdUpThreshType == "inf") {
      updateNumericInput(session, 'pdUpThresh', value = Inf )
    } else {
      updateNumericInput(session, 'pdUpThresh', value = 10000 )
    }
  })

  observeEvent(input$pdClearout, ({
    shinyjs::text("pdTextResult", "")
  }))

  observeEvent(input$pdPrepdocs, ({

    if (is.null(storedData$textprocess)) {
      shinyjs::text("pdTextResult", "You must successfully run textProcessor!")
      return(NULL)
    }

    upperThresh <- Inf
    if (input$pdUpThreshChoice == "manual") {
      upperThresh <- input$pdUpThresh
    }

    tpres <- storedData$textprocess

    shinyjs::text("pdTextResult", "Running Prep Documents...")
    withProgress( message = "Running prepDocuments, please wait...", {
      setProgress(0.5)

      argsForPd <-
        list(documents=tpres$documents,
          vocab=tpres$vocab,
          meta=tpres$meta,
          lower.thresh=input$pdLowThresh,
          upper.thresh=upperThresh,
          subsample=NULL)

      pdOutputRaw <-
        tryCatch({
          paste(
            capture.output(
              isolate(
                storedData$prepdocs <- do.call(prepDocuments, argsForPd)
              )
            ), collapse = "\n")
        }, error=function(e) {
          errorMessage <-
            paste(errorForUi("Error while running prepDocuments:"), e, sep = "\n")
          storedData$prepdocs <- NULL
          return(errorMessage)
        }, warning=function(w) {
          warningMessage <-
            paste("WARNING: Warning while running prepDocuments:", w, sep = "\n")
          storedData$prepdocs <- NULL
          return(warningMessage)
        }, finally={
          setProgress(1)
        })
    })
    shinyjs::text("pdTextResult", pdOutputRaw)
  }))

  ##### STM #####
  onclick("toggleAdvStm", toggle(id = "advStmOptions", anim = TRUE))

  observeEvent(input$stmClearout, ({
    shinyjs::text("stmTextResult", "")
  }))

  observe({
    if(!is.null(storedData$textprocess)) {
      if (length(storedData$textprocess$vocab) < 10000) {
        updateRadioButtons(session, "stmInitType",
          label = "init type",
          choices = list("LDA" = "LDA", "Random" = "Random", "Spectral" = "Spectral"),
          inline = T,
          selected = "Spectral")
      }
    }
  })

  observeEvent(input$stmRun, ({

    pdres <- storedData$prepdocs
    if(is.null(pdres)) {
      shinyjs::text("stmTextResult", "You must successfully run Prep Documents!")
      return(NULL)
    }

    stmArgs <- list()

    if(is.null(input$stmK)) {
      shinyjs::text("stmTextResult", "K is blank!")
      return(NULL)
    }

    withProgress( message = "Generating STM model, please wait...", {
      setProgress(0.5)

      stmArgs <-
        list(documents=pdres$documents,
          vocab=pdres$vocab,
          K=input$stmK,
          data=pdres$meta,
          init.type=input$stmInitType,
          max.em.its=input$stmMaxEm,
          emtol=input$stmEmTol,
          reportevery=input$stmReportEvr,
          LDAbeta=changeStringToLogical(input$stmLdaBeta),
          interactions=changeStringToLogical(input$stmInteractions),
          ngroups=input$stmNgroups,
          gamma.prior=input$stmGamma,
          sigma.prior=input$stmSigma,
          kappa.prior=input$stmKappa)

      prevString <- input$stmPrev
      if (prevString != "") {
        try(append(stmArgs, c(prevalence=formula(prevString))))
      }

      if (!is.null(input$stmSeed)) {
        append(stmArgs, c(seed=input$stmSeed))
      }

      contentString <- input$stmContent
      if (contentString != "") {
        try(append(stmArgs, c(prevalence=formula(contentString))))
      }

      stmOutputRaw <-
        tryCatch({
          paste(
            capture.output(
              isolate(
                storedData$stmresult <- do.call(stm, stmArgs)
              )
            ), collapse = "\n")
        }, error=function(e) {
          errorMessage <-
            paste(errorForUi("Error while running stm:"), e, sep = "\n")
          storedData$stmresult <- NULL
          return(errorMessage)
        }, warning=function(w) {
          warningMessage <-
            paste("WARNING: Warning while running stm:", w, sep = "\n")
          storedData$stmresult <- NULL
          return(warningMessage)
        }, finally={
          setProgress(1)
        })
    })
    shinyjs::text("stmTextResult", stmOutputRaw)
  }))

  observeEvent(input$exportStm, ({
    stmObj <- storedData$stmresult
    if (is.null(stmObj)) {
      shinyjs::text("stmTextResult", "You must successfully run STM before saving!")
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