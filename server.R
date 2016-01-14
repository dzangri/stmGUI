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
  storedData$esteffect <- NULL

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
 #   output$prPlotOutput <- renderPlot({ invisible(NULL) })
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
          vocabLength = length(storedData$textprocess$vocab)
          newMax = as.integer(vocabLength + 0.1*vocabLength)

          updateSliderInput(session, "prPlotRange", "Range:",
            min = 1, max = newMax, value = c(1, newMax/2))
        } else {
          shinyjs::text("tpTextResult", errorForUi(
            paste("textProcessor did not complete correctly! This occurred because",
              "the output of textProcessor contained a vocab size of 0! Perhaps you entered a",
              "nontextual data column as the choice for documents?")))
          storedData$textprocess <- NULL
        }
      } else {
        shinyjs::text("tpTextResult", tpOutputRaw)
      }
    } else {
      shinyjs::text("tpTextResult", errorForUi("Try again with column name of the document vector!"))
    }
  }))

  ##### Plot Removed #####
#   observe({
#     if (length(storedData$textprocess$vocab) > 0) {
#       vocabLength = length(storedData$textprocess$vocab)
#       newMax = as.integer(vocabLength + 0.1*vocabLength)
#
#       updateSliderInput(session, "prPlotRange", "Range:",
#         min = 1, max = newMax, value = c(1, newMax/2))
#     }
#   })
#
#   observeEvent(input$prRun, ({
#     if (is.null(storedData$textprocess)) {
#       return(NULL)
#     }
#
#     plotRange <- as.integer(input$prPlotRange)
#
#     output$prPlotOutput <- renderPlot(
#       isolate(
#         plotRemoved(storedData$textprocess$documents,
#           lower.thresh = seq(from = plotRange[[1]],
#             to = plotRange[[2]],
#             by = 1)
#         )
#       )
#     )
#   }))

  ##### Prep Documents #####
  # calculates on the output from text processor
  # or calculates on the file uploaded by the user
  # **TODO**: expand functionality for case of user-input file
  onclick("toggleAdvPrepDocs", toggle(id = "advPrepDocsOptions", anim = TRUE))

  # Change Upper Thresh
  observe({
    pdUpThreshType <- input$pdUpThreshChoice

    toggleState("pdUpThresh", pdUpThreshType == "manual")

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
      shinyjs::text("stmTextResult", "K cannot be left blank!")
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
        try(stmArgs <- c(stmArgs, prevalence=formula(prevString)))
      }

      inputSeed <- input$stmSeed
      if (!is.na(inputSeed)) {
        try(stmArgs <- c(stmArgs, seed=inputSeed))
      }

      contentString <- input$stmContent
      if (contentString != "") {
        try(stmArgs <- c(stmArgs, content=formula(contentString)))
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

  ##### Estimate Effect #####
  onclick("toggleAdvEstEff", toggle(id = "advEstEffOptions", anim = TRUE))

  observeEvent(input$estEffClearout, ({
    shinyjs::text("estEffTextResult", "")
  }))

  observeEvent(input$estEffRun, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::text("estEffTextResult",
        "You must successfully run STM before running estimateEffect!")
      return(NULL)
    }

    estEffArgs <- list(
      stmobj=stmObj,
      metadata=storedData$prepdocs$meta,
      uncertainty=input$estEffUncertainty,
      nsims=input$estEffNSims
    )
    formParseOutput <- ""

    estEffString <- input$estEffFormula
    if (estEffString != "") {
      try(estEffArgs <- c(estEffArgs, formula=formula(estEffString)))
    } else {
      shinyjs::text("estEffTextResult",
        "You must enter a formula to run estimateEffect!")
      return(NULL)
    }

    if(input$estEffUncertainty == "Local") {
      try(estEffArgs <- c(estEffArgs, documents=storedData$prepdocs$documents))
    }

    prior <- input$estEffPrior
    if (prior != "") {
      try(estEffArgs <- c(estEffArgs,
        prior=changeCsStringToDoubleVectorOrLeaveNull(prior)))
    }

    withProgress( message = "Running estimateEffect, please wait...", {
      setProgress(0.5)

      estEffOutputRaw <-
        tryCatch({
          paste(
            capture.output(
              isolate(
                storedData$esteffect <- do.call(estimateEffect, estEffArgs)
              )
            ), collapse = "\n")
        }, error=function(e) {
          errorMessage <-
            paste(errorForUi("Error while running estimateEffect:"), e, sep = "\n")
          storedData$esteffect <- NULL
          return(errorMessage)
        }, warning=function(w) {
          warningMessage <-
            paste("WARNING: Warning while running estimateEffect:", w, sep = "\n")
          storedData$esteffect <- NULL
          return(warningMessage)
        }, finally={
          setProgress(1)
        })
    })
    shinyjs::text("estEffTextResult", "Ran estimateEffect!")
  }))

  observeEvent(input$estEffPlot, ({
    estEff <- storedData$esteffect

    if (is.null(estEff)) {
      shinyjs::text("estEffTextResult",
        "You must successfully run estimateEffect before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$estEffPlotTopics)
    covVar1 <- changeCsStringToDoubleVectorOrLeaveNull(input$estEffCovVar1)
    covVar2 <- changeCsStringToDoubleVectorOrLeaveNull(input$estEffCovVar2)

    if (is.null(tops)) {
      tops <- estEff$topics
    }

    output$estEffPlot <- renderPlot({
      isolate(
        plot.estimateEffect(x=estEff,
          covariate=input$estEffPlotCov,
          method=input$estEffPlotMethod,
          topics=tops,
          cov.value1=covVar1,
          cov.value2=covVar2,
          moderator=input$estEffMod,
          moderator.value=input$estEffModValue,
          linecol=input$estEffLineCol,
          npoints=input$estEffNPoints
          )
      )
    })
  }))

  ##### plot STM #####
  # currently leaving out as inputs until future input:
  #   family, width, covarlevels, plabels, text.cex
  #   custom.labels, topic.names

  # change default for n automatically
#   observe({
#     plotType <- input$plotStmType
#
#     if (plotType == "labels") {
#       updateNumericInput(session, 'plotStmN', value = 20 )
#     } else if (plotType == "perspectives") {
#       updateNumericInput(session, 'plotStmN', value = 25 )
#     } else {
#       updateNumericInput(session, 'plotStmN', value = 3 )
#     }
#   })

  observeEvent(input$summaryPlotClearout, ({
    shinyjs::text("summaryPlotTextResult", "")
    output$summaryPlot <- renderPlot({ invisible(NULL) })
  }))

  observeEvent(input$summaryPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::text("summaryPlotTextResult",
        "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$summaryPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$summaryPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$summaryPlotYLim)

    output$summaryPlot <- renderPlot({
      isolate(
        plot.STM(stmObj,
          type="summary",
          n=input$summaryPlotN,
          topics=tops,
          labeltype=input$summaryPlotLabelType,
          frexw=input$summaryPlotFrexw,
          main=input$summaryPlotMain,
          xlim=plotXLim,
          ylim=plotYLim)
      )
    })
  }))

    observeEvent(input$labelPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::text("labelPlotTextResult",
        "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$labelPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$labelPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$labelPlotYLim)

    output$labelPlot <- renderPlot({
      isolate(
        plot.STM(stmObj,
          type="labels",
          n=input$labelPlotN,
          topics=tops,
          labeltype=input$labelPlotLabelType,
          frexw=input$labelPlotFrexw,
          main=input$labelPlotMain,
          xlim=plotXLim,
          ylim=plotYLim)
      )
    })
  }))

    observeEvent(input$perspPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::text("perspPlotTextResult",
        "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$perspPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$perspPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$perspPlotYLim)

    output$perspPlot <- renderPlot({
      isolate(
        plot.STM(stmObj,
          type="perspectives",
          n=input$perspPlotN,
          topics=tops,
          labeltype=input$perspPlotLabelType,
          frexw=input$perspPlotFrexw,
          main=input$perspPlotMain,
          xlim=plotXLim,
          ylim=plotYLim)
      )
    })
  }))

    observeEvent(input$histPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::text("histPlotTextResult",
        "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$histPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$histPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$histPlotYLim)

    output$histPlot <- renderPlot({
      isolate(
        plot.STM(stmObj,
          type="hist",
          n=input$histPlotN,
          topics=tops,
          labeltype=input$histPlotLabelType,
          frexw=input$histPlotFrexw,
          main=input$histPlotMain,
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