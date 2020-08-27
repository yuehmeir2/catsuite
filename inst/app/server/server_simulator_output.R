
getClassificationAccuracy <- function(cutThetas, simulationResult) {
  simuleeIds = unique(simulationResult$output$SIM_ID)
  simuleeLastRows = vapply(simuleeIds, function(simuleeId) {
    max(which(simulationResult$output$SIM_ID == simuleeId))
  }, as.integer(0), USE.NAMES = FALSE)
  simuleeTrueThetas = simulationResult$output$TRUE_THETA[simuleeLastRows]
  simuleeFinalThetas = simulationResult$output$THETA[simuleeLastRows]

  breaks = c(-Inf, cutThetas, Inf)
  labels = 1:(length(cutThetas)+1)
  simuleeTrueGroups = cut(simuleeTrueThetas, breaks = breaks, labels = labels)
  simuleeFinalGroups = cut(simuleeFinalThetas, breaks = breaks, labels = labels)

  sameGroup = simuleeTrueGroups == simuleeFinalGroups
  return(sum(sameGroup) / length(sameGroup))
}

output$result_latest_table <- renderDT({

  cat("### ", format(Sys.time(), "%X"), " output$result_latest_table\n")

  req(!is.null(rea_simulator$latest), !is.null(rea_simulator$latest$result))

  df = getMeasurementResult(rea_simulator$latest$result, input$result_cut_theta)
  dt <- datatable(df, selection = "single", options = list(dom = 't'), rownames= FALSE) %>%
    formatRound(names(df), digits = DIGITS)
  dt
})

getMeasurementResult = function(result, cutThetas) {

  decisionAccuracy = getClassificationAccuracy(unique(cutThetas), result)

  df <- tibble(
    bias = result$irt$bias,
    correlation = result$irt$correlation,
    CA = decisionAccuracy,
    csem = result$irt$csem,
    mse = result$irt$mse,
    len = result$isr$testLengthMean,
    itemMaxExposure = result$iec$itemMaxExposure,
    itemsNeverUsed = result$iec$itemsNeverUsed
  ) %>% set_names(c("Bias", "Correlation", "CA", "CSEM", "MSE", "Test Length", "Item Max Exposure", "Items Never Used"))

  # add content alignment
  if (!is.null(result$isr$consViolations)) {
    alignmentRate <- data.frame(t(result$isr$consViolations %>% select(CONS_ID, `0`) ), row.names = NULL) %>%
      set_names(head(.,1)) %>% slice(-1)

    df <- bind_cols(df, alignmentRate)
  }

  return(df)

}

observeEvent(input$result_history_add_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_add_btn\n")
  if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$result) | length(input$result_latest_label) < 1) return ()

  rea_simulator$latest$label = input$result_latest_label
  rea_simulator$history = c(rea_simulator$history, list(rea_simulator$latest))
})

observeEvent(input$result_history_delete_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_delete_btn\n")
  if (length(input$result_history_table_rows_selected) < 1) return()

  rea_simulator$history = rea_simulator$history[-input$result_history_table_rows_selected]

  rea_simulator$selected = rea_simulator$latest
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

observeEvent(input$result_history_clear_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_clear_btn\n")
  rea_simulator$history = list()

  rea_simulator$selected = rea_simulator$latest
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

output$result_history_table <- renderDT({
  req (length(rea_simulator$history) > 0)

  getResult_history_table()

})

getResult_history_table = function () {

  if (verbose) cat("### ", format(Sys.time(), "%X"), " call getResult_history_table() \n")

  dt0 <- map(rea_simulator$history, ~{
    bind_cols(label = .x$label, getMeasurementResult(.x$result, input$result_cut_theta))
  }) %>% bind_rows()

  dt <- datatable(dt0, options = list(dom = 't'), rownames= FALSE, width = "1200") %>%
    formatRound(names(dt0)[-1], digits = DIGITS)

  return(dt)

}

output$result_history_plot <- renderPlotly({
  cat("### ", format(Sys.time(), "%X"), " output$result_history_plot", input$result_history_plot_checkbox, "\n")
  if ((length(rea_simulator$history) < 1) | !input$result_history_plot_checkbox) return ()

  x = seq(length(rea_simulator$history))

  dataBias <- data.frame(
    HISTORY = x,
    BIAS = vapply(rea_simulator$history, function(history) {
      history$result$irt$bias
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pBias <- dataBias %>%
    plot_ly(name = "Bias",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~BIAS)

  dataCorrelation <- data.frame(
    HISTORY = x,
    CORRELATION = vapply(rea_simulator$history, function(history) {
      history$result$irt$correlation
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pCorrelation <- dataCorrelation %>%
    plot_ly(name = "Correlation",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~CORRELATION)

  dataCSEM <- data.frame(
    HISTORY = x,
    CSEM = vapply(rea_simulator$history, function(history) {
      history$result$irt$csem
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pCSEM <- dataCSEM %>%
    plot_ly(name = "CSEM",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~CSEM)

  dataMSE <- data.frame(
    HISTORY = x,
    MSE = vapply(rea_simulator$history, function(history) {
      history$result$irt$mse
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pMSE <- dataMSE %>%
    plot_ly(name = "MSE",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~MSE)

  dataMaxExposure <- data.frame(
    HISTORY = x,
    MAX_EXPOSURE = vapply(rea_simulator$history, function(history) {
      history$result$iec$itemMaxExposure
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pMaxExposure <- dataMaxExposure %>%
    plot_ly(name = "Max Item Exposure",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~MAX_EXPOSURE)

  dataNeverUsed <- data.frame(
    HISTORY = x,
    NEVER_USED = vapply(rea_simulator$history, function(history) {
      history$result$iec$itemsNeverUsed
    }, as.numeric(0), USE.NAMES = FALSE)
  )
  pNeverUsed <- dataNeverUsed %>%
    plot_ly(name = "Items Never Used",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~HISTORY,
            y = ~NEVER_USED)

  p <- subplot(pBias, pCorrelation, pCSEM, pMSE, pMaxExposure, pNeverUsed, nrows = 6, shareX = T) %>%
    layout(xaxis = list(range = c(1, length(rea_simulator$history)), tick0 = 1, dtick = 1, zeroline = FALSE))

  p$elementId <- NULL
  return (p)
})

observeEvent(input$result_latest_table_rows_selected, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_latest_table_rows_selected", input$result_latest_table_rows_selected, "\n")
  selectRows(dataTableProxy("result_history_table", session), selected = NULL)

  rea_simulator$selected = rea_simulator$latest
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

observeEvent(input$result_history_table_rows_selected, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_history_table_rows_selected", input$result_history_table_rows_selected, "\n")
  selectRows(dataTableProxy("result_latest_table", session), selected = NULL)

  rea_simulator$selected = rea_simulator$history[[input$result_history_table_rows_selected]]
  updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
})

output$result_scatter_plot <- renderPlotly({

  req (!is.null(rea_simulator$selected$result))

  getResult_scatter_plot(list(rea_simulator$selected))

})

getResult_scatter_plot = function(resultList) {

  if (length(resultList) < 1) return()

  p <- map(resultList, ~{
    simuleeIds = unique(.x$result$output$SIM_ID)
    simuleeLastRows = vapply(simuleeIds, function(simuleeId) {
      max(which(.x$result$output$SIM_ID == simuleeId))
    }, as.integer(0), USE.NAMES = FALSE)
    simuleeTrueThetas = .x$result$output$TRUE_THETA[simuleeLastRows]
    simuleeFinalThetas = .x$result$output$THETA[simuleeLastRows]

    data = data.frame(ID = simuleeIds, TRUE_THETA = simuleeTrueThetas, ESTIMATE = simuleeFinalThetas)
    cut_theta = unique(input$result_cut_theta)

    CATShinyModules::correlationPlot (
      plotData = data, xVar = "ESTIMATE", yVar = "TRUE_THETA", useDiffAsColor = F, cutsAt = cut_theta,
      xRange = c(THETA_LOWER, THETA_UPPER), addTitle = F, plotName = .x$label)

  })

  if (length(p) == 1) return(p[[1]])

  p %>% subplot(nrows = ceiling(length(p)/2), shareY = T, shareX = T)

}

output$CSEM_scatter_plot <- renderPlotly({

  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()

  genCSEM_scatter_plot(list(rea_simulator$selected))

})

genCSEM_scatter_plot = function (resultList) {

  if (length(resultList) < 1) return()

  p <- map(resultList, ~{

    plotData <- .x$result$output %>%
      group_by(SIM_ID) %>% slice(n()) %>% ungroup() %>% select(THETA, CSEM)

    plotData %>%
      plot_ly(type = "scatter", mode = "markers", x = ~THETA, y = ~CSEM, colors = "Blues", opacity = .4, name = .x$label) %>%
      layout(xaxis = list(title = "Estimated Theta", range = c(THETA_LOWER, THETA_UPPER), zeroline = FALSE),
             yaxis = list(title = "CSEM", range = c(0, max(plotData$CSEM + 0.2)), zeroline = FALSE))
  })

  if (length(p) == 1) return(p[[1]])

  p %>% subplot(nrows = ceiling(length(p)/2), shareY = T, shareX = T)

}

output$adaptivity_plot <- renderPlotly({

  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()

  genAdaptivity_plot(list(rea_simulator$selected), list(input$test_start_theta_slider))

})

genAdaptivity_plot = function (resultList, startThetaList) {

  if (length(resultList) < 1) return()

  data <- map2(resultList, startThetaList, ~{
      label = .x$label
      data0 = .x$result$output
      startTheta = .y

      data2 = data0 %>% select(SIM_ID, PAR_2, THETA) %>%
        group_by(SIM_ID) %>% group_modify(~{
          temp = .x
          if (is.na(temp$THETA[1])) temp$THETA[1] = startTheta
          temp <- temp %>% fill(THETA) %>% mutate(SEQ = 1:nrow(.))
          temp <- temp %>% mutate(DIFF = c(startTheta, temp$THETA[1:(nrow(.)-1)]) - temp$PAR_2)
        }) %>% ungroup()

      n = length(unique(data2$SEQ))
      data2 %>% group_by(SEQ) %>% group_map(~{
        tibble(Simulation = label,
               Item = .y$SEQ,
               Mean = mean(.x$DIFF, na.rm = T),
               Abs_Mean = mean(abs(.x$DIFF), na.rm = T),
               Min = min(.x$DIFF, na.rm = T),
               Max = max(.x$DIFF, na.rm = T),
               Who = .x$SIM_ID[which(.x$DIFF == min(.x$DIFF, na.rm = T))[1]])
      }) %>% bind_rows() %>% pivot_longer(cols = c("Abs_Mean", "Mean", "Min", "Max"))
    }) %>% bind_rows()

  ticks = seq(min(data$Item), max(data$Item))
  y_range = c(floor(min(data$Min)), ceiling(max(data$Max)))

  data %>% mutate_at("name", as.factor) %>%
      group_by(Simulation) %>%
      group_map(~{
        p <- plot_ly(data = .x, x = ~Item, y = ~value, color = ~name, #colors = "Paired",
                linetype = ~name,
                type = "scatter", mode = "lines", showlegend = (.y == data$Simulation[1] ),
                legendgroup = ~Simulation)

        if (length(resultList) > 1) x_title = .x$Simulation[1] else x_title = "Item Sequence"
        xaxis = list(range = c(10, 0))
        p %>%  layout(title = "Adaptivity = Item Selection Theta - Selected Item Difficulty",
                      xaxis = list(title = x_title, tickvals = ticks),
                      yaxis = list(title = "Theta - Item Difficulty", range = y_range))
      }, .keep = T) %>% subplot(nrows = ceiling(length(startThetaList)/2),
                                shareY = T, shareX = F, titleX = T, margin = 0.07) %>%
                        layout(legend = list(itemdoubleclick = F, itemclick = F))


}

output$result_irt_plot <- renderPlotly({
  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()

  # for debugging
  # saveRDS(rea_simulator$selected, file = "selected.rds")

  # --- use CATShinyModules::gen_IRT_plot()

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  if (length(simuleeIds) > MAX_PLOT_N_OBS)
    simuleeIds = sample(simuleeIds, MAX_PLOT_N_OBS)

  itempool = rea_simulator$selected$itempool

  data = rea_simulator$selected$result$output %>% select(SIM_ID, ITEM_ID) %>%
    left_join(itempool)

  p <- CATShinyModules::gen_IRT_plot(
    data = data, IRT_fun = input$irtPlotRadio,
    theta = c(THETA_LOWER, THETA_UPPER),
    IRTParCols = grep("PAR_", names(itempool), value = T),
    IRTModelCol = "MODEL", showLegend = F, groupByCol = "SIM_ID")

  return(p[[1]])

})

output$result_all_simulees_table <- renderDT({
  if (is.null(rea_simulator$selected) | is.null(rea_simulator$selected$result)) return ()
  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)

  simuleeLastRows = vapply(simuleeIds, function(simuleeId) {
    max(which(rea_simulator$selected$result$output$SIM_ID == simuleeId))
  }, as.integer(0), USE.NAMES = FALSE)
  data = rea_simulator$selected$result$output[simuleeLastRows, c("SIM_ID", "SEED", "TRUE_THETA", "THETA", "CSEM")]

  datatable(data, selection = "single", filter = 'top', options = list(pageLength = 10), rownames= FALSE) %>%
    formatRound(c("TRUE_THETA", "THETA", "CSEM"), digits = DIGITS)
})

observeEvent({
  input$result_all_simulees_table_rows_selected
  # input$result_simulee_theta_offset_toggle
}, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$result_all_simulees_table_rows_selected", input$result_all_simulees_table_rows_selected, "\n")
  if (length(input$result_all_simulees_table_rows_selected) < 1) {
    updateSliderInput(session, "result_simulee_likelihood_slider", max = 1, value = 1)
  } else {
    simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
    selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]
    simuleeOutputThetas = rea_simulator$selected$result$output[rea_simulator$selected$result$output$SIM_ID == selectedSimId & !is.na(rea_simulator$selected$result$output$THETA),]
    sliderMax = nrow(simuleeOutputThetas)
    if (input$result_simulee_theta_offset_toggle) {
      sliderMax = max(1, sliderMax - 1)
    }
    updateSliderInput(session, "result_simulee_likelihood_slider", max = sliderMax, value = sliderMax)
  }
})

output$result_simulee_ability_plot <- renderPlotly({
  cat("### ", format(Sys.time(), "%X"), " output$result_simulee_ability_plot", input$result_all_simulees_table_rows_selected, "\n")
  req(length(input$result_all_simulees_table_rows_selected) > 0)

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]

  data =
    rea_simulator$selected$result$output[rea_simulator$selected$result$output$SIM_ID == selectedSimId, c("ITEM_ID", "NC", "PAR_2", "TRUE_THETA", "SCORE", "THETA", "CSEM")]
  max_possible_score = max(data$NC)-1
  data <- data %>%
    mutate(MAX_SCORE = NC - 1) %>%
    mutate(NORMAL_SCORE = SCORE / MAX_SCORE) %>%
    mutate(SCORE_NUM = SCORE) %>%
    mutate(SCORE = factor(SCORE, levels = as.character(0:max_possible_score))) %>%
    mutate(SIZE = (SCORE_NUM+1) * 5)

  thetaInd = which(!is.na(data$THETA))
  dataWithThetas = data[thetaInd,]
  dataWithThetas$ROW = thetaInd

  if (input$result_simulee_likelihood_slider > nrow(dataWithThetas)) return ()
  selectedRow = which(data$ITEM_ID == dataWithThetas$ITEM_ID[input$result_simulee_likelihood_slider])

  color_map <- c('0'="yellow", '1'="red", '2'= "cyan",
                 '3'= "magenta", '4'='limegreen', '5'="blue", '6'="violet", '7'="orange")
  color2 = colorRampPalette(c(brewer.pal(9,"Blues"), "darkblue"))(8)
  names(color2) = as.character(0:7)

  if (input$result_simulee_theta_offset_toggle)
    item_slot = 2:(nrow(data)+1) else item_slot = seq(nrow(data))

  p <- data %>%
    plot_ly() %>%
    layout(xaxis = list(title = "Item Slot", rangemode = "tozero", autotick = FALSE, tick0 = 0),
           yaxis = list(title = "Theta", range = c(THETA_LOWER, THETA_UPPER), zeroline = FALSE),
           legend = list(title=list(text="<b> Score </b>"))
    ) %>%
    add_trace(data, name = "Difficulty",
              type = "scatter", mode = "markers",
              x = ~seq(nrow(data)),
              y = ~PAR_2,
              text = ~NC,
              marker = list(
                size = 2,
                symbol = "diamond",
                line = list(width = 2, color = "purple")
              ),
              hovertemplate = paste(sep = "", "%{y:.2f}<br>"
              ),
              showlegend = T
    ) %>%
    add_trace(name = "True Theta",
              type = "scatter", mode = "lines",
              line = list(width = 2, color = "green"),
              x = seq(nrow(data)),
              y = ~TRUE_THETA,
              hovertemplate = "<b>True Theta</b>:    %{y:.2f}<extra></extra>",
              showlegend = T
    ) %>%
    add_trace(data = dataWithThetas,
              name = "Interim Theta",
              type = "scatter", mode = "lines+markers",
              x = ~item_slot,
              y = ~THETA,
              text = ~TRUE_THETA,
              line = list(width = 2, color = "darkblue"),
              marker = list(
                color = color2[dataWithThetas$SCORE],
                size = ~SIZE,
                symbol = "circle",
                line = list(width = 2, color = "darkblue")
              ),
              hovertemplate = paste(sep = "",
                                    "<b>True Theta</b>:    %{text:.2f}<br>",
                                    "<b>Interim Theta</b>: %{y:.2f}<br>",
                                    "<b>CSEM</b>:             %{error_y.array:.2f}<extra></extra>"
              ),
              error_y = ~list(
                type = "data",
                array = CSEM,
                thickness=6,
                width=0,
                color="orange",
                opacity=0.3
              ),
              showlegend = T
    ) %>%
    add_segments(name = "Item",
                 x = selectedRow, xend = selectedRow,
                 y = THETA_LOWER, yend = THETA_UPPER,
                 color = I(brewer.pal(3, "Dark2")[2]),
                 opacity=0.5,
                 showlegend = F
    )

  p$elementId <- NULL
  return (p)

})

output$result_simulee_likelihood_plot <- renderPlotly({
  cat("### ", format(Sys.time(), "%X"), " output$result_simulee_likelihood_plot", input$result_all_simulees_table_rows_selected, "slider", input$result_simulee_likelihood_slider, "\n")
  req (length(input$result_all_simulees_table_rows_selected) > 0)

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]
  simuleeOutput = rea_simulator$selected$result$output[rea_simulator$selected$result$output$SIM_ID == selectedSimId,]

  simuleeOutputWithThetas = simuleeOutput[!is.na(simuleeOutput$THETA),]
  if (input$result_simulee_likelihood_slider > nrow(simuleeOutputWithThetas)) return ()
  selectedRow = which(simuleeOutput$ITEM_ID == simuleeOutputWithThetas$ITEM_ID[input$result_simulee_likelihood_slider])
  interimTheta = simuleeOutput$THETA[selectedRow]

  assignedItemIndices = match(simuleeOutput$ITEM_ID[1:selectedRow], rea_simulator$selected$itempool$ITEM_ID)
  maxNC = rea_simulator$selected$itempool$NC
  PARCols = str_c("PAR_", 1:9)[str_c("PAR_", 1:9) %in% names(rea_simulator$selected$itempool)]

  item_pars = as.matrix(rea_simulator$selected$itempool[assignedItemIndices,PARCols])

  scores = simuleeOutput$SCORE[1:selectedRow]

  # Calculate the likelihood that each theta in thetaRange is the student's ability
  thetaRange = seq(from=THETA_LOWER, to=THETA_UPPER, by=0.1)
  scoreProb = CATSimulator::scoreProbability.scores(item_pars, thetaRange, scores)

  logLikelihood = apply(scoreProb, 2, function(thetaScoreProb) {
    sum(log(thetaScoreProb), na.rm = T)
  })

  data = data.frame(THETA = thetaRange, LIKELIHOOD = logLikelihood)

  p <- data %>%
    plot_ly(name = "Theta Likelihood",
            type = 'scatter',
            mode = 'lines+markers',
            x = ~THETA,
            y = ~LIKELIHOOD) %>%
    layout(xaxis = list(range = c(THETA_LOWER, THETA_UPPER), zeroline = FALSE),
           yaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) %>%
    add_segments(name = 'Interim Theta',
                 x = interimTheta, xend = interimTheta,
                 y = min(logLikelihood), yend = max(logLikelihood),
                 color = I(brewer.pal(3, "Dark2")[2]), opacity=0.5, showlegend = F)

  p$elementId <- NULL
  return (p)
})

output$result_simulee_table <- renderDT({
  cat("### ", format(Sys.time(), "%X"), " output$result_simulee_table", input$result_all_simulees_table_rows_selected, "\n")
  req (length(input$result_all_simulees_table_rows_selected) > 0)

  simuleeIds = unique(rea_simulator$selected$result$output$SIM_ID)
  selectedSimId = simuleeIds[input$result_all_simulees_table_rows_selected]

  outputRows = which(rea_simulator$selected$result$output$SIM_ID == selectedSimId)
  data = rea_simulator$selected$result$output[outputRows, !names(rea_simulator$selected$result$output) %in% c("SIM_ID", "SEED", "TRUE_THETA")]

  if (!is.null(rea_simulator$selected$constraints) & !is.null(rea_simulator$selected$constraints$content)) {
    assignedItemIndices = match(rea_simulator$selected$result$output$ITEM_ID[outputRows], rea_simulator$selected$itempool$ITEM_ID)
    itemConsIds = rea_simulator$selected$itempool$CONS_IDS[assignedItemIndices]
    data = cbind(data, CONS_IDS = vapply(itemConsIds, function(consIds) {
      paste(consIds, collapse=",")
    }, as.character(0), USE.NAMES = FALSE))
  }
  if (!("PSG_ID" %in% names(data)) & !is.null(rea_simulator$selected$constraints) & !is.null(rea_simulator$selected$constraints$passage)) {
    assignedItemIndices = match(rea_simulator$selected$result$output$ITEM_ID[outputRows], rea_simulator$selected$itempool$ITEM_ID)
    itemPsgId = rea_simulator$selected$itempool$PSG_ID[assignedItemIndices]
    data = cbind(data, PSG_ID = itemPsgId)
  }

  datatable(data, rownames= TRUE, extensions = c('Buttons','Scroller'),
            options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'))) %>%
    formatRound(c("PAR_2", "THETA", "CSEM"), digits = DIGITS)
})

output$downloadResult2 <- downloadHandler(
  filename = "report.html",

  content = function(file) {
    req (length(rea_simulator$history) > 0)
    # need to put rmd into the library?

    tempReport <- file.path(tempdir(), "simulation-report.Rmd")
    file.copy(system.file("Rmd/simulation-report.Rmd", package = "catsuite"), tempReport, overwrite = TRUE)

    rmarkdown::render(tempReport, output_file = file,
                      params = list(
                        history = rea_simulator$history,
                        result_cut_theta = input$result_cut_theta),
                      envir = new.env(parent = globalenv())
    )
  }
)

