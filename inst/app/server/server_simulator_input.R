
output$selection_rule <- reactive({
  rea_simulator$latest$control$itemSelectionRule
})

outputOptions(output, "selection_rule", suspendWhenHidden = FALSE)

output$test_selection_rule <- renderText({ rea_simulator$latest$control$itemSelectionRule })


observeEvent(input$simulation_example_input_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$simulation_example_input_btn\n")

  req(input$simulation_example_select != "pick example")

  simFile = paste0("example/", input$simulation_example_select, ".rds")

  sysFile = system.file(simFile, package = "CATSimulator")

  newSimulation(readRDS(sysFile))

})


observeEvent(input$yourDataInput, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent:yourDataInput\n")

  try({

    newSimulation(readRDS(input$yourDataInput$datapath))

  })
})



newSimulation = function (simulation) {

  rea_simulator$latest <- simulation
  syncTestConfigUI(rea_simulator$latest)

  if (is.null(simulation$control$constraintControl)) {
    data0 = list(constraintControl =
                   tibble(onSlots = character(0), requireAll = character(0), requireAny = character(0), block = character(0))) } else
    data0 = list(constraintControl =
                   convertListOfNamedListToTibble(
                     listOfNamedList = simulation$control$constraintControl,
                     colNames = c("onSlots", "requireAll", "requireAny", "block")))

  rea_simulator$constraintControlData <-
    callModule(editDT2, "specialControlEditTable", data_sets = data0)

}


syncTestConfigUI <- function(simulation) {
  if (is.null(simulation)) return ()

  # copy the test name into the Simulation UI controls
  test_name = ifelse(is.null(simulation$test_name), "My Test", simulation$test_name)
  updateTextInput(session, "simulation_test_name", value = test_name)

  # copy the simulation$control settings into the Test Config UI controls
  minItems = ifelse(is.null(simulation$control$minItems), min(10, nrow(simulation$itempool)), simulation$control$minItems)
  maxItems = ifelse(is.null(simulation$control$maxItems), min(20, nrow(simulation$itempool)), simulation$control$maxItems)
  maxValue = min(nrow(simulation$itempool), max(40, maxItems))
  updateSliderInput(session, "test_length_slider", max = maxValue, value = c(minItems,maxItems))
  updateSelectInput(session, "test_optimal_objective", selected = ifelse("optimalObjective" %in% names(simulation$control), simulation$control$optimalObjective, "inf"))
  updateRadioButtons(session, "test_content_balancing", selected = ifelse(is.null(simulation$control$contentBalancing), "maxinf", simulation$control$contentBalancing))
  if (is.null(simulation$control$consWeight)) {
    updateSliderInput(session, "test_cons_weight", value = paste(seq(1.0, 0.1, by = -0.1), collapse=", "))
  } else {
    updateSliderInput(session, "test_cons_weight", value = paste(simulation$control$consWeight, collapse=", "))
  }
  if (is.null(simulation$control$infWeight)) {
    updateSliderInput(session, "test_inf_weight", value = paste(seq(0.1, 1.0, by = 0.1), collapse=", "))
  } else {
    updateSliderInput(session, "test_inf_weight", value = paste(simulation$control$infWeight, collapse=", "))
  }
  updateSliderInput(session, "test_iec_pars", value = ifelse(is.null(simulation$control$iecPars), 1, simulation$control$iecPars))
  updateRadioButtons(session, "test_termination_rule", selected = ifelse(is.null(simulation$control$terminationRule), "asap", simulation$control$terminationRule))
  updateSliderInput(session, "test_termination_value", value = ifelse(is.null(simulation$control$terminationValue), 0.3, simulation$control$terminationValue))
  updateSliderInput(session, "test_start_theta_slider", value = ifelse(is.null(simulation$control$startTheta), 0.0, simulation$control$startTheta))
  updateRadioButtons(session, "test_ability_estimator", selected = ifelse(is.null(simulation$control$abilityEstimator), "eap", simulation$control$abilityEstimator))
  updateSliderInput(session, "test_eap_mean_slider", value = ifelse(is.null(simulation$control$eapMean), 0.0, simulation$control$eapMean))
  updateSliderInput(session, "test_eap_stddev_slider", value = ifelse(is.null(simulation$control$eapStdDev), 1.0, simulation$control$eapStdDev))
  updateSliderInput(session, "test_eap_nquad_slider", value = ifelse(is.null(simulation$control$eapNquad), 51, simulation$control$eapNquad))

}


output$downloadSimObj <- downloadHandler(

  filename = function() {
    str_c(input$simulation_test_name, ".rds")
  },

  content = function (file2) {

      updateLatestFromUI()
      saveRDS(object = isolate(rea_simulator$latest), file = file2)

  }
)

updateLatestFromUI = function () {

  rea_simulator$latest$test_name                 <- input$simulation_test_name
  rea_simulator$latest$control$minTheta         <- THETA_LOWER
  rea_simulator$latest$control$maxTheta         <- THETA_UPPER
  rea_simulator$latest$control$minItems         <- input$test_length_slider[1]
  rea_simulator$latest$control$maxItems         <- input$test_length_slider[2]
  rea_simulator$latest$control$optimalObjective <- input$test_optimal_objective
  rea_simulator$latest$control$contentBalancing <- input$test_content_balancing
  rea_simulator$latest$control$consWeight       <- as.vector(as.numeric(unlist(strsplit(input$test_cons_weight, split="\\s*,\\s*"))))
  rea_simulator$latest$control$infWeight        <- as.vector(as.numeric(unlist(strsplit(input$test_inf_weight, split="\\s*,\\s*"))))
  rea_simulator$latest$control$iecPars          <- input$test_iec_pars
  rea_simulator$latest$control$terminationRule  <- input$test_termination_rule
  rea_simulator$latest$control$terminationValue <- input$test_termination_value
  rea_simulator$latest$control$startTheta       <- input$test_start_theta_slider
  rea_simulator$latest$control$abilityEstimator <- input$test_ability_estimator
  rea_simulator$latest$control$eapMean          <- input$test_eap_mean_slider
  rea_simulator$latest$control$eapStdDev        <- input$test_eap_stddev_slider
  rea_simulator$latest$control$eapNquad         <- input$test_eap_nquad_slider
  rea_simulator$latest$control$constraintControl<- NULL

  if (!is.null(rea_simulator$constraintControlData)) {
    consControlTbl <- rea_simulator$constraintControlData()[["constraintControl"]]
    if (nrow(consControlTbl) > 0)
      rea_simulator$latest$control$constraintControl <- tblToConstraintControl(consControlTbl)
  }

  rea_simulator$latest$control$solver = list(
    name = input$simulation_solver,
    external = input$simulation_solver_external == "external",
    mipGap = 0.0001,
    timeout = input$simulation_solver_timeout * 1000,
    verbose = F
  )

}


observeEvent(input$simulation_run_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$simulation_run_btn\n")
  closeAlert(session, "catSuiteAlertId")

  tryCatch({
    withProgress(value = 0.1, message = "Simulation preparing...", detail = "", {
      isolate({

        updateLatestFromUI ()

        if (verbose)  print(rea_simulator$latest$control)

        # create simulees from the Simulator Config settings
        numSimulees = input$simulation_simulees_slider
        if (input$simulation_generator == "normal") {
          rea_simulator$latest$simulee = CATSimulator::generateSimuleesByNormal(
            numSimulees = numSimulees,
            mean = input$simulation_mean_slider,
            sd = input$simulation_stddev_slider,
            randomSeeds = input$simulation_seed_slider:(input$simulation_seed_slider + numSimulees - 1)
          )
        } else {
          trueThetas = as.vector(as.numeric(unlist(strsplit(input$simulation_true_thetas, split="\\s*,\\s*"))))

          if (length(trueThetas) < 1) return ()

          rea_simulator$latest$simulee = CATSimulator::generateSimuleesByTrueTheta(
            trueThetas = rep_len(trueThetas, numSimulees),
            randomSeeds = input$simulation_seed_slider:(input$simulation_seed_slider + numSimulees - 1)
          )
        }

        # run the simulation and save the results
        setProgress(value = 0.2, message = "Simulation running...")
        cat("### ", format(Sys.time(), "%X"), " simulation starting...\n")
        rea_simulator$latest = CATSimulator::initSimulation(rea_simulator$latest)
        rea_simulator$latest$timestamp <- Sys.time()
        rea_simulator$latest$label <- paste0(rea_simulator$latest$test_name, "-", format(rea_simulator$latest$timestamp, DATETIME_FORMAT))

        if (input$simulation_parallel_radio == "parallel") {
          runSimFunction = CATSimulator::runParallelSimulation
          # updating setProgress() is disabled for parallel.  Shiny does not like it when its UI components are updated by parallel.
          # each future() is a separate session/environment, and cross-session calls require special handling in Shiny
          progressCallback = NULL
        } else {
          runSimFunction = CATSimulator::runSimulation
          progressCallback = function(completedCounter) {
            if ((completedCounter %% 10) == 0) {
              pctComplete = completedCounter / numSimulees
              detail = paste0(completedCounter, "/", numSimulees)
              setProgress(value = 0.2 + (pctComplete * 0.8), detail = detail)
            }
          }
        }

        rea_simulator$latest$result <- list(
          output = runSimFunction(
            simulation = rea_simulator$latest,
            simulees = rea_simulator$latest$simulee,
            progressCallback = progressCallback
          )
        )
        cat("### ", format(Sys.time(), "%X"), " simulation finished!\n")

        # calculate analytics based on the results
        rea_simulator$latest$result$irt = CATSimulator::getResult.IRT(rea_simulator$latest, rea_simulator$latest$result$output)
        rea_simulator$latest$result$isr = CATSimulator::getResult.ISR(rea_simulator$latest, rea_simulator$latest$result$output)
        rea_simulator$latest$result$iec = CATSimulator::getResult.IEC(rea_simulator$latest, rea_simulator$latest$result$output)
      })
    })

    # switch to the result tab once the simulation is complete
    createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "Simulator", content = "Finished!", append = FALSE, style = "info")
    updateTextInput(session, "result_latest_label", value = rea_simulator$latest$label)
    selectRows(dataTableProxy("result_history_table", session), selected = NULL)
    selectRows(dataTableProxy("result_latest_table", session), selected = 1)
    rea_simulator$selected = rea_simulator$latest
    updateTabItems(session, "tabs", "simulator_output_tab")

  }, error = function(e) {
    errDetail = as.character(e)
    cat("### ", format(Sys.time(), "%X"), " simulation failed: ", errDetail, "\n")
    createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "Simulator", content = errDetail, append = FALSE, style = "warning")
  })

})


observeEvent(input$simulator_reset_btn, {

  reset_reactiveValues("SIMULATOR")

  reset_simulator_UI()

})

reset_simulator_UI = function () {  #later
}

output$test_passage_constraint_table <- renderDT({
  if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$constraints) | is.null(rea_simulator$latest$constraints$passage)) return ()

  datatable(rea_simulator$latest$constraints$passage, selection = "none", rownames= FALSE) %>%
    formatRound(c("LOWER", "UPPER"), digits = DIGITS)
})

output$test_blueprint_table <- renderDT({
  if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$constraints) | is.null(rea_simulator$latest$constraints$content)) return ()

  datatable(rea_simulator$latest$constraints$content, selection = "none", rownames= FALSE) %>%
    formatRound(c("LOWER", "UPPER"), digits = DIGITS)
})

output$test_groups_table <- renderDT({
  if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$modules)) return ()

  data = data.frame(
    MODULE_ID = rea_simulator$latest$modules$MODULE_ID,
    ITEM_IDS = vapply(rea_simulator$latest$modules$ITEM_IDS, function(groupItemIds) {
      paste(groupItemIds, collapse=", ")
    }, as.character(0), USE.NAMES = FALSE)
  )

  if ("PSG_ID" %in% names(rea_simulator$latest$modules)) {
    data = add_column(data, .after = "MODULE_ID",
      PSG_ID = rea_simulator$latest$modules$PSG_ID
    )
  }
  if ("STAGES" %in% names(rea_simulator$latest$modules)) {
    data = add_column(data, .after = "MODULE_ID",
      STAGES = vapply(rea_simulator$latest$modules$STAGES, function(groupStages) {
        paste(groupStages, collapse=", ")
      }, as.character(0), USE.NAMES = FALSE)
    )
  }

  datatable(data, selection = "none", rownames= FALSE)
})


output$pool_cons_histogram <- renderPlotly({

  cat("### ", format(Sys.time(), "%X"), " output$pool_cons_histogram\n")

  if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$itempool) | is.null(rea_simulator$latest$constraints)
      | is.null(rea_simulator$latest$constraints$content)) return ()

  itempool = rea_simulator$latest$itempool

  req("CONS_IDS" %in% names(itempool))

  contentCons = rea_simulator$latest$constraints$content

  contentCons$CONS_ID <- contentCons$CONS_ID %>% as.character

  cons_freq = as.data.frame(table(sort(unlist(itempool$CONS_IDS)))/nrow(itempool),
                            stringsAsFactors = F)

  transform(cons_freq, Var1 = as.character(Var1))

  names(cons_freq) = c(names(contentCons[1]), "POOL_FREQ")

  cons_tbl <- left_join(contentCons, cons_freq) %>%
    gather(key = "key", value = "value", c(UPPER,LOWER,POOL_FREQ))

  # > head(contentCons)
  # CONS_ID WEIGHT LOWER UPPER LABEL  Freq
  # <chr>    <dbl> <dbl> <dbl> <chr> <dbl>
  # 1 CONS01     100   0.1  0.15 1A    0.075
  # 2 CONS02     100   0.1  0.15 1B    0.115

  # > head(cons_tbl)
  # # A tibble: 6 x 5
  # CONS_ID WEIGHT LABEL key   value
  # <chr>    <dbl> <chr> <chr> <dbl>
  # 1 CONS01     100 1A    UPPER  0.15
  # 2 CONS02     100 1B    UPPER  0.15
  # 3 CONS03     100 1C    UPPER  0.15
  # 4 CONS04     100 RC1   UPPER  0.35

  p <- plot_ly(cons_tbl, x = ~ CONS_ID, y = ~value, color = ~ key, colors = LOWER_FREQ_UPPER,
               type = "bar", opacity = '.7')

  p
})

output$pool_IRT_b_scatter_plot <- renderPlotly({

  if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$itempool) | is.null(rea_simulator$latest$constraints)
      | is.null(rea_simulator$latest$constraints$content)) return ()

  itempool = rea_simulator$latest$itempool

  req("CONS_IDS" %in% names(itempool))

  contentCons = rea_simulator$latest$constraints$content
  cons_id = contentCons$CONS_ID

  data <- lapply(1:length(cons_id), function (i) {
    itempool[unlist(map(itempool$CONS_IDS, function (.x) contentCons$CONS_ID[i] %in% .x )), ]
  })

  names(data) = cons_id
  max_y = floor(max(unlist(map(data, nrow)))/2)
  min_x = min(floor(min(itempool$PAR_2)), THETA_LOWER)
  max_x = max(ceiling(max(itempool$PAR_2)), THETA_UPPER)
  # save(data, file = "data.RData" )
  return (gen_histogram_subplots (data, var_name = "PAR_2",
                                  y_range = c(0,max_y),
                                  x_range = c(min_x, max_x)))


})

output$test_itempool_table <- renderDT({

  if (is.null(rea_simulator$latest$itempool)) return ()
  datatable(rea_simulator$latest$itempool, rownames = F)

})
