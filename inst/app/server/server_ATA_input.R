

output$show_gs_ui <- renderUI({

  cat("### ", format(Sys.time(), "%X"), " output$show_gs_ui <- renderUI\n")

  # rea_change_input_suorce()  is for ATA example
  # datafile() is the google sheets

  if (rea_change_input_source() > 0)  editDT2Input("editTable1")

})

observeEvent(input$load_example_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent:input$load_example_btn\n")

  examples = dir(system.file("rdsdata", package = "myFormAssembler"))
  available_examples = grep("^Ex|^MST|^CAT|^Forms", grep("\\.rds$", examples, value = T), value = T)

  # # only limit one example for IACAT
  # available_examples = grep("^Ex03", examples, value = T)
  #

  if (!(input$example_select %in% available_examples)) return ()

  rds_path = system.file("rdsdata", package = "myFormAssembler")
  # rds_file = file.path(rds_path, "MST05.rds")
  rds_file = file.path(rds_path, input$example_select)
  data = read_rds(rds_file)
  rea_change_input_source(rea_change_input_source()+1)

  data0 = data
  data0$mst = NULL

  # print(head(data0))
  # print(class(data0))

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = data0)

  # update the input UI
  # update number of forms

  content = data$constraint

  # get the number of items and points from content

  n_info = get_n_lower_pt_lower(content)

  # update the input widgets for number of items/points
  updateNumericInput(session, "ata_n_form", value = n_info$n_form)
  updateTextInput(session, "ata_n_item", value = paste0(n_info$n_lower, collapse = ","))
  updateTextInput(session, "ata_n_pt", value = paste0(n_info$pt_lower, collapse = ","))

})




observeEvent(input$form_assembly_reset_btn, {

  reset_reactiveValues("FORM_ASSEMBLY")

  reset_form_assembly_UI()

})

reset_form_assembly_UI = function () {

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = list())

  updateNumericInput(session, "ata_n_form", value = 6, min = 1, max = 20, step = 1)
  updateTextInput(session, "ata_n_item",  value = "12,12,12,12,12,12")
  updateTextInput(session, "ata_n_pt", value = "22,22,22,22,22,22")

}


observeEvent(input$ata_n_panel, {
  if (input$ata_n_panel == 1)
    updateNumericInput(session, inputId = "ata_n_panel_to_have",
                       min = 1, max = 4, step = 1, value = 1) else
    updateNumericInput(session, inputId = "ata_n_panel_to_have",
                       min = 1, max = 1, value = 1)

})

observeEvent(rea_input$tibbles, {

  # req(rea_input$tibbles())
  cat("### ", format(Sys.time(), "%X"), " observeEvent: rea_input$tibbles\n")

  # try to not limit only mst here
  # if (input$ata_form_type_radio == 'mst') {

      # this function is not good since it assems there is only 1 first module for each panel
      # n_panel = get_n_panel_from_content(rea_input$tibbles()$constraint)
      n_panel = 1    # design solution later
      updateNumericInput(session, inputId = "ata_n_panel",
                         min = n_panel, max = n_panel,
                         value = n_panel)
  # }

  editDTHas = names(rea_input$tibbles())

  # for adding a row to editDT:
  #        1. render a select input
  #        2. render an action button

  output$add_row_table_select_ui <- renderUI({

    choices = c("Select one table", editDTHas[-which(editDTHas == "item")])
    selectInput(inputId = "add_row_table_select", label = NULL,
                choices = choices)
  })

  output$add_row_btn_ui <- renderUI({

    actionButton(inputId = "add_row_btn", label = "Add a New Row to ", icon = icon("plus"))

  })

  # add the objectitve checkbox based on what are in the editDT tables

  choices = c("Information (tif)" = "tif",
              "Expected Score (tcc)" = "tcc")
              # "Extra Constraints" = "slice")

  has = which(choices %in% editDTHas)

  if (length(has) > 0) { # there is tif, tcc, or slice in the editDT tables

    choices0 = c("Minimum Overlap" = "min_overlap", choices[has])
    updateCheckboxGroupInput(session, "objective_checkbox", choices = choices0, selected = NULL)
  }

  # add_input_checkbox reactive to rea_input$tibbles so the user can add tif/tcc/slice to editDT tables

  output$add_input_table_select_ui <- renderUI({

    if (length(has) < length(choices)) {
      choices0 = choices[setdiff(1:length(choices), has)]
      selectInput(inputId = "add_row_table_select", label = NULL,
                  choices = choices0)
    }

  })

  # nothing can be listed in input$add_input_checkbox

  if (length(input$add_input_checkbox) ==0 || input$add_input_checkbox == "nothing to add") return()

  if (length(has) == length(choices)) {
    updateCheckboxGroupInput(session, "add_input_checkbox", choices = "nothing to add")  # change this later
  }

})


observeEvent(input$mst_path_select, {
  if (input$mst_path_select[1] == "none")
    updateCheckboxGroupInput(session, "mst_path_select",
                             selected = "none")
})



observeEvent(input$add_input_table_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$add_input_table_btn\n")

  validate(
    need(rea_input$tibbles, "Need to load input tables first!"),
           need(input$add_row_table_select, 'Check at least one table first'))

  data = isolate(rea_input$tibbles())

  if ("tif" %in% input$add_row_table_select) {
      tif = tibble(TIF_ID = "1", FORM = "1", THETA = 0.0,
                        TIF_LOWER = "0.2", TIF_UPPER = "0.5",  MARGIN = 0.0, NOTE = "note")
      data[["tif"]] = tif
  }

  if ("tcc" %in% input$add_row_table_select) {
    tcc = tibble(TCC_ID = "1", FORM = "1", SCOPE = NA, THETA = 0.0,
                 TCC_LOWER = "0.2", TCC_UPPER = "0.5",  MARGIN = 0.0, NOTE = "note")
    data[["tcc"]] = tcc
  }

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = data)
})




observeEvent(input$add_row_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$add_row_btn\n")

  req(rea_input$tibbles())
  if (!(input$add_row_table_select %in% names(rea_input$tibbles())))
    return()

  data = isolate(rea_input$tibbles())
  tb = input$add_row_table_select

  if (tb == "alias") {
    data[[tb]] = data[[tb]] %>% add_row(alias = NA)
  } else {
    data[[tb]] = data[[tb]] %>% add_row(MARGIN = NA)
  }

  rea_input$tibbles <- callModule(editDT2, "editTable1", data_sets = data)

})



observeEvent(input$go_solver_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$go_solver_btn\n")
  closeAlert(session, "catSuiteAlertId")

  msg = ""
  tryCatch({
    withProgress(value = 0.1, message = "Solver preparing...", detail = "", {
      isolate({
        req(rea_input$tibbles, input$ata_n_item, input$ata_n_pt)

        # from UI input

        n_form = input$ata_n_form

        n_item = as.integer(unlist(strsplit(input$ata_n_item, ",")))

        n_pt = as.integer(unlist(strsplit(input$ata_n_pt, ",")))

        if (length(n_item) != n_form || length(n_pt) != n_form)
          stop("error")

        # get the tibbles in the editDT

        input_obj <- isolate(rea_input$tibbles())

        lp_obj <- init_lp_obj(
          basic = list(
            job_id = "My task",
            test_name = input$ata_test_name,
            n_form = n_form,

            n_item_l  = n_item,   # rep(12, n_form),
            n_item_u  = n_item,   #  rep(12, n_form),
            n_point_l = n_pt,     #rep(22, n_form),
            n_point_u = n_pt,     #rep(22, n_form),

            form_name = paste0("FORM_", 1:n_form),
            relax = 0),

          input = list(
            files = NULL,
            input_obj = input_obj),

          objective <- if (!is.null(input$objective_checkbox))  c("content", input$objective_checkbox) else "content"

        )

        solver = input$ata_solver_type
        timeout = input$solver_timeout
        gap = 0

        lp_obj$options$solver = solver

        lp_obj$options$timeout = timeout

        lp_obj$options$gap = gap

        lp_obj$options$verbose = verbose

        if (!is.null(lp_obj$files$cat_input_file))
          lp_obj <- build_input_from_cat (lp_obj)

        if (length(setdiff(lp_obj$files, "cat_input_file")) > 0)
          lp_obj <- read_input(lp_obj)

        if (!is.null(lp_obj$input_obj))
          lp_obj <- get_input (lp_obj, lp_obj$input_obj, add_default_alias = FALSE)

        if (!is.null(lp_obj$template) && !is.null(lp_obj$use_template))
          lp_obj <- apply_template(lp_obj)

        setProgress(value = 0.2, message = "Solver running...")

        lp_obj <- to_solve (lp_obj, timeout = timeout, solver = solver, gap = gap, verbose = T)

      }) # end isolate

      if (is.null(lp_obj$error.message) & "x" %in% names(lp_obj)) {
        createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "ATA", content = paste0(msg, "Feasible solutions found and the results updated"), append = FALSE, style = "info")
      } else {
        createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "ATA", content = paste0(msg, lp_obj$error.message), append = FALSE, style = "warning")
      }

      rea_ata$ata_list$output$lp_obj <- lp_obj

    })
  }, error = function(e) {
    errDetail = as.character(e)
    cat("### ", format(Sys.time(), "%X"), " ATA failed: ", errDetail, "\n")
    createAlert(session, "catSuiteAlert", "catSuiteAlertId", title = "ATA", content = errDetail, append = FALSE, style = "warning")
  })
})

observeEvent(rea_ata$ata_list$output, {

  cat("### ", format(Sys.time(), "%X"), " ********  observeEvent(rea_ata$ata_list$output$lp_obj$output\n")

  # req(rea_ata$ata_list$output$lp_obj$output)

  lp_obj = rea_ata$ata_list$output$lp_obj

  req(rea_ata$ata_list$output$lp_obj$output$form)

  uniqueForms = unique(lp_obj$output$form$form_ind)

  if (length(uniqueForms) > 0) {
    updateSelectInput(session, "send_form_select", choices = c(uniqueForms), selected = uniqueForms[0])
  }

  saveRDS(lp_obj, file = "lp_obj.rds")

  form_summary = lp_obj$output$form_summary %>%
    mutate(mean_diff = round(mean_diff, 4),
           sd_diff   = round(sd_diff, 4),
           min_diff  = round(min_diff, 4),
           max_diff  = round(max_diff, 4))


  # add_to_report (what = datatable(form_summary), title = paste0(input$ata_test_name, "--Form Summary "),
  #                 des = "add your Form Summary description here")

  output$parallel_forms_output <- DT::renderDataTable(form_summary)

})




observeEvent(input$send_to_simulator_btn, {
  cat("### ", format(Sys.time(), "%X"), " observeEvent: input$send_to_simulator_btn form: ", input$send_form_select, "\n")
  req(rea_ata$ata_list$output$lp_obj$output$form)

  rea_ata$ata_list$output$lp_obj$test_name = input$ata_test_name
  # save an ATA object for debugging purpose
  # saveRDS(rea_ata$ata_list$output$lp_obj, file = "ata.rds")
  rea_simulator$latest <- formAssemblyToSimulation(rea_ata$ata_list$output$lp_obj, input$send_form_select)
  # save a simulation object for debugging purpose
  # saveRDS(rea_simulator$latest, file = "simulation.rds")

  syncTestConfigUI(rea_simulator$latest)
  updateTabItems(session, "tabs", "simulator_input_tab")
})

formAssemblyToSimulation <- function(lp_obj, selectForm) {
  simulation = list(
    test_name = paste0(lp_obj$test_name, "-form-", selectForm)
  )

  # Handle simulation$constraints$content
  # CONS_ID WEIGHT LOWER UPPER LABEL
  # <chr>    <dbl> <dbl> <dbl> <chr>
  # CONS01     100  0.1   0.15 1A
  # simulation$constraints = list(
  #   content = read_rds(system.file("example/bp_2level.rds", package = "CATSimulator"))
  # )

  # Handle simulation$itempool
  # ITEM_ID MODEL    NC PAR_1 PAR_2 PAR_3 CONS_IDS
  # <chr>   <chr> <int> <dbl> <dbl> <dbl> <list>
  # ITEM001 3PL       2  1.7  -0.48  0.17 <chr [2]>
  # simulation$itempool = read_rds(system.file("example/pool_3pl.rds", package = "CATSimulator"))
  items = lp_obj$items[lp_obj$output$form$item_ind[lp_obj$output$form$form_ind == selectForm],]

  print("in ata")
  print(names(items))
  print(lp_obj$alias)
  simulation$itempool = tibble(
    ITEM_ID = items[[lp_obj$alias$item_id]],
    MODEL = items[[lp_obj$alias$irt_model]],
    NC = items[[lp_obj$alias$point]]+1,
    PAR_1 = items[[lp_obj$alias$irt_par_a]],
    PAR_2 = items[[lp_obj$alias$irt_par_b]],
    PAR_3 = items[[lp_obj$alias$irt_par_c]]
  )
  # for GPCM
  if ("GPC" %in% simulation$itempool$MODEL) {

    print("in GPC")
    for (d in 2:7)
      if (!is.null(lp_obj$alias[[str_c("irt_par_d", d)]]))
        simulation$itempool <- simulation$itempool %>%
          mutate(new_col = items[[lp_obj$alias[[str_c("irt_par_d", d)]] ]]) %>%
          rename(!!lp_obj$alias[[str_c("irt_par_d", d)]] := new_col)

  }
  print(head(simulation$itempool))

  # Handle simulation$control
  simulation$control = list()
  simulation$control$abilityEstimator = "mle"
  simulation$control$startTheta = 0.0
  simulation$control$generateModules = TRUE
  simulation$control$itemSelectionRule = "linear"
  simulation$control$terminationRule = "asap"
  simulation$control$terminationValue = 0.4
  simulation$control$minItems = simulation$control$maxItems = nrow(simulation$itempool)

  return(simulation)
}

output$mst_structure_plot = renderPlot({
  cat("### ", format(Sys.time(), "%X"), " output$mst_structure_plot\n")

  # if (is.null(rea_simulator$latest) | is.null(rea_simulator$latest$mst$path)) return ()
  # all_path = rea_simulator$latest$mst$path

  validate(need(rea_input$tibbles, "Need to load input tables first!"))

  all_path = get_mst_path_from_content (rea_input$tibbles()$constraint)

  # if (is.null(rea_ata$ata_list$output$lp_obj) | is.null(rea_ata$ata_list$output$lp_obj$mst$path)) return ()
  # all_path = rea_ata$ata_list$output$lp_obj$mst$path
  # print(all_path)

  edgeList <- NULL
  for (i in 1:length(all_path)) {
    single_path = all_path[[i]]
    for (j in 1:(length(single_path)-1)) {
      edgeList <- c(edgeList, c(single_path[j], single_path[j+1]))
    }
  }
  edgeList = matrix(edgeList, ncol = 2, byrow = TRUE)

  graph <- graph_from_edgelist(unique(edgeList))
  plot(graph, layout = layout_as_tree, vertex.size=30, vertex.label.dist=0, edge.arrow.size=0.5,
    vertex.color = "#EFEDF5")
    # vertex.color= c("#F0F0F0", "#efedf5", "#bcbddc", "#756bb1", "#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B"))
})
