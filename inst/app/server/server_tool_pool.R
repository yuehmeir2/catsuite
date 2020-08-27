


output$pool_from_UI <- renderUI({
  
  cat("### ", format(Sys.time(), "%X"), " output$pool_from_UI\n")
  
  choices0 = NULL
  if (!is.null(rea_simulator$latest$itempool))   
    choices0 = c(choices0, "Simulator" = "simulator_widget")
  
  if (!is.null(rea_input$tibbles))
    choices0 = c(choices0, "Form Assembly" = "form_assembly_widget")
  
  if (!is.null(module_rea_input$tibbles))
    choices0 = c(choices0, "Module Assembly" = "module_assembly_widget")
  
  if (!is.null(rea_self_input$itempool))
    choices0 = c(choices0, "Self Pool" = "create_input_tool")
  
  if (!is.null(rea_self_MST_input$itempool))
    choices0 = c(choices0, "Self MST Pool" = "create_MST_input_tool")
  

  if (length(choices0) == 0) return()
                                     
  radioButtons(inputId = "pool_from_radio", label = "Pool 
               From: ", 
               choices = choices0,
               selected = character(0))
})

output$pool_choose_UI <- renderUI({

  cat("### ", format(Sys.time(), "%X"), " output$pool_choose_UI\n")

  req(input$pool_from_radio)

  itempool = NULL

  if (input$pool_from_radio == "simulator_widget") {
    itempool = rea_simulator$latest$itempool
  }

  if (input$pool_from_radio == "form_assembly_widget") {
    itempool = rea_input$tibbles()[[1]]

  }

  if (input$pool_from_radio == "module_assembly_widget") {
    itempool = module_rea_input$tibbles()[[1]]
  }

  if (input$pool_from_radio == "create_input_tool") {
    itempool = rea_self_input$itempool
    
  }

  if (input$pool_from_radio == "create_MST_input_tool") {
    itempool = rea_self_MST_input$itempool
  }

  if (is.null(itempool)) {
    rea_pool_tool$pool = NULL
    return()
  }

  rea_pool_tool$pool = itempool
  
  # decide IRTDataConf based on itempool
  
  IRTDataConf = list(IRTParCols = grep("PAR_", names(itempool), value = T),
                              IRTModelCol = grep("MODEL", names(itempool), value = T),
                              IRTScale = 1.0,
                              theta = c(-4,4))
  
  
  rea_pool_tool$IRTDataConf = IRTDataConf
  rea_pool_tool$categoryCols = get_cate_col_names (itempool)
  print("********")
  print(  rea_pool_tool$categoryCols)
  
  num_attr = get_nume_col_names(itempool, excludedCols = EXCLUDED_NUME_COLS)

  selectUIs = tagList()

  attr1_selected = character(0)

  if ("IRT_MAX_POINTS" %in% num_attr) attr1_selected = "IRT_MAX_POINTS"

  attr2_selected = character(0)

  if ("EVIDENCE_STATEMENT" %in% names(itempool)) attr2_selected = "EVIDENCE_STATEMENT"

  selectUIs[[1]] = selectInput("pool_plot_attr_A0_select", label = "Attribute 1",
                               choices = num_attr, selected = attr1_selected, multiple = F)

  # attribute2 excludes EXCLUDED_COLS and list column in the itempool (for CATSimulator, itempool contains CONS)
  attribute2 = setdiff(names(itempool), c(EXCLUDED_COLS, names(itempool)[map_lgl(itempool, is.list)]))
  selectUIs[[2]] = selectInput("pool_plot_attr_B0_select", label = "Attribute 2",
                                 choices = attribute2, selected = attr2_selected, multiple = F)
  
  selectUIs

})




observeEvent(rea_pool_tool$pool, {
  
  cat("### ", format(Sys.time(), "%X"), " observeEvent(rea_pool_tool$pool")
  
  req(rea_pool_tool$pool)

  data = rea_pool_tool$pool
  
  cate_attr_value <- as.list(get_cate_col_names (data))
  updateCheckboxGroupButtons(session, inputId = "cateAttrChoicesCB", choices = cate_attr_value,
                             checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: orange"),
                                              no = tags$i(class = "fa fa-square-o", style = "color: steelblue")))
  cate_attr_value <- c("None", cate_attr_value)
  updateRadioGroupButtons(session, inputId = "cateAttrGroupByCB", choices = cate_attr_value,
                             checkIcon = list(yes = tags$i(class = "fa fa-check-circle", style = "color: orange"),
                                              no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
  
  nume_attr_value <- as.list(get_nume_col_names(data)) 
  updateCheckboxGroupButtons(session, inputId = "numeAttrChoicesCB", choices = nume_attr_value,
                             checkIcon = list(yes = tags$i(class = "fa fa-check-circle", style = "color: orange"),
                                              no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
  updateRadioGroupButtons(session, inputId = "numeAttrGroupByCB", choices = cate_attr_value,
                          checkIcon = list(yes = tags$i(class = "fa fa-check-circle", style = "color: orange"),
                                           no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
  
})


callModule(IRTPlot_mod_server, "plot1", 
           dataset = reactive(rea_pool_tool$pool),
           IRTDataConf = reactive(rea_pool_tool$IRTDataConf),
           groupByCols = reactive(rea_pool_tool$categoryCols)
)

observeEvent(input$updateCateAttrChoicesToPlot, {
  
  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$updateCateAttrChoicesToPlot\n")
  
  req(rea_pool_tool$pool, input$cateAttrChoicesCB)
  
  data = rea_pool_tool$pool
  
  cate_attr_value_chosen = input$cateAttrChoicesCB

  # this block of code should be removed ?
  if (length(cate_attr_value_chosen) == 0) {
    output$cateAttrBarChart_UI <- NULL
    output$cateAttrPieChart_UI <- NULL
    return()
  }

  # for cateAttrBarChart
  
  output$cateAttrBarChart_UI <- renderUI({
    
    UIs = tagList()
    
    plot_output_list <- lapply(cate_attr_value_chosen, function(attr) {
      plotlyOutput(paste0("singleCateAttrBarChart_", attr))
    })
    
    do.call(tagList, plot_output_list)
    
  })
  
  
  # for cateAttrPieChart 
  
  output$cateAttrPieChart_UI <- renderUI({
    
    UIs = tagList()
    
    plot_output_list <- lapply(cate_attr_value_chosen, function(attr) {
      plotlyOutput(paste0("singleCateAttrPieChart_", attr))
    })
    
    do.call(tagList, plot_output_list)
    
  })
  
  # output plots
  
  for(i in 1:length(cate_attr_value_chosen)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlotly() will be the same across all instances, because
    # of when the expression is evaluated
    local({
      my_i <- i
      groupBy = input$cateAttrGroupByCB
      if (groupBy == "None") groupBy = NULL
      plotname <- paste0("singleCateAttrBarChart_", cate_attr_value_chosen[my_i])
      
      output[[plotname]] <- renderPlotly({
        genOneCateAttrPlot (data, attr = cate_attr_value_chosen[my_i], plotType = "histogram", groupBy = groupBy)
      })
      
      plotname <- paste0("singleCateAttrPieChart_", cate_attr_value_chosen[my_i])
      output[[plotname]] <- renderPlotly({
        genOneCateAttrPlot (data, attr = cate_attr_value_chosen[my_i], plotType = "pie", groupBy = groupBy)
      })
    })
  }
  
})


observeEvent(input$updateNumeAttrChoicesToPlot, {
  
  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$updateNumeAttrChoicesToPlot\n")
  
  req(rea_pool_tool$pool, input$numeAttrChoicesCB)
  
  data = rea_pool_tool$pool
  
  nume_attr_value_chosen = input$numeAttrChoicesCB
  
  if (length(nume_attr_value_chosen) == 0) {
    output$numeAttrScatterPlot_UI <- NULL
    output$numeAttrHistogram_UI <- NULL
    return()
  }
  
  # for cateAttrBarChart
  
  output$numeAttrScatterPlot_UI <- renderUI({
    
    UIs = tagList()
    
    plot_output_list <- lapply(nume_attr_value_chosen, function(attr) {
      plotlyOutput(paste0("singleNumeAttrBarChart_", attr))    # the plotlyOutput Ids
    })
    
    do.call(tagList, plot_output_list)
    
  })
  
  # for numeAttrHistogram
  
  output$numeAttrHistogram_UI <- renderUI({

    UIs = tagList()

    plot_output_list <- lapply(nume_attr_value_chosen, function(attr) {
      plotlyOutput(paste0("singleNumeAttrHistogram_", attr))
    })

    do.call(tagList, plot_output_list)

  })

  for(i in 1:length(nume_attr_value_chosen)) {
    local({
      my_i <- i
      groupBy = input$numeAttrGroupByCB
      if (groupBy == "None") groupBy = NULL
      
      plotname <- paste0("singleNumeAttrBarChart_", nume_attr_value_chosen[my_i])
      output[[plotname]] <- renderPlotly({
        genOneNumeAttrPlot (data, attr = nume_attr_value_chosen[my_i], plotType = "scatter", groupBy = groupBy)
      })
      plotname <- paste0("singleNumeAttrHistogram_", nume_attr_value_chosen[my_i])
      output[[plotname]] <- renderPlotly({
        genOneNumeAttrPlot (data, attr = nume_attr_value_chosen[my_i], plotType = "histogram", groupBy = groupBy)
      })
    })
  }
  
})



output$show_cat_attr_plots_UI <- renderUI({
  
  req(rea_pool_tool$pool)
  
  UIs = tagList()
  
  data = rea_pool_tool$pool
  cate_attr_value = getCateCols(data = data)
  
  if (length(cate_attr_value) == 0) return()
  
  plot_output_list <- lapply(cate_attr_value, function(i) {
      plotlyOutput(i)
    })
  
  do.call(tagList, plot_output_list)
   
})

output$show_num_attr_plots_UI <- renderUI({
  
  req(rea_pool_tool$pool)
  
  UIs = tagList()
  
  data = rea_pool_tool$pool
  
  num_attr = names(data)[which(unlist(map(data, is.numeric)))]
  
  num_attr = num_attr[-grep("^IRT_PAR_D|^IRT_SC|^TASK_TYPE|^IRT_CAT|^IRT_MAX_POINTS", num_attr)] 
  
  if (length(num_attr) == 0) return()
  
  plot_output_list <- lapply(1:length(num_attr), function(i) {
    plotlyOutput(paste0("num_plot_", i))
  })
  
  do.call(tagList, plot_output_list)
  
})

observeEvent(input$render_all_plots_btn, {
  
  cat("### ", format(Sys.time(), "%X"), " input$render_all_plots_btn\n")
  
  req(rea_pool_tool$pool)
  
  data = rea_pool_tool$pool
  
  # categorical attributes
  
  cate_attr_value = names(data)[setdiff(which(unlist(map(data, is.categorical))),
                                        {which(map(data,is.na) %>% map(all) %>% unlist())})]

  cate_attr_value = cate_attr_value[-grep("^IRT_PAR_D|^IRT_PAR_C|^IRT_SC", cate_attr_value)] 
  
  if ("EVIDENCE_STATEMENT" %in% names(data))
    cate_attr_value = c(cate_attr_value, "EVIDENCE_STATEMENT")

  for(i in 1:length(cate_attr_value)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlotly() will be the same across all instances
    local({
      my_i <- i
      plotname <- cate_attr_value[my_i]

      output[[plotname]] <- renderPlotly({
        gen_pool_scatter_plot (items = data, fields = plotname)
      })
    })
  }
  
  # numerical attributes 
  num_attr = names(data)[which(unlist(map(data, is.numeric)))]
  
  num_attr = num_attr[-grep("^IRT_PAR_D|^IRT_SC|^TASK_TYPE|^IRT_CAT|^IRT_MAX_POINTS", num_attr)] 
  
  for(i in 1:length(num_attr)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlotly() will be the same across all instances
    local({
      my_i2 <- i
      plotname <- paste0("num_plot_", i)
      
      output[[plotname]] <- renderPlotly({
        p <- plot_ly(data, x = as.formula(paste0("~", num_attr[my_i2])),
                     type = 'histogram', opacity = '.5') %>%
          layout(bargap = 0.15)
      })
    })
  }
  
})

output$pool_tool_scatter_plot <- renderPlotly({
  
  req(input$pool_plot_attr_A0_select,
      rea_pool_tool$pool)
 
  items <- rea_pool_tool$pool
  fields = unique(intersect(c(input$pool_plot_attr_A0_select, input$pool_plot_attr_B0_select), names(items)))
  
  p <- gen_pool_scatter_plot (items, fields) 
  
  p
})

output$category_donut_chart <- renderPlotly({
  
  req(input$pool_plot_attr_A0_select,
      rea_pool_tool$pool)
  
  items <- rea_pool_tool$pool
  fields = unique(intersect(input$pool_plot_attr_B0_select, names(items)))
  
  gen_dount_chart (items, fields)
  
})
