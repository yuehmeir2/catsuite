

observeEvent(input$create_input_reset_btn, {


  reset_reactiveValues("SIMULATOR_CREATE_INPUT")

  reset_simulator_create_input_UI()
})

reset_simulator_create_input_UI = function () {

  selPoolFile <- NULL

}


output$show_sim_gs_ui <- renderUI({

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " output$show_sim_gs_ui <- renderUI  *** b1 ***\n")

  # if (!is.null(selPoolFile()))  editDTInput("selPoolEditTable")

  # if (!is.null(selPoolFile())) {

  return(dataTableOutput("sel_pool_tbl"))


})

output$show_pool_to_cons_gs_ui <- renderUI({

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " output$show_pool_to_cons_gs_ui <- renderUI *** b2 ***\n")

  return(dataTableOutput("sel_pool_col_to_cons_tbl"))


})

output$sel_pool_tbl <- DT::renderDataTable({

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " output$sel_pool_tbl <- DT::renderDataTable *** b3 ***\n")

  # saveRDS(selPoolFile(), file = "selPool.rds")

  datatable(rea_self_input$itempool, extensions = c('Buttons','Scroller'), options = list(

      # for Scroller
      deferRender = TRUE,
      scrollY = 300,
      scroller = TRUE,

      # for Buttons
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'colvis')
    ))
})


output$sel_pool_col_to_cons_tbl <- DT::renderDataTable({

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " output$sel_pool_col_to_cons_tbl <- DT::renderDataTable *** b4 ***\n")

  # data = selPoolFile()[[1]]
  data = rea_self_input$itempool %>% select(-ITEM_ID)

  # get a list of categorical values
  cate_attr_value = map(data,function (x) {
      if (!is.categorical(x)) return ()
      paste0(unique(x), collapse = ",")
  })
  # cate_attr_value$ITEM_ID = NULL

  # get a list of numerical value range
  num_attr_range = map(data,function (x) {
      if (!is.numeric(x)) return ()
      paste0(range(x, na.rm = T), collapse = " ~ ")
  })
  # num_attr_range$ITEM_ID = NULL

  # get # of NA value

  n_NA = map(data, function(x) {
    sum(is.na(x))
  })

  col_to_cons <- tibble(ATTRIBUTE = names(num_attr_range),
                        CATEGORICAL_VALUE = cate_attr_value,
                        NUMERICAL_RANGE = num_attr_range,
                        NUMBER_OF_NA = n_NA
                        )

  datatable(col_to_cons,
            options = list(ordering=F)
    #         ,extensions = c('Scroller'), options = list(
    #
    # # for Scroller
    # deferRender = TRUE,
    # scrollY = 300,
    # scroller = TRUE)
    )

})





observeEvent(input$create_input_upload_btn, {

  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$create_input_upload_btn *** b5 ***\n")
  print(input$create_input_fileInput)
  req(input$create_input_fileInput)

  data <- read.csv(input$create_input_fileInput$datapath, sep = ",")

  rea_self_input$itempool = data
  rea_self_input$attr_to_cons_holder = NULL
  rea_self_input$constraints = NULL

  dataTableOutput("sel_pool_tbl")
})

output$cate_attr_sel_UI<- renderUI({

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " output$cate_attr_sel_UI *** b6 ***\n")

  data = rea_self_input$itempool
  cate_attr_value = setdiff(names(data)[setdiff(which(unlist(map(data, is.categorical))),
                                        {which(map(data,is.na) %>% map(all) %>% unlist())})], "ITEM_ID")

  selectInput(inputId = "cate_attr_sel", label = "select attribute: ",
              choices = cate_attr_value)
})

output$num_attr_sel_UI<- renderUI({

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " output$num_attr_sel_UI *** b7 ***\n")

  data = rea_self_input$itempool
  num_attr = names(data)[which(unlist(map(data, is.numeric)))]

  if (length(num_attr) >= 1)

    selectInput("num_attr_sel", label = "select attribute: ",
                choices = num_attr, multiple = F)
})


output$cate_attr_UI <- renderUI({

  req(rea_self_input$itempool, input$cate_attr_sel)

  cat("### ", format(Sys.time(), "%X"), " output$cate_attr_UI *** b8 ***\n")

  # data = selPoolFile()[[1]]
  data = rea_self_input$itempool

  categories = unique(data[[input$cate_attr_sel]])

  UIs = tagList()
  UIs[[1]] = radioButtons("cate_attr_radiobox","Select Category",
                           choices = categories, selected = character(0))
  UIs[[2]] = sliderInput("cate_attr_upper_lower", "Upper and Lower",
                         min = 0, max = 1, step = 0.01, value = c(0,1))

  UIs[[3]] = textOutput("cate_attr_upper_lower_text")



  return (UIs)
  # checkboxGroupInput("cate_attr_radiobox","Select Category", choices = categories,
  #                    selected = NULL)

})

output$cate_attr_upper_lower_text <-
  renderText({ paste0(round(input$cate_attr_upper_lower[1] * input$test_length_create_slider[1], 0), " to ",
                      round(input$cate_attr_upper_lower[2] * input$test_length_create_slider[2], 0), " items") })

output$add_cate_attr_btn_UI <- renderUI({

  req(rea_self_input$itempool, input$cate_attr_radiobox)

  cat("### ", format(Sys.time(), "%X"), " output$add_cate_attr_btn_UI *** b9 ***\n")

  actionButton("add_cate_attr_btn", label = "Add to Constraints", icon = icon("plus"))

})


observeEvent(input$add_cate_attr_btn, {

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$add_cate_attr_btn *** b10 ***\n")

  for (i in 1:length(input$cate_attr_radiobox)) {
    this_cate_attr_to_cons = list(
      ATTRIBUTE = input$cate_attr_sel,
      ATTRIBUTE_VALUE = input$cate_attr_radiobox[i],
      RANGE_FROM = NA,
      RANGE_TO = NA,
      LOWER = input$cate_attr_upper_lower[1],
      UPPER = input$cate_attr_upper_lower[2],
      LABEL = paste0(input$cate_attr_sel, "-", input$cate_attr_radiobox[i]))

    # add to $attr_to_cons_holder only when it does not exist already
    if (sum(unlist(map(rea_self_input$attr_to_cons_holder, identical, y = this_cate_attr_to_cons))) == 0)
      rea_self_input$attr_to_cons_holder[[length(rea_self_input$attr_to_cons_holder) + 1]] =
        this_cate_attr_to_cons
  }

})


output$num_cuts_slider_UI <- renderUI({

  req(rea_self_input$itempool, input$num_attr_sel)

  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$num_cuts_slider_UI *** b11 ***\n")

  # data = selPoolFile()[[1]]
  data = rea_self_input$itempool

  min_val = min(data[[input$num_attr_sel]], na.rm = T)
  max_val = max(data[[input$num_attr_sel]], na.rm = T)

  sliderInput("num_cuts_slider", "Cuts:", min =  min_val,
                         max = max_val, value = c(min_val, max_val))

})

output$num_attr_UI <- renderUI({

  req(rea_self_input$itempool, input$num_cuts_slider)

  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$num_attr_UI *** b12 ***\n")

  # data = selPoolFile()[[1]]
  data = rea_self_input$itempool

  min_val = min(data[[input$num_attr_sel]], na.rm = T)
  max_val = max(data[[input$num_attr_sel]], na.rm = T)

  ranges = unique(c(min_val, input$num_cuts_slider, max_val))

  if (length(ranges) <= 2) return()

  n_items = c(table(cut(data[[input$num_attr_sel]],
                        include.lowest = T, breaks = ranges )))

  UIs = tagList()
  ranges1 = unlist(lapply(1:((length(ranges)-1)),
                          function(i) {paste0(ranges[i], " ~ ", ranges[i+1], " has ", n_items[i], " items")}))

  # checkboxGroupInput("num_attr_radiobox","Select Ranges", choices = ranges1)
  UIs[[1]] = radioButtons("num_attr_radiobox","Select Ranges", choices = ranges1,
                          selected = character(0))

  UIs[[2]] = sliderInput("num_attr_upper_lower", "Upper and Lower",
                         min = 0, max = 1, step = 0.01, value = c(0,1))

  UIs[[3]] = textOutput("num_attr_upper_lower_text")

  return (UIs)

})


output$create_input_num_plot <- renderPlotly({

  req(rea_self_input$itempool, input$num_attr_sel)

  cat("### ", format(Sys.time(), "%X"), " output$create_input_num_plot <- renderPlotly *** b13 ***\n")

  print("a1")
  data = rea_self_input$itempool
  print(head(data))
  print("a2")
  field1 = input$num_attr_sel
  print(field1)
  p <- plot_ly(data, x = as.formula(paste0("~", field1)),
                               type = 'histogram', opacity = '.5') %>%
       layout(bargap = 0.15)

  p
})

output$create_input_cate_plot <- renderPlotly({

  # req(rea_self_input$itempool, input$num_cuts_slider)
  req(rea_self_input$itempool, input$cate_attr_sel)

  cat("### ", format(Sys.time(), "%X"), " output$create_input_cate_plot <- renderPlotly *** b14 ***\n")

  data = rea_self_input$itempool
  field1 = input$cate_attr_sel


  gen_pool_scatter_plot (items = data, fields = field1)


})




output$num_attr_upper_lower_text <-
  renderText({ paste0(round(input$num_attr_upper_lower[1] * input$test_length_create_slider[1], 0), " to ",
                      round(input$num_attr_upper_lower[2] * input$test_length_create_slider[2], 0), " items") })

output$add_num_attr_btn_UI <- renderUI({

  req(rea_self_input$itempool, input$num_attr_radiobox)

  cat("### ", format(Sys.time(), "%X"), " output$add_num_attr_btn_UI *** b15 ***\n")

  actionButton("add_num_attr_btn", label = "Add to Constraints", icon = icon("plus"))


})


observeEvent(input$add_num_attr_btn, {

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$add_num_attr_btn *** b16 ***\n")

  for (i in 1:length(input$num_attr_radiobox)) {
    radioBoxChoice = strsplit(input$num_attr_radiobox[i], split = "has")[[1]]
    this_num_attr_to_cons = list(
      ATTRIBUTE = input$num_attr_sel,
      ATTRIBUTE_VALUE = NA,
      RANGE_FROM = as.numeric(unlist(strsplit(radioBoxChoice, " ~ "))[1]),
      RANGE_TO = as.numeric(unlist(strsplit(radioBoxChoice, " ~ "))[2]),
      LOWER = input$num_attr_upper_lower[1],
      UPPER = input$num_attr_upper_lower[2],
      LABEL = paste0(input$num_attr_sel, "-", radioBoxChoice[i])
    )

    # add to $attr_to_cons_holder only when it does not exist already

    if (sum(unlist(map(rea_self_input$attr_to_cons_holder, identical, y = this_num_attr_to_cons))) == 0)
       rea_self_input$attr_to_cons_holder[[length(rea_self_input$attr_to_cons_holder) + 1]] =
         this_num_attr_to_cons
  }

})

output$attr_to_cons_tbl <- renderDT({

  req(rea_self_input$itempool, rea_self_input$attr_to_cons_holder)

  cat("### ", format(Sys.time(), "%X"), " output$attr_to_cons_tbl <- renderDT *** b17 ***\n")

  datatable(bind_rows(rea_self_input$attr_to_cons_holder),
            # selection = "none",
            rownames = T, #editable = T
            editable = list(target = 'cell',
                            disable = list(columns = c(0,1,2,7)))
            )

})

proxy = dataTableProxy('attr_to_cons_tbl')

observeEvent(input$attr_to_cons_tbl_cell_edit, {
  cat("### ", format(Sys.time(), "%X"), " input$attr_to_cons_tbl_cell_edit *** bbb ***\n")
  info = input$attr_to_cons_tbl_cell_edit
  str(info)
  i = info$row
  j = info$col
  v = info$value
  print(paste0("i=", i))
  print(paste0("j=", j))
  print(paste0("v=", v))

  print(rea_self_input$attr_to_cons_holder[[i]][j])
  # newValue = DT::coerceValue(v, 1L)
  rea_self_input$attr_to_cons_holder[[i]][j] <<- DT::coerceValue(v, 1.0) #rea_self_input$attr_to_cons_holder[[i]][j])
  print(rea_self_input$attr_to_cons_holder[[i]][j])
  replaceData(proxy, bind_rows(rea_self_input$attr_to_cons_holder), resetPaging = F)  # important
})

output$attr_to_cons_tbl_del_btn_UI <- renderUI({

  req(rea_self_input$itempool, rea_self_input$attr_to_cons_holder)

  cat("### ", format(Sys.time(), "%X"), " output$attr_to_cons_tbl_del_btn_UI *** b17.5 ***\n")

  actionButton("attr_to_cons_tbl_del_btn", "Select & Delete Row(s)", icon = icon("delete"))

})

observeEvent(input$attr_to_cons_tbl_del_btn,{

  if (!is.null(input$attr_to_cons_tbl_rows_selected)) {
    rea_self_input$attr_to_cons_holder = rea_self_input$attr_to_cons_holder[-as.numeric(input$attr_to_cons_tbl_rows_selected)]
  }
})

output$populate_to_pool_cons_btn_UI <- renderUI({

  req(rea_self_input$itempool, rea_self_input$attr_to_cons_holder)

  cat("### ", format(Sys.time(), "%X"), " output$populate_to_pool_cons_btn_UI *** b18 ***\n")

  actionButton("populate_to_pool_cons", label = "Update Pool & Create Blueprint", icon = icon("populate"))

})


observeEvent(input$populate_to_pool_cons, {

  req(rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$populate_to_pool_cons *** b19 ***\n")

  # pool = selPoolFile()[[1]]
  pool = rea_self_input$itempool

  attr_to_cons_holder = bind_rows(rea_self_input$attr_to_cons_holder)

  # remove duplicated rows if any from attr_to_cons_holder (repeated row has been prevented from UI)

  # attr_to_cons_holder = attr_to_cons_holder[!duplicated.data.frame(attr_to_cons_holder),]

  # create constraints based on attr_to_cons_holder

  CONS_cols = lapply (1:nrow(attr_to_cons_holder), function(i) {

    con = attr_to_cons_holder[i,]

    if (!is.na(con$ATTRIBUTE_VALUE)) {

      as.integer(pool[[con$ATTRIBUTE]] %in% con$ATTRIBUTE_VALUE)

    } else {

      as.integer(between(pool[[con$ATTRIBUTE]], con$RANGE_FROM, con$RANGE_TO,
                         incbounds=TRUE))
    }
  })

  # add CONSXY to cons, which will be used as constraint ids

  CONS_ID = paste0("CONS", add_leading_char(1:nrow(attr_to_cons_holder)) )
  names(CONS_cols) = CONS_ID
  attr_to_cons_holder = bind_cols(CONS_ID = CONS_ID, attr_to_cons_holder)


  # update the reactive values--$pool to containts latest constraints through UI

  # remove previous CONSXY

  if ("CONS1" %in% names(pool))
    pool <- pool %>% select(-matches("^CONS\\d+"))

  # add CONSXY

  rea_self_input$itempool = bind_cols(pool, CONS_cols)

  # create blueprint based on latest constraints through UI

  rea_self_input$constraints = create_blueprint(attr_to_cons_holder)

})


create_blueprint = function (attr_to_cons_holder) {
  blueprint = tibble(
    CONS_ID = attr_to_cons_holder$CONS_ID,
    WEIGHT = if ("WEIGHT" %in% names(attr_to_cons_holder)) attr_to_cons_holder$WEIGHT else 100,
    LOWER = if ("LOWER" %in% names(attr_to_cons_holder)) attr_to_cons_holder$LOWER else 0.0,
    UPPER = if ("UPPER" %in% names(attr_to_cons_holder)) attr_to_cons_holder$UPPER else 1.0,
    LABEL = attr_to_cons_holder$LABEL
  )

  return (blueprint)
}

output$my_blueprint_tbl <- renderDT({

  req(rea_self_input$itempool, rea_self_input$constraints)

  cat("### ", format(Sys.time(), "%X"), " output$my_blueprint_tbl <- renderDT *** b20 ***\n")

  rea_self_input$constraints

})

output$from_create_to_simulator_UI <- renderUI({

  req(rea_self_input$constraints, rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " output$from_create_to_simulator_UI *** b21 ***\n")

  UIs = tagList()

  UIs[[1]] = textInput(inputId = "create_input_test_name", label = "Test Name", value = "My Test", placeholder = NULL)

  UIs[[2]] = radioButtons(inputId = "send_to_sim_cb_radio", label = "Content Balancing Method: ", inline = T,
                          choices = c("optimal" = "optimal", "adaptive" = "adaptive"))

  UIs[[3]] = actionButton("from_create_to_simulator_btn", label = "Send to Simulator", icon = icon("send"))

  UIs

})



observeEvent(input$from_create_to_simulator_btn, {

  req(rea_self_input$constraints, rea_self_input$itempool)

  cat("### ", format(Sys.time(), "%X"), " observeEvent(input$from_create_to_simulator_btn *** b22 ***\n")

  rea_simulator$latest <-
    createInputToSimulation(
    test_name = input$create_input_test_name,
    constraints = list(content = rea_self_input$constraints),
    itempool = rea_self_input$itempool)

  newSimulation(rea_simulator$latest)

  updateTabItems(session, "tabs", "simulator_input_tab")
  updateSelectInput(session, inputId = "simulation_example_select", selected = "pick example")

})

createInputToSimulation = function (test_name, constraints, itempool) {

  simulation = list(
    test_name = test_name
  )

  # Handle simulation$constraints
  # CONS_ID WEIGHT LOWER UPPER LABEL
  # <chr>    <dbl> <dbl> <dbl> <chr>
  # CONS01     100  0.1   0.15 1A

  simulation$constraints = constraints

  # Handle simulation$itempool
  # ITEM_ID MODEL    NC PAR_1 PAR_2 PAR_3 CONS_IDS
  # <chr>   <chr> <int> <dbl> <dbl> <dbl> <list>
  # ITEM001 3PL       2  1.7  -0.48  0.17 <chr [2]>

  CONS = itempool[,which(substr(names(itempool), 1,4) == "CONS" )]

  simulation$itempool = tibble(
    ITEM_ID = itempool$ITEM_ID,
    MODEL = itempool$IRT_MODEL,
    NC = itempool$IRT_CAT,

    PAR_1 = itempool$IRT_PAR_A * itempool$IRT_SC,
    PAR_2 = itempool$IRT_PAR_B,
    PAR_3 = itempool$IRT_PAR_C,
    CONS_IDS = lapply(1:nrow(CONS), function(i) {names(CONS)[which(CONS[i,]==1)]})
  )

  # Handle simulation$control
  simulation$control = list()
  simulation$control$abilityEstimator = "mle"
  simulation$control$startTheta = 0.0
  simulation$control$generateModules = TRUE
  simulation$control$itemSelectionRule = input$send_to_sim_cb_radio
  simulation$control$terminationRule = "asap"
  simulation$control$terminationValue = 0.4
  simulation$control$minItems = input$test_length_create_slider[1]
  simulation$control$maxItems = input$test_length_create_slider[2]
  simulation$control$iecPars = 1
  simulation$control$consWeight = 1
  simulation$control$infWeight = 1

  return(simulation)
}
