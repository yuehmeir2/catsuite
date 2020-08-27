

get_n_lower_pt_lower = function (content) {

  # print(content$FORM)

  the_Form = unique(as.character(content$FORM))
  # > the_Form
  # [1] "1,2,4"  "1,2,5"  "1,3,5"  "1,3,6"  "1"      "2"      "3"      "4"      "5"      "6"
  # [11] "7,8,10" "7,8,11" "7,9,11" "7,9,12" "7"      "8"      "9"      "10"     "11"     "12"

  temp = c()
  # map(the_Form, strsplit, split = ",") %>% unlist() %>% unique()
  for (i in seq(length(the_Form)))
    temp = c(temp, eval(parse(text = paste0("c(", the_Form[i], ")"))))

  n_form = length(unique(temp))

  # move this out to the caller
  # if (n_form > 0 && n_form != input$ata_n_form)
    # updateNumericInput(session, "ata_n_form", value = n_form)

  # update number of items for forms

  n_lower = rep(0, n_form)
  n_upper = rep(0, n_form)

  if ("SCOPE" %in% names(content))
    temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "items" & (SCOPE == "WITHIN" | is.na(SCOPE))) %>%
    select("FORM", "SLICE_LOWER", "SLICE_UPPER") else
      temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "items") %>%
    select("FORM", "SLICE_LOWER", "SLICE_UPPER")

  if (nrow(temp) > 0)
    for (i in 1:nrow(temp)) {
      n_lower[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_LOWER[i])
      n_upper[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_UPPER[i])
    }

  # update number of points for forms

  pt_lower = rep(0, n_form)
  pt_upper = rep(0, n_form)

  if ("SCOPE" %in% names(content))
    temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "points" & (SCOPE == "WITHIN" | is.na(SCOPE))) %>%
    select("FORM", "SLICE_LOWER", "SLICE_UPPER") else
      temp <- content %>% filter(ATTRIBUTE == "_FORM" & SLICE_UNIT == "points") %>%
    select("FORM", "SLICE_LOWER", "SLICE_UPPER")

  if (nrow(temp) > 0)

    for (i in 1:nrow(temp)) {
      pt_lower[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_LOWER[i])
      pt_upper[eval(parse(text = paste0("c(", temp$FORM[i], ")")))] <- as.integer(temp$SLICE_UPPER[i])

    } else {
      pt_lower = n_lower
      pt_upper = n_upper
    }

  return (list(n_form = n_form, n_lower = n_lower, n_upper = n_upper, pt_lower = pt_lower, pt_upper = pt_upper))
}


add_leading_char = function (vec, leading_char = "0") {

  n_digit = floor(log10(max(vec))) + 1
  str_pad(vec, width=n_digit, side="left", pad=leading_char)

}






show_consViolations_DT = function (isr) {

  data = isr$consViolations

  dt <- datatable(data,
                  colnames = c('ON-TARGET' = 10),
                  extensions = 'Buttons',
                  options = list(dom = 'Bftrip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf'))) %>%

    formatRound(columns=4:nrow(data), digits=2) %>%

    formatStyle('ON-TARGET',
                fontWeight = styleEqual(1.0, 'bold'),
                backgroundColor = styleInterval(c(seq(0.1,0.9, by = 0.1), 0.99999), c(rev(RED10),  "#FFFFFF")))


  return (dt)

}


show_consAssignedItemCountDistr_DT = function (isr) {

  data = isr$consAssignedItemCountDistr

  temp = as_tibble(matrix(rep(1, nrow(data) * (ncol(data)-3)), nrow = nrow(data)))

  for (i in 1:nrow(data)) {
    if (data[i,2] > 1)  # for MIN_N
      for (j in 4:(4+as.integer(data[i,2])-1)) {
        if (data[i,j] > 0) temp[i,j-3] = 0
      }

    if (data[i,3] < ncol(data) && (as.integer(data[i,3])+5) <= ncol(data))
      for (j in (as.integer(data[i,3])+5):ncol(data)) {

        if (data[i,j] > 0) temp[i,j-3] = 2

      }
  }

  data2 <- bind_cols(data, temp)
  dt <- datatable(data2,
                  extensions = 'Buttons',
                  options = list( dom = 'Bftrip',
                                  buttons = c('copy', 'csv', 'excel'),
                                  columnDefs = list(list(targets = c((ncol(data)+1):(ncol(temp)+ncol(data))),
                                                         visible = FALSE)))) %>%
                  formatStyle(
                    c(4:ncol(data)), c((ncol(data)+1):(ncol(data)+ncol(temp))),
                    backgroundColor = styleEqual(c(0, 1, 2), c('#ffd8d8', '#edfcf8', '#ffd8d8'))
                  )

  return (dt)
}




reset_reactiveValues = function (whichWidget) {

  cat("### ", format(Sys.time(), "%X"), paste0(" reset", whichWidget, "\n"))

  reset_list = c("FORM_ASSEMBLY", "MODULE_ASSEMBLY", "SIMULATOR",
                 "SIMULATOR_CREATE_INPUT" , "MST_CREATE_INPUT", "ALL")

  if (!(whichWidget %in% reset_list)) return ()

  # reset "FORM_ASSEMBLY"

  if (whichWidget == "FORM_ASSEMBLY" || whichWidget == "ALL") {

    rea_input$tibbles = NULL
    rea_change_input_source = 0
    rea_ata$ptr = 0

    rea_ata$ata_list = list(
      input = list(
        input_data_set = NULL
      ),
      output = list()
    )
  }

  # reset "MODULE_ASSEMBLY"

  if (whichWidget == "MODULE_ASSEMBLY" || whichWidget == "ALL") {

    module_rea_input$tibbles = NULL
    module_rea_change_input_source = 0
    module_rea_ata$ptr = 0
    module_rea_ata$ata_list = list(
        input = list(
          input_data_set = NULL
        ),
        output = list()
      )
  }

  # reset "SIMULATOR"

  if (whichWidget == "SIMULATOR" || whichWidget == "ALL") {

    rea_simulator$latest = NULL
    rea_simulator$history = list()
    rea_simulator$selected = NULL
  }

  if (whichWidget == "SIMULATOR_CREATE_INPUT" || whichWidget == "ALL") {

    rea_self_input$itempool = NULL
    rea_self_input$attr_to_cons_holder = NULL
    rea_self_input$constraints = NULL
  }

  if (whichWidget == "MST_CREATE_INPUT" || whichWidget == "ALL") {

    rea_self_MST_input$itempool = NULL
    rea_self_MST_input$attr_to_cons_holder = NULL
    rea_self_MST_input$constraints = NULL
    rea_self_MST_input$all_path = NULL
    rea_self_MST_input$selected_path = NULL
  }
}


add_to_report = function (what, title, des = "", replace = T) {

  cat("### ", format(Sys.time(), "%X"), paste0("Add ", title, " to_report\n"))

  itemToAdd = list(content = what,
                   title = title,
                   description = des)

  if (replace && length(rea_report_tool$report_list) > 0 ) {

    titles <- lapply(rea_report_tool$report_list, "[[", "title") %>% unlist()
    titleAt = which(titles == title)
    if (length(titleAt) > 0) {
      rea_report_tool$report_list[[titleAt]] = itemToAdd # use isolate here not useful
      return()
    }
  }

  rea_report_tool$report_list[[length(rea_report_tool$report_list)+1]] = itemToAdd # use isolate here not useful

}

find_report_at = function (title) {

  if (length(rea_report_tool$report_list) > 0 ) {

    titles <- lapply(rea_report_tool$report_list, "[[", "title") %>% unlist()
    return(which(titles == title))

  } else NULL

}

get_modules_for_selected_path = function (paths) {

  if (class(paths) == "character") {
    strsplit(paste0(gsub("Path--", "", paths), collapse = ","), ",") %>% unlist() %>% unique() %>% sort()
  } else
    paths %>% unlist() %>% unique() %>% sort()

}


convertListOfNamedListToTibble = function (listOfNamedList, colNames = NULL, toCharCols = TRUE) {

  if (is.null(colNames))
    colNames = map(listOfNamedList, ~names(.x)) %>% unlist() %>% unique()

  if (toCharCols)
    return(map (seq_along(colNames), ~{
      map(listOfNamedList, colNames[.x]) %>% as.list() %>% map_chr(~paste(.x, collapse = ","))}) %>%
        set_names(colNames) %>% as_tibble()) else

    return(map (seq_along(colNames), ~{
      map(listOfNamedList, colNames[.x]) %>% as.list() }) %>%
        set_names(colNames) %>% as_tibble())

}


tblToConstraintControl = function (tbl) {

  tbl <- mutate_all(tbl, ~{str_split(.x, pattern = ",")})

  tbl$onSlots <- map(tbl$onSlots, as.integer)

  tbl <- tbl %>% split(seq(nrow(tbl)))

  tbl <- map(seq_along(tbl), ~map(tbl[[.x]], unlist)) %>%
    map( ~{.x[map_lgl(.x, ~{nchar(.x[1]) > 0})]})

  return(tbl)
}



