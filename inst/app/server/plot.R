



get_IRT_plot = function (lp_obj, IRT_fun, data_to_exclude,  to_showlegend = T) {

  cat("### ", format(Sys.time(), "%X"), " get_IRT_plot--", IRT_fun, "\n")

  plotSupported = c("Information", "Expected_Score", "CSEM")

  if (!(IRT_fun %in% plotSupported)) return ()

  # lp_obj <- rea_ata$ata_list$output$lp_obj

  mst_path = lp_obj$mst$path

  items <- as.data.frame(lp_obj$items)

  # if (!is.null(input$IRT_plot_module_exclude) && length(input$IRT_plot_module_exclude) > 1) {
  if (!is.null(data_to_exclude) && length(data_to_exclude) > 1) {

    # get the modules that want to be included in the plot from input$IRT_plot_module_exclude
    # included_module = as.integer(input$IRT_plot_module_exclude)

    included_module = as.integer(data_to_exclude)

    if (length(included_module) < length(mst_path[[1]])) return ()

    forms <- lp_obj$output$form %>%
      filter((form_ind %in% included_module)) %>%
      split.data.frame(f = .$form_ind)

  } else
    forms <- lp_obj$output$form %>% split.data.frame(f = .$form_ind)

  # $`5` # the 5th form
  # A tibble: 36 x 3
  # form_ind item_ind item_id
  # <int>    <int> <chr>
  # 1        5       18 item-018
  # 2        5       27 item-027
  # 3        5       55 item-055

  thetas = round(seq(THETA_LOWER, THETA_UPPER, by = 0.1), 1)

  # find IRT parameter columns
  column = intersect(names(items), unlist(lp_obj$alias[grep("^irt_par", names(lp_obj$alias))]))

  IRT_fun_name <- switch(IRT_fun,
                         Information = "Information",
                         CSEM = "CSEM",
                         Expected_Score = "Expected Score")

  data <- switch(IRT_fun,

                Information =
                       as_tibble(do.call(rbind,
                                        lapply(forms, function (f) {
                                          colSums(fisher(theta     = thetas,
                                                         IRT.par   = items[f$item_ind, column],
                                                         IRT.model = items[f$item_ind, lp_obj$alias$irt_model],
                                                         IRT.scale = items[f$item_ind, lp_obj$alias$irt_scale]))
                                        }))),

                CSEM = {
                       do.call(rbind,
                                    lapply(forms, function (f) {
                                      colSums(fisher(theta     = thetas,
                                                     IRT.par   = items[f$item_ind, column],
                                                     IRT.model = items[f$item_ind, lp_obj$alias$irt_model],
                                                     IRT.scale = items[f$item_ind, lp_obj$alias$irt_scale]))
                                    })) %>% as_tibble() %>% mutate_all(funs(1 / sqrt(.)))},

                Expected_Score =
                  as_tibble(do.call(rbind,
                                    lapply(forms, function (f) {
                                      expected_score(theta     = thetas,
                                                             IRT.par   = items[f$item_ind, column],
                                                             IRT.model = items[f$item_ind, lp_obj$alias$irt_model],
                                                             IRT.scale = items[f$item_ind, lp_obj$alias$irt_scale],
                                                             is_percent_scale = T)
                                    })))

  )

  colnames(data) <- thetas

  data = bind_cols(id = rownames(data), data)

  n_panel <- 1 #input$ata_n_panel

  n_form_per_panel = length(forms)/n_panel

  data = bind_cols(panel = paste0("P", c(sapply(1:n_panel, rep.int, times = n_form_per_panel))),
                   # form = paste0("F", rep.int(1:n_form_per_panel, times = n_panel)),
                   form = paste0("F", add_leading_char(rep.int(as.integer(names(forms)), times = n_panel))),
                   data)

  # change the shape of inf to long format

  data_long  = melt(as.data.frame(data), id=c("id", "panel", "form"))

  colnames(data_long) = c("Id", "Panel", "Form", "Theta", IRT_fun)

  p <- data_long %>%
    group_by(Id) %>%
    plot_ly(x = ~Theta, y = ~get(IRT_fun), color = ~ as.factor(Form), legendgroup = ~ as.factor(Form),
            showlegend = to_showlegend ) %>%
            # linetype = ~as.factor(Panel),
    layout(xaxis = list(range = c(THETA_LOWER, THETA_UPPER)))
    # add_lines(alpha = 0.55)

  if (length(unique(data_long$Panel)) > 1)
      p <- p %>% add_lines(alpha = 0.55, linetype = ~as.factor(Panel)) else
      p <- p %>% add_lines(alpha = 0.55)


  if (IRT_fun == "Expected_Score")
    p <- p %>% layout(yaxis = list(range = c(0, 1), autorange = F, title = IRT_fun_name)) else

  if (IRT_fun == "CSEM")
    p <- p %>% layout(yaxis = list(range = c(0, 4), autorange = F, title = IRT_fun_name)) else

    p <- p %>% layout(yaxis = list(title = IRT_fun_name))

  p$elementId <- NULL  # to avoid warning

  return (p)

}


get_IRT_plot_by_path = function (lp_obj, IRT_fun, mst_path, data_to_exclude, to_showlegend = T) {

  cat("### ", format(Sys.time(), "%X"), " get_IRT_plot_by_path--", IRT_fun,  "\n")

  plotSupported = c("Information", "Expected_Score", "CSEM")

  if (!(IRT_fun %in% plotSupported)) return ()

  # lp_obj <- rea_ata$ata_list$output$lp_obj

  items <- as.data.frame(lp_obj$items)

  good_mst_path = 1:length(mst_path)

  if (!is.null(data_to_exclude) && length(data_to_exclude) > 1) {

    # get the modules that want to be included in the plot from input$IRT_plot_module_exclude
    included_module = as.integer(data_to_exclude)

    if (length(included_module) < length(mst_path[[1]])) return ()

    # forms only include the modules checked on input$IRT_plot_module_exclude
    forms <- lp_obj$output$form %>%
      # filter(form_ind %in% included_module) %>%       #as.integer(input$IRT_plot_module_exclude))) %>%
      split.data.frame(f = .$form_ind)

    # each module in the included_path must be in included_module
    good_mst_path = which(unlist(lapply(mst_path, function (i) length(intersect(i,included_module)) == length(i))))
    if (length(good_mst_path) < 1) return()

    included_path = mst_path[good_mst_path]
    tests = lapply(included_path, function (p) bind_rows(forms[p]))
    # print("included path: ")
    # print(included_path)

  } else {
    forms <- lp_obj$output$form %>% split.data.frame(f = .$form_ind)
    tests = lapply(mst_path, function (p) bind_rows(forms[p]))
  }

  thetas = round(seq(THETA_LOWER, THETA_UPPER, by = 0.1), 1)

  column = intersect(names(items), unlist(lp_obj$alias[grep("^irt_par", names(lp_obj$alias))]))

  IRT_fun_name <- switch(IRT_fun,
                         Information = "Information",
                         CSEM = "CSEM",
                         Expected_Score = "Expected Score")

  data <- switch(IRT_fun,

                 Information =
                   as_tibble(do.call(rbind,
                                     lapply(tests, function (f) {
                                       colSums(fisher(theta     = thetas,
                                                      IRT.par   = items[f$item_ind, column],
                                                      IRT.model = items[f$item_ind, lp_obj$alias$irt_model],
                                                      IRT.scale = items[f$item_ind, lp_obj$alias$irt_scale]))
                                     }))),

                 CSEM = {
                   do.call(rbind,
                           lapply(tests, function (f) {
                             colSums(fisher(theta     = thetas,
                                            IRT.par   = items[f$item_ind, column],
                                            IRT.model = items[f$item_ind, lp_obj$alias$irt_model],
                                            IRT.scale = items[f$item_ind, lp_obj$alias$irt_scale]))
                           })) %>% as_tibble() %>% mutate_all(funs(1 / sqrt(.)))},

                 Expected_Score =
                   as_tibble(do.call(rbind,
                                     lapply(tests, function (f) {
                                       expected_score(theta     = thetas,
                                                      IRT.par   = items[f$item_ind, column],
                                                      IRT.model = items[f$item_ind, lp_obj$alias$irt_model],
                                                      IRT.scale = items[f$item_ind, lp_obj$alias$irt_scale],
                                                      is_percent_scale = T)
                                     })))

  )

  colnames(data) <- thetas

  data = bind_cols(id = rownames(data), data)

  # n_panel <- input$ata_n_panel
  n_panel = 1  #later

  n_test_per_panel = length(tests)/n_panel

  test_names <- map(mst_path[good_mst_path], paste0, collapse = "-") %>% unlist()
  data = bind_cols(panel = paste0("P", c(sapply(1:n_panel, rep.int, times = n_test_per_panel))),
                   test = test_names, #paste0("T", rep.int(1:n_test_per_panel, times = n_panel)),
                   data)

  # change the shape of inf to long format

  data_long  = melt(as.data.frame(data), id=c("id", "panel", "test"))

  colnames(data_long) = c("Id", "Panel", "Test", "Theta", IRT_fun)

  p <- data_long %>%
    group_by(Id) %>%  #
    plot_ly(x = ~Theta, y = ~get(IRT_fun), color = ~ as.factor(Test), legendgroup = ~ as.factor(Test),
            showlegend = to_showlegend) %>%
            # linetype = ~as.factor(Panel))
    layout(xaxis = list(range = c(THETA_LOWER, THETA_UPPER) ))

    # add_lines(alpha = 0.55)


  if (length(unique(data_long$Panel)) > 1)
    p <- p %>% add_lines(alpha = 0.55, linetype = ~as.factor(Panel)) else
    p <- p %>% add_lines(alpha = 0.55)

  if (IRT_fun == "Expected_Score")
    p <- p %>% layout(yaxis = list(range = c(0, 1), autorange = F, title = IRT_fun_name)) else

  if (IRT_fun == "CSEM")
    p <- p %>% layout(yaxis = list(range = c(0, 4), autorange = F, title = IRT_fun_name)) else

    p <- p %>% layout(yaxis = list(title = IRT_fun_name))

  p$elementId <- NULL  # to avoid warning


  return (p)

}


gen_3in1_IRT_plot = function (lp_obj, by_mst_path_or_by_form, module_IRT_plot_module_exclude, mst_path_select = NULL) {


  if (by_mst_path_or_by_form == "mst_path") {

      content = lp_obj$constraint$content
      all_mst_path = get_mst_path_from_content(content)
      mst_path = all_mst_path[as.integer(mst_path_select)]

      p1 <- get_IRT_plot_by_path (lp_obj = lp_obj, IRT_fun = "Information",
                                  mst_path = mst_path, data_to_exclude = module_IRT_plot_module_exclude,
                                  to_showlegend = T)
      p2 <- get_IRT_plot_by_path (lp_obj = lp_obj, IRT_fun = "Expected_Score",
                                  mst_path = mst_path, data_to_exclude = module_IRT_plot_module_exclude,
                                  to_showlegend = F)
      p3 <- get_IRT_plot_by_path (lp_obj = lp_obj, IRT_fun = "CSEM",
                                  mst_path = mst_path, data_to_exclude = module_IRT_plot_module_exclude,
                                  to_showlegend = F)
  } else {

    p1 <- get_IRT_plot (lp_obj = lp_obj, IRT_fun = "Information",
                        data_to_exclude = module_IRT_plot_module_exclude, to_showlegend = T)

    p2 <- get_IRT_plot (lp_obj = lp_obj, IRT_fun = "Expected_Score",
                        data_to_exclude = module_IRT_plot_module_exclude, to_showlegend = F)

    p3 <- get_IRT_plot (lp_obj = lp_obj, IRT_fun = "CSEM",
                        data_to_exclude = module_IRT_plot_module_exclude, to_showlegend = F)

  }

  if (any(is.null(p1), is.null(p2), is.null(p3))) return ()
  p <- subplot(p1, p2, p3, nrows = 3, margin = 0.05, shareX = TRUE, titleY = T) #%>% config(displayModeBar = F)

  p$elementId <- NULL  # to avoid warning

  p
}


gen_hist_by_item_plot = function(lp_obj, field) {
  cat("### ", format(Sys.time(), "%X"), " gen_hist_plot_by_item\n")

  items <- as.data.frame(lp_obj$items)
  if (!(field %in% names(items))) return ()

  fieldCountByForm <- lp_obj$output$form %>%
    mutate(field_val = items[item_ind, field]) %>%
    group_by(field_val, form_ind, .drop = FALSE) %>%
    summarise(field_count = length(field_val), .groups = "drop")

  p_hist <- fieldCountByForm %>%
    plot_ly(type = "bar", orientation = "h", split = ~field_val, x = ~field_count, y = ~form_ind) %>%
    layout(barmode = "group",
           xaxis = list(title = "", showline = T, showgrid = T, gridwidth = 2),
           yaxis = list(title = "", showline = T, showgrid = T, autorange = "reversed", type = "category"))
  return(p_hist)
}

gen_hist_by_pt_plot = function(lp_obj, field) {
  cat("### ", format(Sys.time(), "%X"), " gen_hist_by_pt_plot\n")

  items <- as.data.frame(lp_obj$items)
  if (!(field %in% names(items))) return ()
  alias <- lp_obj$alias

  fieldCountByForm <- lp_obj$output$form %>%
    mutate(field_val = items[item_ind, field], pt = items[item_ind, alias$point]) %>%
    group_by(field_val, form_ind, .drop = FALSE) %>%
    summarise(field_count = sum(pt), .groups = "drop")

  p_hist <- fieldCountByForm %>%
    plot_ly(type = "bar", orientation = "h", split = ~field_val, x = ~field_count, y = ~form_ind) %>%
    layout(barmode = "group",
           xaxis = list(title = "", showline = T, showgrid = T, gridwidth = 2),
           yaxis = list(title = "", showline = T, showgrid = T, autorange = "reversed", type = "category"))
  return(p_hist)
}



genDonutChart = function (data, field, margin = NULL, opacity = NULL, groupBy = NULL) {

  if (is.null(margin)) margin = list(l = 70, r = 70, b = 70, t = 70, pad = 4)
  if (is.null(opacity)) opacity = 0.7

  if (is.null(groupBy)) {
    data = dplyr::count(data, !!as.name(field))

    p <- plot_ly() %>%
      add_pie(data = data, labels = as.formula2(field),
              values = ~n, name = field, opacity = opacity, hole = 0.5) %>%
      layout(title = field, margin = margin, showlegend = T,
             legend=list(title=list(text = str_c('<b> ', field ,' </b>'))), font = list(color = LEGENDCLR))

  } else {

    data <- dplyr::count(data, !!as.name(field), !!as.name(groupBy)) %>%
      mutate(colorCol = as.numeric(factor(.[[groupBy]]))) %>%
      mutate(!!sym(groupBy) := str_c(.[[field]], ":", .[[groupBy]]))

    n1 = length(unique(data[[field]]))
    colors1 = colorRampPalette(brewer.pal(9, "Pastel1"))(n1)
    n2 = length(unique(data$colorCol))
    colorVec = colorRampPalette(brewer.pal(8, "Set2"))(n2)
    colors2 = colorVec[data$colorCol]

    p <- plot_ly() %>%
      add_pie(data = data, labels = as.formula2(field), direction = 'clockwise', sort = F,
              values = ~n, name = groupBy, textinfo = 'label', textposition = 'inside',
              marker = list(colors = colors1), opacity = opacity,
              legendgroup=groupBy,
              domain = list(x=c(.2,.8), y = c(.2,.8))) %>%

      add_pie(data = data, labels = as.formula2(groupBy), values = ~n, name = field, opacity = opacity+.2,
              legendgroup = field,
              marker = list(colors = colors2),
              textinfo = 'label', hole = 0.7, direction = 'clockwise', sort = F) %>%
      layout(title = field, margin = margin,
             legend=list(title=list(text = str_c('<b> ', field, ":", groupBy,' </b>')), font = list(color = LEGENDCLR)))
  }

  p

}


gen_scatter_plot = function (lp_obj, fields) {

  cat("### ", format(Sys.time(), "%X"), " call plot::gen_scatter_plot()\n")

  items = as.data.frame(lp_obj$items)
  form = lp_obj$output$form

  # two variable scatter plots (not both categorical variables)

  if (length(fields) > 1 && sum(c(is.categorical(items[[fields[1]]]), is.categorical(items[[fields[2]]]))) <= 1) {

    print("two variables")

    forms <- form %>% mutate(id = paste0("F", add_leading_char(form_ind))) %>%
      mutate(field1 = items[item_ind, fields[1]]) %>%
      mutate(field2 = items[item_ind, fields[2]],  item_ind = NULL, form_ind = NULL) %>%
      bind_rows(tibble(id = "Pool", field1 = items[, fields[1]], field2 = items[, fields[2]]), .)

    p <- plot_ly(forms, x = ~ field2, y = ~field1, color = ~id, type = 'scatter', mode = 'markers') %>%
         layout(xaxis = list(title = fields[2]), yaxis = list(title = fields[1]))

    return(p)
  }

  # two categorical variables  (UI does not have this combinations)

  if (length(fields) > 1 && sum(c(is.categorical(items[[fields[1]]]), is.categorical(items[[fields[2]]]))) ==2) {

    print("two categorical vars")

    forms <- form %>% mutate(id = paste0("F", add_leading_char(form_ind))) %>%
      mutate(field1 = items[item_ind, fields[1]]) %>%
      mutate(field2 = items[item_ind, fields[2]],  item_ind = NULL, form_ind = NULL) %>%
      bind_rows(tibble(id = "Pool", field1 = items[, fields[1]], field2 = items[, fields[2]]), .)

    forms2 <- forms %>% select(-item_id) %>% group_by(id) %>% table() %>% as_tibble() %>%
              slice (which ((field1 != 0 | field2 != 0 ) & id != "Pool"))

    p <- plot_ly(
      type = 'scatterpolar',
      # type = 'scatter',
      fill = 'toself'
    ) %>%
      # group_by(field1, field2) %>%
      add_trace(
        r = forms2$n[which(forms2$field1 == 1 & forms2$field2 == 0)],
        theta = forms2$id[which(forms2$field1 == 1 & forms2$field2 == 0)]
      )  %>%
      add_trace(
        r = forms2$n[which(forms2$field1 == 0 & forms2$field2 == 1)],
        theta = forms2$id[which(forms2$field1 == 0 & forms2$field2 == 1)]
      ) %>%
      add_trace(
        r = forms2$n[which(forms2$field1 == 1 & forms2$field2 == 1)],
        theta = forms2$id[which(forms2$field1 == 1 & forms2$field2 == 1)]
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,max(forms2$n))
          )
        )
      )
    return(p)
  }

  # # two variables
  #
  # if (length(fields) > 1 && !is.categorical(items[[fields[1]]]) && !is.categorical(items[[fields[2]]])) {
  #
  #   forms <- form %>% mutate(id = paste0("F", form_ind)) %>%
  #     mutate(field1 = items[item_ind, fields[1]]) %>%
  #     mutate(field2 = items[item_ind, fields[2]],  item_ind = NULL, form_ind = NULL) %>%
  #     bind_rows(tibble(id = "Pool", field1 = items[, fields[1]], field2 = items[, fields[2]]), .)
  #
  #   p <- plot_ly(forms, x = ~ field2, y = ~field1, color = ~id)
  #   layout(p, xaxis = list(title = fields[2]), yaxis = list(title = fields[1]))
  #
  #   return(p)
  # }

  # one variable scatter plots

  if (length(fields) == 1) {

    print("one var")

    forms <- form %>% mutate(id = paste0("F", add_leading_char(form_ind))) %>%
      mutate(field1 = items[item_ind, fields[1]],  item_ind = NULL, item_id = NULL, form_ind = NULL) %>%
      bind_rows(tibble(id = "Pool", field1 = items[, fields[1]]), .)

    p <- plot_ly(forms, x = ~ id, y = ~field1, color = ~id,
                 type = 'scatter',
                 mode = 'markers') %>%
      layout(xaxis = list(title = "Module"),
           yaxis = list(title = fields))

    return (p)
  }


}




genOneCateAttrPlot = function (data, attr, plotType = "histogram", groupBy = NULL) {

  if (class(data[[attr]]) == "list")
    data <- convertListColToStr (data = data, listColNames = NULL)

  if (is.null(groupBy))
    data <- data %>% mutate(!!sym(attr) := as.character(.[[attr]])) else
    data <- data %>% mutate(!!sym(attr) := as.character(.[[attr]])) %>%
        mutate(!!sym(groupBy) := as.character(.[[groupBy]]))

  opacity = .6
  m = list(l = 70, r = 70, b = 70, t = 70, pad = 2)

  switch(plotType,

         histogram = {
           plot_ly(data, x = as.formula2(attr), type = plotType,  opacity = opacity,
                             color = {if (is.null(groupBy))  as.formula2(attr) else as.formula2(groupBy)}) %>%
           layout(margin = m,
                  legend=list(title=list(text = str_c('<b> ', {if (is.null(groupBy)) attr else groupBy},' </b>')),
                              font = list(color = LEGENDCLR)))},
         pie = genDonutChart (data, field = attr, margin = m, opacity = opacity, groupBy = groupBy))

}


genOneNumeAttrPlot = function (data, attr, plotType = 'scatter', groupBy = NULL, ...) {

  m = list(l = 70, r = 70, b = 70, t = 70, pad = 2)
  opacity = 0.7

  if (!is.null(groupBy))
    data <- data %>% mutate(!!sym(groupBy) := as.character(.[[groupBy]]))

  p <- switch(plotType,
              scatter = plot_ly(
                data = data, y = 1:length(data[[attr]]), x = as.formula2(attr),
                type = plotType, mode = 'markers', opacity = opacity,
                color = {if (is.null(groupBy)) "blue" else as.formula2(groupBy)}),
              histogram = plot_ly(
                data = data, x = as.formula2(attr), type = plotType,
                marker = list(line = list(color = 'darkgray', width = 2)), opacity = opacity, margin = m,
                color = {if (is.null(groupBy)) "blue" else as.formula2(groupBy)}
              ) %>% layout(barmode="stack",bargap=0.1))
  if (is.null(groupBy))
    p <- p %>% layout(title = attr, margin = m, xaxis = list(title = ""),  ...) else
    p <- p %>% layout(title = attr, margin = m, xaxis = list(title = ""),
                      legend=list(title=list(text = str_c('<b> ',groupBy,' </b>')),
                                  font = list(color = LEGENDCLR)), ...)
}



gen_pool_scatter_plot = function (items, fields) {

  cat("### ", format(Sys.time(), "%X"), " call plot::gen_pool_scatter_plot()\n")

  # one variable scatter plots

  if (length(fields) == 1) {


    if (is.categorical(items[[fields[1]]])) {
      data = as.data.frame(table(items %>% select(fields)))
      names(data)[1] = fields[1]

      p <- plot_ly(data, x = as.formula(paste0("~", fields[1])),
                   y = ~ Freq, color = as.formula(paste0("~", fields[1])),
                   type = 'bar', opacity = '.5',
                   mode = 'markers')

    } else {
      data = items %>% select(fields[1])

      p <- plot_ly(data, x = as.formula(paste0("~", fields[1])),
                   type = 'histogram', opacity = '.5')
                   # color = ~ LINKING_ITEM)
    }

    return (p)
  }

  # two num variables

  if (sum(c(is.categorical(items[[fields[1]]]), is.categorical(items[[fields[2]]]))) == 0) {

    print("two numeric variables")

    p <- plot_ly(items, x = as.formula(paste0("~", fields[2])),
                 y = as.formula(paste0("~", fields[1])),
                 type = 'scatter', mode = 'markers') %>%
      layout(xaxis = list(title = fields[2]), yaxis = list(title = fields[1]))

    return(p)
  }

  # two variable scatter plots (not both categorical variables)

  if (sum(c(is.categorical(items[[fields[1]]]), is.categorical(items[[fields[2]]]))) <= 1) {

    print("two variables with 1 or less categorical variable")

    p <- plot_ly(items, x = as.formula(paste0("~", fields[2])),
                 y = as.formula(paste0("~", fields[1])),
                 type = 'scatter', mode = 'markers') %>%
      layout(xaxis = list(title = fields[2]), yaxis = list(title = fields[1]))

    return(p)
  }

  # two categorical variables

  if ( sum(c(is.categorical(items[[fields[1]]]), is.categorical(items[[fields[2]]]))) ==2) {

      # > head(cons_tbl)
      # # A tibble: 6 x 5
      # CONS_ID WEIGHT LABEL key   value
      # <chr>    <dbl> <chr> <chr> <dbl>
      # 1 CONS01     100 1A    UPPER  0.15
      # 2 CONS02     100 1B    UPPER  0.15
      # 3 CONS03     100 1C    UPPER  0.15
      # 4 CONS04     100 RC1   UPPER  0.35

      print("two categorical variables")

      data = as.data.frame(table(items %>% select(fields)))
      p <- plot_ly(data, x = as.formula(paste0("~", fields[1])),
                 y = ~Freq,
                 color = as.formula(paste0("~", fields[2])),
                 type = 'bar', opacity = '.7') %>%
                 layout(barmode = 'stack')

    return(p)
  }


}




gen_pool_plot2 = function (data, field1, field2 = NULL, plot_type, to_group_by = NULL) {

  cat("### ", format(Sys.time(), "%X"), " call plot::gen_pool_scatter_plot()\n")

  # two categorical variables

  if (plot_type == "two_cate_var_freq_bar")
    to_group_by = field2

  if (plot_type == "one_cate_var_freq_bar" || plot_type == "two_cate_var_freq_bar") {

    if (!is.null(to_group_by) && to_group_by %in% names(data)) {

      data0 = as.data.frame(table(data %>% select(field1, to_group_by)))

      p <- plot_ly(data0, x = as.formula(paste0("~", field1)),
                   y = ~Freq,
                   color = {if (is.null(to_group_by)) as.formula(paste0("~", field1)) else as.formula(paste0("~", to_group_by))},
                   type = 'bar', opacity = '.5') %>%
        layout(barmode = 'stack')
    } else {
      data0 = as.data.frame(table(data %>% select(field1)))

      p <- plot_ly(data0, x = as.formula(paste0("~", field1)),
                   y = ~Freq,
                   type = 'bar', opacity = '.5') %>%
        layout(barmode = 'stack')
    }
    return(p)
  }

  # two numerical variables with optional to_group_by

  if (plot_type == "tow_num_var_scatter") {

    p <- plot_ly(data, x = as.formula(paste0("~", field1)),
                 y = as.formula(paste0("~", field2)),
                 color = {if (is.null(to_group_by)) as.formula(paste0("~", field1)) else as.formula(paste0("~", to_group_by))},
                 type = 'scatter', mode = 'markers') %>%
      layout(xaxis = list(title = field1), yaxis = list(title = field2))

    return(p)
  }


  # one cate and one num scatter plot

  if (plot_type == "one_num_one_cate_scatter") {

    p <- plot_ly(data, x = as.formula(paste0("~", field2)),
                 y = as.formula(paste0("~", field1)),
                 opacity = '.5',
                 type = "scatter", mode = 'markers'
                 ) %>%
      layout(xaxis = list(title = field2), yaxis = list(title = field1))

    return(p)
  }

  # one cate and one num bar plot (histogram by num var and group by cate var)

  if (plot_type == "one_num_one_cate_bar") {

    if (length(unique(data[[field1]])) > length(unique(data[[field2]]))) {
      cate_var = field2
      num_var = field1
    } else {
      cate_var = field1
      num_var = field2
    }
    # data0 = data %>% select(num_var)

    p <- plot_ly(data, x = as.formula(paste0("~", num_var)),
                 color = as.formula(paste0("~", cate_var)),
                 type = 'histogram', opacity = '.5') %>%
      layout(xaxis = list(title = num_var), yaxis = list(title = "Frequency"),
             barmode = "stack", bargap = 0.15)

    return(p)
  }

  # one numerical variable scatter plots with optional group_by

  # if (is.null(field2 )) {
  #
  #   print("one var")
  #
  #   if (is.categorical(items[[fields[1]]])) {
  #     data = as.data.frame(table(items %>% select(fields)))
  #     names(data)[1] = fields[1]
  #     p <- plot_ly(data, x = as.formula(paste0("~", fields[1])),
  #                  y = ~ Freq, color = as.formula(paste0("~", fields[1])),
  #                  type = 'bar', opacity = '.5'
  #                  # mode = 'markers'
  #                  )
  #
  #   } else { # one num variable
  #     data = items %>% select(fields[1])
  #
  #     p <- plot_ly(data, x = as.formula(paste0("~", fields[1])),
  #                  type = 'histogram', opacity = '.5')
  #   }
  #
  #   return (p)
  # }
}


get_content_plot = function (lp_obj, form, items, content_field) {

  cat("### ", format(Sys.time(), "%X"), " get_content_plot\n")

  # saveRDS(form, file = "form.rds")
  # saveRDS(items, file = "items.rds")
  # form = readRDS(file = "catsuite/form.rds")
  # items = readRDS(file = "catsuite/items.rds")
  # content_field = c("EVIDENCE_STATEMENT", "IRT_MAX_POINTS", "TE_FLAG")
 # print(content_field)

  forms <- form %>%
    bind_cols(., items[.$item_ind, content_field]) %>%

    mutate(item_ind = NULL, item_id = NULL) %>%
    aggregate(. ~ form_ind, data = ., sum) %>%
    melt(id = "form_ind") %>% mutate(form_ind = as.factor(form_ind))

  # form_ind variable value
  # 1        1       C1     1
  # 2        2       C1     0
  # 3        3       C1     0

  p <- plot_ly(forms, x = ~ variable, y = ~value, color = ~ form_ind) %>%
    add_bars() %>%
    layout(barmode = "stack")


  return(p)

}


gen_histogram_subplots = function (data, var_name, y_range, x_range) {

  # max_y = floor(max(unlist(map(data, nrow)))/2)
  # min_x = floor(min(itempool$PAR_2))
  # max_x = ceiling(max(itempool$PAR_2))

  # scatter_plot <- function(d, the.name) {
  #   plot_ly(d, x = as.formula(paste0("~", var_name)), type = "histogram", opacity = 0.5,
  #           autobinx = F, name = the.name,
  #           xbins = list(start = min_x, end = max_x, size = 1)) %>%
  #     layout(yaxis = list(range = y_range),
  #            xaxis = list(range = x_range),
  #            bargap=0.2)
  # }

  plot2 <- function(i) {
    plot_ly(data[[i]], x = as.formula(paste0("~", var_name)), type = "histogram", opacity = 0.5,
            autobinx = F, name = names(data[i]),
            xbins = list(start = x_range[1], end = x_range[2], size = 1)) %>%
      layout(yaxis = list(range = y_range),
             xaxis = list(range = x_range),
             bargap=0.2)
  }

  # p <- data %>% lapply(scatter_plot, the.name = "AA") %>%
  p <- lapply(1:length(data), plot2) %>%
    subplot(nrows = floor(length(data)/5)+1, shareY = T, shareX = F, titleX = F) #%>% hide_legend()

  return (p)
}

gen_dot_histtogram = function (data) {

  # # don=don %>% mutate(text=paste("ID: ", rownames(iris), "\n", "Sepal Length: ", Sepal.Length, "\n", "Species:: ", Species, sep="" ))
  # data$form_ind = as.factor(data$form_ind)
  # p <- plot_ly(data, x = ~ variable, y = ~value, color = ~ form_ind) %>%
  #   add_bars() %>%
  #   layout(barmode = "stack")
  # p

  # A classic histogram for the iris data set (left)
  # ggplot(iris, aes(x=Sepal.Length)) +
  #   geom_histogram()

  # # Transform a litte bit the dataset to make dots
  # don = iris %>%
  #   arrange(Sepal.Length) %>% # sort using the numeric variable that interest you
  #   mutate(var_rounded = (Sepal.Length+1) - ( (Sepal.Length+1) %% 0.2 ) ) %>% # This attributes a bin to each observation. Here 0.2 is the size of the bin.
  #   mutate(y=ave(var_rounded, var_rounded, FUN=seq_along)) # This calculates the position on the Y axis: 1, 2, 3, 4...
  #
  # # Make the plot (middle)
  # # ggplot(don, aes(x=var_rounded, y=y) ) +
  # #   geom_point( size=6, color="skyblue" )
  #
  # # Improve the plot, and make it interactive (right)
  # don=don %>% mutate(text=paste("ID: ", rownames(iris), "\n", "Sepal Length: ", Sepal.Length, "\n", "Species:: ", Species, sep="" ))
  # p=ggplot(don, aes(x=var_rounded, y=y) ) +
  #   geom_point( aes(text = text), size = 6, alpha = 0.7, color="skyblue" ) +
  #   xlab('Sepal Length') +
  #   ylab('# of individual') +
  #   theme_classic() +
  #   theme(
  #     legend.position="none",
  #     axis.line.y = element_blank(),
  #     axis.text=element_text(size=15)
  #   )
  # p
  #
  # # Use the magic of ggplotly to have an interactive version
  # ggplotly(p, tooltip="text")
}
