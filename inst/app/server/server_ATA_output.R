




output$form_tif_plot <- renderPlotly({
  
  validate(need(rea_ata$ata_list$output$lp_obj$output$form, message = FALSE))
  
  get_IRT_plot (IRT_fun = "Information")
  
})


output$form_csem_plot <- renderPlotly({

  validate(need(rea_ata$ata_list$output$lp_obj$output$form, message = FALSE))
  
  get_IRT_plot (IRT_fun = "CSEM")
  
})


output$form_tcc_plot <- renderPlotly({
  
  validate(need(rea_ata$ata_list$output$lp_obj$output$form, message = FALSE))
  
  get_IRT_plot (IRT_fun = "Expected_Score")
  
})

output$IRT_plot_module_exclude_UI <- renderUI({
  
  cat("### ", format(Sys.time(), "%X"), " IRT_plot_module_exclude_UI \n")
  
  n_form = rea_ata$ata_list$output$lp_obj$test_inf$n_form
  
  req(rea_ata$ata_list$output$lp_obj$output$form, n_form > 1)
  
  choices = 1:n_form
  names(choices) = paste0("F", 1:n_form)
  
  checkboxGroupInput("IRT_plot_module_exclude", NULL, 
                      choices = choices,
                      selected = choices,
                      inline = T)

})


output$three_in_1_IRT_plot <- renderPlotly({
  
  cat("### ", format(Sys.time(), "%X"), " output$three_in_1_IRT_plot\n")
  
  validate(need(rea_ata$ata_list$output$lp_obj$output$form, message = F))
  
  p = gen_3in1_IRT_plot (lp_obj = rea_ata$ata_list$output$lp_obj, 
                         by_mst_path_or_by_form = "forms", 
                         module_IRT_plot_module_exclude = input$IRT_plot_module_exclude, 
                         mst_path_select = NULL)
  
  return (p)
  
})


output$form_histFieldUI <- renderUI({
  
  req(rea_ata$ata_list$output$lp_obj$output$form)
  
  lp_obj <- rea_ata$ata_list$output$lp_obj
  
  items <- as.data.frame(lp_obj$items)
  
  attributes0 = unique(unlist(strsplit(lp_obj$constraint$content$ATTRIBUTE, ":")))
  attributes0 = attributes0[!grepl('^_', attributes0)]
  attributes0 = union(c(lp_obj$alias$irt_par_a, lp_obj$alias$irt_par_b, attributes0), lp_obj$alias$others)
  attributes0 = attributes0[unlist(
    sapply(attributes0, function (i) (is.categorical(items[,i]) || length(unique(items[, i])) < 3) & 
             length(unique(items[, i])) > 1)
  )]
  if (length(attributes0) <= 0) return ()
  
  print(attributes0)
  selectInput("ATA_hist_attr_select", label = "Attribute", 
              choices = attributes0, multiple = F)
})

output$hist_by_item_plot <- renderPlotly ({
  cat("### ", format(Sys.time(), "%X"), " output$hist_by_item_plot\n")
  req(rea_ata$ata_list$output$lp_obj$output$form)
  req(input$ATA_hist_attr_select)
  
  lp_obj <- rea_ata$ata_list$output$lp_obj
  p_hist <- gen_hist_by_item_plot(lp_obj = lp_obj, field = input$ATA_hist_attr_select)
  return(p_hist)
})

output$hist_by_pt_plot <- renderPlotly ({
  cat("### ", format(Sys.time(), "%X"), " output$hist_by_pt_plot\n")
  req(rea_ata$ata_list$output$lp_obj$output$form)
  req(input$ATA_hist_attr_select)
  
  lp_obj <- rea_ata$ata_list$output$lp_obj
  p_hist <- gen_hist_by_pt_plot(lp_obj = lp_obj, field = input$ATA_hist_attr_select)
  return(p_hist)
})


output$form_scatterFieldUI <- renderUI({
  
  req(rea_ata$ata_list$output$lp_obj$output$form)
  
  lp_obj = rea_ata$ata_list$output$lp_obj
  items <- as.data.frame(lp_obj$items)

  attributes0 = unique(unlist(strsplit(lp_obj$constraint$content$ATTRIBUTE, ":")))
  attributes0 = attributes0[!grepl('^_', attributes0)]
  attributes0 = union(c(lp_obj$alias$irt_par_a, lp_obj$alias$irt_par_b, lp_obj$alias$irt_par_c, attributes0), lp_obj$alias$others)
  attributes0 = attributes0[unlist(
    sapply(attributes0, function (i) (is.numeric(items[,i]))) )]

  attributes0 = attributes0[unlist(sapply(attributes0, function (i) is.numeric(items[,i])))]
  if (length(attributes0) <= 0) return ()
  
  # selectInput("ATA_scatter_attr_select", label = "Attribute", 
  #             choices = attributes0, multiple = F)
  
  selectUIs = tagList()
  selectUIs[[1]] = selectInput("form_attribute_A_select", label = "Attribute", choices = attributes0, 
                               multiple = F)
  
  non_cate_attr_at = !unlist(map(items[,attributes0], is.categorical))
  
  if (length(non_cate_attr_at) > 0)
    selectUIs[[2]] = selectInput("form_attribute_B_select", label = "Attribute", 
                                 choices = attributes0[non_cate_attr_at], multiple = F)
  selectUIs
  
})

output$form_attribute_scatter_plot <- renderPlotly({
  
  req(rea_ata$ata_list$output$lp_obj$output$form)
  
  lp_obj <- rea_ata$ata_list$output$lp_obj
  
  items <- as.data.frame(lp_obj$items)
  fields = unique(intersect(c(input$form_attribute_A_select, input$form_attribute_B_select), names(items)))
  p <- gen_scatter_plot (lp_obj, fields) 
  
  p
})




output$download_checkbox <- downloadHandler(
  
  filename = function() {    
    paste(values$lpObj$name, getMsTimestamp(), ".csv", sep="")
  },
  content = function(file) {
    update.log (action = "DownloadFor", 
                filename = paste(values$lpObj$name,"-timestamp.csv", sep=""))
    write.csv(values$lpObj$form[order(values$lpObj$form$item.seq),], file = file)
  },
  contentType = "text/csv"
)
