


output$table_item_filter_UI <- renderUI({

  req(rea_ata$ata_list$output$lp_obj$output$form)  # later

  lp_obj = rea_ata$ata_list$output$lp_obj

  choices = c("All items", "All forms", lp_obj$form_inf$form_name)
  names(choices) = choices

  selectInput("table_item_filter", "Item Filter", choices = choices, selected = choices[3])

})

output$table_group_by_UI <- renderUI({

  req(rea_ata$ata_list$output$lp_obj$output$form)  # later

  lp_obj = rea_ata$ata_list$output$lp_obj

  choices = c("none",
              get_cate_field_from_constraint (
                items = lp_obj$items,
                fields = c(lp_obj$constraint$content$ATTRIBUTE, lp_obj$alias$others)))

  names(choices) = choices
  selectInput("table_group_by", label = "Group by", choices = choices)

})


output$ata_summary_table <- DT::renderDataTable({

  # req(rea_ata$ata_list$output$lp_obj$output$form)
  #
  # lp_obj = rea_ata$ata_list$output$lp_obj

  # table_item_filter and table_group_by

  # item_summary(lpObj,
  #            group_by   = input$table_group_by,
  #            # which_stat = c("Mean","Median","SD","Minimum","Maximum","N"),
  #            which_var  = get_numeric_field_from_constraint (
  #              items = lp_obj$items,
  #              fields = c(lp_obj$constraint$content$ATTRIBUTE, lp_obj$alias$others)),
  #            item_filter = table_item_filter)
})

