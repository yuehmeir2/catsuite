
output$consViolations_table <- DT::renderDataTable({

  cat("### ", format(Sys.time(), "%X"), " output$consViolations_table\n")

  if (is.null(rea_simulator$selected$result$isr$consViolations)) return ()
  return(show_consViolations_DT(rea_simulator$selected$result$isr))

})

output$consAssignedItemCountDistr_DT <- DT::renderDataTable({


  cat("### ", format(Sys.time(), "%X"), " output$consAssignedItemCountDistr_DT\n")

  if (is.null(rea_simulator$selected$result$isr$consViolations)) return ()
  return(show_consAssignedItemCountDistr_DT(rea_simulator$selected$result$isr))

})
