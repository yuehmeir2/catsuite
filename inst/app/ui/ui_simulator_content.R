

tabItem_simulator_content = tabItem(tabName = "simulator_content_tab",
  fluidRow(

    box(title = "Content Alignment Results", status = "primary", solidHeader = T, collapsible = T, width = 12,
      dataTableOutput("consViolations_table"), br(), br(),
      dataTableOutput("consAssignedItemCountDistr_DT")
    )
  )
)
