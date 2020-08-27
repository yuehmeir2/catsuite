
tabItem_ATA_table_output = tabItem(
  tabName = "ATA_output_table_tab",
  
  uiOutput("table_item_filter_UI"),
  uiOutput("table_group_by_UI"),
  
  DT::dataTableOutput("ata_summary_table")
)
