
source("ui/ui_tool_pool.R", local = T)


source("ui/ui_simulator_create_input.R", local = T)
source("ui/ui_simulator_input.R", local = T)
source("ui/ui_simulator_output.R", local = T)
source("ui/ui_simulator_content.R", local = T)

source("ui/ui_form_assembly_input.R", local = T)
source("ui/ui_form_assembly_output_plot.R", local = T)
source("ui/ui_form_assembly_output_table.R", local = T)


dashboardBody(


  plotOutput("plot1", height = "1px"),

  bsAlert("catSuiteAlert"),

  tabItems(

    tabItem_simulator_input,

    tabItem_simulator_create_input,

    tabItem_simulator_output,

    tabItem_simulator_content,

    tabItem_form_assembly_input,

    tabItem_ATA_plot_output,

    tabItem_tool_pool

  )
)
