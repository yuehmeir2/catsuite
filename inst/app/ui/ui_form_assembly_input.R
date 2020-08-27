

tabItem_form_assembly_input = tabItem(tabName = "form_assembly_input_tab",

  fluidRow(


           box(title = "Source", status = "primary", solidHeader = T, collapsible = T, width = 4,

               selectInput("example_select", "Load Example:",
                           c("pick one example",
                             grep("^Ex|^Forms", grep("\\.rds$", dir(system.file("rdsdata", package = "myFormAssembler")), value = T), value = T)
                             )),

               actionButton(inputId = "load_example_btn", label = "Load"),

               actionButton(inputId = "form_assembly_reset_btn", label = "Reset")


             ),



           box(title = "Setting", status = "primary", solidHeader = T, collapsible = T, width = 4,

               textInput(inputId = "ata_test_name", label = "Test Name", value = "My Linear Form Test", placeholder = NULL),

               numericInput("ata_n_form", "Num of Forms:", 6, min = 1, max = 20, step = 1),

               textInput('ata_n_item', 'Num of Items of Forms', placeholder = "e.g. 12,12,12,12,12,12"),
               textInput('ata_n_pt', 'Num of Points of Forms', placeholder = "e.g. 22,22,22,22,22,22")

           ),


           box(title = "Solver", status = "primary", solidHeader = T, collapsible = T, width = 4,
               checkboxGroupInput("objective_checkbox", "To Apply:",
                                  choices = c("Information (tif)" = "tif",
                                              "Expected Score (tcc)" = "tcc"
                                              # "Extra Constraints" = "slice"
                                              )),

               tags$hr(),
               radioButtons("ata_solver_type", label = "Solver:", choices = c("CBC" = "cbc", "LPSolve" = "lpsolve"),
                            selected = "lpsolve", inline = T),
               sliderInput("solver_timeout", "Timeout (sec):", min = 30, max = 300, value = 30, step = 5),
               actionButton(inputId = "go_solver_btn", label = "Solve"),

               tags$hr(style="border-color: orange;"),

               selectInput("send_form_select", "Form:", choices = c("1"), selected = "1"),

               actionButton(inputId = "send_to_simulator_btn", label = "Send to Simulator")
           )),
fluidRow(
  box(id = "form_box", title = "Parallel Forms", status = "primary", solidHeader = T, collapsible = T, width = 12,
      height = "auto",

      dataTableOutput("parallel_forms_output",  height = "auto")
  )),


fluidRow(
  box(id = "ata_input", title = "Form Assembly Input", status = "primary", solidHeader = T, collapsible = T,
      width = 12, height = "auto",
      uiOutput("show_gs_ui")))
)


