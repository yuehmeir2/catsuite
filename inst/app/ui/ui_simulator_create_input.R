

tabItem_simulator_create_input = tabItem(tabName = "simulator_create_input_tab",  

        
  fluidRow(
    
    column(6,
           box(title = "Your pool must include the following columns:", status = "info", 
               solidHeader = T, collapsible = T, width = 12,
                 
           p("'ITEM_ID' for the item identifier"),
           p("'IRT_MODEL' for the IRT model--3PL(only support 3PL through UI)"),
           p("'IRT_CAT' for the number of category"),
           p("'IRT_SC' for the scaling constant"),
           p("'IRT_PAR_A' for the item discrimination parameter"),
           p("'IRT_PAR_B' for the item difficulty parameter"),
           p("'IRT_PAR_C' for the guessing parameter")
           )),
           
           
    column(4,
           box(title = "Source", status = "primary", solidHeader = T, collapsible = T, width = 12,
               
               sliderInput("test_length_create_slider", "Test Length", min = 5, max = 40, value = c(20,20), 
                           dragRange = T, ticks = T, step = 1),
               
               fileInput("create_input_fileInput", "Upload Pool CSV File",
                         multiple = F,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               actionButton(inputId = "create_input_upload_btn", label = "Upload"),
               tags$hr(style="border-color: orange;"),
               
               actionButton(inputId = "create_input_reset_btn", label = "Reset")
              
             ))
           
    # column(2,
    #        box(title = "Setting", status = "primary", solidHeader = T, collapsible = T, width = 12,
    #            # textOutput("test_selection_rule"),
    #            sliderInput("test_length_create_slider", "Test Length", min = 5, max = 40, value = c(10,15), 
    #                        dragRange = T, ticks = T, step = 1)
    #                        
    #        ))
    ),
    
  tags$hr(style="border-color: orange;"),
  
  fluidRow(
    column(6,
           box(title = "Categorical Attributes", status = "primary", solidHeader = T, collapsible = T, width = 12,
               
               uiOutput("cate_attr_sel_UI"),
               uiOutput("cate_attr_UI"),
               uiOutput("add_cate_attr_btn_UI")

               )
    ),
    column(6,
           box(title = "Attribute Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
               plotlyOutput("create_input_cate_plot")
           )
    )),
  
  fluidRow(
    column(6,
           box(title = "Numerical Attributes", status = "primary", solidHeader = T, collapsible = T, width = 12,
               
               uiOutput("num_attr_sel_UI"),
               uiOutput("num_cuts_slider_UI"),
               uiOutput("num_attr_UI"),
               br(),br(),
               uiOutput("add_num_attr_btn_UI")
           )
    ),
    column(6,
           box(title = "Attribute Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
               plotlyOutput("create_input_num_plot")
           )
    )),
  
  fluidRow(
    column(12,
           box(title = "Attributes to Constraints Table", status = "primary", solidHeader = T, collapsible = T, width = 12,
               dataTableOutput("attr_to_cons_tbl"), br(),
               uiOutput("attr_to_cons_tbl_del_btn_UI"), br(),
               uiOutput("populate_to_pool_cons_btn_UI")
               ))
  ),
  fluidRow(
    column(12,
           box(title = "My Blueprint", status = "primary", solidHeader = T, collapsible = T, width = 12,
               
               dataTableOutput("my_blueprint_tbl"),
               uiOutput("from_create_to_simulator_UI"))
    )),
  fluidRow(
    column(12,
           box(title = "My Pool", status = "primary", solidHeader = T, collapsible = T, width = 12,
               
               uiOutput("show_sim_gs_ui"))
    )),
  fluidRow(
    column(12,
           box(title = "Pool Attributes", status = "primary", solidHeader = T, collapsible = T, width = 12,
               
               uiOutput("show_pool_to_cons_gs_ui"))
    ))
  
    # column(8,
    #        box(title = "Attribute Information", status = "primary", width = 12,
    #            dataTableOutput("")
    #            )))
  
)


