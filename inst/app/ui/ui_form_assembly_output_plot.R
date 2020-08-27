

tabItem_ATA_plot_output = tabItem(
  tabName = "ATA_output_plot_tab", # server_ATA_output.R

  # individual IRT plots

  # fluidRow(
  #   column(4,
  #          box(
  #            title = "Information Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
  #            plotlyOutput("form_tif_plot", width = "100%", height = "300px")
  #          )),
  #   column(4,
  #          box(
  #            title = "Expected Score Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
  #            plotlyOutput("form_tcc_plot", width = "100%", height = "300px")
  #          )),
  #   column(4,
  #          box(
  #            title = "CSEM Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
  #            plotlyOutput("form_csem_plot", width = "100%", height = "300px")
  #          ))),

  # a big IRT plots

  fluidRow(
    box(
      title = "IRT Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
      # radioButtons(inputId = "group_by_mst_path_radio", "Plot by: ",
      #              choices = c("MST path" = "mst_path", "Form or Module" = "forms"), inline = T),
      uiOutput("IRT_plot_module_exclude_UI"),
      plotlyOutput("three_in_1_IRT_plot", width = "100%", height = "600px")
      # plotlyOutput("test_dot_histogram", width = "100%")
      )),

  # content plot

  # fluidRow(
  #   box(
  #     title = "Content_Blueprint Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
  #     uiOutput("content_plot_box_UI")
  #   )),

  # the histogram for categorical data

  uiOutput("form_histFieldUI"),
  fluidRow(
    column(6, box(
      title = "Histogram (by Items)", status = "primary", solidHeader = T, collapsible = T, width = 12,
      plotlyOutput("hist_by_item_plot", width = "100%", height = "400px")
    )),
    column(6, box(
      title = "Histogram (by Points)", status = "primary", solidHeader = T, collapsible = T, width = 12,
      plotlyOutput("hist_by_pt_plot", width = "100%", height = "400px")
    ))
  ),

  # the scatterplot for numerical data
  fluidRow(
    box(
      title = "Scatter Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,
      uiOutput("form_scatterFieldUI"),

      plotlyOutput("form_attribute_scatter_plot", width = "100%")
    )
  )#,

)

