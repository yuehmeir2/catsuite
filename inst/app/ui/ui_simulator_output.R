

tabItem_simulator_output = tabItem(tabName = "simulator_output_tab",
  fluidRow(
    box(title = "Simulation History", status = "primary", solidHeader = T, collapsible = T, width = 12,
      column(width = 5, textInput("result_latest_label", "Simulation Label")),
      column(width = 5),
      column(width = 2, downloadBttn("downloadResult2", label = "Download History", style = "unite", color = "warning", size = "sm")),
      dataTableOutput("result_latest_table"),
      actionButton("result_history_add_btn", "Add", icon = icon("add")),
      actionButton("result_history_delete_btn", "Delete", icon = icon("delete")),
      actionButton("result_history_clear_btn", "Clear", icon = icon("delete")),

      tags$hr(style="border-color: orange;"),
      dataTableOutput("result_history_table")
      # checkboxInput("result_history_plot_checkbox", "Plot History"),
      # conditionalPanel(
      #   condition = "input.result_history_plot_checkbox",
      #   plotlyOutput("result_history_plot")
      # )
    ),

    box(title = "True vs. Estimated Theta", status = "primary", solidHeader = T, collapsible = T, width = 6,
        br(),
        plotlyOutput("result_scatter_plot"),
        sliderInput("result_cut_theta", "Cut theta", min = THETA_LOWER, max = THETA_UPPER, value = c(-1,1), dragRange = T, ticks = T, step = 0.1)
    ),
    box(title = "CSEM", status = "primary", solidHeader = T, collapsible = T, width = 6,
        br(),
        plotlyOutput("CSEM_scatter_plot")
    ),

    box(title = "IRT Function", status = "primary", solidHeader = T, collapsible = T, width = 12,
        radioGroupButtons(inputId = "irtPlotRadio", label = "",
          choices = c("Information", "CSEM", "Expected Percent Score", "Expected Raw Score"),
          individual = F,
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o",
                        style = "color: steelblue"))
        ),
        plotlyOutput("result_irt_plot")
    ),

    box(title = "Adaptivity", status = "primary", solidHeader = T, collapsible = T, width = 12,
        plotlyOutput("adaptivity_plot")
    ),

    box(title = "Simulees", status = "primary", solidHeader = T, collapsible = T, width = 12,
      dataTableOutput("result_all_simulees_table")
    ),

    box(title = "Simulee Plots", status = "primary", solidHeader = T, collapsible = T, width = 12,
      plotlyOutput("result_simulee_ability_plot"),
      prettyToggle("result_simulee_theta_offset_toggle", inline = T, value = F,
                   label_off = "Theta Offset: 0", label_on = "Theta Offset: +1"),
      sliderInput("result_simulee_likelihood_slider", "Interim Thetas", min = 1, max = 1, value = 1, ticks = T, step = 1),
      plotlyOutput("result_simulee_likelihood_plot")
    ),

    box(title = "Simulee Items", status = "primary", solidHeader = T, collapsible = T, width = 12,
      dataTableOutput("result_simulee_table")
    )
  )
)
