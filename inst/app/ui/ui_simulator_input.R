

tabItem_simulator_input = tabItem(tabName = "simulator_input_tab",
  fluidRow(

    box(title = "Simulation Input", status = "primary", solidHeader = T, collapsible = T, width = 3,

        selectInput("simulation_example_select", "Load Example:",
                    c("pick example", sort(gsub("^(.*).rds$", "\\1", SIMULATOR_EXAMPLES)))
        ),

        actionButton(inputId = "simulation_example_input_btn", label = "Load"),
        tags$hr(style="border-color: orange;"),

        fileInput("yourDataInput", "Upload Your Simulation RDS File", multiple = F, accept = c(".rds")),
        tags$hr(style="border-color: orange;"), br(), br(),

        conditionalPanel(condition = "false",  # hidden for NCME
          awesomeRadio(inputId = "simulation_parallel_radio", label = "Run",
                       choices = c("Serial" = "serial", "Parallel" = "parallel"),
                       selected = "serial", inline = T, status = "success")
        ),

        actionBttn(inputId = "simulation_run_btn", label = "Run", style = "simple", color = "primary",
                   icon = icon("play")),
        actionBttn(inputId = "simulator_reset_btn", label = "Reset", style = "simple", color = "primary",
                   icon = icon("cancel")),
        downloadButton("downloadSimObj", "Download Simulation", class = "btn1"),
        tags$head(tags$style(".btn1{background-color:#add8e6;} .btn1{color: white;}"))
    ),
    box(title = "Simulee", status = "primary", solidHeader = T, collapsible = T, width = 4,

      conditionalPanel(condition = "output.selection_rule != null",

        textInput("simulation_test_name", label = "Test Name", value = "", placeholder = NULL),
        tags$hr(style="border-color: orange;"),
        sliderInput("simulation_simulees_slider", "Simulees", min = 1, max = 200, value = 50, ticks = F),
        sliderInput("simulation_seed_slider", "Seed", min = 1, max = 10000, value = 100, ticks = F),
        radioButtons("simulation_generator", "True Ability from: ",
                     c("Normal Distribution" = "normal", "Values Provided" = "truetheta"), "normal", inline = T),
        conditionalPanel(condition = "input.simulation_generator == 'normal'",
          sliderInput("simulation_mean_slider", "Mean of Normal Distribution", min = THETA_LOWER, max = THETA_UPPER, value = 0, ticks = T, step=.1),
          sliderInput("simulation_stddev_slider", "SD of Normal Distribution", min = 0.1, max = 2.0, value = 1.0, ticks = T, step=.1)
        ),
        conditionalPanel(condition = "input.simulation_generator == 'truetheta'",
          textInput('simulation_true_thetas', '',
                    value = paste(seq(-2.0, 2.0, by = 1.0), collapse=", "))
        ),
        conditionalPanel(condition = "output.selection_rule == 'optimal'",
          tags$hr(style="border-color: orange;"),
          radioButtons("simulation_solver", "Item Selection Solver: ",
                       c("CBC" = "cbc", "GLPK" = "glpk", "LPSolve" = "lpsolve"), "lpsolve", inline = T),
          radioButtons("simulation_solver_external", "External/Internal: ",
                       c("external", "internal"), "internal", inline = T),
          sliderInput("simulation_solver_timeout", "Solver Timeout (sec):", min = 1, max = 30, value = 1, step = 1)
        )
      )
    ),

    box(title = "Test Config", status = "primary", solidHeader = T, collapsible = T, width = 5,
      conditionalPanel(
        condition = "output.selection_rule != null",
        strong("Selection Rule"),
        textOutput("test_selection_rule"),
        tags$hr(style="border-color: orange;"),

        sliderInput("test_start_theta_slider", "Starting Ability", min = THETA_LOWER, max = THETA_UPPER, value = 0, ticks = T, step=.1),
        tags$hr(style="border-color: orange;"),

        sliderInput("test_length_slider", "Test Length", min = 5, max = 40, value = c(10,15), dragRange = T, ticks = T, step = 1),
        radioButtons("test_termination_rule", "Termination Rule", c("Fixed Length" = "asap", "CSEM" = "csem"), "asap", inline = T),
        conditionalPanel(condition = "input.test_termination_rule == 'csem'",
          sliderInput("test_termination_value", "Stop when CSEM < ", min = 0.1, max = 0.5, value = 0.3, ticks = T, step=.01)
        ),
        tags$hr(style="border-color: orange;"),

        conditionalPanel(condition = "output.selection_rule == 'adaptive'",
          radioButtons("test_content_balancing", "Content Balancing", c(maxinf = "maxinf", quota = "quota", wpm = "wpm"), "maxinf", inline = T)
        ),
        conditionalPanel(condition = "output.selection_rule == 'optimal'",
          selectInput("test_optimal_objective", "Objective:", c("Information" = "inf", "Constraint Only" = "none"))
        ),
        conditionalPanel(condition = "(output.selection_rule == 'adaptive' && input.test_content_balancing == 'wpm') || (output.selection_rule == 'optimal')",
          textInput('test_cons_weight', 'Constraint Weight', value = paste(seq(1.0, 0.1, by = -0.1), collapse=", ")),
          textInput('test_inf_weight', 'Information Weight', value = paste(seq(0.1, 1.0, by = 0.1), collapse=", "))
        ),
        conditionalPanel(condition = "output.selection_rule == 'adaptive'",
          sliderInput("test_iec_pars", label = "Number of IEC Candidate Items", min = 1, max = 5, value = 1, ticks = T, step = 1)
        ),
        tags$hr(style="border-color: orange;"),

        radioButtons("test_ability_estimator", "Ability Estimation", c(EAP = "eap", MLE = "mle"), "eap", inline = T),
        sliderInput("test_eap_mean_slider", "EAP Mean", min = THETA_LOWER, max = THETA_UPPER, value = 0, ticks = T, step=.1),
        sliderInput("test_eap_stddev_slider", "EAP Std Dev", min = 0.1, max = 2.0, value = 1.0, ticks = T, step=.1),
        sliderInput("test_eap_nquad_slider", "EAP #Quad", min = 31, max = 101, value = 51, ticks = T, step=10)
      )
    ),

    box(title = "Special Control", status = "primary", solidHeader = T, collapsible = T, width = 12,
        editDT2Input("specialControlEditTable")
    ),

    # box(title = "Test Passage Constraints", status = "primary", solidHeader = T, collapsible = T, width = 12,
    #   conditionalPanel(condition = "rea_simulator$latest != null && rea_simulator$latest$constraints$passage != null",
    #     dataTableOutput("test_passage_constraint_table")
    #   )
    # ),

    box(title = "Test Blueprint", status = "primary", solidHeader = T, collapsible = T, width = 12,
      conditionalPanel(condition = "output.selection_rule != null",
        dataTableOutput("test_blueprint_table")
      )
    ),

    box(title = "Histogram by Constraints", status = "primary", solidHeader = T, collapsible = T, width = 12,
        plotlyOutput("pool_cons_histogram", width = "100%"),
        plotlyOutput("pool_IRT_b_scatter_plot", width = "100%")
    ),

    box(title = "Test Item Pool", status = "primary", solidHeader = T, collapsible = T, width = 12,

      conditionalPanel(condition = "output.selection_rule != null",
        dataTableOutput("test_groups_table"), br(), br(),
        p("PAR_1: item discrimination times scaling constant"),
        p("PAR_2: item difficulty"),
        p("PAR_3: guessing parameter"),
        p("PAR_4 to PAR_9: polytomous threshold parameters"), br(), br(),
        dataTableOutput("test_itempool_table")
      )
    )
  )
)
