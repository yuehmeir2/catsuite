

tabItem_tool_pool = tabItem(tabName = "pool_analysis_tab",

  fluidRow(

    box(title = "Input", status = "primary", solidHeader = T, collapsible = T, width = 12,


        column(6,
               uiOutput("pool_from_UI")

        )
  )),

  fluidRow(
    column(6,
      box(title = "Choose Category Attributes for Single Attribute Plots", status = "primary", solidHeader = T, collapsible = T, width = 12,
          br(),
          checkboxGroupButtons("cateAttrChoicesCB", label = "Plot Variable: ", choices = "to be added", individual = T), br(),
          radioGroupButtons("cateAttrGroupByCB", label = "Group by: ", choices = "to be added", individual = T), br(),
          actionBttn(inputId = "updateCateAttrChoicesToPlot", label = "Update Choices", style = "unite", color = "warning"))),

    column(6,
      box(title = "Choose Numerical Attributes for Single Attribute Plots", status = "primary", solidHeader = T, collapsible = T, width = 12,
          br(),
          checkboxGroupButtons("numeAttrChoicesCB", label = "Plot Variable", choices = "to be added", individual = T), br(),
          radioGroupButtons("numeAttrGroupByCB", label = "Group by: ", choices = "to be added", individual = T), br(),
          actionBttn(inputId = "updateNumeAttrChoicesToPlot", label = "Update Choices", style = "unite", color = "warning")))
  ),

  fluidRow(
    box(title = "Single Attribute Plots", status = "primary", solidHeader = T, collapsible = T, width = 12,
        tabBox(
          title = "Categorical",
          id = "catePlotTabBox", #height = "250px",
          tabPanel("Bar Chart",
                   uiOutput("cateAttrBarChart_UI")),
          tabPanel("Pie Chart",
                   uiOutput("cateAttrPieChart_UI"))
        ),
        tabBox(
          title = "Numerical",
          id = "numePlotTabBox", #height = "250px",
          tabPanel("Scatter Plot",
                   uiOutput("numeAttrScatterPlot_UI")),
          tabPanel("Bar Chart",
                   uiOutput("numeAttrHistogram_UI"))
        ))
  ),

  fluidRow(

    box(title = "Pool Scatter Plot", status = "primary", solidHeader = T, collapsible = T, width = 12,

        uiOutput("pool_choose_UI"),
        plotlyOutput("pool_tool_scatter_plot", width = "100%"),
        column(12,
               IRTPlot_mod_ui("plot1"))
    )

  )
)
