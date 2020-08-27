



rea_pool_tool = reactiveValues (
  pool = NULL,
  widget = NULL,
  IRTDataConf = NULL,
  categoryCols = NULL #c("TASK_TYPE", "STANDARD")
)



rea_report_tool = reactiveValues(
  report_list = list() # elements of content, title, and description
)

rea_input = reactiveValues (tibbles = NULL)
rea_change_input_source <- reactiveVal(0)
rea_ata = reactiveValues(

  ptr = 0,
  ata_list = list(
    input = list(
      input_data_set = NULL
    ),
    output = list()
  )
)

module_rea_input = reactiveValues (tibbles = NULL)
module_rea_change_input_source <- reactiveVal(0)
module_rea_ata = reactiveValues(

  ptr = 0,
  ata_list = list(
    input = list(
      input_data_set = NULL
    ),
    output = list()
  )

)

rea_simulator = reactiveValues(
  # All three of these values are 'simulation' objects (history is a list of them).
  # They will each have fields like:
  #   latest$control, latest$constraints$content, latest$itempool from the example rdata file
  #   latest$result, latest$timestamp, latest$label if the simulation has been run
  latest = NULL,
  history = list(),
  selected = NULL,
  constraintControlData = NULL
)

rea_self_input = reactiveValues(
  itempool = NULL,
  attr_to_cons_holder = NULL,
  constraints = NULL)

rea_self_MST_input = reactiveValues(
  itempool = NULL,
  attr_to_cons_holder = NULL,
  constraints = NULL,
  all_path = NULL,
  selected_path = NULL)
