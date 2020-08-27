
shinyServer(function(input, output, session) {

  source('server/reactive_values.R', local = TRUE)
  source('server/plot.R', local=TRUE)
  source('server/common.R', local=TRUE)
  source('server/utility_data.R', local=TRUE)

  source('server/server_tool_pool.R', local=TRUE)

  source('server/server_ATA_input.R', local=TRUE)
  source('server/server_ATA_output.R', local=TRUE)
  source('server/server_ATA_table_output.R', local=TRUE)

  source('server/server_simulator_input.R', local=TRUE)
  source('server/server_simulator_create_input.R', local=TRUE)
  source('server/server_simulator_output.R', local=TRUE)
  source('server/server_simulator_content.R', local=TRUE)



})

