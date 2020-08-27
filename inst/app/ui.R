
shinyUI(
  dashboardPage(
    source("ui/ui_header.R", local = TRUE)$value,
    source("ui/ui_sidebar.R", local = TRUE)$value,
    source("ui/ui_body.R", local = TRUE)$value
    )
)
