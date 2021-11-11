
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css?family=Alegreya+Sans+SC"
    ),
    tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
    tags$style(
      HTML("body{font-family:'Alegreya Sans SC'};")
    )
  ),
  fluidRow(
    map_click_ui(id = "wb_ind_dash")
  )
)



server <- function(input, output, session) {
  map_click_server(id = "wb_ind_dash")
}

shinyApp(ui, server)




