
# load libraries, data, & modules ----------------------------------------------

source("misc.R")
source("mod_map_click_line.R")

# css, js, head content ---------------------------------------------------

head_content <- tags$head(
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css?family=Alegreya+Sans+SC"
  ),
  tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
  tags$style(
    HTML("body{font-family:'Alegreya Sans SC'};")
  )
)


# UI ----------------------------------------------------------------------


shinyUI(
  fluidPage(
    head_content,
    tabsetPanel(
      tabPanel(
        "mod-1",
        fluidRow(map_click_ui(id = "wb_ind_dash"))
      ),
      tabPanel(
        "mod-2",
        fluidRow(map_click_ui(id = "wb_ind_dash_2", select_choice = "Final consumption expenditure"))
      )
    )
  )
)
