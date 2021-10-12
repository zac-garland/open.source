

library(shiny)
library(highcharter)
library(dplyr)
library(shinyWidgets)

# wdi_indicators <- wdi_indicators %>%
#   slice(1)


# img_list <- wdi_indicators %>%
#   filter(region != "Aggregates") %>%
#   distinct(country, iso_2) %>%
#   mutate(iso_2 = str_to_lower(iso_2)) %>%
#   split(.$iso_2) %>%
#   imap(~ {
#     img_src <- glue::glue(
#       "https://raw.githubusercontent.com/zac-garland/iso-country-flags-svg-collection/master/svg/country-4x3/{.y}.svg"
#     )
#
#     tagList(tags$img(
#       src = img_src,
#       width = 20,
#       height = 15
#     ), .x$country)
#   })




map_click_ui <- function(id){

  ns <- NS(id)

div(
  fluidRow(
    column(12,
           pickerInput(
             inputId = ns("indicator"),
             label = "Indicator",
             choices = unique(wdi_indicators$name) %>% str_remove_all(" \\(% of GDP\\)"),
             selected = "Final consumption expenditure"
           )
           )
    ),

  fluidRow(
    column(7,highchartOutput(ns("hcmap"))),
    column(5,highchartOutput(ns("hcline")))


  )


)


}

# SERVER UI
map_click_server <- function(id){

  ns <- NS(id)



  moduleServer(id, function(input, output, session){

    data <- reactive({
      wdi_indicators %>%
        filter(name == paste0(input$indicator," (% of GDP)")) %>%
        unnest(data) %>%
        rename(value = 6,iso_2 = iso2c,iso_3 = iso3c) %>%
        mutate_if(is.factor,as.character)

    })


    click_js <- JS(paste0("function(event) {Shiny.onInputChange('",id,"-hcmapclick',event.point.country);}"))



    output$hcmap <- renderHighchart({
      map_dat <- data() %>%
        filter(region != "Aggregates",year == "2019")

      hcmap(map = "custom/world",
            data =  map_dat,
            value = "value",
            joinBy = c("iso-a3","iso_3"),
            name = input$indicator,
            download_map_data = FALSE) %>%
        hc_colorAxis(stops = color_stops()) %>%
        hc_plotOptions(series = list(events = list(click = click_js)))
    })

    click_data <- reactive({


      validate(
        need(input$hcmapclick, 'Click a country to see historical values')
      )


      data() %>%
        filter(country == input$hcmapclick | country == "World")

    })

    output$hcline <- renderHighchart({
      click_dat <- click_data() %>% arrange(year)

      hchart(click_dat, "line", hcaes(x = year, y = value,group = country))


    })

    output$country <- renderPrint({
      print(input$hcmapclick)
    })
  })
}


ui <- fluidPage(
  tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
  fluidRow(
    map_click_ui(id = "wb_ind_dash")
  )
)



server <- function(input, output, session) {
  map_click_server(id = "wb_ind_dash")
}

shinyApp(ui, server)
