



map_click_ui <- function(id){

  ns <- NS(id)

  div(
    fluidRow(
      column(7,uiOutput(ns("indicator_info"))),
      column(5,
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
        filter(region != "Aggregates",year == "2019") %>%
        mutate(value = winsorize_x(value,0.05))

      mapdata <- JS("Highcharts.maps['custom/world']")

      highchart(type = "map") %>%
        hc_add_series(
          mapData = mapdata, data = map_dat,
          value = "value",
          joinBy = c("iso-a3", "iso_3"),
          name = input$indicator
        ) %>%
        hc_colorAxis(stops = color_stops(colors = rev(viridis::cividis(10)))) %>%
        hc_plotOptions(series = list(events = list(click = click_js))) %>%
        hc_tooltip(valueDecimals = 2, valueSuffix = ' (% of GDP)')

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

      hchart(click_dat, "line", hcaes(x = year, y = value,group = country)) %>%
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          table = TRUE,
          valueDecimals = 2,
          valueSuffix = ' %'
        ) %>%
        hc_colors(c("#1395ba", "#0d3c55", "#f16c20", "#a2b86c", "#5ca793", "#117899",
                    "#0f5b78", "#c02e1d", "#d94e1f", "#ef8b2c", "#ecaa38", "#ebc844"
        ))


    })

    output$indicator_info <- renderUI({

      dat_data <- data()

      fluidRow(
        div(
          class = "col-sm-offset-1 col-sm-11",
          h1(unique(dat_data$name)),
          p(unique(dat_data$description))
        )
      )

    })
  })
}
