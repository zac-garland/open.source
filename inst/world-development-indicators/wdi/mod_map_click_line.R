
# data --------------------------------------------------------------------

load("wdi_indicators.rda")

# map click UI module -----------------------------------------------------

map_click_ui <- function(id, select_choice = "Households and NPISHs final consumption expenditure") {
  ns <- NS(id)

  div(
    fluidRow(
      column(7, uiOutput(ns("indicator_info"))),
      column(5,
        style = "margin-top:50px;",
        pickerInput(
          inputId = ns("indicator"),
          label = "Indicator",
          choices = unique(wdi_indicators$name) %>% str_remove_all(" \\(% of GDP\\)"),
          selected = select_choice
        )
      )
    ),
    fluidRow(
      column(7, highchartOutput(ns("hcmap"))),
      column(5, highchartOutput(ns("hcline")))
    )
  )
}


# map click server module -------------------------------------------------


map_click_server <- function(id) {
  ns <- NS(id)



  moduleServer(id, function(input, output, session) {
    data <- reactive({
      wdi_indicators %>%
        filter(name == paste0(input$indicator, " (% of GDP)")) %>%
        unnest(data) %>%
        rename(value = 6, iso_2 = iso2c, iso_3 = iso3c) %>%
        mutate_if(is.factor, as.character)
    })


    click_js <- JS(paste0("function(event) {Shiny.onInputChange('", id, "-hcmapclick',event.point.country);}"))



    output$hcmap <- renderHighchart({
      map_dat <- data() %>%
        filter(region != "Aggregates", year == "2019") %>%
        mutate(value = winsorize_x(value, 0.05))

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
        hc_tooltip(valueDecimals = 2, valueSuffix = " (% of GDP)") %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: World Bank Development Indicators",
          href = "https://datatopics.worldbank.org/world-development-indicators/"
        )
    })

    click_data <- reactive({
      validate(
        need(input$hcmapclick, "Click a country to see historical values")
      )


      data() %>%
        filter(country == input$hcmapclick | country == "World")
    })

    output$hcline <- renderHighchart({
      click_dat <- click_data() %>% arrange(year)

      hchart(click_dat, "line", hcaes(x = year, y = value, group = country)) %>%
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          table = TRUE,
          valueDecimals = 2,
          valueSuffix = " %"
        ) %>%
        hc_colors(c("#1395ba", "#0d3c55")) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: World Bank Development Indicators",
          href = "https://datatopics.worldbank.org/world-development-indicators/"
        )
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
