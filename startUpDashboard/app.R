

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(formattable)
library(plotly)
library(leaflet)
library(ggplot2)
library(htmlwidgets)

startUp_csv = read.csv(file = 'data/investments_VC.csv', header = TRUE)

#startUp_csv$status = as.factor(startUp_csv$status)
startUp_csv = startUp_csv %>% mutate_at(c('status', 'market',
                                          'country_code', 'state_code', 'region',
                                          'city', 'funding_rounds', 'founded_year',
                                          'founded_quarter'), as.factor)

# 13387 missing values
startUp_csv$funding_total_usd = as.numeric(gsub(",", "", startUp_csv$funding_total_usd))



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'black',

    dashboardHeader(title = 'CB Dashboard'),
    dashboardSidebar(

      sidebarMenu(
        menuItem("Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
        menuItem("Page 2", tabName = 'page2', icon = icon('th'))
      )
    ),

    dashboardBody(
      tabItems(

        tabItem(tabName = 'dashboard',
                fluidRow(
                  #here for second row
                  valueBoxOutput('totalCompaniesBox'),
                  valueBoxOutput('totalCompaniesBox2'),
                  valueBoxOutput('totalCompaniesBox3')
                ),
                fluidRow(
                  uiOutput('miniDatasetBox'),
                  uiOutput('miniplot1Box'),
                  uiOutput('miniplot2Box')
                ),
                fluidRow(
                  #output goes here
                 box(title = "Filters", width = 12,
                     ##first row of filters
                    column(width = 4,
                           selectInput(inputId = 'status', label = 'Status',
                                       unique(startUp_csv$status), selected = NULL, multiple = T)
                             )
                    ,

                    column(width = 4,
                           selectInput(inputId = 'market', label = 'Market',
                                       unique(startUp_csv$market), selected = NULL, multiple = T)   )
                    ,

                    column(width = 4,
                           selectInput(inputId = 'founded_year', label = 'Year of foundation',
                                       unique(startUp_csv$founded_year), selected = NULL, multiple = T))
                    ,

                    ## second row of filters

                    column(width = 4,
                           selectInput(inputId = 'country_code', label = 'Country of origin',
                                       unique(startUp_csv$country_code), selected = NULL, multiple = T)),
                    column(width = 4,
                           selectInput(inputId = 'region', label = 'Region',
                                       unique(startUp_csv$region), selected = NULL, multiple = T)),
                    column(width = 4,
                           selectInput(inputId = 'city', label = 'City',
                                       unique(startUp_csv$city), selected = NULL, multiple = T))

                    ## third row of filters

                     ),
                  uiOutput('mainDataBox')
                ),

                fluidRow(
                  #put plots here
                uiOutput('bigmapBox')
                )
                ),
        tabItem(tabName = 'page2',
                fluidRow(
                  #here for page 2

                ))
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {




  # tasks
  ## as in my other dashboard lets split the big dataset into smaller tables and create reactive plots
  ## we need to come up with ideas for plots
  ## using plotly for interactive plots


  startUpReactive = reactive({

    data = startUp_csv

    if(!is.null(input$status)){
      data = data %>% filter(status %in% input$status)
    }
    if(!is.null(input$market)){
      data = data %>% filter(market %in% input$market)
    }
    if(!is.null(input$founded_year)){
      data = data %>% filter(founded_year %in% input$founded_year)
    }

    # if(!is.null(input$state_code)){
    #   data = data %>% filter(state_code %in% input$state_code)
    # }
    if(!is.null(input$country_code)){
      data = data %>% filter(country_code %in% input$country_code)
    }
    if(!is.null(input$region)){
      data = data %>% filter(region %in% input$region)
    }
    if(!is.null(input$city)){
      data = data %>% filter(city %in% input$city)
    }

    #data
    return(data)
  })



  output$table1 = renderDT(
    startUpReactive(),
    filter = 'top',
    options = list(
      scrollX = TRUE
    )
  )

  output$mainDataBox = renderUI({
    box(title = 'Crunchbase StartUp Funding Data', width = 12,
        DTOutput('table1'))
  })


  # some info boxes on this row with general info

  output$totalCompaniesBox <- renderValueBox({
    totalNum = nrow(startUp_csv) %>%
      comma(digits = 0, big.mark = '.')

    valueBox(
      paste0(totalNum), "Total number of StartUps", icon = icon("list"),
      color = "purple"
      )
    }
  )

  output$totalCompaniesBox2 <- renderValueBox({
    totalCapital = sum(startUp_csv$funding_total_usd, na.rm = TRUE) %>%
      currency(digits = 0L, "$ ", big.mark = '.')
    valueBox(
      paste0(totalCapital), "Total capital raised", icon = icon("credit-card")
    )
  }
  )
  output$totalCompaniesBox3 <- renderValueBox({
    countOpenCompanies = nrow(filter(startUp_csv, status == 'operating' |status == 'acquired'))
    countClosed = nrow(startUp_csv)
    result = formattable((countOpenCompanies/countClosed * 100), digits = 2, format = 'f')
    valueBox(
      paste0(result, " %"), "% of operating companies", icon = icon("thumbs-up", lib = 'glyphicon'),
      color = "yellow"
    )
  }
  )

  # output$capitalRaisedPlot <- renderPlotly({
  #   p <- plot_ly()
  #
  #   s <- input$table1_rows_selected
  #   if (length(s)) {
  #     p <- p %>%
  #       add_trace(data = startUp_csv[s, , drop = FALSE],
  #                 x = ~PLZ, y = ~medianPrice, mode = "markers",
  #                 marker = list(opacity = 1, color = "red")) %>%
  #       layout(showlegend = FALSE)
  #   }
  #   p
  # })
  #
  # output$capitalRaisedPlotBox = renderUI({
  #   box(title = 'Some plot', width = 4,
  #       plotlyOutput('capitalRaisedPlot'))
  # })
  miniDT = select(startUp_csv,country_code, city, funding_total_usd, status) %>%
    group_by(country_code) %>%
    summarise(newcol = n(), .groups = 'drop')

  output$miniDataset = renderDataTable(
   miniDT
  )


  output$miniDatasetBox = renderUI({
    box(title = 'StartUps per Country', width = 4,
        DTOutput('miniDataset'))
  })

  miniDTAdvanced = select(startUp_csv,country_code, city, funding_total_usd, status) %>%
    group_by(country_code, status) %>%
    summarise(count = n(), .groups = 'drop')

  miniDT$country_code = as.character(miniDT$country_code)


  countryCode = c()
  output$miniplot1 = renderPlotly({



    p = plot_ly()

    s = input$miniDataset_rows_selected

    #loop through districtPrice_tbl with s as index to get all PLZ that are selected

    collectCodes = function(code) {
      countryCode <<- c(countryCode, code)

     }

    sapply(s, function(x) collectCodes(miniDT[s,]$country_code))


    if (length(s)) {
      #browser(countryCode)
     # p = plot_ly()
      p <- p %>%
        add_bars(data = filter(miniDTAdvanced, country_code %in% countryCode),
                  x = ~status, y = ~count, color = ~country_code, type = 'bar')
    }
    p
  })

  output$miniplot1Box = renderUI({
    box(title = 'Company status per Country', width = 4,
        plotlyOutput('miniplot1'))
  })

  miniDTCapital = select(startUp_csv,country_code, city, funding_total_usd) %>%
    group_by(country_code) %>%
    summarise(total_capital = sum(funding_total_usd, na.rm = TRUE))

  output$miniplot2 = renderPlotly({

    p = plot_ly()

    s = input$miniDataset_rows_selected

    #loop through districtPrice_tbl with s as index to get all PLZ that are selected
    # countryCode = c()
    # collectCodes = function(code) {
    #   countryCode <<- c(countryCode, code)
    #
    # }
    #
    # sapply(s, function(x) collectCodes(miniDT[s,]$country_code))
    #
    #
    if (length(s)) {
      #browser(countryCode)
      #p = plot_ly()
      p <- p %>%
        add_bars(data = filter(miniDTCapital, country_code %in% countryCode),
                 x = ~country_code, y = ~total_capital, type = 'bar')
    }
    p
  })

  output$miniplot2Box = renderUI({
    box(title = 'Raised Capital per Country', width = 4,
        plotlyOutput('miniplot2'))
  })

  map_df  = select(startUp_csv, city, funding_total_usd) %>%
    group_by(city) %>%
    summarise(total_capital = sum(funding_total_usd, na.rm = TRUE)) %>%
    arrange(desc(total_capital)) %>%
    top_n(20)
  map_df$city = as.character(map_df$city)

  # render function depends on the package you use for plotting
  output$bigmap = renderLeaflet({
    #plot here

    m<- leaflet() %>% setView(lng=16.363449,lat=48.210033, zoom = 1)
    m<- addTiles(m)
    m<- addMarkers(m, lng=-73.935242,lat=40.730610,

                   label = "New York #1",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-122.431297,lat=37.773972,

                   label = "San Francisco #2",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=116.383331,lat=39.916668,

                   label = "Beijing #3",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-122.143936,lat=37.468319,

                   label = "Palo Alto #4",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=0.119167,lat=52.205276,

                   label = "Cambridge #5",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-117.161087,lat=32.715736,

                   label = "San Diego #6",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-0.118092,lat=51.509865,

                   label = "London #7",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-122.083855,lat=37.386051,

                   label = "Mountain View #8",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=37.618423,lat=55.751244,

                   label = "Moscow #9",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-121.893028,lat=37.335480,

                   label = "San Jose #10",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-122.036346,lat=37.368832,

                   label = "Sunnyvale #11",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-122.236115,lat=37.487846,

                   label = "Redwood City #12",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-71.057083,lat=42.361145,

                   label = "Boston #13",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-97.733330,lat=30.266666,

                   label = "Austin #14",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-84.386330,lat=33.753746,

                   label = "Atlanta #15",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-121.955238,lat=37.354107,

                   label = "Santa Clara #16",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=121.469170,lat=31.224361,

                   label = "Shanghai #17",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-122.313057,lat=37.554169,

                   label = "San Mateo #18",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))
    m<- addMarkers(m, lng=-87.623177,lat=41.881832,

                   label = "Chicago #19",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))

    m<- addMarkers(m, lng=-73.866669,lat=45.450001,

                   label = "Kirkland #20",
                   labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                 "color" = "blue",
                                                 "font-size" = "17px")))

  })

  output$bigmapBox = renderUI({
    box(title = 'Top 20 cities ranked by capital funding', width = 12,
        leafletOutput('bigmap'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
