

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(formattable)
library(plotly)

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
                  #plotting here

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
    box(title = 'Mini Dataset', width = 4,
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
    box(title = 'Mini PLot 1', width = 4,
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
    box(title = 'Mini PLot 2', width = 4,
        plotlyOutput('miniplot2'))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
