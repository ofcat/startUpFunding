

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(formattable)

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

  startUp_csv = read.csv(file = 'data/investments_VC.csv', header = TRUE)

  #startUp_csv$status = as.factor(startUp_csv$status)
  startUp_csv = startUp_csv %>% mutate_at(c('status', 'market',
                                            'country_code', 'state_code', 'region',
                                            'city', 'funding_rounds', 'founded_year',
                                            'founded_quarter'), as.factor)

  # 13387 missing values
  startUp_csv$funding_total_usd = as.numeric(gsub(",", "", startUp_csv$funding_total_usd))
  #sum(is.na(startUp_csv$funding_total_usd))

  #data cleaning
  ## 1. Split categories, they have | as divider, make them factor type
  ## 2. funding_total_usd is of character type -> change to double or int
  ## 3. Make these factor type: categories, market,
  ##    country code, state code, region, city,

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

  # rendering the big dataset with DT
  # output$mainData = renderDT(
  #   datatable(startUp_csv,
  #             filter = 'top',
  #             options = list(scrollX = TRUE)
  #             )
  #
  #   )

  #showing it inside the box
  # output$mainDataBox = renderUI({
  #   box(title = 'Crunchbase StartUp Funding Data', width = 12,
  #       DTOutput('mainData'))
  # })

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
      paste0(result, " %"), "% of operating companies", icon = icon("thumb-up", lib = 'glyphicon'),
      color = "yellow"
    )
  }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
