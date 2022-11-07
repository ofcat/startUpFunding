

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

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
                  #output goes here
                 box(title = "Filters", width = 12,
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
                                       unique(startUp_csv$founded_year), selected = NULL, multiple = T)    )

                     ),
                  uiOutput('mainDataBox')
                ),
                fluidRow(
                  #here for second row
                  valueBoxOutput('totalCompaniesBox'),
                  valueBoxOutput('totalCompaniesBox2'),
                  valueBoxOutput('totalCompaniesBox3')
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
    totalNum = nrow(startUp_csv)
    valueBox(
      paste0(totalNum), "Total number of StartUps", icon = icon("list"),
      color = "purple"
      )
    }
  )

  output$totalCompaniesBox2 <- renderValueBox({
    totalNum = nrow(startUp_csv)
    valueBox(
      paste0(totalNum), "Total number of StartUps", icon = icon("list"),
      color = "purple"
    )
  }
  )
  output$totalCompaniesBox3 <- renderValueBox({
    totalNum = nrow(startUp_csv)
    valueBox(
      paste0(totalNum), "Total number of StartUps", icon = icon("list"),
      color = "purple"
    )
  }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
