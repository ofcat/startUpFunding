

library(shiny)
library(shinydashboard)
library(DT)

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

  #data cleaning
  ## 1. Do we need to split categories? they have | as divider
  ## 2. funding_total_usd is of character type -> change to double or int
  ## 3. ..

  # tasks
  ## as in my other dashboard lets split the big dataset into smaller tables and create reactive plots
  ## we need to come up with ideas for plots
  ## using plotly for interactive plots

  # rendering the big dataset with DT
  output$mainData = renderDT(
    datatable(startUp_csv,
              options = list(scrollX = TRUE)
              )

    )

  #showing it inside the box
  output$mainDataBox = renderUI({
    box(title = 'Crunchbase StartUp Funding Data', width = 12,
        DTOutput('mainData'))
  })

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
