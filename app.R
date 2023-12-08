#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

# Define UI for application 
ui <- fluidPage(
  # Application title
  titlePanel("Retirement Planning Simulation"),
  
  # Adding numeric input field for the number of simulation; Adding radio buttons for selecting the result type.
  sidebarLayout(
    sidebarPanel(
      sliderInput("down_pmt", "Down Payment:", min=0, max=1000000, value=100000),
      sliderInput("annual_pmt", "Annual Payment:", min=0, max=50000, value=10000),
      sliderInput("social_income", "Social Security Income:", min=0, max=50000, value=10000),
      sliderInput("annual_spending", "Annual Spending:", min=0, max=100000, value = 50000),
      numericInput("age", "Current Age:", value = 30),
      numericInput("retire_yr", "Retire Year:", min=2023, max= 2123, value = 2030),
      actionButton("simulate", "Simulate")                 # action button to start the simulation
    ),
    
    # Adding a main panel with a title, plot output, and table output.
    mainPanel(
      actionButton("sim_desc", "Tell me about my Retirement Plan"),
      h3("Simulation Results"),
      plotOutput("prortfolio_value"),
      htmlOutput("results_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
