library(tidyverse)
library(shiny)

retire_plan <- function(D, x, age, current_year, retire_year, s, i, p) {
  # Calculate the number of years between current year and retirement
  n <- retire_year - current_year
  
  # Create a data frame for before retirement
  before_retire <- data.frame(
    year = seq(current_year, retire_year, by = 1),
    age = seq(age, age + n, by = 1),
    profile_value = numeric(n + 1)
  )
  
  # Calculate profile values before retirement
  for (j in 0:n) {
    before_retire$profile_value[j + 1] <- D * (1 + i)^j + x * (((1 + i)^j - 1) / i)
  }
  
  # Create a data frame for after retirement
  end_year <- current_year + (100 - age)
  m <- end_year - retire_year
  after_retire <- data.frame(
    year = seq(retire_year + 1, end_year, by = 1),
    age = seq(age + n + 1, 100, by = 1),
    profile_value = numeric(m)
  )
  
  # Calculate profile values after retirement
  current_value <- before_retire$profile_value[n + 1]
  for (j in 1:m) {
    current_value <- current_value * (1 + i) - p + s
    after_retire$profile_value[j] <- current_value
  }
  
  # Combine the two data frames
  profile_df <- rbind(before_retire, after_retire)
  return(profile_df)
}
#simulation function
sim_retirement <- function(D, x, age, current_year, retire_year, s, p, interest_list,num_simulations){
  simulate_result <- data.frame()
  interest_list <- sample(seq(0.01, 0.1, by=0.0005), num_simulations, replace=T)
  for(l in 1:length(interest_list)){
    result <- retire_plan(D, x, age, current_year, retire_year, s, interest_list[l], p)
    result <- result %>%
      mutate(interest_rate=rep(interest_list[l],nrow(result))) %>%
      mutate(sim_num=rep(l,nrow(result))) %>%
      mutate(event=case_when(result[nrow(result),3] >=0 ~ "Success",
                             TRUE ~ "Failure"))
    simulate_result <- rbind(simulate_result, result)
  }
  return(simulate_result)
}


# Define UI for application 
ui <- fluidPage(
  # Application title
  titlePanel("Retirement Planning Simulation"),
  
  # Adding numeric input field for the number of simulation; Adding radio buttons for selecting the result type.
  sidebarLayout(
    sidebarPanel(
      numericInput("num_simulations", "Number of Simulations:", value = 1000),
      sliderInput("down_pmt", "Down Payment:", min=0, max=1000000, value=100000),
      sliderInput("annual_pmt", "Annual Payment:", min=0, max=50000, value=10000),
      sliderInput("social_income", "Social Security Income:", min=0, max=50000, value=10000),
      sliderInput("annual_spending", "Annual Spending:", min=0, max=100000, value = 50000),
      numericInput("age", "Current Age:", value = 30),
      numericInput("current_yr", "Current Year:", min=2023, max= 2123, value = 2023),
      numericInput("retire_yr", "Retire Year:", min=2023, max= 2123, value = 2030),
      actionButton("simulate", "Simulate")                 # action button to start the simulation
    ),
    
    # Adding a main panel with a title, plot output, and table output.
    mainPanel(
      actionButton("sim_desc", "Tell me about my Retirement Plan"),
      h3("Simulation Results"),
      plotOutput("portfolio_value"),
      tableOutput("results_table")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Pop-up text for the simulation description
  observeEvent(input$sim_desc, {
    showModal(modalDialog(title = "Retirement Planning Simulation Description", 
                          HTML("In this simulation,")))
  })
  
  
  # Simulated results preparation - will be reactive when a new simulation is run
  simulated_results <- eventReactive(input$simulate, {
    num_simulations <- input$num_simulations
    D <- input$down_pmt 
    x <- input$annual_pmt 
    age <- input$age 
    current_year <- input$current_yr
    retire_year <- input$retire_yr
    s <- input$social_income 
    p <- input$annual_spending 
    simulate_result <- sim_retirement(D, x, age, current_year, retire_year, s, p, interest_list,num_simulations)
    
    return(simulate_result)
  })
  
  #create a plot
  output$portfolio_value <- renderPlot({
    # Get the results from the simulation
    results <- simulated_results()
    retire_year <- input$retire_yr
    # Plot the results
    p <- ggplot(results, aes(x = year, y = profile_value, group = sim_num)) +
      geom_line(aes(color = factor(event)), alpha = 1, size = 0.1) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Portfolio Value",
        color = "Event Status"
      ) +
      geom_vline(xintercept = retire_year, linetype = "dashed", color = "black") +
      annotate("text", x = retire_year+10, y = 4900000, label = paste("Retire Year:", retire_year), hjust = 1) +
      scale_color_manual(values = c("Success" = "chartreuse3", "Failure" = "orangered1")) +
      coord_cartesian(ylim = c(0, 5000000), xlim = c(min(results$year), retire_year+30))
    
    p
  })
  
  # Create a table
  results_table <- reactive({
    results <- simulated_results()
    table <- head(results)
    table
  })
  output$results_table <- renderTable({
    results_table()
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
