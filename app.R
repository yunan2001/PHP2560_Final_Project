library(tidyverse)
library(shiny)
library(plotly)

retire_plan <- function(D, x, age, current_year, retire_year, s, i, r, p) {
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
    before_retire$profile_value[j + 1] <- D * (1 + (i-r)/(1+r))^j + x * (((1 + (i-r)/(1+r))^j - 1) / (i-r)/(1+r))
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
    current_value <- current_value * (1 + (i-r)/(1+r)) - p + s
    after_retire$profile_value[j] <- current_value
  }
  
  # Combine the two data frames
  profile_df <- rbind(before_retire, after_retire)
  return(profile_df)
}
#simulation function
sim_retirement <- function(D, x, age, current_year, retire_year, s, p, interest_list, infla_list, num_simulations){
  simulate_result <- data.frame()
  
  interest_rate <- sample(seq(0.01, 0.1, by = 0.0005), num_simulations, replace = TRUE)
  infla_rate <- sample(seq(0.01, 0.07, by = 0.0001), num_simulations, replace = TRUE)
  
  length(interest_rate)
  length(infla_rate)
  while (sum(as.numeric(interest_rate == infla_rate)) > 0) {
    interest_rate <- sample(seq(0.01, 0.1, by = 0.0005), num_simulations, replace = TRUE)
    infla_rate <- sample(seq(0.01, 0.07, by = 0.0001), num_simulations, replace = TRUE)
  }
  
  interest_infla_pairs <- cbind(interest_rate, infla_rate)
  
  for(r in 1:nrow(interest_infla_pairs)){
    interest_rate <- interest_infla_pairs[r,1]
    infla_rate <- interest_infla_pairs[r,2]
    result <- retire_plan(D, x, age, current_year, retire_year, s, interest_rate, infla_rate, p)
    result <- result %>%
      mutate(interest_rate = rep(interest_rate, nrow(result))) %>%
      mutate(infla_rate = rep(infla_rate, nrow(result))) %>%
      mutate(sim_num=rep(r, nrow(result))) %>%
      mutate(event=case_when(result[nrow(result),3] >=0 ~ "Success",
                             TRUE ~ "Failure"))
    simulate_result <- rbind(simulate_result, result)
    r = r+1
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
      numericInput("retire_yr", "Retire Year:", min=2023, max= 2123, value = 2050),
      actionButton("simulate", "Simulate")                 # action button to start the simulation
    ),
    
    # Adding a main panel with a title, plot output, and table output.
    mainPanel(
      actionButton("sim_desc", "Tell me about my Retirement Plan"),
      h3("Simulation Results"),
      plotOutput("portfolio_value"),
      plotlyOutput("Guage_prob"), 
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
    simulate_result <- sim_retirement(D, x, age, current_year, retire_year, s, p, interest_list, infla_list, num_simulations)
    
    return(simulate_result)
  })
  
  # Create a plot
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
  # Create a guage
  output$Guage_prob <- renderPlotly({
    results <- simulated_results()
    num_simulations <- input$num_simulations
    prob_table <- results %>%
      select(year, profile_value, sim_num, event) %>%
      filter(year == max(year)) %>%
      group_by(event) %>%
      summarize(
        year = year,
        sim_num = sim_num, 
        event = event,
        prob = case_when(event == "Success" ~ sum(event == "Success")/num_simulations,
                         event == "Failure" ~ sum(event == "Failure")/num_simulations),
        mean_profile = case_when(event == "Success" ~ mean(profile_value),
                                 event == "Failure" ~ mean(profile_value))) %>%
      filter(event == "Success")
    
    prob_success <- prob_table$prob[1]
    success = c(80, 100)
    warning = c(60, 79.99999)
    danger = c(0, 59.99999)
    ranges <- unique(c(danger, warning, success))
    probSucColor <- c("red", "orange", "green")[findInterval(prob_success, ranges, rightmost.closed = TRUE)]
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = prob_success,
      title = list(text = "Probabilty of Success [%]"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        bar = list(color = probSucColor),
        axis = list(range = list(0, 100)),
        steps = list(
          list(range = danger, color = "lightgray"),
          list(range = warning, color = "gray")),
        threshold = list(
          line = list(color = "green", width = 4),
          thickness = 0.75,
          value = 100))) 
    fig <- fig %>% layout(margin = list(l=30, r=30, t=80, b=30))
  })
  
  
  
  # Create a table
  results_table <- reactive({
    results <- simulated_results()
    prob_table <- results %>%
      select(year, profile_value, sim_num, event) %>%
      filter(year == max(year)) %>%
      group_by(event) %>%
      summarize(
        year = year,
        sim_num = sim_num, 
        event = event,
        prob = case_when(event == "Success" ~ sum(event == "Success")/nrow(prob_table),
                         event == "Failure" ~ sum(event == "Failure")/nrow(prob_table)),
        mean_profile = case_when(event == "Success" ~ mean(profile_value),
                                 event == "Failure" ~ mean(profile_value))) %>%
      filter(event == "Success")
    
    table <- prob_table
    table
  })
  output$results_table <- renderTable({
    results_table()
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
