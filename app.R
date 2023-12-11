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
    current_value <- as.integer(current_value * (1 + (i-r)/(1+r)) - p + s)
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
      plotlyOutput("portfolio_value"),
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
  output$portfolio_value <- renderPlotly({
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
      annotate("text", x = retire_year, y = 4900000, label = paste("Retire Year:", retire_year), hjust = 1) +
      scale_color_manual(values = c("Success" = "chartreuse3", "Failure" = "orangered1")) +
      coord_cartesian(ylim = c(0, 5000000), xlim = c(min(results$year), retire_year+30))
    fig <- ggplotly(p)
  })
  # Create a guage
  output$Guage_prob <- renderPlotly({
    results <- simulated_results()
    prob_table <- results %>%
      filter(year == max(year)) %>%
      group_by(event) %>%
      summarize(
        count = n(),
        prob = count / input$num_simulations,
        mean_profile = mean(profile_value))
    
    prob_success <- 100*prob_table$prob[prob_table$event == "Success"]
    success = c(80, 100)
    warning = c(50, 79.99999)
    danger = c(0, 49.99999)
    
    colors = c("red", "orange", "green")
    color_index = findInterval(prob_success, c(danger[1], warning[1], success[1]), rightmost.closed = TRUE)
    probSucColor = colors[color_index]
    
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
          value = 100)),
      number = list(suffix = "%")) # Add this line to include the percentage sign) 
    fig <- fig %>% layout(margin = list(l=30, r=30, t=80, b=30))
  })
  
  
  
  # Create a table
  results_table <- reactive({
    results <- simulated_results()
    
    results$profile_value <- pmax(results$profile_value, 0)
    
    years <- c(input$current_yr+5, 
               input$current_yr+10,  
               input$current_yr+15, 
               input$current_yr+20, 
               input$current_yr+25, 
               max(results$year))
    
    results_filter <- results %>%
      filter(year %in% years)
    
    
    quantile_df <- data.frame(Trials=c("Best", "25th Percentile", "50th Percentile", "75th Percentile", "Worst"))
    
    for (x in years) {
      year_data <- results_filter[results_filter$year==x, "profile_value"]
      year_quantiles <- round(quantile(year_data, probs = c(1, 0.75, 0.5, 0.25, 0)), 0)
      year_quantiles_df <- setNames(as.data.frame(year_quantiles), paste("Year", x))
      quantile_df <- cbind(quantile_df, year_quantiles_df) 
    }
    
    for (z in 2:ncol(quantile_df)) {
      quantile_df[,z] <- prettyNum(quantile_df[,z], big.mark = ",")
      quantile_df[,z] <- paste("$", quantile_df[,z], sep = "")
    }
    
    year_go0_df <- results %>% 
      group_by(sim_num) %>%
      filter(profile_value==0) %>%
      slice_min(year)
    
    mean_year_go0 <- round(mean(year_go0_df$year), 0)
    
    quantile_df <- quantile_df %>%
      mutate(mean_year=c("", "", "", "", mean_year_go0))
    
    colnames(quantile_df) <- c("Trials", "Year 5", 
                               "Year 10", "Year 15", 
                               "Year 20", "Year 25", 
                               "End Year", "Average Year Money Goes to 0")
    quantile_df
  })
  
  output$results_table <- renderTable({
    results_table()
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
