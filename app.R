library(tidyverse)
library(shiny)
library(plotly)

retire_plan <- function(D, x, age, current_year, retire_year, s, i, r, p) {
  #' Returns a data frame of result from a single simulation for the profile value of user
  #' @param D, the amount of down payment the user wants to put
  #' @param x, the amount of annual payment the user wants to put
  #' @param age, the age of the user
  #' @param current_year, the year the user wants to start the retirement plan
  #' @param retire_year, the year the user wants to retire
  #' @param s, the amount of social security money the user will get annually after retirement
  #' @param i, the return rate of the retirement plan
  #' @param r, the inflation rate in the following years
  #' @param p, the amount of money the user expected to spend annually after retirement
  #' @return a data frame containing year, age, and profile value for each year
  
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
  #' Returns a data frame of result from all simulations for the profile value of user
  #' @param D, the amount of down payment the user wants to put
  #' @param x, the amount of annual payment the user wants to put
  #' @param age, the age of the user
  #' @param current_year, the year the user wants to start the retirement plan
  #' @param retire_year, the year the user wants to retire
  #' @param s, the amount of social security money the user will get annually after retirement
  #' @param p, the amount of money the user expected to spend annually after retirement
  #' @param interest_list, a list of sampled return rate for the retirement plan
  #' @param infla_list, a list of sampled inflation rate in the following years
  #' @param num_simulations, the number of simulation the user wants to perform
  #' @return a data frame containing year, age, profile value for each year, 
  #' number of simulation, and result for all simulations
  
  #set up an empty data frame
  simulate_result <- data.frame()
  
  #sample the return and inflation rate for the number of simulations and pair the two rates
  interest_rate <- sample(seq(0.01, 0.1, by = 0.0005), 
                          num_simulations, replace = TRUE)
  infla_rate <- sample(seq(0.01, 0.07, by = 0.0001), 
                       num_simulations, replace = TRUE)
  
  while (sum(as.numeric(interest_rate == infla_rate)) > 0) {
    interest_rate <- sample(seq(0.01, 0.1, by = 0.0005), 
                            num_simulations, replace = TRUE)
    infla_rate <- sample(seq(0.01, 0.07, by = 0.0001), 
                         num_simulations, replace = TRUE)
  }
  
  interest_infla_pairs <- cbind(interest_rate, infla_rate)
  
  #for each pair of rates, call the retire plan function to get the data frame and combine all simulations'result together
  for(r in 1:nrow(interest_infla_pairs)){
    interest_rate <- interest_infla_pairs[r,1]
    infla_rate <- interest_infla_pairs[r,2]
    result <- retire_plan(D, x, age, current_year, retire_year, 
                          s, interest_rate, infla_rate, p)
    
    #compile the number of simulation and the result to the data frame
    result <- result %>%
      mutate(interest_rate = rep(interest_rate, nrow(result))) %>%
      mutate(infla_rate = rep(infla_rate, nrow(result))) %>%
      mutate(sim_num=rep(r, nrow(result))) %>%
      mutate(event=case_when(result[nrow(result),3] >= 0 ~ "Success",
                             TRUE ~ "Failure"))
    simulate_result <- rbind(simulate_result, result)
    r = r+1
  }
  return(simulate_result)
}



ui <- fluidPage(
  titlePanel("Retirement Planning Simulator"),
  actionButton("sim_desc", "How Does This Retirement Plan Simulator Work"),
  h3("Tell Me About Your Plan"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_simulations", "Number of Simulations:", value = 1000),
      sliderInput("down_pmt", "Down Payment:", 
                  min = 0, max = 1000000, 
                  value = 500000, 
                  round=TRUE,step=5000),
      sliderInput("annual_pmt", "Annual Payment:", 
                  min = 0, max = 100000, 
                  value = 30000, 
                  round=TRUE,step=5000),
      sliderInput("social_income", "Social Security Income:", 
                  min = 0, max = 100000, 
                  value = 20000, 
                  round=TRUE,step=5000),
      sliderInput("annual_spending", "Annual Spending:", 
                  min = 0, max = 100000, 
                  value = 30000, 
                  round=TRUE,step=5000),
      numericInput("age", "Your Age:", value = 30),
      numericInput("current_yr", "Plan Start Year:", 
                   min = 2023, max = 2123, 
                   value = 2023),
      numericInput("retire_yr", "Expected Retire Year:", 
                   min = 2023, max = 2123, 
                   value = 2050),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput("portfolio_value"), 
                           div(plotlyOutput("Guage_prob", 
                                            width = "400px", 
                                            height = "400px"), 
                               align = "center")),
                  tabPanel("Table", tableOutput("results_table"))
    )
  )
))


server <- function(input, output) {
  
  #Pop-up text for the simulation description
  observeEvent(input$sim_desc, {
    showModal(modalDialog(title = "Retirement Planning Simulation Description", 
                          HTML("In this simulation,")))
  })
  
  #Simulated results preparation - will be reactive when a new simulation is run
  simulated_results <- eventReactive(input$simulate, {
    #named all inputs
    num_simulations <- input$num_simulations
    D <- input$down_pmt 
    x <- input$annual_pmt 
    age <- input$age 
    current_year <- input$current_yr
    retire_year <- input$retire_yr
    s <- input$social_income 
    p <- input$annual_spending
    
    #call the simulation function
    simulate_result <- sim_retirement(D, x, age, current_year, retire_year, 
                                      s, p, interest_list, infla_list, num_simulations)
    
    return(simulate_result)
  })
  
  #Create a plot
  output$portfolio_value <- renderPlotly({
    
    #Get the results from the simulation
    results <- simulated_results()
    retire_year <- input$retire_yr
    
    #Plot the results
    p <- ggplot(results, aes(x = year, y = profile_value, group = sim_num)) +
      geom_line(aes(color = factor(event), 
                    text = paste0("Profile Value:$", 
                                  prettyNum(profile_value,big.mark = ","),
                                  '\n', "Year:", year)),
                alpha = 1, size = 0.1) +
      theme_minimal() +
      labs(x = "Year",
           y = "Portfolio Value",
           color = "Event Status") +
      geom_vline(xintercept = retire_year, 
                 linetype = "dashed", color = "black") +
      annotate("text", x = retire_year, y = 4900000, 
               label = paste("Retire Year:", retire_year), 
               hjust = 1) +
      scale_color_manual(values = c("Success" = "chartreuse3", 
                                    "Failure" = "orangered1")) +
      coord_cartesian(ylim = c(0, 5000000), 
                      xlim = c(min(results$year), retire_year+30))
    
    #convert the plot to animated
    fig <- ggplotly(p, tooltip = "text")
  })
  
  #Create a guage
  output$Guage_prob <- renderPlotly({
    
    #Get the results from the simulation
    results <- simulated_results()
    
    #calculate the probability of success and failure trials, and the average profile value
    prob_table <- results %>%
      filter(year == max(year)) %>%
      group_by(event) %>%
      summarize(
        count = n(),
        prob = count / input$num_simulations,
        mean_profile = mean(profile_value))
    
    #convert the probability to percentage and set the range for different level
    prob_success <- 100*prob_table$prob[prob_table$event == "Success"]
    success = c(80, 100)
    warning = c(50, 79.99999)
    danger = c(0, 49.99999)
    
    #set the color for each range level
    colors = c("red", "orange", "green")
    color_index = findInterval(prob_success, c(danger[1], warning[1], success[1]), rightmost.closed = TRUE)
    probSucColor = colors[color_index]
    
    #set up the gauge plot
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
      number = list(suffix = "%"))
    
    fig <- fig %>% layout(margin = list(l=30, r=30, t=80, b=30))
  })
  
  # Create a table
  results_table <- reactive({
    
    #Get the results from the simulation
    results <- simulated_results()
    
    #convert all negative profile value to 0
    results$profile_value <- pmax(results$profile_value, 0)
    
    #filter all rows which have year in the years list
    years <- c(input$current_yr+5, 
               input$current_yr+10,  
               input$current_yr+15, 
               input$current_yr+20, 
               input$current_yr+25, 
               max(results$year))
    
    results_filter <- results %>%
      filter(year %in% years)
    
    #find the quantile of each year's profile value and combine all results together to get the table
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
    
    #find the average year for our retirement plan running out of money and compile it to our df
    year_go0_df <- results %>% 
      group_by(sim_num) %>%
      filter(profile_value==0) %>%
      slice_min(year)
    
    mean_year_go0 <- round(mean(year_go0_df$year), 0)
    
    quantile_df <- quantile_df %>%
      mutate(mean_year=c("", "", "", "", mean_year_go0))
    
    #modify the column names
    colnames(quantile_df) <- c("Trials", "Year 5", 
                               "Year 10", "Year 15", 
                               "Year 20", "Year 25", 
                               "End Year", "Average Year Money Goes to 0")
    quantile_df
  })
  
  #print the table
  output$results_table <- renderTable({
    results_table()
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
