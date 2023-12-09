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
  infla_rate <- sample(seq(0.01, 0.1, by = 0.0001), num_simulations, replace = TRUE)
  
  while (sum(as.numeric(interest_rate == infla_rate)) > 0) {
    interest_rate <- sample(seq(0.01, 0.1, by = 0.0005), num_simulations, replace = TRUE)
    infla_rate <- sample(seq(0.01, 0.1, by = 0.0001), num_simulations, replace = TRUE)
  }
  
  interest_infla_pairs <- cbind(interest_rate, infla_rate)
    
  # interest_infla_pairs <- cbind(
  #   sample(seq(0.01, 0.1, by = 0.005), num_simulations, replace = TRUE),
  #   sample(seq(0.01, 0.1, by = 0.005), num_simulations, replace = TRUE)
  # )
  
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

test_result <- sim_retirement(100000, 10000, 35, 2023, 2050, 10000, 30000, interest_list, infla_list, 10)
prob_table <- test_result %>%
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

prob_success <- prob_table$prob[1]
prob_success

  # mutate(prob = case_when(event == "Success" ~ sum(event == "Success")/nrow(prob_table),
  #                         event == "Failure" ~ sum(event == "Failure")/nrow(prob_table))) %>%
  # mutate(mean_profile = )
  






