# function to compare police strategies 
compareStrategies <- function(police_strategies = c("reactive", "proactive", "saturation"),
                                      road_budget_values = seq(0, 50000000, by = 10000000),
                                      police_budget_values = seq(0, 50000000, by = 5000000),
                                      reps = 30) {
  all_results <- list()
  
  for (strategy in police_strategies) {
    for (rb in road_budget_values) {
      for (pb in police_budget_values) {
        cat("Strategy:", strategy, "| Road:", rb, "| Police:", pb, "\n")
        
        run_data <- data.frame()
        
        for (i in 1:reps) {
          sim <- simulateSideshow(police_budget = pb,
                                  road_budget = rb,
                                  police_strategy = strategy)
          
          # summarize total incidents by strategy
          sim_data <- data.frame(
            total_incidents = sim$total_incidents,
            strategy = strategy,
            road_budget = rb,
            police_budget = pb,
            rep = i
          )
          
          run_data <- rbind(run_data, sim_data)
        }
        
        all_results[[paste(strategy, rb, pb, sep = "_")]] <- run_data
      }
    }
  }
  
  # Combine and summarize
  combined_df <- do.call(rbind, all_results)
  
  summary_df <- combined_df |>
    dplyr::group_by(strategy, road_budget, police_budget) |>
    dplyr::summarise(
      mean_incidents = mean(total_incidents),
      sd_incidents = sd(total_incidents),
      .groups = "drop"
    )
  
  # Add million-dollar budget labels
  summary_df$police_mil <- summary_df$police_budget / 1e6
  summary_df$road_mil <- summary_df$road_budget / 1e6
  
  return(summary_df)
}
