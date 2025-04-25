## loop simulation with set parameters

simulateSideshowloop <- function(reps = 500, 
                                 police_budget = 0,
                                 road_budget = 0,
                                 n_time_steps = 104,
                                 n_locations = 57,
                                 return_list = TRUE) {
  
  # Initialize vector to store total incidents
  incident_totals <- numeric(reps)
  
  # Monte Carlo loop
  for (i in 1:reps) {
    sim <- simulateSideshow(police_budget = police_budget,
                            road_budget = road_budget,
                            n_time_steps = n_time_steps,
                            n_locations = n_locations)
    incident_totals[i] <- sim$total_incidents
  }
  
  # Basic stats
  incident_mean <- mean(incident_totals)
  incident_sd <- sd(incident_totals)
  incident_range <- range(incident_totals)
  incident_cv <- incident_sd / incident_mean
  
  # 95% Confidence Interval
  error_margin <- qt(0.975, df = reps - 1) * incident_sd / sqrt(reps)
  ci_95 <- c(incident_mean - error_margin, incident_mean + error_margin)
  
  # Print summary
  cat("==== Monte Carlo Summary ====\n")
  cat("Runs:", reps, "\n")
  cat("Mean incidents:", round(incident_mean), "\n")
  cat("Standard deviation:", round(incident_sd, 1), "\n")
  cat("Range:", round(incident_range[1]), "-", round(incident_range[2]), "\n")
  cat("95% Confidence Interval:", paste(round(ci_95[1]), "-", round(ci_95[2])), "\n\n")
  
  # Plot histogram with density
  hist(incident_totals,
       breaks = 30,
       probability = TRUE,
       main = paste0("Incident Distribution (", reps, " runs)\n",
                     "$", police_budget / 1e6, "M Police | $", road_budget / 1e6, "M Road"),
       xlab = "Total Incidents",
       col = "lightgray",
       border = "white")
  
  # Overlay mean and density curve
  abline(v = incident_mean, col = "red", lwd = 2, lty = 2)
  lines(density(incident_totals), col = "blue", lwd = 2)
  legend("topright", legend = c("Mean", "Density Curve"),
         col = c("red", "blue"), lwd = 2, lty = c(2, 1), bty = "n")
  
  # Optionally return a summary list
  if (return_list) {
    return(list(
      mean = round(incident_mean),
      sd = round(incident_sd),
      range = incident_range,
      ci_95 = round(ci_95),
      cv = round(incident_cv, 3),
      values = incident_totals
    ))
  }
}

# test the function
simulateSideshowloop(reps = 50,
                     police_budget = 0,
                     road_budget = 0, 
                     return_list = TRUE)


