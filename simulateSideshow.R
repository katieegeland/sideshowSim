# function to simulate sideshow incidents
  
  simulateSideshow <- function(n_locations = 57,
                               n_time_steps = 104,
                               police_strategy = "reactive",
                               police_budget = 7500000,
                               road_budget = 1000000) {
    
    # ==== Load Packages ====
    library(ggplot2)
    library(reshape2)
    
    # ==== Parameter Setup ====
    police_units <- ifelse(police_budget > 0, floor(police_budget / (2500 * n_time_steps)), 0)
    road_interventions <- ifelse(road_budget > 0, floor(road_budget / 50000), 0)
    
    # ==== Initialize Data Structures ====
    baseline_probs <- runif(n_locations, 0.1, 0.7)
    locations <- data.frame(
      location_id = 1:n_locations,
      baseline_probs = baseline_probs,
      reckless_driving_prob = baseline_probs,
      intervention_count = 0,
      police_presence_count = 0,
      total_incidents = 0,
      arrested_count = 0
    )
    
    intervention_steps <- if (road_interventions > 0) {
      if (road_interventions <= n_time_steps) {
        round(seq(1, n_time_steps, length.out = road_interventions))
      } else {
        rep(1:n_time_steps, length.out = road_interventions)
      }
    } else integer(0)
    
    probability_over_time <- matrix(0, nrow = n_time_steps, ncol = n_locations)
    police_presence <- matrix(0, nrow = n_time_steps, ncol = n_locations)
    intervention_presence <- matrix(0, nrow = n_time_steps, ncol = n_locations)
    reckless_driving_incidents <- matrix(0, nrow = n_time_steps, ncol = n_locations)
    unused_police_units <- numeric(n_time_steps)
    
    # ==== Helper Functions ====
    
    add_road_intervention <- function(locations, intervention_presence, current_week, interventions_per_week) {
      top_locs <- order(locations$total_incidents, decreasing = TRUE)[1:interventions_per_week]
      for (loc in top_locs) {
        locations$intervention_count[loc] <- locations$intervention_count[loc] + 1
        intervention_presence[current_week, loc] <- locations$intervention_count[loc]
      }
      list(locations = locations, intervention_presence = intervention_presence)
    }
    
    get_patrol_locs <- function(strategy, locations, n_units, week, incidents_matrix) {
      n <- nrow(locations)
      if (strategy == "proactive") {
        order(locations$baseline_probs, decreasing = TRUE)[1:min(n_units, n)]
      } else if (strategy == "current_prob") {
        order(locations$reckless_driving_prob, decreasing = TRUE)[1:min(n_units, n)]
      } else if (strategy == "random") {
        sample(1:n, size = min(n_units, n), replace = FALSE)
      } else if (strategy == "saturation") {
        high_risk <- which(locations$reckless_driving_prob > 0.4)
        if (length(high_risk) > 0) {
          head(high_risk[order(locations$reckless_driving_prob[high_risk], decreasing = TRUE)], min(n_units, length(high_risk)))
        } else integer(0)
      } else if (strategy == "reactive" && week > 1) {
        order(incidents_matrix[week - 1, ], decreasing = TRUE)[1:min(n_units, n)]
      } else {
        order(locations$reckless_driving_prob, decreasing = TRUE)[1:min(n_units, n)]
      }
    }
    
    assign_patrol_counts <- function(patrol_locs, strategy, n_units, week, incidents_matrix) {
      if (strategy == "reactive" && week > 1) {
        last_week <- incidents_matrix[week - 1, patrol_locs]
        patrol_counts <- if (sum(last_week) > 0) round((last_week / sum(last_week)) * n_units) else rep(0, length(patrol_locs))
        patrol_counts[is.na(patrol_counts)] <- 0
        pmin(patrol_counts, 10)
      } else {
        if (length(patrol_locs) == 0) return(numeric(0))
        base <- floor(n_units / length(patrol_locs))
        remainder <- n_units %% length(patrol_locs)
        patrol_counts <- rep(base, length(patrol_locs))
        if (remainder > 0) patrol_counts[1:remainder] <- patrol_counts[1:remainder] + 1
        patrol_counts
      }
    }
    
    # ==== Simulation Loop ====
    
    for (week in 1:n_time_steps) {
      if (week %in% intervention_steps) {
        n_this_week <- sum(intervention_steps == week)
        result <- add_road_intervention(locations, intervention_presence, week, n_this_week)
        locations <- result$locations
        intervention_presence <- result$intervention_presence
      }
      if (week > 1) intervention_presence[week, ] <- pmax(intervention_presence[week, ], intervention_presence[week - 1, ])
      
      patrol_locs <- get_patrol_locs(police_strategy, locations, police_units, week, reckless_driving_incidents)
      patrol_counts <- assign_patrol_counts(patrol_locs, police_strategy, police_units, week, reckless_driving_incidents)
      
      unassigned <- police_units - sum(patrol_counts)
      if (unassigned > 0 && length(patrol_locs) > 0) {
        extra_locs <- patrol_locs[which(patrol_counts < 10)]
        extra_assignment <- rep(0, length(extra_locs))
        for (i in 1:unassigned) extra_assignment[(i %% length(extra_locs)) + 1] <- extra_assignment[(i %% length(extra_locs)) + 1] + 1
        patrol_counts[match(extra_locs, patrol_locs)] <- patrol_counts[match(extra_locs, patrol_locs)] + extra_assignment
      }
      
      patrol <- numeric(n_locations)
      patrol[patrol_locs] <- patrol_counts
      police_presence[week, ] <- patrol
      locations$police_presence_count <- locations$police_presence_count + patrol
      unused_police_units[week] <- max(0, police_units - sum(patrol_counts))
      
      seasonality <- 0.7 + 0.3 * sin((2 * pi * week) / 52)
      locations$reckless_driving_prob <- sapply(1:n_locations, function(i) {
        prob <- locations$baseline_probs[i] * seasonality
        if (locations$intervention_count[i] > 0) prob <- max(prob * (1 - 0.2 * locations$intervention_count[i]), locations$baseline_probs[i] * 0.05)
        if (patrol[i] > 0) prob <- max(prob * (1 - 0.1 * patrol[i]), locations$baseline_probs[i] * 0.2)
        prob
      })
      
      if (week > 1) {
        arrests <- round(police_presence[week, ] * 0.1 * reckless_driving_incidents[week - 1, ])
        arrests[is.na(arrests)] <- 0
        locations$arrested_count <- locations$arrested_count + arrests
        locations$reckless_driving_prob <- pmin(locations$reckless_driving_prob + (0.05 * (reckless_driving_incidents[week - 1, ] > 0)), 1)
      }
      
      min_presence <- min(police_units, max(1, round(police_units * 0.5)))
      if (sum(police_presence[week, ]) < min_presence) {
        remaining_units <- min_presence - sum(police_presence[week, ])
        top_locs <- order(locations$reckless_driving_prob, decreasing = TRUE)
        police_presence[week, top_locs[1:length(top_locs)]] <- police_presence[week, top_locs[1:length(top_locs)]] + 1
      }
      
      global_factor <- rbinom(1, n_locations, 0.6) / n_locations
      incident_counts <- sapply(1:n_locations, function(i) {
        if (rbinom(1, 1, locations$reckless_driving_prob[i] * global_factor) == 1) rbinom(1, 4, locations$reckless_driving_prob[i]) + 1 else 0
      })
      
      locations$arrested_count <- pmax(locations$arrested_count - round(locations$arrested_count * 0.2), 0)
      reckless_driving_incidents[week, ] <- incident_counts
      locations$total_incidents <- locations$total_incidents + incident_counts
      probability_over_time[week, ] <- locations$reckless_driving_prob
    }
    
    police_savings <- sum(unused_police_units) * 2500
    
    risk_group <- cut(locations$baseline_probs,
                      breaks = c(-Inf, 0.25, 0.55, Inf),
                      labels = c("low", "medium", "high"))
    
    final_interventions <- intervention_presence[n_time_steps, ]
    total_incidents <- colSums(reckless_driving_incidents)
    
    interventions_by_group <- data.frame(
      group = c("low", "medium", "high"),
      interventions = c(0, 0, 0),
      incidents = c(0, 0, 0)
    )
    
    for (grp in c("low", "medium", "high")) {
      locs <- which(risk_group == grp)
      interventions_by_group$interventions[interventions_by_group$group == grp] <- sum(final_interventions[locs])
      interventions_by_group$incidents[interventions_by_group$group == grp] <- sum(total_incidents[locs])
    }
    
    missed_high_risk <- sum(final_interventions[which(risk_group == "high")] == 0)
    
    # Visualizataions
    
    incidents_by_location_chart <- ggplot(locations, aes(x = factor(location_id), y = total_incidents)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Total Reckless Driving Incidents Per Location", x = "Location", y = "Total Incidents") +
      theme_minimal()
    
    weekly_incidents <- rowSums(reckless_driving_incidents)
    time_series_chart <- ggplot(data.frame(Week = 1:n_time_steps, Incidents = weekly_incidents), aes(x = Week, y = Incidents)) +
      geom_line(color = "red", size = 1) +
      labs(title = "Weekly Reckless Driving Incidents", subtitle = paste0("Scenario: $", police_budget / 1e6, "M Police | $", road_budget / 1e6, "M Road"), x = "Week", y = "Number of Incidents") +
      theme_minimal()
    
    incident_totals <- rowSums(reckless_driving_incidents)
    avg_probabilities <- rowMeans(probability_over_time)
    prob_chart <- ggplot(data.frame(Week = 1:n_time_steps, Avg_Probability = avg_probabilities, Total_Incidents = incident_totals), aes(x = Week)) +
      geom_line(aes(y = Avg_Probability, color = "Avg Probability")) +
      geom_line(aes(y = Total_Incidents / max(incident_totals), color = "Total Incidents")) +
      scale_y_continuous(sec.axis = sec_axis(~ . * max(incident_totals), name = "Total Incidents")) +
      labs(title = paste("Probability vs Incidents Over Time\nPolice Budget:", police_budget, "Road Budget:", road_budget, "Total Incidents:", sum(incident_totals)), x = "Week") +
      theme_minimal()
    
    intervened_locs <- which(locations$intervention_count > 0)
    non_intervened_locs <- which(locations$intervention_count == 0)
    incidents_intervened <- rowSums(reckless_driving_incidents[, intervened_locs, drop = FALSE])
    incidents_non_intervened <- rowSums(reckless_driving_incidents[, non_intervened_locs, drop = FALSE])
    
    intervention_chart <- ggplot(data.frame(Week = 1:n_time_steps, Intervened = incidents_intervened, NonIntervened = incidents_non_intervened), aes(x = Week)) +
      geom_line(aes(y = Intervened, color = "Intervened Locations")) +
      geom_line(aes(y = NonIntervened, color = "Non-Intervened Locations")) +
      labs(title = "Impact of Road Interventions Over Time", x = "Week", y = "Total Incidents") +
      theme_minimal()
  
    # Prepare return list
    results <- list(
      locations = locations,
      police_presence = police_presence,
      intervention_presence = intervention_presence,
      probability_over_time = probability_over_time,
      incidents_over_time = reckless_driving_incidents,
      unused_police_units = unused_police_units,
      total_savings = sum(unused_police_units) * 2500,
      total_incidents = sum(reckless_driving_incidents),
      interventions_by_group = interventions_by_group,
      missed_high_risk = missed_high_risk,
      # charts
      incidents_by_location_chart = incidents_by_location_chart,
      time_series_chart = time_series_chart,
      prob_chart = prob_chart,
      intervention_chart = intervention_chart
    )
    
    # Print summary only
    cat("Total incidents:", results$total_incidents, "\n",
        "Road interventions funded:", road_interventions, "\n",
        "Police units funded:", police_units, "\n",
        "Savings from unused police units:", results$total_savings, "\n")
    
    invisible(results)
  }
  
# test the function
simulateSideshow()
