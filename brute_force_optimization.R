# Budget values in $5M steps from $0 to $50M
budget_values <- seq(0, 50000000, by = 5000000)
results30 <- data.frame()

# Run simulations
for (pb in budget_values) {
  for (rb in budget_values) {
    cat("Running 30 sims for Police Budget:", pb, "Road Budget:", rb, "\n")
    
    incident_totals <- numeric(30)
    
    for (i in 1:30) {
      sim <- simulateSideshow(police_budget = pb, road_budget = rb)
      incident_totals[i] <- sim$total_incidents
    }
    
    results30 <- rbind(results30, data.frame(
      police_budget = pb,
      road_budget = rb,
      mean_incidents = mean(incident_totals),
      sd_incidents = sd(incident_totals)
    ))
  }
}

head(results30) 
which.min(results30$mean_incidents)
# show results sorted by lowest incidents
results30[order(results30$mean_incidents),]
# mean incidents deciles
round(quantile(results30$mean_incidents, probs = seq(0, 1, 0.1)), 0)

# load packages for visualizations
library(ggplot2)
library(plotly)
library(reshape2)

# Convert to millions for better axis labels
results30$police_budget_mil <- results30$police_budget / 1e6
results30$road_budget_mil <- results30$road_budget / 1e6
results30$total_budget <- results30$road_budget + results30$police_budget
results30$total_budget_mil <- results30$total_budget / 1e6

# confidence intervals
# Number of repetitions per budget combo
n_reps <- 30
t_val <- qt(0.975, df = n_reps - 1)  # 95% CI for df = 29 ≈ 2.045

# Add CI columns to results30
results30$se <- results30$sd_incidents / sqrt(n_reps)
results30$ci_lower <- results30$mean_incidents - t_val * results30$se
results30$ci_upper <- results30$mean_incidents + t_val * results30$se

# round columns
results30$mean_incidents <- round(results30$mean_incidents, 0)
results30$sd_incidents <- round(results30$sd_incidents, 0)
results30$ci_lower <- round(results30$ci_lower, 0)
results30$ci_upper <- round(results30$ci_upper, 0)

# facet chart of mean incidents with 95% confidence intervals by road budget
ggplot(results30, aes(x = police_budget_mil, 
                      y = mean_incidents#, 
                      #color = as.factor(road_budget_mil)
                      )) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.4) +
  facet_wrap(~ road_budget_mil, labeller = label_both) +
  labs(
    title = "Mean Total Incidents with 95% Confidence Intervals",
    subtitle = "Faceted by Road Budget (in millions)",
    x = "Police Budget ($M)",
    y = "Mean Total Incidents"
  ) +
  theme_minimal()

# Generate plot for just one road budget level (in millions)
road_budget_filter <- 30000000
plot_data <- results30 %>%
  filter(road_budget == road_budget_filter) %>%
  mutate(
    police_budget_mil = police_budget / 1e6,
    road_budget_mil = road_budget / 1e6
  )

# Plot mean incidents ± SD for this road budget
ggplot(plot_data, aes(x = police_budget_mil, y = mean_incidents)) +
  geom_point(color = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.5, color = "steelblue") +
  labs(
    title = "Average Total Incidents by Police Budget",
    subtitle = paste0("Road Budget: $", road_budget_filter/1e6, "M"),
    x = "Police Budget ($M)",
    y = "Average Total Incidents"
  ) +
  theme_minimal()

# group by total budget level
# Add label column
results30$scenario_label <- paste0("$", results30$police_budget_mil, "M Police | $", results30$road_budget_mil, "M Road")

ggplot(results30, aes(x = reorder(scenario_label, mean_incidents), y = mean_incidents)) +
  geom_point(size = 2, color = "darkred") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.4, color = "darkred") +
  coord_flip() +  # Flip axes for readability
  labs(
    title = "Mean Total Incidents by Budget Scenario",
    subtitle = "With 95% Confidence Intervals from 30 Simulations",
    x = "Budget Scenario",
    y = "Mean Total Incidents"
  ) +
  theme_minimal()


# 3d surface plot 

# Create z-matrix for the surface
z_matrix <- acast(results30, road_budget_mil ~ police_budget_mil, value.var = "mean_incidents")

# Get 10 lowest points
top_10 <- results30 %>% arrange(mean_incidents) %>% head(10)

# Base surface plot
p <- plot_ly(
  x = as.numeric(colnames(z_matrix)),  # police budget
  y = as.numeric(rownames(z_matrix)),  # road budget
  z = z_matrix,
  type = "surface"
)

# Add markers for the top 10 lowest points
p <- p %>%
  add_markers(
    data = top_10,
    x = ~police_budget_mil,
    y = ~road_budget_mil,
    z = ~mean_incidents,
    marker = list(color = 'magenta', size = 4),
    text = ~paste0("Incidents: ", round(mean_incidents), 
                   "<br>Police $", police_budget_mil, "M",
                   "<br>Road $", road_budget_mil, "M"),
    hoverinfo = "text",
    name = "Top 10 Lowest"
  )

# Final layout
p <- p %>% layout(
  title = "3D Surface Plot with Top 10 Lowest Incident Points",
  scene = list(
    xaxis = list(title = "Police Budget ($M)"),
    yaxis = list(title = "Road Budget ($M)"),
    zaxis = list(title = "Avg Total Incidents")
  )
)

# Show it
p
