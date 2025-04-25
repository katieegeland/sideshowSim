# visualize results for budgets limited to $30M

# filter results dataset
results30_limit30M <- results30 %>%
  filter(total_budget_mil <= 30) %>%
  arrange(mean_incidents)
results30_limit30M

# group by total budget level - $30M or less
ggplot(results30_limit30M, aes(x = reorder(scenario_label, mean_incidents), y = mean_incidents)) +
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

# heatmap - $30M or less
ggplot(results30_limit30M, aes(x = police_budget_mil, y = road_budget_mil, fill = mean_incidents)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_incidents),
                color = mean_incidents > 2100),  # Conditional color
            size = 3) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white"), guide = "none") +
  scale_fill_viridis_c(name = "Mean Incidents", option = "b") +
  scale_x_continuous(
    breaks = seq(0, 30, by = 5),
    labels = function(x) paste0("$", x, "M")
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    labels = function(y) paste0("$", y, "M")
  ) +
  labs(
    title = "Mean Incidents Under $30M Budget Limit",
    x = "Police Budget ($M)",
    y = "Road Budget ($M)"
  ) +
  theme_minimal()

# scatter plot with error bars

ggplot(results30_limit30M, aes(x = police_budget_mil, y = mean_incidents, color = road_budget_mil)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_incidents - sd_incidents,
                    ymax = mean_incidents + sd_incidents), width = 0.5) +
  labs(
    title = "Incident Outcomes by Police and Road Budget Mix",
    subtitle = "Only combinations with total ≤ $30M",
    x = "Police Budget ($M)",
    y = "Mean Total Incidents",
    color = "Road Budget ($M)"
  ) +
  theme_minimal()

# allocations with fewer than 1000 sideshows 

results30_under1000 <- results30 %>%
  filter(ci_upper <= 1000) %>%
  arrange(mean_incidents)
results30_under1000

# heatmap - 1000 incidents or less
ggplot(results30_under1000, aes(x = police_budget_mil, y = road_budget_mil, fill = mean_incidents)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_incidents),
                color = mean_incidents > 890),  # Conditional color
            size = 3) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white"), guide = "none") +
  scale_fill_viridis_c(name = "Mean Incidents", option = "c") +
  scale_x_continuous(
    breaks = seq(0, 50, by = 5),
    labels = function(x) paste0("$", x, "M")
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    labels = function(y) paste0("$", y, "M")
  ) +
  labs(
    title = "Budget Options Resulting in 1,000 or Fewer Incidents",
    x = "Police Budget ($M)",
    y = "Road Budget ($M)"
  ) +
  theme_minimal()


# full heatmap
ggplot(results30, aes(x = police_budget_mil, y = road_budget_mil, fill = mean_incidents)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_incidents),
                color = mean_incidents > 2000),  # Conditional color
            size = 3) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white"), guide = "none") +
  scale_fill_viridis_c(name = "Mean Incidents", option = "B") +
  scale_x_continuous(
    breaks = seq(0, 50, by = 5),
    labels = function(x) paste0("$", x, "M")
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    labels = function(y) paste0("$", y, "M")
  ) +
  labs(
    title = "Mean incidents for all budget combinations",
    x = "Police Budget ($M)",
    y = "Road Budget ($M)"
  ) +
  theme_minimal()

