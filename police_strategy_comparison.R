# run the function to compare reactive, proactive, and saturation stategies
# this takes 15-20 minutes to run
police_strategy_risk_summary <- compareStrategies(police_strategies = c("reactive", "proactive", "saturation"),
                                                          road_budget_values = seq(0, 50000000, by = 10000000),
                                                          police_budget_values = seq(0, 50000000, by = 5000000),
                                                          reps = 30)
police_strategy_risk_summary


# visualization the comparison
library(ggplot2)

# visualize total incidents by strategy
ggplot(police_strategy_risk_summary, aes(x = police_mil, y = mean_incidents, color = strategy, fill = strategy)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ road_mil, labeller = label_both) +
  labs(
    title = "Total Mean Incidents by Police Strategy",
    subtitle = "Faceted by Road Budget (in millions)",
    x = "Police Budget ($M)",
    y = "Total Mean Incidents"
  ) +
  theme_minimal()

# filter only reactive strategy
police_strategy_risk_summary_reactive <-police_strategy_risk_summary |>
  dplyr::filter(strategy %in% c("reactive"))

# visualize total incidents by strategy
ggplot(police_strategy_risk_summary_reactive, aes(x = police_mil, y = mean_incidents, color = strategy, fill = strategy)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ road_mil, labeller = label_both) +
  labs(
    title = "Total Mean Incidents by Police Strategy",
    subtitle = "Faceted by Road Budget (in millions)",
    x = "Police Budget ($M)",
    y = "Total Mean Incidents"
  ) +
  theme_minimal()

