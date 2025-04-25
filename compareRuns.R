compareRuns <- function(single_value, mc_values) {
  # Monte Carlo stats
  mean_val <- mean(mc_values)
  sd_val <- sd(mc_values)
  n <- length(mc_values)
  
  # 95% Confidence Interval
  error_margin <- qt(0.975, df = n - 1) * sd_val / sqrt(n)
  ci_lower <- mean_val - error_margin
  ci_upper <- mean_val + error_margin
  
  # Z-score of single run
  z_score <- (single_value - mean_val) / sd_val
  inside_ci <- single_value >= ci_lower && single_value <= ci_upper
  
  # Output summary
  cat("===== Comparison of Single Simulation Run to Monte Carlo Distribution =====\n")
  cat("Single run total incidents:", single_value, "\n")
  cat("Monte Carlo mean:", round(mean_val), "\n")
  cat("Standard deviation:", round(sd_val, 1), "\n")
  cat("95% Confidence Interval:", round(ci_lower), "-", round(ci_upper), "\n")
  cat("Z-score of single run:", round(z_score, 2), "\n")
  cat("Is the single run within the 95% CI of the mean?", ifelse(inside_ci, "✅ YES", "❌ NO"), "\n\n")
  
  # Plot histogram using frequency (not density)
  hist_data <- hist(mc_values, breaks = 30, col = "lightgray", main = "Monte Carlo Distribution of Total Incidents",
                    xlab = "Total Incidents", ylab = "Number of Simulation Runs")
  
  # Overlay CI band
  usr <- par("usr")
  rect(ci_lower, usr[3], ci_upper, usr[4], col = rgb(0, 0, 1, 0.1), border = NA)
  
  # Add vertical lines for mean and single run
  abline(v = mean_val, col = "red", lwd = 2, lty = 2)
  abline(v = single_value, col = "darkgreen", lwd = 2, lty = 3)
  
  # Add labels
  text(x = mean_val, y = max(hist_data$counts) * 0.9, 
       labels = paste("Mean:", round(mean_val)), col = "red", pos = 4, cex = 0.8)
  text(x = single_value, y = max(hist_data$counts) * 0.8, 
       labels = paste("Single Run:", single_value), col = "darkgreen", pos = 4, cex = 0.8)
  
  # Legend
  legend("topright",
         legend = c("Mean", "95% CI", "Single Run"),
         col = c("red", rgb(0, 0, 1, 0.3), "darkgreen"),
         lty = c(2, NA, 3),
         lwd = c(2, NA, 2),
         fill = c(NA, rgb(0, 0, 1, 0.1), NA),
         border = c(NA, "blue", NA),
         bty = "n")
  
  # Optional return object
  return(invisible(list(
    single = single_value,
    mean = mean_val,
    sd = sd_val,
    ci_95 = c(ci_lower, ci_upper),
    z_score = z_score,
    inside_ci = inside_ci
  )))
}
