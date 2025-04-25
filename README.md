# Sideshow Simulation for City Budget Optimization

This project simulates reckless driving incidents, aka "sideshows", in an urban environment 
to analyze the effects of funding different levels of police patrols and road design interventions. 
The simulation and accompanying analysis were developed for a graduate-level thesis paper 
exploring how budget allocation strategies can influence public safety outcomes.
The research focuses on Oakland, CA and explores emergent dynamics between law enforcement 
and environmental design.

For questions or collaboration inquiries, contact [Katie Egeland](mailto:katie.egeland@gmail.com)

---

## Project Overview

This repository includes:

- A simulation model of sideshow incidents
- Functions to enable Monte Carlo simulations and comparison of policing strategies
- Analysis and visualizations of various budget scenarios, including under different constraints
- Visualizations comparing the outcomes of different policing strategies

---

## Repository Structure

```
R/                  # Core simulation functions
  ├── simulateSideshow.R
  ├── simulateSideshowLoop.R
  ├── compareRuns.R
  └── compareStrategies.R
  

analysis/           # R Markdown or R scripts for scenario analysis
  ├── budget_scenario_comparison.R
  ├── brute_force_optimization.R
  ├── budget_constraints.R
  └── police_strategy_comparison.R

figures/            # Generated plots used in thesis or presentation
TBA

```

## How to Run

### Prerequisites
This project uses R and the following packages:
- `ggplot2`
- `dplyr`
- `reshape2`
- `plotly`

Install packages (if needed):

```r
install.packages(c("ggplot2", "dplyr", "reshape2", "plotly"))
```


### Load the functions
First load the `simulateSideshow()` function from simulateSideshow.R.
This is the main simulation function.

Load the `simulateSideshowloop()` function.
This enables running the simulation multiple times and generates the mean, 
standard deviation, range, and 95% confidence intervals based on the number of reps.

Load the `compareRuns()` function to help look at statistics for the Monte Carlo simulations.

Load the `compareStrategies()` function to run `simulateSideshow()` across both multiple budget scenarios
and police strategies.

### Compare selected budget scenarios
Run the code in budget_scenario_comparison.R to run different budget scenarios to establish how much sideshows incidents
might change with different levels of funding for each intervention. 
Each scenario has code for the single run, including the time series chart showing 
weekly reckless driving incidents.
This analysis also uses the `compareRuns()` function to generate a visualization of the 500 rep
outputs and how a single run with the same budget levels compares.

### Use brute force optimization
Use the analysis code in brute_force_optimization.R to generate a data frame with results from
running `simulateSideshow()` 30 times each across 11 different budget levels for road and police. This
runs only the "reactive" police strategy which was the subject of the primary analysis.

### Add constraints
Use the analysis code in budget_constraints.R to generate datasets and visualizations to explore the following constraints:
- Show only scenarios with $30M or less total budget
- Show only scenarios where the upper CI of sideshows is below 1,000 incidents

### Compare police strategies
Use the analysis code in police_strategy_comparison.R to re-run `simulateSideshow()` with 3 different 
police strategies (reactive, proactive, and saturation). Each strategy runs 30 times across 6 budget
levels for both road and police.
This function takes the longest to run (15-20 minutes) because it's running across multiple strategies.