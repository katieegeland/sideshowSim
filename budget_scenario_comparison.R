# SCENARIO 1: Baseline, no budget
set.seed(1919)
simresult_baseline <- simulateSideshow(police_budget = 0,
                                       road_budget = 0,
                                       n_time_steps = 104) # 2 years


simresult_baseline_total <- simresult_baseline$total_incidents

# run 500 times
mcresult_baseline <- simulateSideshowloop(reps = 500, 
                                          police_budget = 0,
                                          road_budget = 0,
                                          return_list = TRUE)
mcresult_baseline_total <- mcresult_baseline$values

# charts
simresult_baseline$time_series_chart
compareRuns(simresult_baseline_total, mcresult_baseline_total)

# SCENARIO 2: Run with all the budget going to police, moderate funding level
set.seed(2025)
simresult_police_moderate <- simulateSideshow(police_budget = 25000000,
                                          road_budget = 0, 
                                          n_time_steps = 104)
simresult_police_moderate_total <- simresult_police_moderate$total_incidents

# run 500 times
mcresult_police_moderate <- simulateSideshowloop(reps = 500,
                                                 police_budget = 25000000,
                                                 road_budget = 0,
                                                 return_list = TRUE)
mcresult_police_moderate_total <- mcresult_police_moderate$values

#charts
simresult_police_moderate$time_series_chart
compareRuns(simresult_police_moderate_total, mcresult_police_moderate_total)

# t-test
t.test(mcresult_baseline_total, mcresult_police_moderate_total)
# percent reduction
(mcresult_police_moderate$mean - mcresult_baseline$mean) / mcresult_baseline$mean # vs baseline


# SCENARIO 3: Run with all the budget going to police, heavy funding
set.seed(1846)
simresult_police_high <- simulateSideshow(police_budget = 50000000,
                                          road_budget = 0, 
                                          n_time_steps = 104)
simresult_police_high_total <- simresult_police_high$total_incidents
rowSums(simresult_police_high$police_presence)
rowSums(simresult_police_high$incidents_over_time)

# run 500 times
set.seed(1845)
mcresult_police_high <- simulateSideshowloop(reps = 500,
                                             police_budget = 50000000,
                                             road_budget = 0,
                                             return_list = TRUE)
mcresult_police_high_total <- mcresult_police_high$values

# charts
simresult_police_high$time_series_chart
compareRuns(simresult_police_high_total, mcresult_police_high_total)

#t-test
t.test(mcresult_baseline_total, mcresult_police_high_total)
# percent reduction
(mcresult_police_high$mean - mcresult_baseline$mean) / mcresult_baseline$mean # vs baseline
(mcresult_police_high$mean - mcresult_police_moderate$mean) / mcresult_police_moderate$mean # vs police moderate

# SCENARIO 4: All budget going to road, moderate funding
set.seed(95)
simresult_road_moderate <- simulateSideshow(police_budget = 0,
                                               road_budget = 25000000, 
                                               n_time_steps = 104)
simresult_road_moderate_total <- simresult_road_moderate$total_incidents


# run 500 times
set.seed(102)
mcresult_road_moderate <- simulateSideshowloop(reps = 500,
                                             police_budget = 0,
                                             road_budget = 25000000,
                                             return_list = TRUE)
mcresult_road_moderate_total <- mcresult_road_moderate$values

# charts
simresult_road_moderate$time_series_chart
compareRuns(simresult_road_moderate_total, mcresult_road_moderate_total)

#t.test
t.test(mcresult_baseline_total, mcresult_road_moderate_total)
t.test(mcresult_police_moderate_total, mcresult_road_moderate_total)
# percent reduction
(mcresult_road_moderate$mean - mcresult_baseline$mean) / mcresult_baseline$mean # vs baseline
(mcresult_road_moderate$mean - mcresult_police_moderate$mean) / mcresult_police_moderate$mean # vs police moderate


# SCENARIO 5: 50% police 50% road, moderate
set.seed(735)
simresult_balance_moderate <- simulateSideshow(police_budget = 12500000,
                                      road_budget = 12500000, 
                                      n_time_steps = 104)
simresult_balance_moderate_total <- simresult_balance_moderate$total_incidents

# run 500 times
set.seed(435)
mcresult_balance_moderate <- simulateSideshowloop(reps = 500,
                                               police_budget = 12500000,
                                               road_budget = 12500000,
                                               return_list = TRUE)
mcresult_balance_moderate_total <- mcresult_balance_moderate$values

# charts
simresult_balance_moderate$time_series_chart
compareRuns(simresult_balance_moderate_total, mcresult_balance_moderate_total)

# t-test
t.test(mcresult_baseline_total, mcresult_balance_moderate_total) # vs baseline
t.test(mcresult_police_moderate_total, mcresult_balance_moderate_total) # vs police moderate
t.test(mcresult_road_moderate_total, mcresult_balance_moderate_total) # vs road moderate
# percent reduction
(mcresult_balance_moderate$mean - mcresult_baseline$mean) / mcresult_baseline$mean # vs baseline
(mcresult_balance_moderate$mean - mcresult_police_moderate$mean) / mcresult_police_moderate$mean # vs police moderate
(mcresult_balance_moderate$mean - mcresult_road_moderate$mean) / mcresult_road_moderate$mean # vs road moderate

# SCENARIO 6: 100% road, high funding
set.seed(599)
simresult_road_high <- simulateSideshow(police_budget = 0,
                                        road_budget = 50000000,
                                        n_time_steps = 104)
simresult_road_high_total <- simresult_road_high$total_incidents

# run 500 times
set.seed(122)
mcresult_road_high <- simulateSideshowloop(reps = 500,
                                           police_budget = 0,
                                           road_budget = 50000000,
                                           return_list = TRUE)
mcresult_road_high_total <- mcresult_road_high$values

#charts
simresult_road_high$time_series_chart
compareRuns(simresult_road_high_total, mcresult_road_high_total)

# t-test
t.test(mcresult_baseline_total, mcresult_road_high_total) # vs baseline
t.test(mcresult_police_high_total, mcresult_road_high_total) # vs police high
# percent reduction
(mcresult_road_high$mean - mcresult_baseline$mean) / mcresult_baseline$mean # vs baseline
(mcresult_road_high$mean - mcresult_police_high$mean) / mcresult_police_high$mean # vs police high
(mcresult_road_high$mean - mcresult_road_moderate$mean) / mcresult_road_moderate$mean # vs road moderate

# SCENARIO 7: 50% police and 50% road, high funding
set.seed(302)
simresult_balance_high <- simulateSideshow(police_budget = 25000000,
                                      road_budget = 25000000, 
                                      n_time_steps = 104)
simresult_balance_high_total <- simresult_balance_high$total_incidents

# run 500 times
set.seed(592)
mcresult_balance_high <- simulateSideshowloop(reps = 500,
                                                  police_budget = 25000000,
                                                  road_budget = 25000000,
                                                  return_list = TRUE)
mcresult_balance_high_total <- mcresult_balance_high$values

# charts
simresult_balance_high$time_series_chart
compareRuns(simresult_balance_high_total, mcresult_balance_high_total)

# t-test
t.test(mcresult_baseline_total, mcresult_balance_high_total) # vs baseline
t.test(mcresult_police_high_total, mcresult_balance_high_total) # vs police high
t.test(mcresult_road_high_total, mcresult_balance_high_total) # vs road high

# percent reduction
(mcresult_balance_high$mean - mcresult_baseline$mean) / mcresult_baseline$mean # vs baseline
(mcresult_balance_high$mean - mcresult_police_high$mean) / mcresult_police_high$mean # vs police high
(mcresult_balance_high$mean - mcresult_road_high$mean) / mcresult_road_high$mean # vs road high

# COMBINE SCENARIO RESULTS
# get mc results into a table for plotting
mcresult_baseline$mean
mcresult_baseline$ci_95

# generate a plot that combines the mcresults from each scenario

  