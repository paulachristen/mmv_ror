
aggregate_and_save_results <- function(check_df){
  
result_df <- check_df %>%
  group_by(run_id, p, method, year) %>%
  summarise(
    health_systems_costs_by_run = sum(health_systems_costs),
    monetized_dalys_averted_by_run = sum(monetized_dalys_averted),
    cash_flow_by_run = sum(cash_flow),
    investment_by_run = sum(in_2022_prices)
  )


result_df_avg <- result_df %>%
  group_by(p, method) %>%
  summarise(
    hs_costs = mean(health_systems_costs_by_run),
    mon_dalys = mean(monetized_dalys_averted_by_run),
    investment_avg = mean(investment_by_run)
  )

result_df_avg_avg <- result_df_avg %>%
  group_by(p) %>%
  summarise(
    hs_costs = mean(hs_costs),
    mon_dalys = mean(mon_dalys),
    investment_avg = mean(investment_avg)
  )

# Write the estimates and confidence intervals to a CSV file
write.csv(result_df,
          paste0("./irr_estimates/overall_results/",
                 "overall_results_for_manuscript.csv"), row.names = FALSE)

# Write the estimates and confidence intervals to a CSV file
write.csv(result_df_avg,
          paste0("./irr_estimates/overall_results/",
                 "overall_results_for_manuscript_aggregated.csv"), row.names = FALSE)

# Write the estimates and confidence intervals to a CSV file
write.csv(result_df_avg_avg,
          paste0("./irr_estimates/overall_results/",
                 "overall_results_for_manuscript_aggregated_avg.csv"), row.names = FALSE)

}