
perform_irr_analysis <- function(p, 
                                 mmv_calculations_dalys, 
                                 treatment_costs, 
                                 ochalek_dalys_priced, 
                                 countries_regions, 
                                 ochalek_dalys_priced_missing_placeholders, 
                                 ochalek_dalys_priced_missing_placeholders_region, 
                                 procurement_rdt,
                                 who_choice_prices, 
                                 mmv_investments, 
                                 non_spec_check){

# Creates a character vector with the names of the columns where the monetized DALY values are stored.

# Initialize empty vector for storing IRR estimates
irr_estim_vector <- c()
# Creates an empty numeric vector to store the calculated Internal Rate of Return (IRR) estimates.

# Initialize empty dataframe for storing IRR estimates and corresponding treatment codes
irr_estim_df <- data.frame(matrix(data = 0, nrow = 0, ncol = 2))
# Creates an empty dataframe with two columns. This dataframe will be used to store the calculated IRR estimates and
# the corresponding treatment codes.
# Get unique treatment codes from the main dataset
treatment_codes <- unique(mmv_calculations_dalys$treatment)

# Define the DALY columns for analysis
daly_columns <- c("dalys_monetized_1")

# Outer loop: Iterate over different DALY columns
for (daly in 1:length(daly_columns)) {
  
  column_daly <- daly_columns[daly] # Get the current DALY column name
  
  # Inner loop: Perform simulations for each DALY column
  for (n in 1:N) {
    
    # Initialize dataframe for intermediate results
    df_meta <- data.frame()
    
    # Innermost loop: Iterate over treatment codes
    for (t in treatment_codes) {
      
      # Read in the preprocessed data for the treatment
      dataframe <- read.csv(paste0(paths[p],
                                   "/by_treatment_dataset/",
                                   t,
                                   ".csv"))
      
      # Extract relevant columns for DALY calculations
      df_empty <- dataframe[, c("country", "year", "doses_country", column_daly)]
      names(df_empty)[4] <- "monetized_dalys_averted"
      
      # Extract WHO Choice delivery costs data
      who_choice_psa <- dataframe[, c("country", "year", "average_low_95_unc_interval",
                                      "average_model_prediction", "average_high_95_unc_interval")]
      
      # Extract treatment costs data
      t_psa <- dataframe[, c("country", "year", "mini_t_price_country_imputed",
                             "mode_country_t_imputed", "maxi_t_price_country_imputed")]
      t_psa[is.na(t_psa)] <- 0       # Replace NAs in treatment costs with 0
      t_psa <- t_psa[complete.cases(t_psa), ]  # Remove rows with missing values
      
      # Extract RDT costs data
      rdt_psa <- dataframe[, c("country", "year", "mini_rdt_price_country_imputed",
                               "mode_country_rdt_imputed", "maxi_rdt_price_country_imputed")]
      rdt_psa[is.na(rdt_psa)] <- 0      # Replace NAs in RDT costs with 0
      rdt_psa <- rdt_psa[complete.cases(rdt_psa), ] # Remove rows with missing values
      
      # Initialize vectors to store simulated values
      t_psa_simulated_vector <- c()
      rdt_psa_simulated_vector <- c()
      who_choice_psa_simulated_vector <- c()
      
      # Simulate costs using PERT distribution
      for (i in 1:nrow(df_empty)) {
        who_choice_psa_simulated <- round(freedom::rpert(n = 1, x.min = who_choice_psa[i, 3],
                                                         x.mode = who_choice_psa[i, 4], x.max = who_choice_psa[i, 5], lambda = 4), 4)
        t_psa_simulated <- round(freedom::rpert(n = 1, x.min  = t_psa[i, 3],
                                                x.mode = t_psa[i, 4], x.max = t_psa[i, 5], lambda = 4), 4)
        rdt_psa_simulated <- round(freedom::rpert(n = 1, x.min = rdt_psa[i, 3],
                                                  x.mode = rdt_psa[i, 4], x.max = rdt_psa[i, 5], lambda = 4), 4)
        
        # Store the simulated values
        who_choice_psa_simulated_vector <- rbind(who_choice_psa_simulated_vector, who_choice_psa_simulated)
        t_psa_simulated_vector <- rbind(t_psa_simulated_vector, t_psa_simulated)
        rdt_psa_simulated_vector <- rbind(rdt_psa_simulated_vector, rdt_psa_simulated)
      }
      
      # Add simulated values to the dataframe
      df_empty$rdt_psa_simulated <- rdt_psa_simulated_vector
      df_empty$t_psa_simulated <- t_psa_simulated_vector
      df_empty$who_choice_simulated <- who_choice_psa_simulated_vector
      
      # Calculate health systems costs
      df_empty$health_systems <- ((df_empty$rdt_psa_simulated * 1.55) +
                                    (df_empty$t_psa_simulated * 1.8) +
                                    df_empty$who_choice_simulated) * df_empty$doses_country
      
      # Add empty years (2000-2010) with zero values for all columns
      #clean_years <- data.frame(matrix(0, ncol = ncol(df_empty), 11))
      #names(clean_years) <- names(df_empty)
      #clean_years$year <- 2000:2010
      
      #df_empty <- rbind(df_empty, clean_years) # Combine the simulated data with the empty years

      # Create directory if it doesn't exist and save disaggregated results
      save_dir <- file.path("./disaggregated_results/", t)
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
      
      df_empty$run_id <- n
      df_empty$d <- column_daly
      df_empty$treatment <- t
      
      # Select relevant columns for further analysis
      df_add <- df_empty[ , which(names(df_empty) %in% c("year",
                                                         "treatment",
                                                         "run_id",
                                                         "d",
                                                         "health_systems",
                                                         "monetized_dalys_averted"))]
      df_meta <- rbind(df_meta, df_add)
    }
    
    # Aggregate costs and DALYs averted by year
    check <- df_meta %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(health_systems_costs = sum(health_systems),
                       monetized_dalys_averted = sum(monetized_dalys_averted))
    
    # Merge with MMV investments data
    check <- merge(check, mmv_investments[, c("year", "in_2022_prices")],
                   by = "year", all = TRUE)
    
    check[is.na(check)] <- 0
    
    # Calculate cash flow
    check$cash_flow <- check$monetized_dalys_averted - check$health_systems - check$in_2022_prices
    
    check$method <- column_daly
    scenario <- gsub("files for R ", "", tail(strsplit(paths[p], "/")[[1]], 1))
    check$p <- scenario
    check$run_id <- n
    check_df <<- rbind(check_df, check)
    
    non_spec_check <- rbind(non_spec_check, check)
    # Calculate IRR and store results
    irr_estim <- jrvFinance::irr(cf = check$cash_flow)
    irr_estim_df <- rbind(irr_estim_df, c(column_daly, irr_estim))
  }
}

# Assign column names to the IRR estimation dataframe
names(irr_estim_df) <- c("daly_method", "irr_estimate")

# Replace DALY column names with more readable labels
irr_estim_df$daly_method <- ifelse(irr_estim_df$daly_method == "dalys_monetized_1", "DALY 1",
                                   ifelse(irr_estim_df$daly_method == "dalys_monetized_2", "DALY 2",
                                          ifelse(irr_estim_df$daly_method == "dalys_monetized_3", "DALY 3",
                                                 ifelse(irr_estim_df$daly_method == "dalys_monetized_4", "DALY 4", NA))))




# Create a density histogram and density plot of IRR estimates by DALY method
plot <- ggplot(irr_estim_df, aes(x = as.numeric(irr_estimate))) +
  geom_histogram(aes(y = ..density..), bins = 100, alpha = 0.6, position = "identity") +   # Histogram with density on y-axis
  geom_density(alpha = 0.2) +                                                              # Add density plot on top
  labs(title = "Density Histogram of IRR Estimates by method of estimating DALYs",
       x = "IRR Estimate",
       y = "Density",
       fill = "DALY Estimation Method") +
  theme_minimal()                                                                           # Use a minimalist theme

# Save the plot to a file
ggsave(paste0("./irr_estimates/",
              tail(strsplit(paths[p], "/")[[1]], 1),
              "_irr_final.png"),
       width = 10, height = 8)                                                             # Specify plot dimensions

# Convert IRR estimates to numeric format for calculations
irr_estim_df$irr_estimate <- as.numeric(irr_estim_df$irr_estimate)

# Perform t-test on all IRR estimates
overall_t <- t.test(irr_estim_df$irr_estimate)

# Get unique DALY methods for further analysis
methods <- unique(irr_estim_df$daly_method)

# Initialize dataframe to store estimates and confidence intervals
estimates_intervals <- data.frame(matrix(data = 0, nrow = 0, ncol = 4))

# Loop through DALY methods and perform t-tests and store results
for (m in methods) {
  df_temp <- irr_estim_df %>% filter(daly_method == m) # Filter for specific method
  t_test <- t.test(df_temp$irr_estimate)                # Perform t-test
  
  # Store mean estimate and confidence interval
  estimates_intervals <- rbind(estimates_intervals,
                               c(m, mean(df_temp$irr_estimate), t_test$conf.int))
}

# Set column names for the estimates dataframe
names(estimates_intervals) <- c("method", "mean", "lower 95% CI", "upper 95% CI")

# Round values in the estimates dataframe
estimates_intervals$mean <- round(as.numeric(estimates_intervals$mean), 4)
estimates_intervals$`lower 95% CI` <- round(as.numeric(estimates_intervals$`lower 95% CI`), 4)
estimates_intervals$`upper 95% CI` <- round(as.numeric(estimates_intervals$`upper 95% CI`), 4)

# Add overall IRR estimate and CI to the estimates dataframe
estimates_intervals <- rbind(c("overall",
                               round(mean(irr_estim_df$irr_estimate), 4),
                               round(overall_t$conf.int,4)),
                             estimates_intervals)

# Write the estimates and confidence intervals to a CSV file
write.csv(estimates_intervals,
          paste0("./irr_estimates/",
                 tail(strsplit(paths[p], "/")[[1]], 1),
                 "_irr_final.csv"), row.names = FALSE)

# Write the estimates and confidence intervals to a CSV file
write.csv(non_spec_check,
          paste0("./irr_estimates/disaggregated results/",
                 tail(strsplit(paths[p], "/")[[1]], 1),
                 "irr_estimates_over_time.csv"), row.names = FALSE)

}
