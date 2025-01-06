




#function to calculate mode
mode_function <- function(x) unique(x)[which.max(tabulate(match(x, unique(x))))] 

#function to remove outliers
remove_outlier <- function(df, var, n) {
  var <- rlang::parse_expr(quo_name(enquo(var)))
  df %>% filter(abs(!!var - mean(!!var, na.rm = TRUE)) <= n * sd(!!var, na.rm = TRUE))
}


model_per_treatment <- function(t, 
                                mmv_calculations_dalys, 
                                treatment_costs, 
                                ochalek_dalys_priced, 
                                countries_regions, 
                                ochalek_dalys_priced_missing_placeholders, 
                                ochalek_dalys_priced_missing_placeholders_region, 
                                procurement_rdt, 
                                who_choice_prices){
  
  # Print the current treatment name
  print(paste0("Beginning with run:",t))
  
  # Filter regions based on the availability of costing data for different treatments
  
  if (t %in% c("ASAQ", "SPAQ", "RAS")) {                  # Check if the treatment 't' is one of ASAQ, SPAQ, or RAS
    m_burden <- malaria_burden %>% filter(cover == "Africa")  # If so, filter malaria burden data for Africa only
  } else { }                                               # Empty 'else' block (no action needed if 't' is not in the list)
  
  if (t %in% c("AS-PYR", "ASMQ")) {                      # Check if the treatment 't' is one of AS-PYR or ASMQ
    m_burden <- malaria_burden %>% filter(cover == "nonAfrica")  # If so, filter malaria burden data for non-Africa only
  } else { }                                               # Empty 'else' block (no action needed if 't' is not in the list)
  
  if (!(t %in% c("ASAQ", "SPAQ", "RAS", "AS-PYR", "ASMQ"))) { # Check if the treatment 't' is NOT in any of the previous lists
    m_burden <- malaria_burden %>% filter(cover == "global_burden")  # If so, filter malaria burden data for global burden
  } else { }                                               # Empty 'else' block (no action needed if 't' is in any of the lists)
  
  # Filter MMV and treatment costing data based on the selected treatment 't'
  mmv <- mmv_calculations_dalys %>% filter(treatment == t)  # Filter MMV data for the selected treatment
  treat_costs <- treatment_costs %>% filter(ref_category == t)  # Filter treatment cost data for the selected treatment
  
  # Merge malaria burden data with MMV data
  df <- merge(m_burden[, c("year", "country", "distribution")], mmv, by = "year", all.x = TRUE)  # Merge based on the 'year' column, keeping all rows from 'm_burden'
  
  # Merge the main dataframe (df) with DALY data by country
  df <- merge(df, ochalek_dalys_priced, by = c("country","year"), all.x = TRUE)
  # This combines data from the `df` dataframe (which contains information related to the treatment and malaria burden)
  # with DALY estimates from the `ochalek_dalys_priced` dataset,
  # matching based on the 'country' column. It keeps all rows from the 'df' dataset.
  
  # Add regional estimated DALY if country-specific data is unavailable
  df_temp <- merge(df, countries_regions, by.x = "country", by.y = "name", all.x = T)
  # This merges the combined dataframe (`df_temp`) with the `countries_regions` dataset
  # to get regional information for each country, using the 'country' column in `df_temp`
  # and the 'name' column in `countries_regions`.  It keeps all rows from `df_temp`.
  
  df_temp <- merge(df_temp, ochalek_dalys_priced_missing_placeholders, by = c("sub_region","year"), all.x = T)
  # This further merges `df_temp` with the `ochalek_dalys_priced_missing_placeholders` dataset,
  # which likely contains pre-calculated average DALY values for sub-regions, using the 'sub_region' column.
  # It keeps all rows from `df_temp`.
  
  # Impute missing DALY values at the sub-region level if country-level data is not available.
  # For each DALY type, if the value is NA (missing), the corresponding sub-region average is used.
  df_temp$daly <- ifelse(is.na(df_temp$daly), df_temp$daly_sub_region, df_temp$daly)
  
  # Merge with regional DALY estimates
  new <- merge(df_temp, ochalek_dalys_priced_missing_placeholders_region, by = c("region","year"), all.x = T)
  # This merges `df_temp` with `ochalek_dalys_priced_missing_placeholders_region` (which contains average DALYs for regions),
  # using the 'region' column. It keeps all rows from `df_temp`.
  
  # Impute missing DALY values at the region level if sub-region level data is not available
  # Similar to before, missing values (NA) are filled with regional average values for each DALY type.
  new$daly_new <- ifelse(is.na(new$daly), new$daly_region, new$daly)
  
  # Create a new dataframe 'df' from selected columns of the 'new' dataframe
  df <- data.frame(
    country = new$country,          # Extract the country column
    year = new$year,                # Extract the year column
    distribution = new$distribution,# Extract the distribution column
    treatment = new$treatment,      # Extract the treatment column
    dalys_averted = new$dalys_averted,  # Extract the dalys_averted column
    doses = new$doses,              # Extract the doses column
    daly = as.numeric(new$daly_new)  # Extract & convert to numeric the daly_1_new column (imputed DALY values)
  )
  
  # Calculate country-specific DALYs averted
  df$dalys_averted_country <- df$dalys_averted * df$distribution
  # This calculates the DALYs averted for each country by multiplying the total DALYs averted
  # (`dalys_averted`) by the distribution proportion for that country (`distribution`).
  
  # Monetize DALYs averted by country using different DALY values (daly_1, daly_2, etc.)
  df$dalys_monetized_1 <- df$dalys_averted_country * df$daly
  
  # These calculations determine the monetary value of the DALYs averted in each country based on
  # different DALY valuations (likely representing different scenarios or assumptions).
  
  # Calculate country-specific doses based on global malaria estimates
  df$doses_country <- df$doses * df$distribution
  # This calculates the number of doses for each country by multiplying the total doses (`doses`)
  # by the distribution proportion for that country (`distribution`).
  
  #-------------------
  # TREATMENT COSTS
  #-------------------
  
  # Remove outliers in treatment cost data (presumably based on the 'unit_cost_usd_2022_price_inflated' column)
  treat_costs <- treat_costs %>%
    remove_outlier(unit_cost_usd_2022_price_inflated, 2) %>%  # Remove outliers beyond 2 standard deviations from the mean
    ungroup()                                             # Remove any previous grouping from the 'treat_costs' data
  
  # Prepare a subset of treatment costs for region-level analysis
  treat_costs_region <- treat_costs[, c("year", "region", "n_t_cost", "unit_cost_usd_2022_price_inflated")]
  # Select relevant columns for region-level analysis: year, region, number of treatment costs (`n_t_cost`), and the cost per unit
  treat_costs_region <- treat_costs_region[complete.cases(treat_costs_region), ]
  # Remove any rows with missing values in the selected columns
  
  # Check if there are countries/regions missing treatment cost data
  
  # If there are no rows in the 'treat_costs_region' dataset...
  if (nrow(treat_costs_region) == 0) {
    cat("Region dataframe is empty. Skipping function.\n")  # Print a message indicating the dataframe is empty and the function is skipped
  } else {                                                           # If the dataframe is not empty...
    # Calculate mode, minimum, and maximum treatment costs per region and year
    treat_costs_region_modes <- treat_costs_region %>%
      dplyr::group_by(year, region) %>%                                  # Group data by year and region
      dplyr::summarise(mode_region_t = mode_function(unit_cost_usd_2022_price_inflated),  # Calculate mode of 'unit_cost_usd_2022_price_inflated'
                       mini_t_price_region = min(unit_cost_usd_2022_price_inflated),     # Calculate minimum of 'unit_cost_usd_2022_price_inflated'
                       maxi_t_price_region = max(unit_cost_usd_2022_price_inflated))     # Calculate maximum of 'unit_cost_usd_2022_price_inflated'
  }
  
  # Calculate min/max/mode treatment cost for sub-region and year
  treat_costs_sub_region <- treat_costs[, c("year", "sub_region", "unit_cost_usd_2022_price_inflated")]
  treat_costs_sub_region <- treat_costs_sub_region[complete.cases(treat_costs_sub_region), ] # Filter out rows with missing values
  
  # If there are no rows in the 'treat_costs_sub_region' dataset...
  if (nrow(treat_costs_sub_region) == 0) {
    cat("Sub_region dataframe is empty. Skipping function.\n")  # Print a message indicating the dataframe is empty and the function is skipped
  } else {                                                           # If the dataframe is not empty...
    # Calculate mode, minimum, and maximum treatment costs per sub-region and year
    treat_costs_sub_region_modes <- treat_costs_sub_region %>%
      dplyr::group_by(year, sub_region) %>%                                # Group data by year and sub-region
      dplyr::summarise(mode_sub_region_t = mode_function(unit_cost_usd_2022_price_inflated),  # Calculate mode of 'unit_cost_usd_2022_price_inflated'
                       mini_t_price_sub_region = min(unit_cost_usd_2022_price_inflated),     # Calculate minimum of 'unit_cost_usd_2022_price_inflated'
                       maxi_t_price_sub_region = max(unit_cost_usd_2022_price_inflated))     # Calculate maximum of 'unit_cost_usd_2022_price_inflated'
  }
  
  # Calculate min/max/mode treatment cost for country and year
  treat_costs_country_teritorry <- treat_costs[, c("year", "country_teritorry", "n_t_cost", "unit_cost_usd_2022_price_inflated")]
  
  # Merge with country/region information
  treat_costs_country_teritorry <- merge(treat_costs_country_teritorry, countries_regions,
                                         by.x = "country_teritorry",
                                         by.y = "name",
                                         all.x = T)
  
  # Set country_territory to NA if it matches region or sub-region (to avoid duplicates)
  treat_costs_country_teritorry$country_teritorry <- ifelse(treat_costs_country_teritorry$country_teritorry == treat_costs_country_teritorry$region |
                                                              treat_costs_country_teritorry$country_teritorry == treat_costs_country_teritorry$sub_region,
                                                            NA, treat_costs_country_teritorry$country_teritorry)
  
  # Filter out rows with missing values
  treat_costs_country_teritorry <- treat_costs_country_teritorry[complete.cases(treat_costs_country_teritorry), ]
  # Check if there are any country-specific treatment costs
  if (nrow(treat_costs_country_teritorry) == 0) {
    # If there are no rows in the 'treat_costs_country_teritorry' dataframe:
    cat("Country dataframe is empty. Skipping function.\n")  # Print a message indicating the dataframe is empty and the function is skipped
    treat_costs_country_teritorry_modes <- data.frame()    # Create an empty dataframe to store the results
  } else {
    # If the dataframe is not empty:
    treat_costs_country_teritorry_modes <- treat_costs_country_teritorry %>%
      dplyr::group_by(year, country_teritorry, n_t_cost) %>%   # Group by year, country, and number of treatment costs
      dplyr::summarise(                                      # Summarize the data within each group
        mode_country_t = mode_function(unit_cost_usd_2022_price_inflated),  # Calculate mode of treatment cost
        mini_t_price_country = min(unit_cost_usd_2022_price_inflated),       # Calculate minimum treatment cost
        maxi_t_price_country = max(unit_cost_usd_2022_price_inflated)        # Calculate maximum treatment cost
      )
    
    # Merge with country/region information
    treat_costs_country_teritorry_modes <- merge(treat_costs_country_teritorry_modes, countries_regions,
                                                 by.x = "country_teritorry",       # Match on 'country_teritorry' in the modes dataframe
                                                 by.y = "name",                    # Match on 'name' in the countries_regions dataframe
                                                 all.x = T)                       # Keep all rows from 'treat_costs_country_teritorry_modes'
  }
  
  # If there are no country-level treatment costs, use sub-region level data instead
  if (nrow(treat_costs_country_teritorry_modes) == 0) {
    df_temp <- treat_costs_sub_region_modes                       # Use sub-region data as a fallback
    
    cr <- countries_regions[, c("region", "sub_region")]          # Extract region and sub-region columns
    cr <- cr %>% dplyr::distinct()                                # Remove duplicate region-sub_region pairs
    
    # Merge the sub-region data with region information
    df_temp <- merge(df_temp, cr,
                     by = "sub_region",                           # Match on 'sub_region'
                     all.x = T)                                  # Keep all rows from 'df_temp'
  } else {
    # If country-level treatment costs are available, merge with sub-region data
    df_temp <- merge(treat_costs_country_teritorry_modes, treat_costs_sub_region_modes,
                     by = c("year", "sub_region"),                # Match on both 'year' and 'sub_region'
                     all.x = T)                                  # Keep all rows from 'treat_costs_country_teritorry_modes'
  }
  # Merge country/region information into the main dataframe (df)
  df <- merge(df, countries_regions,
              by.x = "country",             # Match using the "country" column in `df`
              by.y = "name",               # Match using the "name" column in `countries_regions`
              all.x = T)                  # Keep all rows from `df` even if no match is found
  
  # If the "region" column is not present in df_temp, add it from countries_regions
  if (!"region" %in% names(df_temp)) {
    cr <- countries_regions[,c("region", "sub_region")]     # Extract region and sub-region columns from countries_regions
    cr <- cr %>% dplyr::distinct()                          # Remove duplicate region-sub_region pairs
    df_temp <- merge(df_temp, cr,
                     by = "sub_region",                       # Match on 'sub_region'
                     all.x = T)                              # Keep all rows from 'df_temp'
  }
  
  # Rename the 3rd column in df_temp to "name" if it's currently "country_teritorry"
  if ("country_teritorry" %in% names(df_temp)) {
    names(df_temp)[3] <- "name"
  }
  
  # If "name" column exists in df_temp, merge it with df based on country and year, otherwise merge on sub-region and year
  if ("name" %in% names(df_temp)) {
    names(df_temp)[3] <- "country"                    # Rename "name" to "country" for consistency
    df_temp <- df_temp[ , -which(names(df_temp) %in% c("region", "sub_region"))] # Remove the 'region' column
    df <- merge(df, df_temp, by = c("country", "year"), all.x = TRUE)   # Merge on "country" and "year"
  } else {
    df_temp <- df_temp[ , -which(names(df_temp) %in% c("region"))] # Remove the 'region' column
    df <- merge(df, df_temp,
                by = c("sub_region", "year"),          # Merge on "sub_region" and "year"
                all.x = TRUE)                         # Keep all rows from `df`
  }
  
  # Merge regional treatment cost modes into the main dataframe
  df <- merge(df, treat_costs_region_modes,
              by = c("year", "region"),       # Match on 'year' and 'region'
              all.x = T)                      # Keep all rows from `df`
  
  # If the "mode_sub_region_t" column doesn't exist, add it with NA values
  if (!"mode_sub_region_t" %in% names(df)) {
    df$mode_sub_region_t <- NA
  }
  
  # Check if country-level mode treatment cost data exists
  if (!"mode_country_t" %in% names(df)) {
    # If the "mode_country_t" column is not in the 'df' dataframe:
    df$mode_country_t <- NA      # Add the "mode_country_t" column and fill it with NA values
    df$mini_t_price_country <- NA  # Add the "mini_t_price_country" column and fill it with NA values
    df$maxi_t_price_country <- NA  # Add the "maxi_t_price_country" column and fill it with NA values
  }
  
  # Calculate global mode, min, and max treatment costs per year
  treat_costs_region_modes_global <- treat_costs_country_teritorry %>%
    dplyr::group_by(year) %>%                                       # Group data by year
    dplyr::summarise(                                               # Summarize within each year
      mode_global_t = mode_function(unit_cost_usd_2022_price_inflated), # Calculate the mode of treatment cost
      mini_t_price_global = min(unit_cost_usd_2022_price_inflated),       # Calculate the minimum treatment cost
      maxi_t_price_global = max(unit_cost_usd_2022_price_inflated)        # Calculate the maximum treatment cost
    )
  
  # Merge global treatment cost modes into the main dataframe
  df <- merge(df, treat_costs_region_modes_global, by = "year", all.x = T)
  # Merge the `treat_costs_region_modes_global` dataframe (containing global statistics) into the main `df` dataframe
  # using the "year" column as the key for matching.
  # The `all.x = TRUE` argument ensures that all rows from `df` are kept, even if there's no matching row in the other dataframe.
  
  # Check if the "n_t_cost" column exists in the main dataframe
  if (!"n_t_cost" %in% names(df)) {
    # If the "n_t_cost" column does not exist:
    df$n_t_cost <- NA               # Add the "n_t_cost" column and fill it with NA values
  }
  
  # Impute missing treatment cost MODE values with a hierarchical fallback strategy
  df$mode_country_t <- coalesce(
    df$mode_country_t,                                              # Use the existing 'mode_country_t' if available
    ifelse(is.na(df$mode_country_t) | df$mode_country_t == "NaN",     # If missing or "NaN", use 'mode_sub_region_t'
           df$mode_sub_region_t, NA),
    ifelse(is.na(df$n_t_cost) | df$n_t_cost == "NaN" | df$n_t_cost == 1,  # If 'n_t_cost' is missing, "NaN", or 1, use 'mode_sub_region_t'
           df$mode_sub_region_t, NA),
    ifelse(is.na(df$mode_sub_region_t) | df$mode_sub_region_t == "NaN", # If 'mode_sub_region_t' is missing or "NaN", use 'mode_region_t'
           df$mode_region_t, NA),
    ifelse(is.na(df$mode_region_t) | df$mode_region_t == "NaN",       # If 'mode_region_t' is missing or "NaN", use 'mode_global_t'
           df$mode_global_t, NA),
    df$mode_global_t                                               # Finally, if all else fails, use the global mode
  )
  
  # Impute missing treatment cost MINIMUM values with a hierarchical fallback strategy
  df$mini_t_price_country <- coalesce(
    df$mini_t_price_country,                                        # Use the existing 'mini_t_price_country' if available
    ifelse(is.na(df$mini_t_price_country) | df$mini_t_price_country == "NaN", # If missing or "NaN", use 'mini_t_price_sub_region'
           df$mini_t_price_sub_region, NA),
    ifelse(is.na(df$n_t_cost) | df$n_t_cost == "NaN" | df$n_t_cost == 1,  # If 'n_t_cost' is missing, "NaN", or 1, use 'mini_t_price_sub_region'
           df$mini_t_price_sub_region, NA),
    ifelse(is.na(df$mini_t_price_sub_region) | df$mini_t_price_sub_region == "NaN", # If 'mini_t_price_sub_region' is missing or "NaN", use 'mini_t_price_region'
           df$mini_t_price_region, NA),
    ifelse(is.na(df$mini_t_price_region) | df$mini_t_price_region == "NaN", # If 'mini_t_price_region' is missing or "NaN", use 'mini_t_price_global'
           df$mini_t_price_global, NA),
    df$mini_t_price_global                                        # Finally, if all else fails, use the global minimum
  )
  
  # Impute missing treatment cost MAXIMUM values with a hierarchical fallback strategy
  df$maxi_t_price_country <- coalesce(
    df$maxi_t_price_country,                                        # Use the existing 'maxi_t_price_country' if available
    ifelse(is.na(df$maxi_t_price_country) | df$maxi_t_price_country == "NaN", # If missing or "NaN", use 'maxi_t_price_sub_region'
           df$maxi_t_price_sub_region, NA),
    ifelse(is.na(df$n_t_cost) | df$n_t_cost == "NaN" | df$n_t_cost == 1,  # If 'n_t_cost' is missing, "NaN", or 1, use 'maxi_t_price_sub_region'
           df$maxi_t_price_sub_region, NA),
    ifelse(is.na(df$maxi_t_price_sub_region) | df$maxi_t_price_sub_region == "NaN", # If 'maxi_t_price_sub_region' is missing or "NaN", use 'maxi_t_price_region'
           df$maxi_t_price_region, NA),
    ifelse(is.na(df$maxi_t_price_region) | df$maxi_t_price_region == "NaN", # If 'maxi_t_price_region' is missing or "NaN", use 'maxi_t_price_global'
           df$maxi_t_price_global, NA),
    df$maxi_t_price_global                                        # Finally, if all else fails, use the global maximum
  )
  
  # Section for calculating RDT (Rapid Diagnostic Test) costs
  
  # Remove outliers in RDT procurement data
  procurement_rdt <- procurement_rdt %>%
    remove_outlier(rdt_price_usd_2022_price_inflated, 2) %>%    # Remove outliers beyond 2 standard deviations in the inflated price
    ungroup()                                                  # Remove any previous grouping from the dataframe
  
  # Calculate min/max/mode RDT cost for each region and year
  rdt_costs_region <- procurement_rdt[, c("year", "region", "rdt_price_usd_2022_price_inflated")]
  # Create a subset of the data with only the relevant columns (year, region, and inflated price)
  rdt_costs_region <- rdt_costs_region[complete.cases(rdt_costs_region), ]
  # Remove rows with missing values (NA) in the subset
  
  # Check if there are any rows in the region-level RDT cost data
  if (nrow(rdt_costs_region) == 0) {
    # If there are no rows (empty dataframe):
    cat("Region dataframe is empty. Skipping function.\n")  # Print a message indicating that the function will be skipped
  } else {
    # If the dataframe is not empty:
    rdt_costs_region_modes <- rdt_costs_region %>%
      dplyr::group_by(year, region) %>%                                  # Group data by year and region
      dplyr::summarise(                                              # Summarize within each group
        mode_region_rdt = mode_function(rdt_price_usd_2022_price_inflated), # Calculate the mode of the RDT price
        mini_rdt_price_region = min(rdt_price_usd_2022_price_inflated),     # Calculate the minimum RDT price
        maxi_rdt_price_region = max(rdt_price_usd_2022_price_inflated)      # Calculate the maximum RDT price
      )
  }
  
  # Calculate min/max/mode RDT cost for each sub-region and year
  rdt_costs_sub_region <- procurement_rdt[, c("year", "sub_region", "rdt_price_usd_2022_price_inflated")]
  # Create a subset of the data with relevant columns (year, sub-region, and inflated price)
  rdt_costs_sub_region <- rdt_costs_sub_region[complete.cases(rdt_costs_sub_region), ]
  # Remove rows with missing values (NA) in the subset
  
  # Check if there are any rows in the sub-region level RDT cost data
  if (nrow(rdt_costs_sub_region) == 0) {
    cat("Sub_region dataframe is empty. Skipping function.\n")  # Print a message indicating the dataframe is empty and the function is skipped
  } else {
    # Calculate mode, minimum, and maximum RDT costs per sub-region and year
    rdt_costs_sub_region_modes <- rdt_costs_sub_region %>%
      dplyr::group_by(year, sub_region) %>%                                # Group data by year and sub-region
      dplyr::summarise(                                               # Summarize within each group
        mode_sub_region_rdt = mode_function(rdt_price_usd_2022_price_inflated), # Calculate the mode of RDT price
        mini_rdt_price_sub_region = min(rdt_price_usd_2022_price_inflated),     # Calculate the minimum RDT price
        maxi_rdt_price_sub_region = max(rdt_price_usd_2022_price_inflated)      # Calculate the maximum RDT price
      )
  }
  
  # Calculate min/max/mode RDT cost for each country and year
  rdt_costs_country <- procurement_rdt[, c("year", "country", "n_rdt", "rdt_price_usd_2022_price_inflated")]
  
  # Check if there are any rows in the country level RDT cost data
  if (nrow(rdt_costs_country) == 0) {
    cat("Country dataframe is empty. Skipping function.\n")  # Print a message indicating the dataframe is empty and the function is skipped
    rdt_costs_country_modes <- data.frame()    # Create an empty dataframe to store the results
  } else {
    # Calculate mode, minimum, and maximum RDT costs per country and year
    rdt_costs_country_modes <- rdt_costs_country %>%
      dplyr::group_by(year, country, n_rdt) %>%       # Group data by year, country, and number of RDTs
      dplyr::summarise(                             # Summarize within each group
        mode_country_rdt = mode_function(rdt_price_usd_2022_price_inflated), # Calculate the mode of RDT price
        mini_rdt_price_country = min(rdt_price_usd_2022_price_inflated),       # Calculate the minimum RDT price
        maxi_rdt_price_country = max(rdt_price_usd_2022_price_inflated)        # Calculate the maximum RDT price
      )
    
    # Merge with country/region information
    rdt_costs_country_modes <- merge(rdt_costs_country_modes, countries_regions,
                                     by.x = "country",             # Match on "country" in the modes dataframe
                                     by.y = "name",               # Match on "name" in the countries_regions dataframe
                                     all.x = T)                   # Keep all rows from 'rdt_costs_country_modes'
  }
  
  # If there are no country-level RDT costs, use sub-region level data instead
  if (nrow(rdt_costs_country_modes) == 0) {
    df_temp <- rdt_costs_sub_region_modes                  # Use sub-region mode data as a fallback
    
    cr <- countries_regions[, c("region", "sub_region")]  # Extract region and sub-region columns
    cr <- cr %>% dplyr::distinct()                        # Remove duplicate region-sub_region pairs
    
    # Merge the sub-region data with region information
    df_temp <- merge(df_temp, cr,
                     by = "sub_region",                    # Match on 'sub_region'
                     all.x = T)                            # Keep all rows from 'df_temp'
    
  } else {
    # If country-level RDT costs are available, merge with sub-region data
    df_temp <- merge(rdt_costs_country_modes, rdt_costs_sub_region_modes,
                     by = c("year", "sub_region"),         # Match on both 'year' and 'sub_region'
                     all.x = T)                            # Keep all rows from 'rdt_costs_country_modes'
  }
  
  # Ensure the region information is available in df_temp
  if (!"region" %in% names(df_temp)) {          # Check if the 'region' column exists in df_temp
    cr <- countries_regions[, c("region", "sub_region")]  # Extract region and sub_region columns
    cr <- cr %>% dplyr::distinct()                   # Remove duplicate region-sub_region pairs
    df_temp <- merge(df_temp, cr,
                     by = "sub_region",            # Merge df_temp with region/sub_region data based on "sub_region"
                     all.x = T)                   # Keep all rows from df_temp (left join)
  }
  
  # Rename the column "country" to "name" if it exists in df_temp
  if ("country" %in% names(df_temp)) {             # Check if the 'country' column exists in df_temp
    names(df_temp)[3] <- "name"                   # Rename the 3rd column (assumed to be 'country') to 'name'
  }
  
  # Merge df_temp with df based on either "country" and "year" or "sub_region" and "year"
  if ("name" %in% names(df_temp)) {                # Check if the 'name' column exists in df_temp
    names(df_temp)[3] <- "country"                 # Rename the 3rd column to "country"
    df_temp <- df_temp[ , -which(names(df_temp) %in% c("region", "sub_region"))] # Remove the 'region' column
    df <- merge(df, df_temp, by = c("country", "year"), all.x = TRUE)   # Merge based on "country" and "year"
  } else {                                        # If "name" column doesn't exist
    df_temp <- df_temp[ , -which(names(df_temp) %in% c("region"))] # Remove the 'region' column
    df <- merge(df, df_temp,
                by = c("sub_region", "year"),    # Merge based on "sub_region" and "year"
                all.x = TRUE)                   # Keep all rows from `df` (left join)
  }
  
  # Merge regional RDT cost modes into the main dataframe
  df <- merge(df, rdt_costs_region_modes,
              by = c("year", "region"),       # Match on 'year' and 'region' columns
              all.x = T)                      # Keep all rows from `df` (left join)
  
  # Check if certain RDT-related columns exist, create them with NA if missing
  if (!"mode_sub_region_rdt" %in% names(df)) {  # Check if "mode_sub_region_rdt" column exists
    df$mode_sub_region_t <- NA                 # If not, add it and fill with NA
  }
  
  if (!"mode_country_rdt" %in% names(df)) {   # Check if "mode_country_rdt" column exists
    df$mode_country_t <- NA                   # If not, add it and fill with NA
    df$mini_t_price_country <- NA              # If not, add it and fill with NA
    df$maxi_t_price_country <- NA              # If not, add it and fill with NA
  }
  
  # Calculate global RDT cost modes for each year
  rdt_costs_region_modes_global <- rdt_costs_country %>%
    dplyr::group_by(year) %>%                                       # Group data by year
    dplyr::summarise(                                               # Summarize within each year
      mode_global_rdt = mode_function(rdt_price_usd_2022_price_inflated), # Calculate the mode of RDT price
      mini_rdt_price_global = min(rdt_price_usd_2022_price_inflated),       # Calculate the minimum RDT price
      maxi_rdt_price_global = max(rdt_price_usd_2022_price_inflated)        # Calculate the maximum RDT price
    )
  
  # Merge global RDT cost modes into the main dataframe
  df <- merge(df, rdt_costs_region_modes_global, by = "year", all.x = T)
  # This merges the `rdt_costs_region_modes_global` dataframe (containing aggregated RDT costs per year at the global level)
  # into the main `df` dataframe using the `year` column as the matching key. It keeps all rows from `df` (left join).
  
  # Check and add missing 'n_rdt' column (number of RDTs)
  if (!"n_rdt" %in% names(df)) {
    df$n_rdt <- NA  # If the 'n_rdt' column is missing, add it and fill with NA values
  }
  
  # Impute missing RDT cost MODE values with a hierarchical fallback strategy
  df$mode_country_rdt <- coalesce(
    df$mode_country_rdt,                                                # Use existing country-level mode if available
    ifelse(is.na(df$mode_country_rdt) | df$mode_country_rdt == "NaN",     # If missing or "NaN", use sub-region mode
           df$mode_sub_region_rdt, NA),
    ifelse(is.na(df$n_rdt) | df$n_rdt == "NaN" | df$n_rdt == 1,          # If 'n_rdt' is missing, "NaN", or 1, use sub-region mode
           df$mode_sub_region_rdt, NA),
    ifelse(is.na(df$mode_sub_region_rdt) | df$mode_sub_region_rdt == "NaN", # If sub-region mode is missing or "NaN", use region mode
           df$mode_region_rdt, NA),
    df$mode_global_rdt                                                 # Finally, if all else fails, use the global mode
  )
  
  # Impute missing RDT cost MINIMUM values with a hierarchical fallback strategy
  df$mini_rdt_price_country <- coalesce(
    df$mini_rdt_price_country,                                          # Use existing country-level minimum if available
    ifelse(is.na(df$mini_rdt_price_country) | df$mini_rdt_price_country == "NaN", # If missing or "NaN", use sub-region minimum
           df$mini_rdt_price_sub_region, NA),
    ifelse(is.na(df$n_rdt) | df$n_rdt == "NaN" | df$n_rdt == 1,           # If 'n_rdt' is missing, "NaN", or 1, use sub-region minimum
           df$mini_rdt_price_sub_region, NA),
    ifelse(is.na(df$mini_rdt_price_sub_region) | df$mini_rdt_price_sub_region == "NaN", # If sub-region minimum is missing or "NaN", use region minimum
           df$mini_rdt_price_region, NA),
    df$mini_rdt_price_global                                            # Finally, if all else fails, use the global minimum
  )
  
  # Impute missing RDT cost MAXIMUM values with a hierarchical fallback strategy
  df$maxi_rdt_price_country <- coalesce(
    df$maxi_rdt_price_country,                                          # Use existing country-level maximum if available
    ifelse(is.na(df$maxi_rdt_price_country) | df$maxi_rdt_price_country == "NaN", # If missing or "NaN", use sub-region maximum
           df$maxi_rdt_price_sub_region, NA),
    ifelse(is.na(df$n_rdt) | df$n_rdt == "NaN" | df$n_rdt == 1,           # If 'n_rdt' is missing, "NaN", or 1, use sub-region maximum
           df$maxi_rdt_price_sub_region, NA),
    ifelse(is.na(df$maxi_rdt_price_sub_region) | df$maxi_rdt_price_sub_region == "NaN", # If sub-region maximum is missing or "NaN", use region maximum
           df$maxi_rdt_price_region, NA),
    df$maxi_rdt_price_global                                            # Finally, if all else fails, use the global maximum
  )
  
  # Filter intervention costs based on treatment type
  if ((t == "InjAS")) {
    # If the treatment is InjAS, filter out costs from health centers without beds
    intervention_cost <- who_choice_prices %>% dplyr::filter(delivery_option != "health_centre_no_beds")
  } else {
    # For other treatments, filter for costs from health centers without beds
    intervention_cost <- who_choice_prices %>% dplyr::filter(delivery_option == "health_centre_no_beds")
  }
  
  # Summarize intervention costs by region/country
  intervention_cost_summarized <- intervention_cost %>%
    dplyr::group_by(region_country) %>%                                 # Group data by region/country
    dplyr::summarise(                                               # Summarize within each group
      average_model_prediction = mean(model_prediction, na.rm = T),  # Calculate average model prediction
      average_mean_value_from_sample = mean(mean_value_from_sample, na.rm = T), # Calculate average mean value from sample
      average_high_95_unc_interval = mean(high_95_unc_interval, na.rm = T), # Calculate average upper 95% uncertainty interval
      average_low_95_unc_interval = mean(low_95_unc_interval, na.rm = T), # Calculate average lower 95% uncertainty interval
      average_sd = mean(sd, na.rm = T)                               # Calculate average standard deviation
    )
  
  # Merge summarized intervention costs into the main dataframe
  df <- merge(df, intervention_cost_summarized, by.x = "country", by.y = "region_country", all.x = TRUE)
  # This combines the summarized intervention costs with the main dataframe (`df`), matching based on country names.
  
  # Remove the "country_teritorry" column if it exists (likely to avoid duplicates)
  if ("country_teritorry" %in% names(df)) {
    df <- df[ , -which(names(df) %in% c("country_teritorry"))] # Remove the 'region' column
    
  }
  
  # Impute missing values in RDT cost columns using values from the previous or next year within the same country
  df <- df %>%
    group_by(year, country) %>%                                           # Group by year and country
    fill(., .direction = "downup") %>%                                  # Fill NA values using previous or next non-NA value within the group
    mutate(                                                          # Create new columns with imputed values
      mode_country_rdt_imputed = ifelse(is.na(mode_country_rdt), lag(mode_country_rdt), mode_country_rdt),
      mini_rdt_price_country_imputed = ifelse(is.na(mini_rdt_price_country), lag(mini_rdt_price_country), mini_rdt_price_country),
      maxi_rdt_price_country_imputed = ifelse(is.na(maxi_rdt_price_country), lag(maxi_rdt_price_country), maxi_rdt_price_country),
      mode_country_t_imputed = ifelse(is.na(mode_country_t), lag(mode_country_t), mode_country_t),
      mini_t_price_country_imputed = ifelse(is.na(mini_t_price_country), lag(mini_t_price_country), mini_t_price_country),
      maxi_t_price_country_imputed = ifelse(is.na(maxi_t_price_country), lag(maxi_t_price_country), maxi_t_price_country)
    ) %>%
    ungroup()                                                           # Remove the grouping
  
  # The imputation is done 3 times to propagate values across multiple consecutive missing values
  
  print(class(t))
  print(t)
  return(df)
  # This creates a file path based on the treatment name 't' and saves the dataframe to that location.
}

