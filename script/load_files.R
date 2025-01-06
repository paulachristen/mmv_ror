
load_and_preprocess_data <- function(p){

non_spec_check <- data.frame()

##########load universal files

#list all xlsx files
xlsx_files <- list.files(path = "./data/universal", pattern = "\\.xlsx$", full.names = TRUE)

#read in each xlsx file
for (i in xlsx_files) {
  filename <- basename(i)        # Get filename with extension (e.g., "data.xlsx")
  filename <- tools::file_path_sans_ext(filename)
  assign(filename, read_excel(i))     # Create variable dynamically
}

#index for which files subsequent code should be run
path <- paths[p]

#list all xlsx files
xlsx_files <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE)

#read in each xlsx file
for (i in xlsx_files) {
  filename <- basename(i)        # Get filename with extension (e.g., "data.xlsx")
  filename <- tools::file_path_sans_ext(filename)
  assign(filename, read_excel(i))     # Create variable dynamically
}

#create list of dataframes
data_frames <- Filter(function(x) is(x, "data.frame"), mget(ls()))

#clean names in dataframes
result <- lapply(data_frames, clean_names)

#for each dataframe assign the name of the file
for (i in seq_along(data_frames)) {
  df_name <- names(data_frames)[i]  # Get the original data frame name
  assign(df_name, result[[i]])       # Assign the cleaned version
}

## Treatment Costs ##

# Merge treatment costs with inflation rates (keep all treatment cost entries)
treatment_costs <- merge(treatment_costs, inflation_rates, by = "year", all.x = TRUE)

# Merge treatment costs with country names and their regions (keep all treatment cost entries)
treatment_costs <- merge(treatment_costs, countries_regions, by.x = "country_teritorry", by.y = "name", all.x = TRUE)

# Create flags to identify if data entry is at the region or sub-region level
treatment_costs$region_check    <- ifelse(treatment_costs$region == treatment_costs$country_teritorry, 1, 0)
treatment_costs$sub_region_check <- ifelse(treatment_costs$sub_region == treatment_costs$country_teritorry, 1, 0)

# Set 'country_teritorry' to NA if the entry is for a region or sub-region
treatment_costs$country_teritorry <- ifelse(treatment_costs$region_check == 1 | treatment_costs$sub_region_check == 1,
                                            NA, treatment_costs$country_teritorry)

# Calculate inflation-adjusted unit cost in 2022 USD
treatment_costs$unit_cost_usd_2022_price_inflated <- treatment_costs$unit_cost_usd_2022_price * (treatment_costs$inflation_rate)

# Aggregate: Count treatment cost observations by country/territory, category, and year
count_data_treatment_costs <- treatment_costs %>%
  dplyr::group_by(country_teritorry, ref_category, year) %>%
  dplyr::summarize(n_t_cost = n())

# Merge the count data back into the main treatment cost dataset
treatment_costs <- merge(treatment_costs, count_data_treatment_costs,
                         by = c("country_teritorry", "year", "ref_category"), all.x = TRUE)

## RDT Costs ##

# Merge RDT costs with inflation rates (keep all RDT cost entries)
procurement_rdt <- merge(procurement_rdt, inflation_rates, by = "year", all.x = TRUE)

# Calculate inflation-adjusted RDT unit cost in 2022 USD
procurement_rdt$rdt_price_usd_2022_price_inflated <- procurement_rdt$unit_cost_usd_in_2022_prices * (procurement_rdt$inflation_rate)

# Aggregate: Count RDT observations by country and year
count_data_procurement_rdt <- procurement_rdt %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarize(n_rdt = n())

# Merge the count data back into the main RDT cost dataset
procurement_rdt <- merge(procurement_rdt, count_data_procurement_rdt,
                         by = c("country", "year"), all.x = TRUE)

# Merge RDT costs with country names and their regions (keep all RDT cost entries)
procurement_rdt <- merge(procurement_rdt, countries_regions,
                         by.x = "country", by.y = "name", all.x = TRUE)


## Malaria Burden Data Preparation ##

# Merge global malaria burden data with country names and regions, and mark as "global_burden"
global_malaria_burden <- merge(global_malaria_burden, countries_regions, by.x = "country", by.y = "name", all.x = TRUE)
global_malaria_burden$cover <- "global_burden"

# Merge Africa-specific malaria burden data with country names and regions, and mark as "Africa"
global_malaria_burden_Africa <- merge(global_malaria_burden_Africa, countries_regions, by.x = "country", by.y = "name", all.x = TRUE)
global_malaria_burden_Africa$cover <- "Africa"

# Merge non-Africa malaria burden data with country names and regions, and mark as "nonAfrica"
global_malaria_burden_nonAfrica <- merge(global_malaria_burden_nonAfrica, countries_regions, by.x = "country", by.y = "name", all.x = TRUE)
global_malaria_burden_nonAfrica$cover <- "nonAfrica"

# Combine all three malaria burden datasets into a single dataframe
malaria_burden <<- rbind(global_malaria_burden, global_malaria_burden_Africa, global_malaria_burden_nonAfrica)

################
#Monetized DALYs
################

ochalek_dalys_priced_long <- dalys_priced

ochalek_dalys_priced_long$value <- ifelse(ochalek_dalys_priced_long$value == "NA", NA, ochalek_dalys_priced_long$value)

ochalek_dalys_priced_long$value <- as.numeric(as.character(ochalek_dalys_priced_long$value))

ochalek_dalys_priced <- ochalek_dalys_priced_long

names(ochalek_dalys_priced)[4] <- "daly" 
  
# Merge region information into a temporary copy of 'ochalek_dalys_priced'
ochalek_dalys_priced_temp <- merge(ochalek_dalys_priced, countries_regions,
                                   by.x = "country", by.y = "name", all.x = TRUE)

# Calculate mean DALY values for each region, excluding missing data
ochalek_dalys_priced_missing_placeholders_region <- ochalek_dalys_priced_temp %>%
  dplyr::group_by(region, year) %>%                                            # Group the data by 'region'
  dplyr::summarise(daly_region = mean(daly, na.rm = T))              # Calculate mean of 'daly_4' per region (removing NAs)


# Calculate mean DALY values for each sub-region, excluding missing data
ochalek_dalys_priced_missing_placeholders <- ochalek_dalys_priced_temp %>%
  dplyr::group_by(sub_region, year) %>%                                            # Group the data by 'region'
  dplyr::summarise(daly_sub_region = mean(daly, na.rm = T))              # Calculate mean of 'daly_4' per region (removing NAs)


# Merge calculated mean DALY values for sub-regions back into the main dataset
ochalek_dalys_priced_temp <- merge(ochalek_dalys_priced_temp,
                                   ochalek_dalys_priced_missing_placeholders[,c("sub_region","year", "daly_sub_region")],
                                   by = c("sub_region","year"))  # Merge based on the 'sub_region' column


# Impute missing DALY values with sub-region means if the value is NA or "NaN" (Not a Number)
ochalek_dalys_priced_temp$daly <- ifelse(is.na(ochalek_dalys_priced_temp$daly) |
                                             ochalek_dalys_priced_temp$daly == "NaN",
                                           ochalek_dalys_priced_temp$daly_sub_region,
                                           ochalek_dalys_priced_temp$daly)  # Impute missing 'daly_1' values

# Update the original 'ochalek_dalys_priced' dataset with the imputed DALY values from the temporary dataset
ochalek_dalys_priced <- ochalek_dalys_priced[,-c(4)]
ochalek_dalys_priced <- merge(ochalek_dalys_priced, ochalek_dalys_priced_temp[,c(2:5)], 
                              by = c("country", "method", "year"), 
                              all.x = TRUE)

################
#WHO CHOICE PRICES (original 2010 prices)
################
# Filter and extract the 2010 inflation rate from the 'inflation_rates' dataset
inflation_rate_2010 <- inflation_rates %>% filter(year == "2010")
inflation_rate_2010 <- inflation_rate_2010$inflation_rate

# Convert columns 3 through 7 in 'who_choice_prices' to numeric format
who_choice_prices <- who_choice_prices %>%
  mutate_at(vars(3:7), as.numeric)

# Merge region information from 'countries_regions' into 'who_choice_prices' based on country/region names
who_choice_prices <- merge(who_choice_prices, countries_regions, by.x = "region_country", by.y = "name", all.x = TRUE)

# Adjust financial values in 'who_choice_prices' for inflation and discounting
who_choice_prices$model_prediction <- as.numeric(who_choice_prices$model_prediction) * (1.03^(2022 - 2010)) * (inflation_rate_2010)  # Adjust model predictions
who_choice_prices$mean_value_from_sample <- as.numeric(who_choice_prices$mean_value_from_sample) * (1.03^(2022 - 2010)) * (inflation_rate_2010)  # Adjust mean values
who_choice_prices$high_95_unc_interval <- as.numeric(who_choice_prices$high_95_unc_interval) * (1.03^(2022 - 2010)) * (inflation_rate_2010)   # Adjust upper 95% uncertainty interval
who_choice_prices$low_95_unc_interval <- as.numeric(who_choice_prices$low_95_unc_interval) * (1.03^(2022 - 2010)) * (inflation_rate_2010)   # Adjust lower 95% uncertainty interval
who_choice_prices$sd <- as.numeric(who_choice_prices$sd) * (1.03^(2022 - 2010)) * (inflation_rate_2010)                                      # Adjust standard deviation

# Get a list of unique treatment codes
treatment_codes <- unique(mmv_calculations_dalys$treatment)

# In your calling function
mmv_copy <- data.table::copy(mmv_calculations_dalys)
treat_costs_copy <- data.table::copy(treatment_costs)
ochalek_dalys_priced_copy <- ochalek_dalys_priced
countries_regions_copy <- countries_regions
ochalek_dalys_priced_missing_placeholders_copy <- ochalek_dalys_priced_missing_placeholders
ochalek_dalys_priced_missing_placeholders_region_copy <- ochalek_dalys_priced_missing_placeholders_region
procurement_rdt_copy <- procurement_rdt
who_choice_prices_copy <- who_choice_prices

# Run analysis for each treatment code
for (code in treatment_codes) {         # Iterate over each unique treatment code
  dataframe <- model_per_treatment(code, 
                      mmv_copy, 
                      treat_costs_copy, 
                      ochalek_dalys_priced_copy, 
                      countries_regions_copy, 
                      ochalek_dalys_priced_missing_placeholders_copy, 
                      ochalek_dalys_priced_missing_placeholders_region_copy, 
                      procurement_rdt_copy, 
                      who_choice_prices_copy) 
  # Save the final dataframe as a CSV file
  write.csv(dataframe, paste0(path,
                       "/by_treatment_dataset/",
                       code,
                       ".csv"), row.names = FALSE)
}

return(list(mmv_calculations_dalys, 
            treatment_costs, 
            ochalek_dalys_priced, 
            countries_regions, 
            ochalek_dalys_priced_missing_placeholders, 
            ochalek_dalys_priced_missing_placeholders_region, 
            procurement_rdt,
            who_choice_prices, 
            mmv_investments, 
            non_spec_check))

}
