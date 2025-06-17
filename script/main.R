
# main.R

options(scipen=999)

# Load libraries
library(writexl)
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(janitor)
library(naniar)
library(data.table)
library(fs)
library(jrvFinance)
library(freedom)

# Set file paths and global variables
#file paths - each folder has equivalent files with the difference of estimated averted DALYs and doses
paths <- c("./data/files for R Relative Coartem disp only")

# Source other R files
source("./script/functions.R")
source("./script/load_files.R")
source("./script/irr_analysis.R")
source("./script/aggregation.R")

# Set number of iterations
N <- 1000

# Create an empty dataframe to store final results
check_df <- data.frame() 
p <- 1 #Relative Coartem disp only

  # Load and pre-process data for the current path
df_for_irr <- load_and_preprocess_data(p) 

# Perform IRR analysis for the current path
perform_irr_analysis(p, 
                       mmv_calculations_dalys = df_for_irr[[1]], 
                       treatment_costs = df_for_irr[[2]], 
                       ochalek_dalys_priced = df_for_irr[[3]], 
                       countries_regions = df_for_irr[[4]], 
                       ochalek_dalys_priced_missing_placeholders = df_for_irr[[5]], 
                       ochalek_dalys_priced_missing_placeholders_region = df_for_irr[[6]], 
                       procurement_rdt = df_for_irr[[7]],
                       who_choice_prices = df_for_irr[[8]], 
                       mmv_investments = df_for_irr[[9]], 
                       non_spec_check = df_for_irr[[10]]) 


# Aggregate and save the final results
aggregate_and_save_results(check_df) 
