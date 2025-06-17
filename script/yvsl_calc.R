library(dplyr)
library(tidyr)
library(openxlsx)
library(janitor)
wmr <- readxl::read_xlsx("~/Documents/mmv_ror/raw sources/wmr2024_annex_4f.xlsx")

wmr <- clean_names(wmr)

wmr <- wmr[!is.na(wmr$year), ]

wmr_filled <- wmr %>%
  fill(country, .direction = "down")

wmr_filled$country <- gsub("[0-9,]", "", wmr_filled$country)

###############################################################################
######################GNI per capita, PPP######################################
###############################################################################
gni_per_capita <- read.csv("data/gni_per_capita.csv")

gni_per_capita_long <- gni_per_capita %>%
  pivot_longer(cols = - c("country_name", "code"),  # Columns to pivot (excluding ID)
               names_to = "year",  # New column for month names
               values_to = "gni_per_capita") # New column for values

gni_per_capita_long$year <- as.numeric(gsub("X", "", gni_per_capita_long$year))

# Fill missing values using the previous year's data
gni_per_capita_long <- gni_per_capita_long %>%
  arrange(code, year) %>% # Ensure correct order
  group_by(code) %>% # Group by country
  fill(gni_per_capita, .direction = "down") # Fill missing values with previous year

###############################################################################
###################### merge GNI with WMR #####################################

# Create manual mapping for unmatched country names
# no GNI per capita for French Guiana
country_name_mapping <- c(
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Gambia, The" = "Gambia",
  "Bolivia" = "Bolivia (Plurinational State of)",
  "Cote d'Ivoire" = "Côte d’Ivoire",
  "Turkiye" = "Turkey",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Congo, Rep." = "Congo",
  "Tanzania" = "United Republic of Tanzania",
  "Lao PDR" = "Lao People’s Democratic Republic",
  "Korea, Dem. People's Rep." = "Democratic People’s Republic of Korea",
  "Korea, Rep." = "Republic of Korea",
  "Viet.Nam" = "Viet Nam",
  "Cabo.Verde" = "Cabo Verde",
  "Central.African.Republic" = "Central African Republic",
  "Congo..Rep." = "Congo",
  "Costa.Rica" = "Costa Rica",
  "Dominican.Republic" = "Dominican Republic",
  "Egypt, Arab Rep." = "Egypt",
  "El.Salvador" = "El Salvador",
  "Equatorial.Guinea" = "Equatorial Guinea",
  "Guinea.Bissau" = "Guinea-Bissau",
  "Iran, Islamic Rep." = "Iran (Islamic Republic of)",
  "Kyrgyz.Republic" = "Kyrgyzstan",
  "Lao People's Democratic Republic" = "Lao People’s Democratic Republic",
  "Sao.Tome.and.Principe" = "Sao Tome and Principe",
  "Saudi.Arabia" = "Saudi Arabia",
  "Sierra.Leone" = "Sierra Leone",
  "South.Africa" = "South Africa",
  "South.Sudan" = "South Sudan",
  "Syrian.Arab.Republic" = "Syrian Arab Republic",
  "United.Arab.Emirates" = "United Arab Emirates",
  "Tanzania" = "United Republic of Tanzania",
  "Yemen, Rep." = "Yemen", 
  "Venezuela, RB" = "Venezuela (Bolivarian Republic of)"
)

# Apply name corrections
gni_per_capita_long$country_name <- ifelse(gni_per_capita_long$country_name %in% names(country_name_mapping), 
                      country_name_mapping[gni_per_capita_long$country_name], 
                      gni_per_capita_long$country_name)

# Merge datasets using country codes
df <- wmr_filled %>%
  left_join(gni_per_capita_long, by = c("country" = "country_name", "year"))

df$gni_per_capita[is.na(df$gni_per_capita)] <- mean(df$gni_per_capita, na.rm = TRUE)

global_malaria_burden <- df

distr2023 <- df[df$year == 2023, ]

df <- distr2023 %>%                          # Group by year
  arrange(desc(cases_point)) %>%              # Arrange scores in descending order
  mutate(distribution = cases_point / sum(cases_point)) 

###############################################################################
# We then multiplied this share of DALYs at the country level (DALYs_share) by the GNI per capita, 
# PPP (current international $) figures that we extracted from the World Bank. If the GNI figure was 
# not available for our reference year 2022, we used the next most recent figure available.

df$GNI_DALYs_weighted <- df$gni_per_capita * df$distribution

VSL <- 12300000*(sum(df$GNI_DALYs_weighted)/80450)^1.2

years <- 40
discount_rate <- 0.03
inverse_discount_rate <- 1/(1+discount_rate)
VSLY <- (VSL*(1-inverse_discount_rate))/(1-(inverse_discount_rate^years))*(1+discount_rate)
