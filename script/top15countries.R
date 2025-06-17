library(tidyr)
library(openxlsx)

wmr <- readxl::read_xlsx("~/Documents/mmv_ror/raw sources/wmr2024_annex_4f.xlsx")

wmr_filled <- wmr %>%
  fill(country, .direction = "down")

wmr_filled$country <- gsub("[0-9,]", "", wmr_filled$country)

wmr_filled$country <- ifelse(wmr_filled$country == "Congo Dem. Rep.", "Congo, Dem. Rep.", wmr_filled$country)
wmr_filled$country <- ifelse(wmr_filled$country == "Korea Republic of", "Korea, Republic of", wmr_filled$country)

wmr_filtered <- wmr_filled %>%
  filter(!is.na(year))

top_countries <- wmr_filtered %>%
  group_by(year) %>%                          # Group by year
  arrange(desc(cases_point)) %>%              # Arrange scores in descending order
  slice_max(cases_point, n = 15) %>%          # Select the top 15 rows by estimated cases
  mutate(distribution = cases_point / sum(cases_point)) %>% # Calculate distribution
  ungroup()   

result <- wmr_filtered %>%
  group_by(year) %>%
  mutate(total_cases_year = sum(cases_point)) %>% # Total cases_point by year
  arrange(desc(cases_point)) %>%                 # Arrange in descending order
  slice_max(cases_point, n = 15) %>%             # Select the top 15 entries
  summarise(
    total_top_15 = sum(cases_point),             # Total cases_point for top 15
    total_cases_year = first(total_cases_year),  # Retrieve total cases_point
    proportion_top_15 = total_top_15 / total_cases_year # Proportion of top 15
  ) %>%
  ungroup()

min(result$proportion_top_15)
max(result$proportion_top_15)

#African countries
countries <- readxl::read_xlsx("~/Documents/mmv_ror/data/universal/countries_regions.xlsx")

wmr_with_region <- merge(wmr_filtered, countries, by.x = "country", by.y = "name", all.x = TRUE)

wmr_Africa <- wmr_with_region %>%
  filter(region == "Africa")

top_countries_Africa <- wmr_Africa %>%
  group_by(year) %>%                          # Group by year
  arrange(desc(cases_point)) %>%              # Arrange scores in descending order
  slice_max(cases_point, n = 15) %>%          # Select the top 15 rows by estimated cases
  mutate(distribution = cases_point / sum(cases_point)) %>% # Calculate distribution
  ungroup()  

wmr_nonAfrica <- wmr_with_region %>%
  filter(region != "Africa")

top_countries_nonAfrica <- wmr_nonAfrica %>%
  group_by(year) %>%                          # Group by year
  arrange(desc(cases_point)) %>%              # Arrange scores in descending order
  slice_max(cases_point, n = 15) %>%          # Select the top 15 rows by estimated cases
  mutate(distribution = cases_point / sum(cases_point)) %>% # Calculate distribution
  ungroup()

top_countries <- top_countries[,c("year", "country", "cases_point", "distribution")]
names(top_countries) <- c("Year",	"Country",	"Cases estimated",	"Distribution")
write.xlsx(top_countries, file = "./data/universal/global_malaria_burden.xlsx")

top_countries_Africa <- top_countries_Africa[,c("year", "country", "cases_point", "distribution")]
names(top_countries_Africa) <- c("Year",	"Country",	"Cases estimated",	"Distribution")
write.xlsx(top_countries_Africa, file = "./data/universal/global_malaria_burden_Africa.xlsx")

top_countries_nonAfrica <- top_countries_nonAfrica[,c("year", "country", "cases_point", "distribution")]
names(top_countries_nonAfrica) <- c("Year",	"Country",	"Cases estimated",	"Distribution")
write.xlsx(top_countries_nonAfrica, file = "./data/universal/global_malaria_burden_nonAfrica.xlsx")

### GDP per capita data
gdp <- read.csv("data/gdp_per_capita_original_reduced.csv")

library(dplyr)
library(tidyr)
gdp <- gdp %>%
  pivot_longer(
    cols = starts_with("X"), # Specify columns to pivot
    names_to = "year",        # Name of the new column for variable names
    values_to = "gdp_in2023"          # Name of the new column for values
  )

gdp$year <- as.numeric(gsub("[A-Za-z]", "", gdp$year))
names(gdp)[1] <- c("country")

income_group <- readxl::read_xlsx("data/universal/income_classification.xlsx")

income_group <- income_group %>%
  pivot_longer(
    cols = c(2:38), # Specify columns to pivot
    names_to = "year",        # Name of the new column for variable names
    values_to = "income_group"          # Name of the new column for values
  )
names(income_group)[1] <- c("country")

income_group$year <- as.numeric(income_group$year)

wb_data <- merge(gdp, income_group, by = c("country", "year"), all = T)

wb_data <- wb_data %>%
  group_by(country) %>%
  fill(gdp_in2023, .direction = "up")

wb_data$income_group <- ifelse(wb_data$income_group == "..", NA, wb_data$income_group)
wb_data <- wb_data %>%
  group_by(country) %>%
  fill(income_group, .direction = "up")
