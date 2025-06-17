
library(tidyr)

# Read the CSV file (replace 'your_file.csv' with your file's path)
data <- read.csv("data/universal/gni_per_capita.csv")

data <- data %>%
  mutate(across(everything(), ~ fill(., .direction = "down")))

data2 <- data %>%                                           # Group by year and country
  fill(., .direction = "updown")

test <- data %>% tidyr::fill(.)
names(data)[1] <- c("year")

# Transform the data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(2:266), # Adjust to match your column names, e.g., "Year2000", "Year2001"
    names_to = "country",          # New column to hold year names
    values_to = "value"         # New column to hold values
  )

data_long <- arrange(data_long,desc(country))

write.csv(data_long,"data/universal/gni_per_capita_long.csv", row.names = FALSE)

data_long <- read.csv("data/universal/gni_per_capita_long.csv")
