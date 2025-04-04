# Guess Tarriffs
# 2025-04-03
# Alistair Bailey

# Set-up
library(tidyverse)
library(readxl)
library(here)

# Read the Excel file
trade_data <- read_excel(here("04-big-or-small/country.xlsx"))

# Filter for 2024 data and select relevant columns
country_totals_2024 <- trade_data |>
  filter(year == "2024") |>
  select(CTYNAME, IYR, EYR) |>
  # Calculate the trade balance
  mutate(Balance = EYR - IYR) |>
  # Rename columns for clarity
  rename(
    Country = CTYNAME,
    Imports = IYR,
    Exports = EYR
  )

# Export to CSV
country_totals_2024 |>
  write_csv(here("04-big-or-small/us_trade_by_country_2024.csv"))

# Create a summary table
trade_summary <- country_totals_2024 |>
  select(Country, Exports, Imports, Balance) |>
  # Round to 1 decimal place
  mutate(across(c(Exports, Imports, Balance), ~round(., 1)),
Tarriff_Est = (abs(Balance)/Imports)/2)

trade_summary |>
  write_csv(here("04-big-or-small/us_guessed_tarriff.csv"))

# Display summary for Bangladesh to check against FT estimate
trade_summary |>
  filter(Country == "Bangladesh")
