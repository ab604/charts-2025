# Plot lung cancer data --------------------------------------------------------
library(tidyverse)
library(MetBrewer) # Andrew Heiss reccomended artsy color palettes
library(scales)
library(ggdark)
library(patchwork)
library(here)

# Set-up theme------------------------------------------------------------------
theme_int <- function(base_size = 14) { # Default base size of 14
  dark_theme_minimal(base_family = "Inter", base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "black", color = NA),
      plot.title = element_text(face = "bold", size = base_size + 4), # 18pt
      axis.title = element_text(face = "bold", size = base_size + 2), # 16pt
      axis.text = element_text(size = base_size), # 14pt
      strip.text = element_text(face = "bold", size = base_size), # 14pt
      strip.background = element_rect(fill = "grey10", color = NA),
      legend.title = element_text(face = "bold", size = base_size), # 14pt
      legend.text = element_text(size = base_size - 2) # 12pt
    )
}

# Use the Archambault color palette
clrs <- met.brewer("Archambault")

# Load data --------------------------------------------------------------------
tbl_s3 <- read_csv(here("01-fractions/data/table_s3_clean.csv"))
tbl_s4 <- read_csv(here("01-fractions/data/table_s4_clean.csv"))

tbl_s3_small <- tbl_s3 |>
  slice(5:12) |>
  drop_na() |>
  rename(Age = Characteristic)

tbl_s3_long <- tbl_s3_small |>
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |>
  group_by(Age) |>
  mutate(proportion = value / sum(value)) |>
  ungroup()


tbl_s4_small <- tbl_s4 |>
  slice(5:12) |>
  drop_na() |>
  rename(Age = Characteristic, Unknown = Missing)

tbl_s4_long <- tbl_s4_small |>
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |>
  group_by(Age) |>
  mutate(proportion = value / sum(value)) |>
  ungroup()


# Plot -------------------------------------------------------------------------
p3 <- tbl_s3_long |>
  ggplot(aes(x = Age, y = proportion, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = met.brewer("Johnson")) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "England accounts for the majority of lung cancer diagnoses across all age groups in the UK",
    subtitle = "45,563 patients diagnosed in the UK from January 2000 to December 2021",
    x = NULL,
    y = NULL
  ) +
  theme_int() +
  theme(legend.position = "bottom", legend.title = element_blank())


p4 <- tbl_s4_long |>
ggplot(aes(x = Age, y = proportion, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values=clrs) +
  # scale_fill_viridis_d(option = "G") +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "In most age groups, people diagnosed with lung cancer are usually smokers",
    subtitle = "45,563 patients diagnosed in the UK from January 2000 to December 2021",
    x = NULL,
    y = NULL,
    caption = "Source data from DOI: 10.21037/tlcr-24-241, Made by: Alistair Bailey, Code: https://github.com/ab604/charts-2025") +
  theme_int() +
  theme(legend.position = "bottom", legend.title = element_blank(),
plot.caption = element_text(hjust = 0))


p3 / p4

ggsave(here("01-fractions/img/uk-lung-cancer-stats-2025-04-01.png"), width = 13, height = 15)
