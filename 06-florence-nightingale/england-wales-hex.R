# Load required packages
library(tidyverse)
library(sf)
library(viridis)
library(classInt)
library(here)
library(readODS)
library(janitor)
library(ggdark)

# Set-up theme------------------------------------------------------------------
theme_int <- function(base_size = 14) { # Default base size of 14
  dark_theme_minimal(base_family = "Inter", base_size = base_size) +
    theme(
      #panel.grid.minor = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "black", color = NA),
      plot.title = element_text(face = "bold", size = base_size + 4), # 18pt
      #axis.title = element_text(face = "bold", size = base_size + 2), # 16pt
      #axis.text = element_text(size = base_size), # 14pt
      strip.text = element_text(face = "bold", size = base_size), # 14pt
      strip.background = element_rect(fill = "grey10", color = NA),
      legend.title = element_text(face = "bold", size = base_size), # 14pt
      legend.text = element_text(size = base_size - 4) # 12pt
    )
}

# Vaccination data from https://www.gov.uk/government/statistics/cover-of-vaccination-evaluated-rapidly-cover-programme-2024-to-2025-quarterly-data
df <- read_ods(here("06-florence-nightingale/cover-data-tables-quarter-3-2024-to-2025.ods"),
sheet = "Table7", skip = 4, na = c("[x]","[z]")) |> clean_names()

# Selct the columns we need
df.t <- df |> select(1:3,MMR1 = 8)

# Read the local shapefile for UK Upper Tier Local Authorities
uk <- read_sf(here("06-florence-nightingale/UTLA_MCTY_DEC_2022_UK_BGC.shp"))

# Join the vaccination data with the shapefile
# Using UTLA22CD as the joining field from what we can see in your data
uk_with_data <- uk |>
  inner_join(df.t, by = c("UTLA22CD" = "code"))

# Create a hexagonal grid across England
grid_size <- 15000 # 15km hexagons

# Create a bounding box around all the data
# This is safer than filtering by a country name field that might not exist
england_bbox <- st_bbox(uk_with_data)
england_bbox <- st_as_sfc(england_bbox)

hexgrid <- st_make_grid(england_bbox,
  cellsize = c(grid_size, grid_size),
  square = FALSE
) |>
  st_sf() |>
  mutate(id = row_number())

# Intersect hexagons with local authorities to assign vaccination data
hex_with_authorities <- st_join(hexgrid, uk_with_data)

# Get the actual name of the geometry column
geom_col <- attr(hex_with_authorities, "sf_column")

# First, calculate the intersection area
hex_with_areas <- hex_with_authorities |>
  mutate(intersection_area = st_area(.data[[geom_col]]))

# Group by hexagon ID and find the authority with max area for each hexagon
hex_mmr1 <- hex_with_areas |>
  group_by(id) |>
  arrange(desc(intersection_area)) |>
  slice(1) |>
  ungroup()

hex_mmr1_filt <- hex_mmr1 |> filter(!is.na(MMR1)) # Filter NAs to remove background

# Create the MMR1 hexagonal map
hex_mmr1_filt |>
ggplot() +
  # Plot hexagons colored by MMR1 coverage
  geom_sf(aes(fill = MMR1), color = "white", size = 0.1) +
  scale_fill_viridis(
    option = "D",
    direction = -1,
    name = element_blank(),
    na.value = NULL,
    limits = c(75, 100),
    breaks = c(75, 87.5, 100)
  ) +
  # Add title and subtitle
  labs(
    subtitle = "Local Authority 2024 measles (MMR1) vaccination coverage at Age 5",
    title = "One acute measles-related death was reported in 2024 in\na young person who was known to have other medical conditions",
    caption = "Not reported are black, Data: GOV.UK, Made by: Alistair Bailey, Code: https://github.com/ab604/charts-2025"

  ) +
  # Format the plot
  theme_int() +
  theme(legend.position = "bottom", axis.text = element_blank(),
plot.caption = element_text(
  hjust = 0.5, # Align the caption to the left (0 = left, 1 = right)
  color = "white",
  size = 10,
  margin = margin(t = 15) # Adds some space between plot and caption
)
)

ggsave(here("06-florence-nightingale/measles.png"), width = 10, height = 10)
