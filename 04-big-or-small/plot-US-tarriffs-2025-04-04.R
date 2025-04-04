# Plot US tarriffs

# Set-up -----------------------------------------------------------------------
library(tidyverse)
library(here)
library(scales)
library(ggdark)
library(ggrepel)

trump <- read_csv(here("04-big-or-small/trump_tarrifs.csv")) |>
  rename(Country = country, Tariff = tariff) |>
  mutate(Country = case_when(str_detect(Country,"McD") ~ "Heard and McDonald Islands", TRUE ~ Country))

guess <- read_csv(here("04-big-or-small/us_guessed_tarriff.csv"))

#trump_countries <- trump |> select(Country)
#guess_countries <- guess |> select(Country)

missing_countries <- trump |> filter(!(Country %in% guess$Country))
# Create a mapping dataframe
country_mapping <- tribble(
  ~name, ~Country,
  "British Indian Ocean Territory", "British Indian Ocean Terr.",
  "Côte d'Ivoire", "Cote d'Ivoire",
  "Curaçao", "Curacao",
  "Democratic Republic of the Congo", "Congo (Kinshasa)",
  "Falkland Islands", "Falkland Islands(Islas Malvin",
  "Monteserrat", "Montserrat",
  "Myanmar (Burma)", "Burma",
  "North Macedonia", "Macedonia",
  "Republic of the Congo", "Congo (Brazzaville)",
  "Réunion", "Reunion",
  "Saint Helena", "St Helena",
  "Saint Kitts and Nevis", "St Kitts and Nevis",
  "Saint Lucia", "St Lucia",
  "Saint Pierre and Miequelon", "St Pierre and Miquelon",
  "Saint Vincent and the Grenadines", "St Vincent and the Grenadines",
  "São Tomé and Príncipe", "Sao Tome and Principe",
  "South Korea", "Korea, South",
  "Svalbard and Jan Mayen", "Svalbard, Jan Mayen Island",
  "Timor-Leste", "East Timor",
  "Tuyalu", "Tuvalu",
  "Yemen", "Republic of Yemen",
)

guess_rename <- guess |> left_join(country_mapping) |>
 mutate(Country = case_when(
  !is.na(name) ~ name, TRUE ~ Country)) |>
  select(-name) |>
  mutate(Tarriff_Est = case_when(is.infinite(Tarriff_Est) ~ 0,
is.na(Tarriff_Est) ~ 0, TRUE ~ Tarriff_Est))

eu <- guess |> filter(Country %in% c("Spain","France","Denmark","Czech Republic","Germany", "Italy",
"Romania", "Hungary", "Austria","Ireland", "Slovenia")) |>
  mutate(Tariff = 0.2)

df_join <- guess_rename |>
  left_join(trump) |>
  mutate(Tariff = Tariff/100) |> # Re-scale
  drop_na() |>
  bind_rows(eu) |>
  mutate(type = case_when(Country %in% c(
  "Spain", "France", "Denmark", "Czech Republic", "Germany", "Italy",
  "Romania", "Hungary", "Austria", "Ireland", "Slovenia"
) ~ "EU country", TRUE ~ "Everyone Else"))
#trump |> filter(!(Country %in% df_join$Country))

# Plot -------------------------------------------------------------------------

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
clrs <- MetBrewer::met.brewer("Archambault")


# X: Guessed Tarriff
# Y: New Tarriff
# Size: Imports
# Colour: Guessed Tarriff | Tarriff == 20%
# Title: Galaxy brain trade policy
# Subtitle: How the Administration used this one weird trick to solve all the country's problems

df_join |>
  filter(Tarriff_Est < 1, Country != "European Union") |> # Remove estimates > 100%
  ggplot(aes(x = Tarriff_Est, y = Tariff, size = Imports, colour = type)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(
    data = df_join |> filter(Country == "China"),
    aes(label = "China: $439 billon"),
    color = "white",
    size = 4,
    fontface = "bold",
    point.padding = 0.3,
    nudge_x = -.12,
    box.padding = 0.5,
    nudge_y = .02,
    segment.curvature = -0.1,
    force = 1,
    segment.color = "white",
    segment.size = 0.5,
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  scale_x_continuous(limits = c(0,1),labels = label_percent()) +
  scale_y_continuous(limits = c(0.1,0.5), labels = label_percent()) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_manual(name = "type",
                     values = c("#C1D100","#74C9E5")) +
  # Hide imports legend, show only type legend
  guides(# Hide the size legend
    size = "none",
    # Make the color legend circles bigger
    color = guide_legend(override.aes = list(size = 5))) +
  annotate("text",
    x = 0.15, y = 0.45,
    label = "Circle represents size\nof imports ($) to USA.", hjust = 0, size = 4
  ) +
  annotate("text",
    x = 0.4, y = 0.12,
    label = "Lower tariff cap of 10% for lots of countries, including the UK", hjust = 0, size = 4
  ) +
  annotate("text",
    x = 0.6, y = 0.4,
    label = expression(paste("Claimed formula: ", Delta*tau[i] == frac(x[i] - m[i], epsilon * phi * m[i]))), hjust = 0, size = 4
  ) +
  annotate("text",
    x = 0.6, y = 0.31,
    label = expression(paste("Looks more like: ", frac(frac(Balance, Imports), 2))), hjust = 0, size = 4
  ) +
  labs(
  title = "Galaxy brain trade policy",
  subtitle = "How the Administration used this one weird trick to solve all the country's problems. ",
  x = "Guessed Tariff", y = "Tariff",
  caption = "FT Alphaville: https://on.ft.com/4cddkuC, Data: census.gov, Made by: Alistair Bailey, Code: https://github.com/ab604/charts-2025"
) +
  theme_int() +
  theme(legend.position = "bottom", legend.title = element_blank(),
plot.caption <- element_text(
  hjust = 0, # Align the caption to the left (0 = left, 1 = right)
  color = "white",
  size = 8,
  margin = margin(t = 15) # Adds some space between plot and caption
)
)

ggsave(here("04-big-or-small/us-tarriffs-2025-04-04.png"), width = 10, height = 6)
