# Plot HPV vaccination
library(tidyverse)
library(ggdark)
library(here)

# Use the Archambault color palette
clrs <- MetBrewer::met.brewer("Archambault")

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

# Screening started in 1989
# Model covers 30 years, oldest born in 1989 followed to 2019

# X-axis: Oldest to Youngest
# Y-axis: Rate per 100,000
# Data from Table S6 Model 3
df <- tibble(age = c(20,24.5, 26, 30,
                      20,24.5, 26, 30,
                    20,24.5, 26, NA,
                  20,24.5, NA, NA),
year = c(rep(1989,4),
                    rep(1990,4),
                    rep(1993,4),
                    rep(1999,4)),
cohort = c(rep(4,4),
rep(5, 4),rep(6, 4),rep(7, 4)),
rate = c(0,9.6,50.1,118.7,
        0,6.3,33.2,78.7,
        0,3.6,18.9,NA,
        0,1.2,NA,NA),
lower =c(0,8.2,45.1,108.4,
          0,5.5,30.1,71.5,
        0,2.7,14.6,NA,
      0,0.3,NA,NA),
upper = c(0,10.9,55.2,129,
         0,7.2,36.4,85.8,
        0,4.5,23.2,NA,
      0,2.2,NA,NA)
)


# Separate dataset for the labels
manual_labels <- tibble(
  cohort = c(4, 5, 6, 7),
  label = c("1989-90 cohort", "1990-93 cohort", "1993-95 cohort", "1995-99 cohort"),
  x_pos = c(30.1, 30.1, 26.1, 24.6), # Manually positioned x coordinates
  y_pos = c(118.7, 78.7, 18.9, 0.2) # Estimated y values - adjust based on your data
)

# Plot
df |>
  ggplot(aes(
    x = age, y = rate, group = cohort,
    colour = as_factor(cohort)
  )) +
  # Add confidence interval ribbons
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as_factor(cohort)),
    alpha = 0.2, colour = NA
  ) +
  # Add lines and points for the main data
  geom_line() +
  geom_point() +
  # Add text annotations for cohort labels using coord_cartesian instead of scale limits
  geom_text(
  data = manual_labels,
  aes(
    x = x_pos, y = y_pos,
    label = label,
    color = as_factor(cohort)
  ),
  hjust = 0,
  fontface = "bold",
  show.legend = FALSE
) +
  # Annotation 1
  annotate("text",
    x = 20.1, y = max(df$rate, na.rm = TRUE) * 1,
    label = "HPV vaccination began for girls\naged 12-13 in the UK in 2008\nand then for boys in 2019", hjust = 0, size = 4
  ) +
  # Annotation 2
  annotate("text",
    x = 20.1, y = 75,
    label = "The average age of screening began was 24.5\n1989 cohort not offered vaccine\n1990 cohort offered vaccine at age 16-18\n1993 cohort offered vaccine at age 14-16\n1995 cohort offered vaccine at 12-14", hjust = 0, size = 4
  ) +
  labs(
    title = "Cervical cancer rates declined in England following national HPV vaccination programmes",
    subtitle = "Cumulative cases of invasive cervical cancer per 100,000 women",
    x = "Age", y = "",
    caption = "Source data: https://doi.org/10.1016/S0140-6736(21)02178-4, Made by: Alistair Bailey, Code: https://github.com/ab604/charts-2025"
  ) +
  coord_cartesian(xlim = c(20, 32)) +
  scale_x_continuous(
    breaks = c(20, 24.5, 26, 30),
    labels = c("20", "24.5", "26", "30")
  ) +
  theme_int() +
  scale_fill_manual(values = clrs[c(1, 4, 5, 6)]
) +
  scale_color_manual(values = clrs[c(1, 4, 5, 6)]
) +
  theme(legend.position = "",plot.caption = element_text(hjust = 0))

ggsave(here("02-slopes/hpv-cancer-2025-04-02.png"), width = 13, height = 10)
