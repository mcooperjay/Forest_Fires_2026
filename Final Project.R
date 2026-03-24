library(ggplot2)
library(tidyverse)
library(refund)
library(survival)

fires <- vroom::vroom("forestfires.csv")

# Variables of interest (Functional)
# FIRE_SIZE
# DISCOVERY_DATE (we want in months)
# X (LATITUDE)
# Y (LONGITUDE)
# CONT_DATE (when the fire was contained)

# Variables of interest (Non-Functional)
# STATE, COUNTY
# NWCG_CAUSE_CLASSIFICATION, NWCG_GENERAL_CAUSE (cause of fire)
# FIRE_YEAR (time grouping)
# FIRE_SIZE_CLASS

# Exploratory Analysis ----------------------------------------------------

# Monthly mean fire size by year
fires_month <- fires |>
  mutate(
    year = FIRE_YEAR,
    month = month(DISCOVERY_DATE, label = TRUE)
  ) |>
  group_by(year, month) |>
  summarise(total_size = mean(FIRE_SIZE, na.rm = TRUE), .groups = "drop")

ggplot(
  fires_month,
  aes(x = month, y = total_size, color = factor(year), group = year)
) +
  geom_line() +
  labs(
    title = "Total area burned per month",
    x = "Month",
    y = "Total FIRE_SIZE",
    color = "Year"
  ) +
  theme_minimal()


# Monthly fire counts by year
fires_counts <- fires |>
  mutate(
    year = FIRE_YEAR,
    month = month(DISCOVERY_DATE, label = TRUE)
  ) |>
  count(year, month, name = "n_fires")

ggplot(
  fires_counts,
  aes(x = month, y = n_fires, color = factor(year), group = year)
) +
  geom_line() +
  labs(
    title = "Fire counts per month",
    x = "Month",
    y = "Number of fires",
    color = "Year"
  ) +
  theme_minimal()

# Spatial distribution
fires2 <- fires |>
  mutate(
    FIRE_SIZE_CLASS = factor(
      FIRE_SIZE_CLASS,
      levels = c("A", "B", "C", "D", "E", "F", "G"),
      ordered = TRUE
    ),
    size_num = as.numeric(FIRE_SIZE_CLASS) # A=1, ..., G=7
  )

ggplot(fires2, aes(x = LONGITUDE, y = LATITUDE, color = size_num)) +
  geom_point(alpha = 0.6, size = 1) +
  coord_quickmap() +
  scale_color_gradient(
    low = "white",
    high = "red",
    name = "Size class",
    breaks = 1:7,
    labels = c("A", "B", "C", "D", "E", "F", "G")
  ) +
  labs(title = "Fire locations by size class", x = "Longitude", y = "Latitude")


# Cause exploration
library(forcats)

ggplot(fires, aes(x = fct_rev(fct_infreq(NWCG_GENERAL_CAUSE)))) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Fire counts by general cause",
    x = "NWCG general cause",
    y = "Count"
  )

# Analysis ----------------------------------------------------------------
# Is there some relationship between fire counts curve, and total area burned and how do I model or capture that
# - FPCA combined with a linear and non-linear regression model
# - Functional historical linear regression model
# - Fit fpca on the two graphs
# - Two years variables, multivariate response regression
