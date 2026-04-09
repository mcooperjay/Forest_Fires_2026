library(ggplot2)
library(tidyverse)
library(refund)

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

ggplot(fires_month,
       aes(x = month, y = total_size, color = factor(year), group = year)) +
  geom_line() +
  labs(title = "Total area burned per month",
       x = "Month", y = "Total FIRE_SIZE", color = "Year") +
  theme_minimal()
  

# Monthly fire counts by year
fires_counts <- fires |>
  mutate(
    year  = FIRE_YEAR,
    month = month(DISCOVERY_DATE, label = TRUE)
  ) |>
  count(year, month, name = "n_fires")

ggplot(fires_counts,
       aes(x = month, y = n_fires, color = factor(year), group = year)) +
  geom_line() +
  labs(title = "Fire counts per month",
       x = "Month", y = "Number of fires", color = "Year") +
  theme_minimal()

# Spatial distribution
fires2 <- fires |>
  mutate(
    FIRE_SIZE_CLASS = factor(
      FIRE_SIZE_CLASS,
      levels = c("A","B","C","D","E","F","G"), ordered = TRUE
    ),
    size_num = as.numeric(FIRE_SIZE_CLASS)   # A=1, ..., G=7
  )

ggplot(fires2,
       aes(x = LONGITUDE, y = LATITUDE, color = size_num)) +
  geom_point(alpha = 0.6, size = 1) +
  coord_quickmap() +
  scale_color_gradient(
    low  = "white",
    high = "red",
    name = "Size class",
    breaks = 1:7,
    labels = c("A","B","C","D","E","F","G")
  ) +
  labs(title = "Fire locations by size class",
       x = "Longitude", y = "Latitude")


# Cause exploration
library(forcats)

ggplot(fires,
       aes(x = fct_rev(fct_infreq(NWCG_GENERAL_CAUSE)))) +
  geom_bar() +
  coord_flip() +
  labs(title = "Fire counts by general cause",
       x = "NWCG general cause", y = "Count")

# Analysis ----------------------------------------------------------------
# Is there some relationship between fire counts curve, and total area burned and how do I model or capture that
# - FPCA combined with a linear and non-linear regression model
# - Functional historical linear regression model
# - Fit fpca on the two graphs
# - Two years variables, multivariate response regression


# Practice Using pffr -----------------------------------------------------

## Key Notes
# where z is a constant
# ff -> "curve affects curve"
# s(z) -> "normal variable but flexible", so it might not have a straight line effect and could be curved
# c(z) -> "constant over time"
# z -> time-varying effect

# We are doing univariate where Y is an n x T matrix

# univariate model:
# Y(t) = f(t)  + \int X1(s)\beta(s,t)ds + eps
set.seed(2121)
data1 <- pffrSim(scenario="ff", n=40)
t <- attr(data1, "yindex")
s <- attr(data1, "xindex")
m1 <- pffr(Y ~ ff(X1, xind=s), yind=t, data=data1) 
summary(m1)
plot(m1, pages=1)

# Practice using our data
# Making the dataset for our Y matrix
Y_dat <- fires_month |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from = total_size
  )

# Then I want to make this a matrix
Y_mat <- as.matrix(Y_dat)[,-1]

# Then do the same for the X matrix

# Create the dataset
X_dat <- fires_counts |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from =n_fires
  )

# Then I want to make this a matrix
X_mat <- as.matrix(X_dat)[,-1]

t <- 1:ncol(Y_mat)  # response index (months)
s <- 1:ncol(X_mat)  # predictor index (months)

# for total area burned take the log
Y_log <- log(Y_mat+1)

# Get our model
model <- pffr(
  Y_log ~ ff(X_mat, 
             xind = s,
             limits = function(s, t) s < t),
  yind = t
)
summary(model)
# There is a statistically significant influence, fire counts do affect fire size patterns
# The effect is structured over time (not constant)

# Mid-season accumulation
  # Fire counts early in season can influence later burned area due potentially to:
  # fuel buidup patterns
  # weather persistence (dry spells)
  # the effect weakens when months are far apart from each other

plot(model, pages = 2)
plot(model, scheme = 2)
gratia::fvisgam(model)

gratia::fvisgam(m1, se = FALSE) +
  scale_fill_viridis_c(
    name = "Influence β(s,t)",
    option = "C"
  ) +
  theme_minimal()

coef(m1)
predict(m1)
